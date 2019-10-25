#!/bin/bash

# Script for installing Racket for Travis use, originally by Greg Hendershott
# from https://github.com/greghendershott/travis-racket, adapted by Alex
# Harsanyi.

set -e
SCRIPT_NAME=${0##**/}

# These variables can be set in the .travis.yml file, but we provide suitable
# defaults.  NOTE that you can specify Chez versions by appending a -cs to the
# version, e.g. 7.4-cs.
RACKET_DIR=${RACKET_DIR:=~/racket}
RACKET_VERSION=${RACKET_VERSION:=7.1}
RACKET_MINIMAL=${RACKET_MINIMAL:=0}

# Helper variables to construct the download URL.
BASE="https://mirror.racket-lang.org/installers"
HBASE="https://plt.eecs.northwestern.edu/snapshots/current/installers"
PBASE="https://pre-release.racket-lang.org/installers"
CBASE="https://www.cs.utah.edu/plt/snapshots/current/installers" # Racket on Chez Scheme

# Strip off CS suffix from the version
V=`echo $RACKET_VERSION | sed s/-cs$//`
[[ "$RACKET_MINIMAL" = "1" ]] && M="minimal-" || M=""

case "$RACKET_VERSION" in
    HEAD)
        if [[ "$RACKET_MINIMAL" = "1" ]]; then
            URL="${HBASE}/min-racket-current-x86_64-linux-precise.sh"
        else
            URL="${HBASE}/racket-test-current-x86_64-linux-precise.sh"
        fi
        ;;
    HEADCS)
        if [[ "$RACKET_MINIMAL" = "1" ]]; then
            URL="${CBASE}/min-racket-current-x86_64-linux-cs-xenial.sh"
        else
            URL="${CBASE}/racket-current-x86_64-linux-cs-xenial.sh"
        fi
        ;;
    PRERELEASE)
        URL="${PBASE}/racket-${M}current-x86_64-linux.sh"
        ;;
    6.[0-4] | 6.[0-4].[0-9])
        URL="${BASE}/${V}/racket-${M}${V}-x86_64-linux-ubuntu-precise.sh"
        ;;
    7.*-cs)
        # NOTE: 7.4 is the first version which has a Chez variant
        URL="${BASE}/${V}/racket-${M}${V}-x86_64-linux-cs.sh"
        ;;
    6.* | 7.*)
        URL="${BASE}/${V}/racket-${M}${V}-x86_64-linux.sh"
        ;;
    *)
        echo "$SCRIPT_NAME: unsupported Racket version ${RACKET_VERSION}"
        exit 1
esac

DFILE=$(mktemp ${TMP:-/tmp}/$SCRIPT_NAME.XXXXXXXXXX)
trap 'rm --force -- $DFILE' INT TERM HUP EXIT

if curl --head --silent --location $URL 2>&1 | grep --quiet 404.Not.Found ; then
    echo "$SCRIPT_NAME: Installer not available at $URL"
    if [[ "$RACKET_VERSION" = "HEAD" ]]; then
        echo "$SCRIPT_NAME: Did the build fail? Check the logs at https://plt.eecs.northwestern.edu/snapshots/current/log/"
    fi
    exit 1
fi

echo "$SCRIPT_NAME: downloading $URL ..."
curl --location --output $DFILE $URL

# Only use sudo if installing to /usr
if [[ "$RACKET_DIR" = /usr** ]]; then
    RACKET_INSTALLER="sudo /bin/bash ${DFILE}"
else
    RACKET_INSTALLER="/bin/bash ${DFILE}"
fi

echo "$SCRIPT_NAME: installing Racket into $RACKET_DIR ..."

$RACKET_INSTALLER --in-place --dest "$RACKET_DIR"
rm --force -- $DFILE

exit 0
