#!/bin/bash

# Script for installing Racket for Travis use, by Greg Hendershott from
# https://github.com/greghendershott/travis-racket, adapted by Alex Harsanyi.

set -e
SCRIPT_NAME=$(basename $0)

# Setup any missing environment variables here (they should be placed in
# .travis.yml), and warn the user...

if [[ "$RACKET_VERSION" = "" ]]; then
    RACKET_VERSION=7.0
    echo "$SCRIPT_NAME: missing RACKET_VERSION, defaulting to $RACKET_VERSION"
fi

if [[ "$RACKET_DIR" = "" ]]; then
    RACKET_DIR=~/racket
    echo "$SCRIPT_NAME: missing RACKET_DIR, defaulting to $RACKET_DIR"
fi

if [[ "$RACKET_MINIMAL" = "1" ]]; then
    MIN="minimal-"
else
    MIN=""
fi

DL_BASE="https://mirror.racket-lang.org/installers"

INSTALLER=$(mktemp ${TMP:-/tmp}/$SCRIPT_NAME.XXXXXXXXXX)
trap 'rm -f -- $INSTALLER' INT TERM HUP EXIT

if [[ "$RACKET_VERSION" = "HEAD" ]]; then
    if [[ "$RACKET_MINIMAL" = "1" ]]; then
        URL="http://plt.eecs.northwestern.edu/snapshots/current/installers/min-racket-current-x86_64-linux-precise.sh"
    else
        URL="http://plt.eecs.northwestern.edu/snapshots/current/installers/racket-test-current-x86_64-linux-precise.sh"
    fi
elif [[ "$RACKET_VERSION" = 5.3* ]]; then
    if [[ "$RACKET_MINIMAL" = "1" ]]; then
        URL="${DL_BASE}/${RACKET_VERSION}/racket-textual/racket-textual-${RACKET_VERSION}-bin-x86_64-linux-debian-squeeze.sh"
    else
        URL="${DL_BASE}/${RACKET_VERSION}/racket/racket-${MIN}${RACKET_VERSION}-bin-x86_64-linux-debian-squeeze.sh"
    fi
elif [[ "$RACKET_VERSION" = "RELEASE" ]]; then
    URL="http://pre-release.racket-lang.org/installers/racket-${MIN}current-x86_64-linux.sh"
elif [[ "$RACKET_VERSION" = 5.9* ]]; then
    URL="${DL_BASE}/${RACKET_VERSION}/racket-${MIN}${RACKET_VERSION}-x86_64-linux-ubuntu-quantal.sh"
elif [[ "$RACKET_VERSION" = 6.[0-4] ]] || [[ "$RACKET_VERSION" = 6.[0-4].[0-9] ]]; then
    URL="${DL_BASE}/${RACKET_VERSION}/racket-${MIN}${RACKET_VERSION}-x86_64-linux-ubuntu-precise.sh"
elif [[ "$RACKET_VERSION" = 6.* ]]; then
    URL="${DL_BASE}/${RACKET_VERSION}/racket-${MIN}${RACKET_VERSION}-x86_64-linux.sh"
elif [[ "$RACKET_VERSION" = 7.* ]]; then
    URL="${DL_BASE}/${RACKET_VERSION}/racket-${MIN}${RACKET_VERSION}-x86_64-linux.sh"
else
    echo "Unsupported version ${RACKET_VERSION}"
    exit 1
fi

echo "$SCRIPT_NAME: Checking if installer URL is good ..."
if curl -I -L $URL 2>&1 | grep 404.Not.Found ; then
    echo "Installer not available at $URL"
    if [[ "$RACKET_VERSION" = "HEAD" ]]; then
        echo "Did the build fail? Check the logs at https://plt.eecs.northwestern.edu/snapshots/current/log/"
    fi
    exit 1
fi

echo "$SCRIPT_NAME: downloading Racket from $URL ..."
curl -L -o $INSTALLER $URL

# Only use sudo if installing to /usr
if [[ "$RACKET_DIR" = /usr* ]]; then
    RUN_INSTALLER="sudo /bin/bash ${INSTALLER}"
else
    RUN_INSTALLER="/bin/bash ${INSTALLER}"
fi

echo "$SCRIPT_NAME:: installing Racket ..."

$RUN_INSTALLER --in-place --dest "$RACKET_DIR"
rm -f -- $INSTALLER

if [[ "$RACKET_MINIMAL" = "1" ]]; then
    echo "$SCRIPT_NAME: installing packages for raco test ..."
    ${RACKET_DIR}/bin/raco pkg install --auto --scope user rackunit-lib compiler-lib
fi

exit 0
