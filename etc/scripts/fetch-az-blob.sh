#!/bin/bash

# This script requires tar, gzip, gpg and awk
#
# This script will download, decrypt and un-archive data files from Azure Blog
# storage Drive.  The url to download should be available in the TEST_DATA
# environment variable.
#
# The following additional arguments are also supported:
#
# * "-C DIR" when specified, the resulting file is extracted using "tar" and
#   the "-C DIR" argument is passed to the tar command. DIR is the directory
#   where the data will be extracted.
#
# * "-o FILE", when specified, the downloaded file will be copied to FILE --
#   this option is mutually exclusive with the -C option.
#
# * "-s HASH", when specified, it represents the expected SHA1 checksum of the
#   file.  The script will fail if the downloaded file does not have this
#   checksum.
#
# * "-d", when specified, data will be decrypted first, the password is taken
#   from the TESTDATAPW environment variable.
#
# NOTES:
#
# * To encrypt files use:
#       gpg --symmetric --batch --yes --passphrase $KEY $INPUTFILE
#
# * The TESTDATAPW environment variable will only be set on the build pipeline
#   and only for builds from the master branch or branches that are created by
#   the repository author (this is probably a good idea).

# Enable "strict mode", see
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

script_name=${0##**/}
decrypt=0
sha1sum=""
output_dir=""
destination=""

while getopts "C:ds:o:" OPT; do
    case $OPT in
        C|+C)
            output_dir="$OPTARG"
            ;;
        o|+o)
            destination="$OPTARG"
            ;;
        d|+d)
            if [[ -z ${TESTDATAPW:-} ]]; then
                echo "$script_name: asked for decrypt, but TESTDATAPW is missing"
                exit 0    # must return 0, as we use this from build pipelines
            fi
            decrypt=1
            ;;
        s|+s)
            sha1sum="$OPTARG";
            ;;
        *)
            echo "usage: $script_name [-C output_dir] [-d] [-o destination] [-s expected_sha1] [--] file_id"
            exit 2
    esac
done
shift $(( OPTIND - 1 ))
OPTIND=1

if (($# > 0)); then
    echo "extra arguments: $@"
    exit 1
fi

if [[ -z $output_dir ]] && [[ -z $destination ]]; then
    echo "no idea what to do with downloaded file (missing -C or -o options)"
    exit 1
fi

if [[ -z "$DATA_FILE" ]]; then
    echo "missing DATA_FILE environment variable"
    exit 1
fi

# Curl man page: https://curl.haxx.se/docs/manpage.html

ofile=$(mktemp ${TMP:-/tmp}/$script_name.XXXXXXXXXX)

trap 'rm --force -- $ofile' INT TERM HUP EXIT

curl --location --output $ofile "$DATA_FILE"

if [[ ! -z $sha1sum ]]; then
    # We use gpg to get the SHA1 checksum, to avoid requiring an extra program
    # (sha1sum)
    actual=`gpg --print-md sha1 $ofile | awk '{gsub(/^.*: /, ""); gsub(/ +/, ""); print tolower($0)}'`
    # actual=`sha1sum $ofile | awk '{print $1}'`
    if [[ $sha1sum != $actual ]]; then
       echo "$script_name: bad hash, expecting $sha1sum got $actual"
       rm $ofile
       exit 1
    fi
fi

[[ ! -z $output_dir ]] && mkdir --parent $output_dir > /dev/null 2>&1

if ((decrypt == 1)); then

    if [[ ! -z $output_dir ]]; then
        gpg --decrypt --quiet --batch --yes --passphrase $TESTDATAPW $ofile |\
            tar xvz -C ${output_dir:-.}
    elif [[ ! -z $destination ]]; then
        gpg --decrypt --quiet --batch --yes --passphrase $TESTDATAPW --output $destination $ofile
    else
        echo "no idea what to do with downloaded file (missing -C or -o options)"
    fi

else

    if [[ ! -z $output_dir ]]; then
        tar xvzf -C ${output_dir:-.} $ofile
    elif [[ ! -z $destination ]]; then
        mv $ofile $destination
    else
        echo "no idea what to do with downloaded file (missing -C or -o options)"
    fi

fi

# Clean up after ourselves
rm --force -- $ofile
