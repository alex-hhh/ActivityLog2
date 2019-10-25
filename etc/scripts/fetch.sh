#!/bin/bash

# This script will download and un-archive test data from Google Drive.  This
# data contains actual private information, and as such it is encrypted and it
# will only be able to be extracted on the Travis servers.
#
# To download the data, the decryption password needs ot be set in the
# TESTDBPW environment variable, this will only be set on the Travis build
# server and only for builds from the master branch or branches that are
# created by the repository author (this is probably a good idea).  The script
# is designed such that it will not fail if the password is not set, but will
# not download the test data either and as a result some of the tests will not
# run.
#
# The script needs to be passed at least one argument, the file id for the
# file to download.  A "-C" argument can specify the directory to un-archive
# the data, it is passed to the tar command as the -C argument.

set -e
script_name=${0##**/}
decrypt=0

while getopts :C:d OPT; do
    case $OPT in
        C|+C)
            output_dir="$OPTARG"
            ;;
        d|+d)
            if [ -z $TESTDATAPW ]; then
                echo "$script_name: missing TESTDATAPW env var, will not download anything"
                exit 0    # must return 0, as we use this from build pipelines
            fi
            decrypt=1
            ;;
        *)
            echo "usage: $script_name [+-C output_dir] [-d] [--] file_id"
            exit 2
    esac
done
shift $(( OPTIND - 1 ))
OPTIND=1

file_id=$1

if [ -z $file_id ]; then
    echo "$script_name: missing file id"
    exit 1
fi

# Curl man page: https://curl.haxx.se/docs/manpage.html

gurl=https://drive.google.com/uc
gcookies=$(mktemp ${TMP:-/tmp}/$script_name.XXXXXXXXXX)
trap 'rm --force -- $gcookies' INT TERM HUP EXIT
curl --silent \
     --cookie-jar $gcookies \
     "$gurl?export=download&id=$file_id" > /dev/null
code="$(awk '/_warning_/ {print $NF}' $gcookies)"
ofile1=$(mktemp ${TMP:-/tmp}/$script_name.XXXXXXXXXX)

trap 'rm --force -- $ofile1' INT TERM HUP EXIT

curl --location \
     --junk-session-cookies \
     --cookie $gcookies \
     --output $ofile1 \
     "$gurl?export=download&confirm=$code&id=$file_id"

if ((decrypt == 1)); then
    ofile2=$(mktemp ${TMP:-/tmp}/$script_name.XXXXXXXXXX)
    trap 'rm --force -- $ofile2' INT TERM HUP EXIT
    openssl enc -d -aes-256-ctr -md sha256 \
            -pass pass:$TESTDATAPW -in $ofile1 -out $ofile2
else
    ofile2=$ofile1
fi

mkdir --parent "${output_dir:-.}" > /dev/null 2>&1
tar xvzf $ofile2 -C "${output_dir:-.}"

# Clean up after ourselves
rm --force -- $gcookies $ofile1 $ofile2
