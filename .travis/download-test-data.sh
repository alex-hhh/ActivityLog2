#!/bin/bash

# This script will download test data from Google Drive.  This data contains
# actual private information, and as such it is encrypted and it will only be
# able to be extracted on the Travis servers.
#
# To download the data, the decryption password needs ot be set in the
# TESTDBPW environment variable, this will only be set on the Travis build
# server and only for builds from the master branch or branches that are
# created by the repository author (this is probably a good idea).  The script
# is designed such that it will not fail if the password is not set, but will
# not download the test data either and as a result some of the tests will not
# run.

set -e
script_name=${0##**/}

if [ -z $TESTDATAPW ]; then
    echo "$script_name: missing TESTDATAPW env var, will not download anything"
    exit 0
fi

# NOTE: this script runs from the root of the repository.  This needs to be
# accounted for in the file paths

function download_file {
    file_id=$1

    if [ -z $file_id ]; then
        echo "$script_name: download_file function needs a file id"
        return
    fi

    gurl=https://drive.google.com/uc
    gcookies=$(mktemp ${TMP:-/tmp}/$script_name.XXXXXXXXXX)
    trap 'rm -f -- $gcookies' INT TERM HUP EXIT
    curl -sc $gcookies "$gurl?export=download&id=$file_id" > /dev/null
    code="$(awk '/_warning_/ {print $NF}' $gcookies)"
    ofile1=$(mktemp ${TMP:-/tmp}/$script_name.XXXXXXXXXX)
    trap 'rm -f -- $ofile1' INT TERM HUP EXIT
    curl -LJ -o $ofile1 -b $gcookies "$gurl?export=download&confirm=$code&id=$file_id"
    ofile2=$(mktemp ${TMP:-/tmp}/$script_name.XXXXXXXXXX)
    trap 'rm -f -- $ofile2' INT TERM HUP EXIT
    openssl aes-256-cbc -d -k $TESTDATAPW -in $ofile1 -out $ofile2
    # We run in the root of the repo, but output test databases into the test
    # folder
    tar xvzf $ofile2 -C test
    # Clean up after ourselves
    rm -f -- $gcookies $ofile1 $ofile2
}

download_file "1l-COKZ8pMef4JUX6E7HBVEEYMAYO2Noo" # test-db
download_file "1IfWTfzTowCqhgrPRysGlvzFkrqMCGgA3" # test-fit
