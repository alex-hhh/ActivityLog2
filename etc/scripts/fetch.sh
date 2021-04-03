#!/bin/bash

# This script will download decrypt and un-archive data files from Google
# Drive.  It needs to be passed at least one argument, the file id for the
# file to download.  This file also needs to be publicly shared on Google
# Drive (there is no support for authentication)
#
# The script has the following additional arguments:
#
# * "-C DIR" when specified, the resulting file is extracted using "tar" and
#  the "-C DIR" argument is passed to the tar command. DIR is the directory
#  where the data will be extracted.
#
# * "-o FILE", when specified, the downloaded file will be copied to FILE --
#  this option is mutually exclusive with the -C option.
#
# * "-s HASH", when specified, it represents the expected SHA1 checksum of the
#  file.  The script will fail if the downloaded file does not have this
#  checksum.
#
# * "-d", when specified, data will be decrypted first, the password is taken
#   from the TESTDATAPW environment variable.  this will only be set on the
#   build pipeline and only for builds from the master branch or branches that
#   are created by the repository author (this is probably a good idea).

set -e
script_name=${0##**/}
decrypt=0

while getopts "C:ds:o:" OPT; do
    case $OPT in
        C|+C)
            output_dir="$OPTARG"
            ;;
        o|+o)
            destination="$OPTARG"
            ;;
        d|+d)
            if [ -z $TESTDATAPW ]; then
                echo "$script_name: missing TESTDATAPW env var, will not download anything"
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

if ! [ -z $sha1sum ]; then
    actual=`sha1sum $ofile1 | awk '{print $1}'`
    if [[ $sha1sum != $actual ]]; then
       echo "$script_name: bad hash, expecting $sha1sum got $actual"
       rm $ofile1
       exit 1
    fi
fi

if ((decrypt == 1)); then
    ofile2=$(mktemp ${TMP:-/tmp}/$script_name.XXXXXXXXXX)
    trap 'rm --force -- $ofile2' INT TERM HUP EXIT
    openssl enc -d -aes-256-ctr -md sha256 \
            -pass pass:$TESTDATAPW -in $ofile1 -out $ofile2
else
    ofile2=$ofile1
fi

if ! [ -z $output_dir ]; then
    mkdir --parent "${output_dir:-.}" > /dev/null 2>&1
    tar xvzf $ofile2 -C "${output_dir:-.}"
elif ! [ -z $destination ]; then
    cp $ofile2 $destination
fi

# Clean up after ourselves
rm --force -- $gcookies $ofile1 $ofile2
