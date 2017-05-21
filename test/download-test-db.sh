#!/bin/bash

# NOTE: this script runs from the root of the repository.  This needs to be
# accounted for in the file paths

set -e
file_id=0B5h4XOdkim72cDJ4Z2RZR05UOFE
file_name=test-db.tar.gz.enc
file_name2=test-db.tar.gz
gurl=https://drive.google.com/uc
curl -sc ./gcookies "$gurl?export=download&id=$file_id" > /dev/null
code="$(awk '/_warning_/ {print $NF}' ./gcookies)"  
curl -LJ -o ./$file_name -b ./gcookies "$gurl?export=download&confirm=$code&id=$file_id"
rm ./gcookies
openssl aes-256-cbc -d -k $TESTDBPW -in $file_name -out $file_name2
rm $file_name
# We run in the root of the repo, but output test databases into the test folder
tar xvzf $file_name2 -C test
rm $file_name2
