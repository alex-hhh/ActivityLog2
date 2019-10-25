#!/bin/bash

# This script will set up the package catalog to install the packages that
# depend on ActivityLog2 using the packages in the $PROJECT_ROOT/pkgs folder
# (which are all GIT submodules).
#
# ISOLATION MODE: the -i option of this script will setup the
# $PROJECT_ROOT/pkgs as the **ONLY** package catalog, it is generally a bad
# idea to use this option on a developer machine, as you won't be able to
# install any other packages anymore.  The option is used on build machines
# and it is used to ensure that all required packages are available as GIT
# submodules.

set -e
SCRIPT_NAME=${0##**/}
ISOLATED=0

while getopts :i OPT; do
    case $OPT in
        i|+i)
            ISOLATED=1
            ;;
        *)
            echo "usage: ${0##*/} [-i] [--] CATALOG-FOLDER"
            echo "options:"
            echo "  -i             -- isolation mode (existing package catalogs will be discarded)"
            echo "  CATALOG-FOLDER -- folder containing packages to be installed"
            exit 2
    esac
done
shift $(( OPTIND - 1 ))
OPTIND=1

CATALOG_FOLDER=$1

if [ -z $CATALOG_FOLDER ]; then
    echo "Need to specify a package catalog folder"
    exit 2
elif ! [ -d $CATALOG_FOLDER ]; then
    echo "Specified package catalog folder not a directory: $CATALOG_FOLDER"
    exit 2
fi

# Index the specified folder as a package catalog using the `pkg/dirs-catalog`
# function.  This will create a folder named "catalog" in that directory with
# racket specific information about the packages in that folder.

echo "*** Indexing $CATALOG_FOLDER as a Racket package catalog..."
# NOTE we could use --check-metadata here, but we know our packages are
# missing authors and description tags, and there is nothing we can do about
# that.
pushd $CATALOG_FOLDER
racket -l- pkg/dirs-catalog --link catalog .
THIS_CATALOG_PATH=`pwd`
if ! [ -z `which cygpath` ]; then
    THIS_CATALOG_PATH=`cygpath -w $THIS_CATALOG_PATH`
fi
popd

CFILE=$(mktemp ${TMP:-/tmp}/$SCRIPT_NAME.XXXXXXXXXX)
trap 'rm --force -- $CFILE' INT TERM HUP EXIT

echo "file://$THIS_CATALOG_PATH/catalog" > $CFILE

if [ $ISOLATED -eq 1 ]; then
    echo "*** Discarding these existing package catalogs from this Racket installation:"
    raco pkg config catalogs
    
else
    # If we are not isolated, add the existing package catalog files
    raco pkg config catalogs >> $CFILE
fi

raco pkg config --set catalogs `cat $CFILE`
rm $CFILE

echo "*** Package catalog setup complete"
echo "*** $THIS_CATALOG_PATH will be checked for packages by 'raco pkg install'"
