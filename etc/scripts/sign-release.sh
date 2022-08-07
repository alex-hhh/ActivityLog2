#!/bin/bash

# Script to sign an installer file using GPG secure key downloaded from Azure.
# The keys are downloaded in the Azure Pipelines file.  This script depends on
# several environment variables used by the Azure Pipelines build.

# Enable "strict mode", see
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

if [ -z $AGENT_TEMPDIRECTORY ]; then
    echo "AGENT_TEMPDIRECTORY environment variable not set"
    exit 1
fi

if [ -z $SIGNDATAPW ]; then
    echo "SIGNDATAPW not set, will not sign anything"
    exit 0
fi

ATMP=`cygpath -u "$AGENT_TEMPDIRECTORY"`

if ! [ -d $ATMP ]; then
    echo "AGENT_TEMPDIRECTORY is not a valid directory: $AGENT_TEMPDIRECTORY"
    exit 1
fi

KEYA=$ATMP/al2_sign.tar.gpg

if [ -r $KEYA ]; then
    export GNUPGHOME=$ATMP/$BUILD_BUILDID.gnupg
    mkdir -p $GNUPGHOME
    gpg --decrypt --quiet --batch --yes --passphrase $SIGNDATAPW "$KEYA" |\
        tar xv -C $GNUPGHOME
    PUBKEY=$GNUPGHOME/al2_sign_pub.asc
    SECKEY=$GNUPGHOME/al2_sign_sec.bin
    gpg --import $PUBKEY
    [ $? -ne 0 ] && echo "Public key import failed." && exit 1
    gpg --allow-secret-key-import --import $SECKEY
    [ $? -ne 0 ] && echo "Private key import failed." && exit 1
else
    # If I configured the pipeline correctly, the keys will not be downloaded
    # for pull requests outside the main repository -- need to handle this
    # case gracefully and not fail the build...
    echo "No signing keys found, will not sign anything"
    exit 0
fi

EXIT_CODE=0;
PKG=`ls ActivityLog2Setup-*.exe 2>/dev/null`

if [ -z $PKG ]; then
    echo "Could not find installer file to sign"
    EXIT_CODE=1
else
    echo "Will sign $PKG"
    SIG=`echo $PKG | sed 's/\\.exe/.sig/'`
    echo "Signature file will be $SIG"

    if [ -e $SIG ]; then
        echo "Removing previous signature file..."
        rm "$SIG"
    fi

    gpg --detach-sign --armor --output $SIG $PKG

    if [ $? -ne 0 ]; then
        echo "Failed to create signature"
        EXIT_CODE=1
    else
        echo "Created signature."
    fi
fi

MANIFEST=./dist/manifest-sha256.txt

if [ -z $MANIFEST ]; then
    echo "Could not manifest to sign, skipping."
    EXIT_CODE=0
else
    echo "Will sign $MANIFEST"
    # Note SIG name is based on package name, we run late in the process, and
    # cannot patch the installer.
    SIG=`echo $PKG | sed 's/\\.exe/-manifest.sig/'`
    echo "Signature file will be $SIG"

    if [ -e $SIG ]; then
        echo "Removing previous signature file..."
        rm "$SIG"
    fi

    gpg --detach-sign --armor --output $SIG $MANIFEST

    if [ $? -ne 0 ]; then
        # Even though we skip signing if we don't find the manifest, once we
        # find it, we fail if we fail to sign it.
        echo "Failed to create signature"
        EXIT_CODE=1
    else
        echo "Created signature."
    fi
fi

rm -rf "$GNUPGHOME"
exit $EXIT_CODE
