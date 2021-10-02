$exit_code = 0;

# NOTE: race condition here between deleting the temporary file and creating a
#  directory with the same name

# Change to the directory where this script lives -- we run `fetch.sh` from
# the same directory.
$scriptpath = $MyInvocation.MyCommand.Path
$scriptdir = Split-Path $scriptpath
cd $scriptdir

$tdir = New-TemporaryFile;
Remove-Item $tdir
New-Item -ItemType Directory -Path $tdir
$dfile = Join-Path $tdir "Inno-Installer.exe";
Write-Output "Downloading into $dfile ..."

# NOTE: Downloading from Google Drive is tricky, as the download needs to pass
# through a confirmation dialog.  This is implemented once only in the
# `fetch.sh` script, and we just call this script to download the installer
# for us.

$file_id = "1n40We1JLV7mM4CKdmLzaIKoGLAeK1oy9";
$file_hash = "c3bd3170cf95c8712561220392a074d9991995b6";

$dprocess = Start-Process -FilePath bash `
  -ArgumentList "./fetch.sh -o $dfile -s $file_hash $file_id" `
  -Wait -PassThru -WindowStyle Hidden

if ($dprocess.ExitCode -ne 0) {
    Write-Output "Failed to download the installer!"
    $exit_code = 1;
}
else {

    # NOTE: with Inno Setup 6, installing in C:/Program Files did not seem to
    # work...

    if ($Env:INNO_DIR) {
        Write-Output "Installing Inno Setup in $Env:INNO_DIR";
        $iargs = "/SILENT /CURRENTUSER /DIR=`"$Env:INNO_DIR`""
    } else {
        Write-Output "Installing Inno Setup in %LOCALAPPDATA%\Programs\Inno Setup 6";
        $iargs = "/SILENT /CURRENTUSER"
    }

    $iprocess = Start-Process -FilePath $dfile -ArgumentList $iargs `
      -Wait -PassThru -WindowStyle Hidden
    If ($iprocess.ExitCode -ne 0) {
        Write-Output "Inno Setup installation failed!";
        $exit_code = 1;
    }
}

Write-Output "Cleaning up temporary files..."
Remove-Item -Path $dfile
Remove-Item -Path $tdir
exit $exit_code
