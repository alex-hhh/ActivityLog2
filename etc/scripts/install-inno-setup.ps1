# This is the site from which Inno Setup can be downloaded, unfortunately,
#  this fails occasionally (maybe it is hosted on a small server?).  This
# script actually downloads it from my Google Drive account
$original_url = "http://www.jrsoftware.org/download.php/is-unicode.exe";

# The site for Inno Setup has no HTTPS so, we ensure that only the expected
#  version is actually downloaded and installed.

$expected_hash = "5856471BA4DF94772FE415687F70CC87ADD8BC228EFB289DDADAE55BF0F2759B";

# Google Drive file location
$gd_file_id = "1UQ4587WM_CPt9gAbkt-J8gJrJl4sFSdv";
$gd_url = "https://drive.google.com/uc?export=download&id=$gd_file_id"

$exit_code = 0;
$url = $gd_url;                         # download from Google Drive

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

$file_id = "1UQ4587WM_CPt9gAbkt-J8gJrJl4sFSdv";
$file_hash = "510902ffe43e3ef3504e4567ece45f7cd694f5df";

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
