

# This is the site from which Inno Setup can be downloaded, unfortunately,
#  this fails occasionally (maybe it is hosted on a small server?).  This
# script actually downloads it from my Google Drive account
$original_url = "http://www.jrsoftware.org/download.php/is-unicode.exe";

# The site for Inno Setup has no HTTPS so, we ensure that only the expected
#  version is actually downloaded and installed.

$expected_hash = "27D49E9BC769E9D1B214C153011978DB90DC01C2ACD1DDCD9ED7B3FE3B96B538";

# Google Drive file location
# https://drive.google.com/open?id=1378vBnd-UfvmTM_-i4EJNfMJmM0IDptN

$gd_file_id = "1378vBnd-UfvmTM_-i4EJNfMJmM0IDptN";
$gd_url = "https://drive.google.com/uc?export=download&id=$gd_file_id"

$exit_code = 0;
$url = $gd_url;                         # download from Google Drive

# NOTE: race condition here between deleting the temporary file and creating a
#  directory with the same name

$tdir = New-TemporaryFile;
Remove-Item $tdir
New-Item -ItemType Directory -Path $tdir
$dfile = Join-Path $tdir "Inno-Installer.exe";
Write-Output "Downloading into $dfile ..."
Write-Output "Downloading from $url ..."
Invoke-WebRequest -Uri $url -OutFile $dfile `
  -SessionVariable session `
  -Headers @{"Cache-Control"="no-cache"} -ErrorAction Stop

$h = Get-FileHash -Algorithm SHA256 -Path $dfile

if ($h.Hash -ne $expected_hash) {
    Write-Output "Bad hash for $dfile, refusing install";
    Write-Output "Actual hash: $($h.Hash)"
    $exit_code = 1;
} else {
    if ($Env:INNO_DIR) {
        Write-Output "Installing Inno Setup in $Env:INNO_DIR";
        $iargs = "/SILENT /DIR=`"$Env:INNO_DIR`""
    } else {
        Write-Output "Installing Inno Setup in default location";
        $iargs = "/SILENT"
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
