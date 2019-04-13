
$v = $Env:RACKET_VERSION;  # 7.0 or newer -- as pre 7.0 naming convention is different
$p = $Env:RACKET_PLATFORM; # i386 or x86_64
$exit_code = 0

if (! $v -or $v -eq "") {
    $v = "7.2"; # default to 7.2 if a version was not specified
}

if (! $p -or $p -eq "") {
    $p = "x86_64" # default to 64 bit if a plaform was not specified
}

if ($Env:RACKET_MINIMAL) {
    $m = "minimal-"
} else {
    $m = ""
}

# 10/04/2019 -- mirror.racket-lang.org is down, use Utah one
$base = "https://www.cs.utah.edu/plt/installers"
# $base = "https://mirror.racket-lang.org/installers"

$url = "$base/$v/racket-$m$v-$p-win32.exe";

$tdir = New-TemporaryFile;
# NOTE: race condition here between deleting the temporary file and creating a
# directory with the same name
Remove-Item $tdir
New-Item -ItemType Directory -Path $tdir
$dfile = Join-Path $tdir "Racket-Installer.exe";
Write-Output "Downloading from $url ..."
Write-Output "Downloading into $dfile ..."
Invoke-WebRequest -Uri $url -OutFile $dfile `
  -Headers @{"Cache-Control"="no-cache"} -ErrorAction Stop

# Installer arguments: we pass /S for a silend install, but if $RACKET_DIR is
# present, we use it as the installation directory
#
# NOTE: the use of the $RACKET_DIR does not appear to work in Azure Pipelines
# -- Racket is still installed in the default location even if the /D switch
# is passed to the application...  this needs more investigation.

if ($Env:RACKET_DIR) {
    Write-Output "Installing Racket in $Env:RACKET_DIR";
    $iargs = @("/S", "/D=`"$Env:RACKET_DIR`"");
} else {
    Write-Output "Installing Racket in default location";
    $iargs = @("/S")
}

$iprocess = Start-Process -FilePath $dfile -ArgumentList $iargs `
  -Wait -PassThru -WindowStyle Hidden

if ($iprocess.ExitCode -ne 0) {
    Write-Output "Racket installation failed!";
    $exit_code = 1;
}

Write-Output "Cleaning up temporary files..."
Remove-Item -Path $dfile
Remove-Item -Path $tdir
exit $exit_code
