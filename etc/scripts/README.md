This directory contains scripts used for continuous integration builds and for
building an installer on Windows.  The scripts are:

* `install-racket.sh` -- this is a script that downloads and installs Racket
  on a Windows machine.  It is based on a script originally by Greg
  Hendershott from https://github.com/greghendershott/travis-racket, but has
  been heavily modified, mostly as an exercise in writing terse Bash Scripts.
  The script is intended for use in continuous integration builds and as such
  it is controlled by environment variables.  RACKET_DIR specifies the
  directory where Racket should be installed, while RACKET_VERSION specifies
  the version.  Both variables have suitable defaults inside the script and
  will work even if these environment are not present.

* `install-racket.ps1` -- this script will install Racket on Windows.  It uses
  the same environment variables as `install-racket.sh`, but at this time
  RACKET_DIR seems to be ignored by the racket installer itself when running
  on Azure Pipelines

* `install-inno-setup.ps1` -- this script will install [Inno
  Setup](http://www.jrsoftware.org/isinfo.php), it can currently install only
  a specific version in the default location.  Given that the Inno setup site
  does not support high amounts of traffic, the installer was placed on Google
  Drive and this script will download it from there.  **NOTE** this is not
  used anymore, since Inno Setup is already installed on the build image.

* `setup-catalog.sh` -- this script will setup the directory at
  $PROJECT_ROOT/pkgs to be a Racket package catalog which is consulted by
  `raco pkg install` when looking for packages -- it is used to install
  specific versions of the packages that ActivityLog2 depends on.

* `install.iss` -- this is the Inno Setup file used to build the ActivityLog2
  installer on Windows.  The setup file will build an installer with version 0
  if run in the Inno Setup GUI, you'll need to pass the version as a command
  line parameter to the inno setup compiler, see the azure-pipelines.yml for
  the actual commands to build the installer.

* `verpatch.exe` is a command line tool for adding and editing the version
  information of Windows executable files (applications, DLLs, kernel drivers)
  without rebuilding the executable.  It is used in azure-pipelines.yml to
  update the version number and description in the ActivityLog2 executable.
  The source for this utility is here: https://github.com/alex-hhh/ddverpatch

* `fetch.sh` -- this script will download and decrypt test data used by the
  unit tests.  It can be used for both Linux and Windows builds (git on
  windows has all the commands needed by this script).  The script itself can
  download and decrypt a file specified on the command line (using the Google
  Drive file ID) and reads the password from the TESTDATAPW environment
  variable.  **NOTE** this script is not used anymore, as Google Drive is not
  designed for automatic downloading of files and the script needs to be
  constantly updated to work around Google Drive changes.  It is here as a
  reference only.

* `fetch-az-blob.sh` -- this script will download and decrypt test data from
  Azure Blob Storage, data is used by the unit tests.  It can be used for both
  Linux and Windows builds (git on windows has all the commands needed by this
  script).  The script itself can download and decrypt a file specified in the
  DATA_FILE environment variable and reads the password from the TESTDATAPW
  environment variable.

* `sign-release.sh` is a script which will sign an installer using GPG.  The
  windows installer is signed with keys stored in Azure DevOps.  **NOTE:**
  while the code signing is probably correct (and hopefully secure), the keys
  were just generated and signed by me, so there is not much trust in them, as
  such, this is more of an exercise in how to sign build artifacts in Azure.

* `azure-pipelines.yml` and `azure-common.yml`-- these is the Azure Pipelines
  build files for the application.

## Continuous Integration on Azure Pipelines

Azure Pipelines is used to build and test ActivityLog2.  Travis is still
enabled for now (see the `.travis.yml` file in the root of the repository),
but given the uncertainty about the future of Travis, it will not be
maintained anymore.

The Azure Pipelines build is controlled by the `azure-pipelines.yml` file,
which does not have to be in the root directory of the project.  The
application is built on Linux and Windows platforms, runs the test suite, on
Windows only, it builds an installer than publishes it as a **build
artifact**.  This installer is produced by every build and can be downloaded
from the AzureDevOps site at the location below, however, it is only retained
for a limited time:

https://dev.azure.com/alexharsanyi0641/ActivityLog2/_build?definitionId=1

### Implementation Notes for the Build Process

**YAML schema** reference for the `azure-pipelines.yml` file can be found
here: https://aka.ms/yaml

On Linux, **Xvfb, the X virtual frame buffer** is needed to run the tests.
Since it takes a while to start up, we run it early in the build, as a
separate step.  **NOTE:** this task seems to hang for about 10 seconds, even
though Xvfb is started in the background, not sure how to fix this.

**Test Data Download** -- Some of the tests need test data which is downloaded
from Google Drive -- this data is my personal training data and it is
encrypted.  This download step requires a decryption key which is stored in
the Azure Pipelines account and mapped in this pipeline to the `TESTDATAPW`
variable.

**API Keys** -- On Windows, building the installer requires API keys for the
Tunderforest mapping service and DarkSky weather data service.  These keys are
stored in the Azure Pipelines account and mapped in this pipeline to the
`AL2TFAPIKEY` and `AL2DSAPIKEY` variables.

**Secret Variables** -- The `TESTDATAPW`, `AL2TFAPIKEY` and `AL2DSAPIKEY`
variables are marked as secret and only mapped in the pipeline for builds from
the original repository, for builds from outside the repository, such as pull
requests, the installer will not contain these API keys and it will not be
able to use those services -- otherwise, the installer will be usable.

**Installing Software** -- The build requires Racket to be installed and, on
Windows, the Inno Setup as well.  These packages are installed as individual
steps, by running the "install-racket.sh" script on linux and
"install-racket.ps1" script on Windows.  Inno Setup is installed by
"install-inno-setup.ps1".  These installations are controlled by some
environment variables, see each script for details.  There is currently a
problem with `indtall-racket.ps1`: the racket scripts are supposed to install
racket in RACKET_DIR, but, unfortunately setting RACKET_DIR on the Windows job
seems to be ignored by the Racket installer when run from install-racket.ps1,
not sure why, but on Windows, Racket will only install in default location for
now...

**PLTSTDERR** -- The build uses the `PLTSTDERR` environment variable set to
"error warning@optimizer", this will result in some warnings printed out
during the build.  My experience is that these warnings are always errors at
runtime (such as passing only one argument to `cons`).  Unfortunately, I found
no way to fail the build if any of these warnings are printed out.
