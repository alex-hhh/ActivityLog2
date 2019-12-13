# Running the application from source

Before you can build or run the application, you will need to install some
packages that ActivityLog2 depends on, on the command line run the following
command:

    raco pkg install --auto tzinfo tzlookup data-frame plot-container

**NOTE** if you are on a Windows platform, you may have to setup a package
catalog to pick up packages from the pkgs folder, see the `Building
ActivityLog2` section below.

After that, the simplest way to run ActivityLog2 from source is to start
DrRacket, open the file "run.rkt" and click on the "Run" button, or press
"Ctrl-R". You can also run ActivityLog2 from the command line using the
following command:

    racket run.rkt

The above will take a long time, especially the first time it is run.  You can
also build a standalone distribution, see below.

# Building a stand alone distribution

## Prerequisites

### Racket

Before you can build and run ActivityLog2 you will need to install Racket.
This can be downloaded from "http://racket-lang.org/".  ActivityLog2 is build
and tested using the latest Racket version and previous versions may or may
not work.

### Installer (optional)

For creating a Windows installer, see the documentation in the [scripts
folder](../etc/scripts/README.md), this is not needed if you only want to
build and run the application on your local machine.

## API Keys for web services

ActivityLog2 uses web services for some of the functionality.  Currently two
services are used: [DarkSky.net](https://www.darksky.com/) is used to retrieve
weather data for an activity and [Thunderforest](http://thunderforest.com/) is
used for map tiles.  These services require API keys to access them.  The
built (and released) version of the application has these API keys embedded in
the executable, however the keys are kept separate from the code base.

ActivityLog2 will work without these API keys, however, weather data will not
be downloaded and only [OpenStreetMap](http://www.openstreetmap.org/) map
tiles will be available.

You will need to obtain your own API keys, if you wish to build your own
application.  To use these keys, you need to set two environment variables:

* `AL2TFAPIKEY` -- contains the API key for Thunderforest maps
* `AL2DSAPIKEY` -- contains the API key for the DarkSky weather service

These environment variables will be used while ActivityLog2 is running during
development and they will also be embedded in any built executable.

## Building ActivityLog2

ActivityLog2 depends on some additional packages and the official ActivityLog2
build has them version controlled in the "pkgs/" folder.  After you cloned
this repository, you will need to update the submodules using the commands:

    git submodule update --init --recursive

After than you will need to add the pkgs folder to the list of Racket catalogs
by running the command below:

    sh etc/scripts/setup-catalog.sh pkgs/

The above step is not necessary for casual use, as the packages are also
available in the standard package catalog, but some of the packages might
contain additional fixes and updates which are not available in the official
package catalog yet.

You will need to install the dependent packages:

    raco pkg install --auto tzinfo tzgeolookup data-frame plot-container

An ActivityLog2 executable and an installer can be built from the command line
by typing the command below:

    racket build.rkt

This command will create a stand-alone application in the "dist" folder and
this application will run even on machines that don't have Racket installed.
If you have Inno setup installed and are on Windows, the above command will
also create an installer executable.

# Developer Notes

You can separate the build steps if you open the file "build.rkt" in DrRacket,
run it by clicking the "Run" button or pressing "Ctrl-R", than type the
following expressions in the interactions window:

    (compile-app) ; builds ZO files from the Racket source files
    (build-app)   ; creates the ActivityLog2 executable
    (mkdist)      ; packages the executable and required files into the dist folder

While working on the source code, it might be worth compiling the files first
to speed things up:

    raco make run.rkt rkt/main.rkt

Note that if compiled files are present, racket will not re-compile modified
files, you will need to recompile them explicitly, otherwise you will
encounter load errors when trying to run ActivityLog2.

## Some other notes in separate documents

* [database.md](./database.md) describes how data is stored and used by the
  application
* [xdata.md](./xdata.md) describes how XDATA (Garmin IQ data) is stored and
  used by the application.
* [data-frame.md](./data-frame.md) describes the data frame structure which is
  used inside the application to store data from sessions and other types of
  data.
* [session-df.md](./session-df.md) describes the contents of a data frame
  containing session data.
* [gui-consistency.md](./gui-consistency.md) describes how various GUI parts
  communicate changes between each other.
* [hrv-notes.md](./hrv-notes.md) describes how to access HRV data in FIT files.

## Versioning Scheme

The application uses the MAJOR.MINOR.PATCH.BUILD versioning scheme, where:

* `MAJOR` represents the major version number, which should increment only on
  a major application re-write (I don't expect that to happen.)
* `MINOR` represents the minor version number, and it increments when new
  features are added, or existing features are changed/updated
* `PATCH` increments for bug-fixes, the special value "99" indicates an
  unreleased version (e.g. 1.8.99)
* `BUILD` is the Azure pipelines build id, and should be a unique number which
  always increments for every Azure build, regardless of the other numbers.

The file "version.txt" contains the current version number, without the BUILD
part (e.g. 1.8.99), and the build script picks up the version from this file
-- when changing version numbers, this is the only file that should change.

## Test suite

ActivityLog2 has a test suite, this runs on Travis, each time data is pushed
to a branch or a pull request is created.  The main aim of the test suite is
to ensure that the application builds and packages cleanly and that the data
storage and basic data operations work.

Tests can also be run manually using the following commands:

    raco test test/db-test.rkt
    raco test test/df-test.rkt

Some tests require data that is private and you will not be able to run them
-- these tests run automatically on Azure Pipelines.  For the list of all the
tests and how to run them, see the `test/` folder and the
[azure-pipelines.yml](/etc/scripts/azure-pipelines.yml) build file.

## Debugging tips

### Extra debugging options

Some features can be enabled manually to help debug the application, they can
be enabled by running a Racket program such as using "al-interactive.rkt" and
using `put-pref` (and `get-pref` to obtain their state):

```racket
(require "al-interactive.rkt")
(put-pref 'activity-log:debug:show-stop-points? #t)
```

The following options are deifned:

* `activity-log:debug:show-stop-points?` -- when set to `#t`, the session
  graphs and elevation graph on map view will show vertical lines for stop
  points (blue) and teleport points (red).  See
  [session-df.md](./session-df.md) for the meaning of these.

### Log files

ActivityLog2 uses a log file to record various debug information while it
runs.  On a Windows machine, the log file is located in
"%APPDATA%/Local/ActivityLog/ActivityLogDbg.log", for other platforms, see
`maybe-init-log-port` in [rkt/utilities.rkt](../rkt/utilities.rkt).  It is
worth consulting this file when trying to diagnose problems as it usually
contains more information than what it just reported to the user using the
GUI.

When ActivityLog2 throws an exception it can be logged in two places: the
console or the log file and exceptions thrown from separate threads will
usually be logged to the log file.

The `dbglog`, `dbglog-exception` and `thread/dbglog` functions, defined in
"rtk/utilities.rkt" can also be used to add additional logging as needed.

### Stack traces for exceptions

By default, the exceptions that are logged will not contain a stack trace and
it may be difficult to identify where an exception occurred.  To enable stack
traces in exceptions, first remove all your compiled files from the "compiled"
folders, than run the command below.  This will take a long time to load and
will run slow:

    racket -l errortrace -t run.rkt

### Tracing function calls

The [rkt/al-profiler.rkt](../rkt/al-profiler.rkt) module contains definitions
which allow tracing individual function calls.  It is more practical than the
"trace" module shipped with Racket, as it allows to explicitly specify which
functions to trace.  To use it, require the "al-profiler.rkt" module and
replace `define` calls with `define/trace` calls.  This also works for method
names, where `define/public` can be replaced with `define/public/trace`.

The "rkt/al-profiler.rkt" module also contains a small profiler, see that file
for more details.
