# Building and Running the Application

## Prerequisites

### Racket

Before you can build and run ActivityLog2 you will need to install Racket.
This can be downloaded from "http://racket-lang.org/".  ActivityLog2 is build
and tested using the latest Racket version and previous versions may or may
not work.

### Installer (Windows only, and optional)

For creating a Windows installer, see the documentation in the [scripts
folder](../etc/scripts/README.md), this is not needed if you only want to
build and run the application on your local machine.

## API Keys for Web Services (optional)

----

**NOTE** You can no longer obtain an API key for weather download.  There is
[a plan](https://github.com/alex-hhh/ActivityLog2/issues/46) to replace the
service.

----

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

## Building and Running ActivityLog2

ActivityLog2 depends on some additional packages and the official ActivityLog2
build has these under version control in the "pkgs/" sub-folder and will need
to be installed before building or running the application.  Once installed
these packages will be available for all Racket programs (Unfortunately,
Racket has no concept of virtual environments for packages.)

You don't need to install dependencies as outlined below.  If you want, you
can install the following packages directly from the racket package catalog:
`tzinfo`, `tzgeolookup`, `data-frame`, `plot-container`, `gui-widget-mixins`,
`map-widget` and `al2-test-runner` (for running the tests).  If you go this
route, you may not have the exact versions which are used by ActivityLog2, and
may encounter problems.

**Update Submodules** after you cloned this repository, you will need to
update the submodules using the commands:

    git submodule update --init --recursive

**Setup Package Catalog** next, you will need to add the "pkgs/" sub-folder to
the list of Racket catalogs by running the command below (`bash` is available
on Windows as part of the `git` installation):

    bash etc/scripts/setup-catalog.sh pkgs/

**Install Dependencies** Once the catalog is set up, you will need to install
dependencies:

    raco pkg install --auto al2-dependencies

**Build Or Run the Application** Once the packages are set up, you can run the
application using the following command (you can also open the `run.rkt` file
in DrRacket and run it from within the IDE):

    racket run.rkt

... or build the ActivityLog2 executable and an installer using the following
command:

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

## Some Other Notes in Separate Documents

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

## Test Suite

ActivityLog2 has a test suite, this runs on Azure Pipelines, each time data is
pushed to a branch or a pull request is created.  The main aim of the test
suite is to ensure that the application builds and packages cleanly and that
the data storage and basic data operations work.

Tests can also be run manually using commands such as:

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

The following options are defined:

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

### Profiling and Tracing Functions

The [al2-profiler](../pkgs/al2-profiler) package contains definitions which
allow tracing individual function calls.  It is more practical than the
"trace" module shipped with Racket, as it allows to explicitly specify which
functions to trace.  To use it, require the "al-profiler.rkt" module and
replace `define` calls with `define/trace` calls.  This also works for method
names, where `define/public` can be replaced with `define/public/trace`.

The [al2-profiler](../pkgs/al2-profiler) module also contains a small
profiler, see that file for more details.

This package is not installed by default, you can install it using

```
raco pkg install al2-profiler
```

or, if you didn't setup the package catalog using

```
rack pkg install ./pkgs/al2-profiler
```
