# Running the application from source

The simplest way to run ActivityLog2 from source is to start DrRacket, open
the file "run.rkt" and click on the "Run" button, or press "Ctrl-R". You can
also run ActivityLog2 from the command line using the following command:

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

### Inno Setup

To create a windows installer you will need to install [Inno
Setup](http://www.jrsoftware.org/isinfo.php).  This is optional.

If Inno Setup is installed, the build script will create an installed by
compiling the [etc/scripts/setup.iss](../etc/scripts/setup.iss) file.

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

## Test suite

ActivityLog2 has a test suite, this runs on Travis, each time data is pushed
to a branch or a pull request is created.  The main aim of the test suite is
to ensure that the application builds and packages cleanly and that the data
storage and basic data operations work.

Tests can also be run manually using the following commands:

    raco test test/db-test.rkt
    raco test test/df-test.rkt

Some tests require data that is private and you will not be able to run them
-- these tests run automatically on Travis.

## Debugging tips

### Log messages and exceptions

When ActivityLog2 throws an exception it can be logged in two places: the
console or the log file.  On a Windows machine, the log file is located in
"%APPDATA%/Local/ActivityLog/ActivityLogDbg.log", for other platforms, see
`maybe-init-log-port` in [rkt/utilities.rkt](../rkt/utilities.rkt).  In
particular, exceptions thrown from separate threads will be logged in the log
file.

The `dbglog`, `dbglog-exception` and `thread/dbglog` functions, defined in
"rtk/utilities.rkt" can also be used to add additional logging as needed.
These log messages will go in the log file.

### Stack traces for exceptions

By default, the exceptions that are logged will not contain a stack trace --
this makes it difficult to identify where an exception occurred.  To enable
stack traces in exceptions, first remove all your compiled files from the
"compiled" folders, than run the command below.  This will take a long time to
load and will run slow:

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
