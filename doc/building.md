# Building a stand alone distribution

## Prerequisites

Before you can build and run ActivityLog2 you will need to install Racket.
This can be downloaded from "http://racket-lang.org/".  ActivityLog2 is build
and tested using the latest Racket version and previous versions may or may
not work.

To create a windows installer you will need to install [Inno
Setup](http://www.jrsoftware.org/isinfo.php) and compile the "setup.iss" file.

## API Keys for web services

ActivityLog2 uses web services for some of the functionality.  Currently two
services are used: [Wunderground](https://www.wunderground.com/) is used to
retrieve weather data for an activity and
[Thunderforest](http://thunderforest.com/) is used for map tiles.  These
services require API keys to access them.  The built version of the
applications has these API keys embedded, however the keys are kept separately
from the code base.

ActivityLog2 will work without these API keys, however, weather data will not
be downloaded and only [OpenStreetMap](http://www.openstreetmap.org/) map
tiles will be available.

You will need to obtain your own API keys, if you wish to build your own
application.  To use these keys, you need to set two environment variables:

* `AL2TFAPIKEY` -- contains the API key for Thunderforest maps
* `AL2WUAPIKEY` -- contains the API key for the Wunderground service

These environment variables will be used while ActivityLog2 is running and
they will also be embedded in any built executable.

## Running or building ActivityLog2

The simplest way to run ActivityLog2 is to start DrRacket, open the file
"run.rkt" and click on the "Run" button, or press "Ctrl-R". You can also run
ActivityLog2 from the command line using the following command:

    racket run.rkt

An installer can be built from the command line by typing the command below.
This requires to have Inno Setup installed, otherwise the last step will
fail:

    racket build.rkt

Alternatively, open the file "build.rkt" in DrRacket, run it by clicking the
"Run" button or pressing "Ctrl-R", than type the following expressions in the
interactions window:

    (compile-app)
    (build-app)
    (mkdist)

This will create a stand-alone application in the "dist" folder.  This
application will run even on machines that don't have Racket installed.

# Developer Notes

While working on the source code, it might be worth compiling the files first
to speed things up:

    raco make run.rkt rkt/main.rkt

Note that if compiled files are present, racket will not re-compile modified
files, you will need to recompile them explicitly, otherwise you will
encounter load errors when trying to run ActivityLog2.

## Debugging tips

### Log messages and exceptions

When ActivityLog2 throws an exception it can be logged in two places: the
console or the log file.  On a Windows machine, the log file is located in
"%APPDATA%/Local/ActivityLog/ActivityLogDbg.log", for other platforms, see
`maybe-init-log-port` in "rkt/dbglog.rkt".  In particular, exceptions thrown
from separate threads will be logged in the log file.

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

The "rkt/al-profiler.rkt" module contain definitions which allow tracing
individual function calls.  It is more practical than the "trace" module
shipped with Racket, as it allows to explicitly specify which functions to
trace.  To use it, require the "al-profiler.rkt" module and replace `define`
calls with `define/trace` calls.  This also works for method names, where
`define/public` can be replaced with `define/public/trace`.

The "rkt/al-profiler.rkt" also contains a small profiler, see that file for
more details.

### Test suite

ActivityLog2 has a small test suite.  Tests can be run using:

    raco test test/db-test.rkt
    raco test test/df-test.rkt

### Database stuff

    pragma integrity_check
    pragma foreign_key_check

## Database management

Activity data is stored in a [SQLite](https://sqlite.org/) database and the
database schema is defined in "sql/db-schema.sql".  The schema is versioned
using a value in the "SCHEMA_VERSION" table.  ActivityLog2 will check this
version against its expected version and will automatically upgrade an older
version database.

When the schema is updated, the "sql/db-schema.sql" file is simply updated to
reflect the latest schema version.  New databases will be created with the new
version.  At the same time, a database patch is written which will upgrade an
existing database from the previous version (see the "sql" folder for
patches).

A database can be manually upgraded using the following command (the sqlite3
utility can be downloaded from the SQLite web site):

    sqlite3 dbname.db < patch.sql
