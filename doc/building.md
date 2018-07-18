
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

### Plot library

If you are running Racket 7.0 or newer, you don't need to worry about this
step.

The application makes use of features that are not yet (2 Feb 2018) present in
the plot library that ships with Racket.  The application will work with the
built in plot library, but it will be missing overlays on various plots.  To
get that functionality, you will need to install the plot library from
https://github.com/alex-hhh/plot and checkout the "ah/interactive-overlays"
branch.  For more information on how to do that, see [this blog
post](https://alex-hhh.github.io/2018/01/changing-built-in-racket-packages.html)

### Inno Setup

To create a windows installer you will need to install [Inno
Setup](http://www.jrsoftware.org/isinfo.php) and compile the "setup.iss" file.

## API Keys for web services

> It seems that Wunderground is no longer supplying API keys, see issue #33.

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

## Building ActivityLog2

An installer can be built from the command line by typing the command below:

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

## Test suite

ActivityLog2 has a test suite, this runs on Travis, each time data is pushed
to a branch or a pull request is created.  The main aim of the test suite is
to ensure that the application builds and packages cleanly and that the data
storage and basic data operations work.

Tests can also be run manually using the following commands:

    raco test test/db-test.rkt
    raco test test/df-test.rkt

## Data storage and database management

ActivityLog2 stores all session and athlete data in a
[SQLite](https://sqlite.org/) database.  The application configuration data --
that is column widths in various views, what graphs are selected by default
and trend chart setup is stored in a separate preference file, see `get-pref`
and `put-pref` in [rkt/utilities.rkt](../rkt/utilities.rkt)

The **database schema** is defined in
[sql/db-schema.sql](../sql/db-schema.sql), SQL statements form this file are
executed when a new database is created, and the low level implementation that
does this is in [rkt/dbutil.rkt](../rkt/dbutil.rkt).  The `open-activity-log`
function defined in [rkt/dbapp.rkt](../rkt/dbapp.rkt) is used to either open
an existing database and upgrade it if is an older version, or create a new
database (if the file it needs to be opened does not exist.

**Database backups** are periodically created (once a week) in the
"db-backups" directory under the `(data-directory)` folder -- this function is
defined in [rkt/utilities.rkt](../rkt/utilities.rkt), the location is platform
dependent.  A backup is also created just before a database upgrade.  There
are no mechanisms to automatically restore databases, but the files can be
just renamed an copied in place as needed.  The databases are in single file
only.

To **update the database schema** the following need to be done:

* Update the database schema in [sql/db-schema.sql](../sql/db-schema.sql) as
  needed -- this file should always create the latest database version.
* Update the SCHEMA_VERSION in db-schema.sql, incrementing the value; this
  value is used by the application to determine if the opened database needs
  an upgrade.
* Write a database patch which updates the schema from the previous version to
  the current version.  This database patch should also update the
  SCHEMA_VERSION to the current version.  There are several examples in the
  [sql](../sql) folder.
* Update [dbapp.rkt](../rkt/dbapp.rkt) to update the schema version, add the
  database patch both as a patch file with `define-runtime-path` and to the
  update hash, `upgrade-patches`.

The database upgrade mechanism is a simple one and there are several
considerations required to make it work:

* Older versions of ActivityLog2 should work with newer databases.  Adding new
  tables, fields, or updating indexes should be fine, other changes require
  more careful consideration.  In particular, renaming tables or fields just
  to "improve clarity" is probably a bad idea.
* The database schema is considered a public API.  Other applications can and
  should be able to access and update data in the database outside of
  ActivityLog2 (if they corrupt the data, it is their problem).  I use this
  feature to track my training plan in an Excel worksheet and connect to the
  database to get some basic metrics and reports, but it could be used for
  other things as well.
* The number of database upgrades should be minimized.  This means that new
  features need careful consideration and aim to get the database schema right
  the first time, or at least try to.

While working on a database upgrade, start with the upgrade patch and don't
update the SCHEMA_VERSION.  The patch can be manually applied using the
`sqlite` command line utility and modified as needed during development,
including adding the corresponding ActivityLog2 code that uses the new
features.  Once the changes are worked out, update the database schema and
upgrade paths.  Consider adding tests to test that the new database features
added work or not (see [test/db-schema.rkt](../test/db-schema.rkt) for
examples).  The test framework should test the database upgrade code
automatically.

The following commands can be used to check the integrity of an activity log
database:

    pragma integrity_check
    pragma foreign_key_check

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
