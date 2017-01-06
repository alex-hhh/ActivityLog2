# Building a stand alone distribution

Before you can build and run the application you will need to install Racket.
This can be downloaded from "http://racket-lang.org/".  The application is
build and tested using the latest Racket version and previous versions may or
may not work.

The simplest way to run the application is to open the file "run.rkt" in
DrRacket and click on the "Run" button, or press "Ctrl-R".

To build an executable, open the file "build.rkt" in DrRacket, run it by
clicking the "Run" button or pressing "Ctrl-R", than type the following
expressions in the interactions window:

    (compile-app)
    (build-app)
    (mkdist)

This will create a stand-alone application in the "dist" folder.  This
application will run even on machines that don't have Racket installed.

A windows installer can be created using Inno Setup
(http://www.jrsoftware.org/isinfo.php) by compiling the "setup.iss" file.

# Developer Notes

The application can be run from the command line using the following command:

    racket run.rkt

If this is done repeatedly, it might be worth compiling the files first to
speed things up:

    raco make run.rkt rkt/main.rkt

Note that if compiled files are present, racket will not re-compile modified
files, you will need to recompile them explicitly, otherwise you will
encounter load errors when trying to run the application.

## Debugging tips

When the application throws an exception it can be logged in two places: the
console or the log file.  The log file is located in
"%APPDATA%/Local/ActivityLog/ActivityLogDbg.log" (for other platforms, see
`maybe-init-log-port` in "rkt/dbglog.rkt").  In particular, exceptions thrown
from separate threads will be logged in the log file.

To enable stack traces in exceptions, first remove all your compiled files
from the "compiled" folders, than run (this will take a long time to load and
will run slow):

    racket -l errortrace -t run.rkt

The application has a small test suite.  Tests can be run using:

    raco test test/db-test.rkt
    raco test test/df-test.rkt

## Database management

Activity data is stored in a SQLite (https://sqlite.org/) database and the
database schema is defined in "sql/db-schema.sql".  The schema is versioned
using a value in the "SCHEMA_VERSION" table.  The application will check this
version against its expected version and will refuse to open a database with a
different version.

When the schema is updated, the "sql/db-schema.sql" file is simply updated to
reflect the latest schema version.  New databases will be created with the new
version.  At the same time, a database patch is written which will upgrade an
existing database from the previous version (see the "sql" folder for
patches).

At this time, there is no automatic upgrading, instead, a database needs to be
manually upgraded using the following command (the sqlite3 utility can be
downloaded from  the SQLite web site):

    sqlite3 dbname.db < patch.sql
