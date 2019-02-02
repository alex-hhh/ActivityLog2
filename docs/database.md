# Data storage and database management

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

