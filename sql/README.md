This directory contains data files used by the ActivityLog2 application at
runtime.  The files are referenced by `define-runtime-path` and packaged into the distribution by "raco dist" during the build process.

* `db-schema.sql` and the `migrations` directory contain the SQL statements
  that create a new database and upgrade an older database to the latest
  version, you can find more about them in the
  [database.md](../docs/database.md) document.

* the `queries` directory contains SQL queries used by the application, which
  are stored in their own files instead of being embedded as strings inside
  the Racket source files.

* `osmtc-schema.sql` contains the database schema used for the map tile cache.

* `defaut-theme.rktd` contains the colors used for data series, sport icon,
  zones and some other things.

* `xdata-defs.json` contains declarations for various XDATA series, the
  structure of the file is described in the [xdata.md](../docs/xdata.md)
  document.

* `fit-product-defs.json` contains mapping between the manufacturer ID and
  product ID used in fit files and is used to name devices.  These are used
  when a device serial number is first seen and a new EQUIPMENT entry is
  created in the database.

* `fthr-defs.json` contains definitions for sport zones based on power, pace
  or heart rate thresholds.  It is used by [fthr.rkt](../rkt/models/fthr.rkt)
  and it is described in [this blog
  post](https://alex-hhh.github.io/2020/05/threshold-analysis-in-activitylog2.html)
