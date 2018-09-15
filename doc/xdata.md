
Garmin allowed third party applications to run on their devices and these
application can either collect data from new sensors (such as a Stryd running
power meter), or add various calculated values to the data recorded by the
device.  This data is stored in the FIT file using "developer data fields" and
ActivityLog2 allows importing and analyzing this data.  The term used for all
this data is "XDATA", which is the same term what GoldenCheetah uses.

This document is an overview on how XDATA and related meta data is stored and
used by ActivityLog2.

## The database schema

The database schema mimics the structure of the XDATA in the fit file and it
is composed of the following tables:

The `XDATA_APP` table information about the application that recorded the
XDATA (one row for each application).  Inside the FIT file, the application is
identified by a 16 byte GUID and a 16 byte developer GUID, and this is the
only information that is stored in the table.  The application has no access
to the name of the application, but this can be put in a config file (see the
"xdata-defs.json" section below).

All the example FIT files I have seen have the developer GUID set to
"ffffffffffffffffffffffffffffffff" so the application GUID alone uniquely
identifies the application.

The `XDATA_FIELD` table holds information about a data field -- this is a
value type that can be recorded by a particular application. For example, the
running power field recorded by an application will have one entry in this
table.

XDATA fields are identified by their name and need to be unique within an
application -- this seems fragile, but the FIT file does not contain any other
information about the field.  The pair "application GUID", "field name" should
uniquely identify a data field across all applications.

For each xdata field, the measurement units are also stored as a text value,
as read from the FIT file.  This represents the units of measure for the
values of this data field (e.g. "watts", "calories", etc), however, the FIT
file has no standardized way to represent these units so the only thing that
can be done with this field is to display it to the user.  As an example, one
application has the units stored as "ft or m", which is completely useless for
data processing or conversion.

The "native_message" column in the table stores the FIT message in which the
field can appear -- this tells us that the field can appear in "records"
(message 20) or "session summary" (message 18) fields.

The "native_field" column in the table stores the corresponding FIT field that
this XDATA field represents conceptually.  For example, an application
recording running power might indicate that its "running power" xdata field
has a native field of 7 which is the "power" field that a FIT file normally
records from a power meter.  This is currently used by ActivityLog2 to
determine how to construct a `series-metadata%` object for this XDATA field
(see below).

The `XDATA_VALUE` and `XDATA_SUMMARY_VALUE` tables store actual data for the
XDATA fields (that is, this is where running power data is stored).  The
`XDATA_VALUE` table associates a value for an XDATA field with a track point
in the `A_TRACKPOINT` table while the `XDATA_SUMMARY_VALUE` associates a value
for an XDATA field with a `SECTION_SUMMARY` row, and therefore it is a summary
value for a session or a lap.

## The `series-metadata%` objects

The application uses `series-metadata%` class instances, or instances of
classes derived from this to store meta-data information about the data series
it can process.  A "data series" represents a series of values recorded for an
activity, for example power data is a data series, and so is speed or cadence.

The `series-metadata%` class determines things such as the headline to display
in various GUI selection boxes or the color to use for plots of a specific
data series.  The class is defined in
["rkt/session-df/series-metadata.rkt"](../rkt/session-df/series-metadata.rkt)
file, which also contains documentation for all the methods that it provides.

For "native" or built-in data series, the meta-data objects are defined in
["rkt/session-df/native-series.rkt"](../rkt/session-df/native-series.rkt), but
since the application has no prior knowledge of what XDATA fields might be
available, it creates `series-metadata%` instances on the fly for XDATA
fields.  This works as follows (all this is defined in the
["rkt/session-df/xdata-series.rkt"](../rkt/session-df/xdata-series.rkt), in
the `make-xdata-series-metadata` function):

* Basic information about the XDATA field is retrieved in the database (such
  as the name and the application GUID).
  
* Based on the application GUID and the field name, a field definition is
  looked up in the "xdata-defs.json" file -- this file provides information
  such as the "headline" to use for a field.
  
* A new `series-metadata%` class is created which returns values either from
  the field definition JSON file or provides default values.

The idea is that metadata about XDATA fields are stored in an external file,
"xdata-defs.json", which can be used to customize how the data series are
processed.  This file needs to be updated for each XDATA application and its
fields.  Without an entry in this file, data from a new XDATA application will
still be able to be processed and displayed, but it might not be processed
correctly some of the time -- read below for the details.

### Structure of the `xdata-defs.json` file

There is a version of this file shipped with the application , it is in
["../sql/xdata-defs.json"](../sql/xdata-defs.json), and the user can also
place another "user defined" one in the application data folder on his machine
-- this allows updating this file without re-releasing the application.  The
toplevel structure of the file is an array where each entry defines an
application:

```json
[
  {
    "app_id" : "660a581e5301460c8f2f034c8b6dc90f",
    "app_name" : "Stryd",
    "fields" : [ ... ]
  }
]
```

Each application JSON object has three fields:

* `app_id` -- represents the GUID for the application, as stored in the
  database or the FIT file
  
* `app_name` -- a user defined name for the application

* `fields` -- represents an array of field objects, which define metadata for
  each XDATA field of that application
  
Each field is a JSON object that looks as shown below. In this example, the
default values are shown and any defaults could be omitted from the field.

```json
{
    "name" : "Leg Spring Stiffness",
    "series" : "stryd_lss",
    "headline" : "XDATA Leg Spring Stiffness (kN/m)",
    "axis_label" : "XDATA Leg Spring Stiffness (kN/m)",
    "should_filter" : true,
    "fractional_digits" : 0,
    "histogram_bucket_slot" : 1,
    "inverted_mean_max" : false,
    "missing_value" : 0
}
```

* `name` -- represents the name of the XDATA field, and should be the same as
  the value in the XDATA.name database column -- fields are identified by this
  name.

* `series` -- represents the name of the data series to use in session data
  frames.  If this is missing, the name is constructed in the format "data-ID"
  where ID is the XDATA.id for the field.  This field is not visible to the
  end user except when exporting data frames as CSV files, in which case it is
  shown as the header for the CSV column.  See also notes on renaming fields
  below.
  
* `headline` -- represents the text to display in GUI selection boxes.  If
  missing, a default text is constructed by concatenating "XDATA", the field
  name and the field units.

* `axis_label` -- represents the text to display on plots for this data
  series.  If missing, a default text as for `headline` is constructed.

* `should_filter` -- when true (default), data can be filtered in the graphs
  view.  If false, this data is not filtered, regardless of the filter setting
  in the graph view.

* `fractional_digits` -- number of relevant digits for values of this series.
  For example, while a device might record a distance as 3.123456 meters, it
  is unlikely that the device has micro-meter precision and probably
  everything after the first two digits is [false
  precision](https://en.wikipedia.org/wiki/False_precision).  Setting
  `fractional_digits` affects how grouping works in scatter plots and how
  values are exported.

* `histogram_bucket_slot` -- The default "slice" when calculating histograms.
  For values that are integer, such as watts, a value of 1 will indicate that
  histogram buckets are calculated for each watt value (e.g. one bucket for
  200 watts, one for 201, one for 202, etc).  This default does not work well
  for values that have fractional digits, so a slot width of less than 1 can
  be used in that case.  As a general rule, this should be expt(10, -
  fractional_digits) -- that is, if `fractional_digits` is 2,
  `histogram_bucket_slot` should be 0.01.

* `inverted_mean_max` -- when true, it indicates that smaller is better for
  values in this series.  This should be false most of the time, but things
  like pace and ground contact time should have this set to true, since a
  smaller pace is better as it indicates a higher speed.  This affects how
  mean maximal plots are constructed.

* `missing_value` -- a value to replace missing values in the series.  The
  default of 0 is good for most cases, but the "false" value has the special
  meaning to remove missing values from processing altogether -- this needs to
  be set when 0 is a valid value, for example for the altitude series.
  
### On changing values on the `xdata-defs.json` file

There are several considerations when editing the xdata-defs.json file and
changing field definitions, these are outlined below.

* The `series` is used to restore the last selection for various GUI controls,
  as well as the settings for trend charts, changing the series name will
  invalidate these.  In particular, any trend chart for the renamed XDATA
  series will have to be deleted and re-created.  Renaming data series will
  also leave "garbage" in the `BAVG_CACHE`, `HIST_CACHE` and `SCATTER_CACHE`
  tables.
  
* `fractional_digits`, `histogram_bucket_slot` and `inverted_mean_max` are
  used to store cached data for trend charts in the `BAVG_CACHE`, `HIST_CACHE`
  and `SCATTER_CACHE` tables, if these values change, the existing cache
  entries will not be changed and mixing data produced with different
  parameters will result in meaningless plots.
  
The conclusion is that whenever any of these values change, the cached data
should also be deleted from the "Tools/Delete Cached Data" menu.

### User defined `xdata-defs.json` file

The default `xdata-defs.json` file is shipped with the application and
normally cannot be changed by the user.  However, ActivityLog2 also looks for
a file with the same name in the user data folder see `data-directory` in
["rkt/utilities"](../rkt/utilitites.rkt), and if it finds one, it merges its
contents with the default one.  This way, the user can define new XDATA
definitions for the applications that they use or override default ones.

The user data file has the same format as the main one, and it needs only to
list the XDATA field entries that are to be customized. Field entries
themselves are not merged, but replaced, as such, the entire XDATA field entry
needs to be present in the user JSON file.
