# Session data-frame% objects

Session data is loaded into a data-frame object (defined in
"rkt/data-frame/df.rkt") by `session-df`.  Data frames contain the track data
for a session, and all data operations and plotting is done on these data
frame objects.

This document describes the contents of the data frame object as created by
`session-df`.

## Properties

The data frame object can store a set of properties, which are key - value
mappings. The following properties are present in the session data frame,
while other parts of the application can attach additional properties:

* **is-lap-swim?** is #t if this is a swim activity
* **session-id** the database id for the session (A_SESSION.id)
* **sport** is a vector of sport-id and subsport-id, as defined in the `E_SPORT`
  and `E_SUB_SPORT` database tables.
* **stop-points** is a list of timestamps where the recording was stopped.
* **laps** is a list of timestamps where the laps start
* **weight-series** is the name of the series that is used as a weight when
  operating on the data.  This is needed, as recordings do not necessarily
  happen at constant intervals (e.g. 1 second), so the "weight" can be
  different for each data point when calculating an average.  This can be set
  to #f if no weighting is to be done.
* **critical-power** the critical power value used for this session.  If this
  is a bike session, it is a power value in watts, if it is a run session, the
  value is a speed in meters/second
* **wprime** is the value for the W' parameter in the critical power
  equation. If this is a bike session, it is a "work" value in joules, if it
  is a run session, the value is a distance in meters.
* **tau** is the W' reconstitution time constant.  It represents the time it
  takes for 63% of W' to be recovered when the athlete stops exercising
  entirely.
  
For more information about the critical-power, wprime and tau properties, see
the implementation notes on [Critical Power](./critical-power.md)

Properties can be accessed using the `get-property` method.  For example, this
will return the session id for the session data:

    (df-get-property df 'session-id)

Regarding the **laps** and **stop-points** properties, these are lists that
contain time stamp values in the **timestamp** series, to convert them to
indexes, so they can be used to reference positions in other series, use the
`get-index` and `get-index*` methods, and to reference items in other series,
use the `ref` and `ref*` methods:

    (df-get-index df "timestamp" 1471209289))
    => 52561
    (df-get-index* df "timestamp" 1471209289 1471220111)
    => '(52561 58728)
    (df-ref df 52561 "hr")
    => 96.0
    (df-ref* df 52561 "speed" "cad" "pwr")
    => '#(30.636 58.0 103.0)

## Data Series

The data frame object contains several series for the session data.  Some of
the data series come from the database, others some is calculated.  Below is a
brief explanation what each series means.  Not all series will be present in
every data frame, as series are created only if there is data for them in the
session.  To check if a data frame contains some series, use the
`df-contains?` function:

    (df-contains? df "lat" "lon" "alt")
    => #t

### Time Series

* **timestamp** is the UNIX timestamp in seconds when each recording was made
* **timer** counts number of seconds since the start of the activity, but
  ignores stopping times.
* **elapsed** counts number of seconds since the start of the activity, does
  not ignore stop points.  For a point N, this is really timestamp[N] -
  timestamp[0]
* **duration** is the duration of the current point. This is used in lap
  swimming activities, where each point is a length of the pool.

### Position and Distance Series

* **lat**, **lon** are the latitude, longitude coordinates for the point
* **alt**, **calt** are the altitude (as recorded by the device) and corrected
  altitude
* **grade** is grade (incline) at the current point, as a percentage
* **dst** is distance traveled in meters since the start of the activity
* **distance** is traveled since the start of the activity using the most
  convenient metric.  Can be Km or Miles or yards and meters (for swim
  activities)

### Speed and Pace Series

* **spd** is the speed in meters per second
* **speed** is the speed, in a convenient metric (either Km/Hour or
  Miles/Hour)
* **pace** is the pace (time/distance), in a convenient metric (either
  Seconds/Km or Seconds/Mile, or, for swimming, Seconds/100m or
  Seconds/100yd).
* **speed-zone** is the speed zone, of speed zones are defined for the sport.
* **gap** is the Grade Adjusted Pace -- the equivalent pace that can be
  maintained on flat ground for the same level of effort.
* **gaspd** is the Grade Adjusted Speed (in meters/second) -- same idea as
  *gap*, but for speed.
  
Grade Adjusted Pace is calculated using the model described
[here][strava-gap-link].

### Heart Rate Series

* **hr** is the rate as beats per minute (BPM)
* **hr-pct** is the heart rate as percentage of maximum heart rate, if HR
  zones are defined for the sport
* **hr-zone** is the heart rate zone , if HR zones are defined for the sport

### Cadence and Stride Series

* **cad** is the cadence as steps per minute or rotations per minute (for
  cycling)
* **stride** is the length of the step

### Running Dynamics Series

* **vosc** is the vertical oscillation in millimeters
* **vratio** is the vertical ratio, the ratio of **vosc** and **stride**, as a
  percentage
* **gct** is the ground contact time in milliseconds
* **pgct** is the ground contact time as a percentage of the step duration

### Power and Cycling Dynamics Series

* **pwr** is the power, in watts
* **pwr-zone** is the power zone, if power zones are defined for the sport
* **lrbal** left right balance, either for running or for cycling (with a
  power meter)
* **lteff**, **rteff** is the torque effectiveness, for left and right pedal
  respectively
* **lpsmth**, **rpsmth** is the pedal smoothness, for left and right pedal
  respectively
* **lpco**, **rpco** is the platform center offset for left and right pedal
  respectively.  The value is in millimeters.
* **lpps**, **lppe**, **rpps**, **rppe** is the power phase start and end
  angle, for the left and right pedals respectively.  The angle is in degrees,
  where 0 is the top, 180 is the bottom of the pedal stroke.
* **lppa**, **rppa** is the power phase angle for the left and right pedal
  respectively.  The angle is in degrees.
* **lppps**, **lpppe**, **rppps**, **rpppe** is the peak power phase start and
  end angle, for the left and right pedals respectively.  The angle is in
  degrees, where 0 is the top, 180 is the bottom of the pedal stroke.
* **lpppa**, **rpppa** -- is the peak power angle for the left and right pedal
  respectively.  The angle is in degrees.

### W' Balance series

* **wbal** is the W' Balance, calculated using the differential method based
  on a method by Andy Froncioni and Dave Clarke.  For cycle activities, the
  *pwr* series is used as a base for effort, for running activities the *gap*
  series is used.

For more information about the "wbal" series, see the implementation notes on
[Critical Power](./critical-power.md)

### Swim Specific Series

* **swim_stroke** is the swim stroke for the recorded length
* **strokes** is the number of strokes in the length
* **swolf** is the SWOLF metric (duration + strokes)

## Processing of Data in the Data Frame

The series in the data frame are created from data in the A_TRACKPOINT table,
which contains the track points for a session.  In addition, some series are
"derived", that is, computed from already existing series (e.g. stride is
computed from speed and cadence series).  Some series have corrections applied
to them, so data in the data frame is different than what data is available in
the data base.  These corrections makes it easier to work with the data and
should have no detrimental effect, however, the corrections are described
below:

* "pace" series is filtered (smoothed) at the start (first 2 minutes) to
  compensate for erratic readings
  
* "gct", "pgct", "cad", "vosc" and "vratio" series are filtered (smoothed) at
  the start (first 30 seconds) to compensate for erratic readings
  
* "gct" and "pgct" series have their 0 values converted to #f.  It makes data
  processing easier as 0 is an invalid value.

* "lrbal" series has the values 0 and 100 converted to #f.  It makes data
  processing easier as 0 and 100 are invalid values.
  
* "lpps", "lppps", "rpps", "rppps" series have the angle range converted from
  0 .. 360 to -180 .. 180 range, to avoid discontinuity at 0 (when crossing
  from 360 to 0)

## Caching Session Data Frames

Data frame objects are cached in memory, so a second invocation of
`session-df` for the same session id will return a cached object instead of
reading data from the database and re-computing the series.  It is therefore
cheap to call `session-df` repeatedly.

Normally, data frames should not change, however, to force reloading them, the
function `clear-session-df-cache` will invalidate data frames.  When called
with a single argument, a session id, it will invalidate the data frame for a
single session, with no arguments it will invalidate all cached data frames.

## Interactive Use and Exporting Data

**NOTE** we assume the working directory is ActivityLog2/etc, otherwise the
path to "al-interactive.rkt" will need to be adjusted.

Evaluating the code below will open the default database (this is the database
that was last opened in the ActivityLog2 application).  If you haven't opened
a database yet, the code will fail.

    #lang racket
    (require "al-interactive.rkt")

Load the data frame for a session and save it to a CSV file.  The session id
can be obtained from the ActivityLog2 application using the "Activity / Copy
session id to clipboard..." menu:

    (define df (sid->df SESSION-ID))
    (df->csv df "session.csv")

We can also load and save the HRV data for a session, if that data is present:

    (define hrv (hrv->df SESSION-ID))
    (df->csv df "session-hrv.csv")

To open the CSV file in R as a data frame (the Racket code uses #f as the NA
value):

    data <- read.csv("session.csv", header = TRUE, na.strings = c(""))

[strava-gap-link]: https://medium.com/strava-engineering/an-improved-gap-model-8b07ae8886c3
