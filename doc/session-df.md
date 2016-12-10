# Some notes on using session data frames

Session data is loaded into a `data-frame%` object (defined in
"data-frame.rkt") by `session-df`.  Data frames are used to store in memory
the data for the activities, all data operations are done on these data frame
objects

This document describes the contents of the data frame object as created by
`session-df`

## Properties in a data frame

The following properties are present:

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

Properties can be accessed using the `get-property` method.  For example, this
will return the session id for the session data:

    (send df get-property 'session-id)

Regarding the **laps** and **stop-points** properties, these are lists that
contain time stamp values in the **timestamp** series, to convert them to
indexes, so they can be used to reference positions in other series, use the
`get-index` and `get-index*` methods, and to reference items in other series,
use the `ref` and `ref*` methods:

    (send df get-index "timestamp" 1471209289))
    => 52561
    (send df get-index* "timestamp" 1471209289 1471220111)
    => '(52561 58728)
    (send df ref 52561 "hr")
    => 96.0
    (send df ref* 52561 "speed" "cad" "pwr")
    => '#(30.636 58.0 103.0)

## Series in the session data frame

The data-frame% object will containing several series for the session data.
Some of the data comes from the database, others some is calculated.  Below is
a brief explanation what each series means.  Not all series will be present in
each data-frame, as series are created only if there is data for them in the
session.  To check if a data frame contains some series, use the `contains?`
method:

    (send df contains? "lat" "lon" "alt")
    => #t

### Time related fields

* **timestamp** is the UNIX timestamp in seconds when each recording was made
* **timer** counts number of seconds since the start of the activity, but
  ignores stopping times.
* **elapsed** counts number of seconds since the start of the activity, does
  not ignore stop points.  For a point N, this is really timestamp[N] -
  timestamp[0]
* **duration** is the duration of the current point. This is used in lap
  swimming activities, where each point is a length of the pool.

### Position and distance related fields

* **lat**, **lon** are the latitude, longitude coordinates for the point
* **alt**, **calt** are the altitude (as recorded by the device) and corrected
  altitude
* **grade** is grade (incline) at the current point, as a percentage
* **dst** is distance traveled in meters since the start of the activity
* **distance** is traveled since the start of the activity using the most
  convenient metric.  Can be Km or Miles or yards and meters (for swim
  activities)

### Speed related fields

* **spd** is the speed in meters per second
* **speed** is the speed, in a convenient metric (either Km/Hour or
  Miles/Hour)
* **pace** is the pace (time/distance), in a convenient metric (either
  Seconds/Km or Seconds/Mile, or, for swimming, Seconds/100m or
  Seconds/100yd).
* **speed-zone** is the speed zone, of speed zones are defined for the sport.

### Heart rate related fields

* **hr** is the rate as beats per minute (BPM)
* **hr-pct** is the heart rate as percentage of maximum heart rate, if HR
  zones are defined for the sport
* **hr-zone** is the heart rate zone , if HR zones are defined for the sport

### Cadence and stride related fields

* **cad** is the cadence as steps per minute or rotations per minute (for
  cycling)
* **stride** is the length of the step

### Running dynamics fields

* **vosc** is the vertical oscillation in millimeters
* **vratio** is the vertical ratio, the ratio of **vosc** and **stride**, as a
  percentage
* **gct** is the ground contact time in milliseconds
* **pgct** is the ground contact time as a percentage of the step duration

### Power related fields

* **pwr** is the power, in watts
* **pwr-zone** is the power zone, if power zones are defined for the sport
* **lrbal** left right balance, either for running or for cycling (with a
  power meter)
* **lteff**, **rteff** are the left and right torque effectiveness
* **lpsmth**, **rpsmth** are the left and right pedal smoothness
* **lpco**, **rpco** are the left and right platform center offset, in
  millimeters
* **lpps**, **lppe**, **rpps**, **rppe** are the left and right power phase
  start/end, as an angle in degrees, where 0 is the top, 180 is the bottom of
  the pedal stroke.
* **lppa**, **rppa** are the left and right power phase angles, in degrees
* **lppps**, **lpppe**, **rppps**, **rpppe** are left and right peak power
  phase start/end
* **lpppa**, **rpppa** -- left and right peak power phase angles

Swim activities also contain the following:

* **swim_stroke** is the swim stroke for the recorded length
* **strokes** is the number of strokes in the length
* **swolf** is the SWOLF metric (duration + strokes)

## Interactive use and exporting data

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

    (define hrv (hrv-df SESSION-ID))
    (df->csv df "session-hrv.csv")

To open the CSV file in R as a data frame (the Racket code uses #f as the NA
value):

    data <- read.csv("session.csv", header = TRUE, na.strings = c(""))

