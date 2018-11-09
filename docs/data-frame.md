# Notes on the data-frame object

Inside the application, the track data for a session is loaded into a
data-frame object.  This object (inspired by the R language data frame) allows
efficient access to data.

## Rationale

The basic idea behind the data frame is to represent observations as a series
of columns where the same measurement points from different observations are
close to each other.

Consider an example: during an activity, data is recorded at periodic
intervals. For example, a device might record every second: time stamp,
latitude, longitude, heart rate, distance, speed, cadence, power, etc.  In a
real activity up to 40 such measurements are recorded for each time stamp. See
[session-df.md](session-df.md) for all the data series that are supported by
ActivityLog2.

A simple approach for representing this data, is to define a "DataPoint"
structure, containing members for each possible values and represent the
entire activity as a vector of data points.  This approach has several
problems:

* If a true structure is used, it will need to have up to 40 or so members,
  but most of the time they would be empty, wasting memory.  Since we never
  know what data might be collected (this depends on the number and types of
  sensors that are active), we cannot save much by defining sub-types (e.g. A
  RunDataPoint or a BikeDataPoint)

* Operations on the data is done for one or only a few measurements at the
  time.  For example, to find the average heart rate, one needs to traverse
  all the data points and look a the "hr" member of such a structure, if the
  structure is big, every reference to the "hr" member will be a cache miss.

A data frame object addresses the problems above by representing the entire
activity as a whole and measurements for the same parameter are stored
together.  Essentially, all heart rate measurements are stored together in a
vector, all cadence measurements are stored together in a different vectors.
All these vectors have the same number of elements (the number of data points)
and the same position in each such vector represents data at a certain point
in time.   This data organization has some advantages:

* Memory is only used for data that actually exists.  For example, if no power
  data is recorded, there will be no power data series in the data frame.

* Operations on the data have efficient memory access.  Calculating the
  average heart rate involves just referencing elements in a continuous
  vector.

## Data frame object overview

The following examples all use a data frame loaded for a session, the
following prelude is assumed:

    #lang racket
    (require "al-interactive.rkt")
    (define sid 1816)
    (define df (sid->df sid))

This will load the data frame for session id 1816 from the default database.

A simple way to get an overview of the contents of the data frame is the
`df-describe` function:

    scratch.rkt> (df-describe df)
    data-frame: 39 series, 4882 items
    properties:
      is-lap-swim?  #f
      session-id    1816
      sport         #(2 #f)
      stop-points   (1480802649 1480803106 1480804370 1480805370)
      weight-series timer
      laps          #(1480802483 1480802896 1480803311 1480803693 1480804096 14808
    series:
                  NAs           min           max          mean        stddev
      alt           0          -5.4          38.8         10.89          9.21
      cad          32             0           114         73.21         23.86
      calt          0           0.1         44.05         16.66          9.28
      distance      0             0         36.17         18.42         10.54
      dst           0           0.9      36171.06       18417.2      10535.92
      elapsed       0           0.5        4950.5       2491.93       1426.36
      grade        10        -38.38         18.26          0.45          3.19
      hr            0            91           169        144.02         12.28

      ### MANY OTHER SERIES OMITED ###

      timestamp     0  1480802483.5  1480807433.5 1480804974.93       1426.36
    scratch.rkt>

## Working with properties

A data frame object can store properties, which are simple key value pairs.
This is used for things such as the session id and the sport type.

* `get-property-names` -- returns a list of property names stored in the
  object
* `put-property`, `get-property` -- are used to set and retrieve properties
* `set-default-weight-series`, `get-default-weight-series` -- are used to set
  and retrieve the special "weight-series" property.  This stores the name of
  the series used by all statistics functions as the weight.  It is most
  useful if samples are not collected at regular intervals.

For example:

    scratch.rkt> (df-property-names df)
    '(is-lap-swim? session-id sport stop-points weight-series laps)
    scratch.rkt> (df-get-property df 'session-id)
    1816
    scratch.rkt> (df-put-property df 'test-property 'hello)
    scratch.rkt> (df-get-property df 'test-property)
    'hello
    scratch.rkt> (df-get-default-weight-series df)
    "timer"
    scratch.rkt>

## Working with series names

The following functions can be used to get information about what series are
available in the data frame:

* `df-series-names` -- returns a list of all the series names
* `df-contains?` -- returns #t if the data frame contains *all* of the specified
  series
* `df-contains/any?` -- returns #t if the data frame contains *any* of the
  specified series
* `df-row-count` -- returns the number of elements in each series of the data
  frame (all series have the same number of elements)

For example:

    scratch.rkt> (df-series-names df)
    '("timer" "lpsmth" "rpppa" "rpps" "rppa" "hr" "lon" "lteff"
      "calt" "lat" "lppps" "lppa" "lppe" "elapsed" "pwr" "dst"
      "rpppe" "grade" "hr-zone" "lpps" "lpppe" "stride" "spd"
      "speed" "distance" "pwr-zone" "timestamp" "alt" "rpsmth"
      "lrbal" "pace" "cad" "rteff" "rppe" "rpco" "rppps" "hr-pct"
      "lpppa" "lpco")
    scratch.rkt> (df-contains? df "timer")
    #t
    scratch.rkt> (df-contains? df "lat" "lon")
    #t
    scratch.rkt> (df-contains? df "non-existent")
    #f
    scratch.rkt> (df-contains? df "timer" "non-existent")
    #f
    scratch.rkt> (df-contains/any? df "timer" "non-existent")
    #t
    scratch.rkt>  (df-row-count df)
    4882

## Accessing the data using `df-select` and `df-select*`

    (df-select df #:filter (filter-fn #f) #:start (start #f) #:stop (stop #f) name)
    (df-select* df #:filter (filter-fn #f) #:start (start #f) #:end (stop #f) . names)

The `df-select` and `df-select*` functions can be used to retrieve data from a
data frame.  The simplest method, is to just ask for the entire series. The
following retrieves the entire set of heart rate values:

    scratch.rkt> (df-select df "hr")
    '#(89.0 93.0 94.0 94.0 94.0 ...)

Data can also be filtered.  For example, the following retrieves only heart
rate values greater than 150 BPM:

    scratch.rkt> (df-select df "hr" #:filter (lambda (hr) (> hr 150)))
    '#(151.0 152.0 152.0 151.0 152.0 152.0 152.0 ... )

A series will contain the value `#f` if there is no data at that point (the NA
value) To filter these out, a predefined function exists, named `valid-only`.
The following only retieves valid HR values:

    scratch.rkt> (df-select df "hr" #:filter valid-only)
    '#(89.0 93.0 94.0 94.0 94.0 ...)

Finally, a subset of the data points can be retrieved by specifying start and
end indexes (see below on how to retrieve useful indexes):

    scratch.rkt> (df-select df "hr" #:start 100 #:stop 105)
    '#(123.0 122.0 123.0 123.0 123.0)

The `df-select*` function can be used to retrieve data from multiple series.
It will return a vector containing a vector for each data point selected.  For
example, the code below can be used to retrieve the GPS track from a data
series:

    scratch.rkt> (df-select* df "lat" "lon" #:filter valid-only)
    '#(#(-22.475327365100384 118.560850918293)
       #(-22.475248826667666 118.5613826662302)
       #(-22.475329376757145 118.56146103702486)
       #(-22.475371956825256 118.56151258572936)
       #(-22.475371873006225 118.56151392683387)
       #(-22.475372292101383 118.56153203174472)
       ...)

## Find positions using `df-index-of` and `df-index-of*`

    (df-index-of df series value)
    (df-index-of* df series . values)

The `df-index-of` and `df-index-of*` functions can be used to find the
position where a value is stored in a data series (the data series will have
to make sorted for this to work).  `df-index-of` retrieves a single position,
while `df-index-of**` retrieves multiple values at once.  For example:

Find the timestamps where the lap start:

    scratch.rkt> (df-get-property df 'laps)
    '#(1480802483 1480802896 1480803311 1480803693 1480804096 1480804492
       1480804870 1480805324 1480805750 1480806117 1480806508 1480806929
       1480807412)

Find the position where the second and third laps start:

    scratch.rkt> (df-index-of df "timestamp" 1480802896)
    392
    scratch.rkt> (df-index-of df "timestamp" 1480803311)
    785
    scratch.rkt> (df-index-of* df "timestamp" 1480802896 1480803311)
    '(392 785)

Extract heart rate data for the second lap:

    scratch.rkt> (df-select df "hr" #:start 392 #:end 785)
    '#(157.0 157.0 157.0 158.0 158.0 158.0 159.0 ...)

Or to extract the GPS track for the second lap, use:

    (df-select* df "lat" "lon" #:start 392 #:end 785 #:filter valid-only)

The same mechanism can be used to find the positions for distances, or time,
etc.  For example to get the positions for the second KM in the activity, use:

    scratch.rkt> (df-index-of* df "dst" 1000 2000)
    '(131 271)

These indexes could be used to retrieve the GPS track for the second KM of the
activity.

## Retrieving individual values using  `df-ref` and `df-ref*`

    (df-ref df index series)
    (df-ref* df index . series)

The `df-ref` and `df-ref*` functions can be used to retrieve a single value
from an index in a series or in multiple series (`df-ref*`).  Using the
examples above, to retrieve the heart rate at the start of the second lap:

    scratch.rkt> (df-ref df 392 "hr")
    157.0
    
And to retrieve the GPS location where the second lap starts:

    scratch.rkt> (df-ref* df 392 "lat" "lon")
    '#(-22.475329376757145 118.56146103702486)
    
*NOTE* unlike the `df-index-of`, `df-index-of*` functions, the `df-ref` and
`df-ref*` methods have the index specified before the series names.

## Lookups using `df-lookup` and `df-lookup*`

    (df-lookup df base-series series value)
    (df-lookup* df base-series series . values)

The `df-index-of` and `df-ref` functions can be combined into a single
function, `df-lookup` which can be used to lookup a value in a base series and
return the corresponding value in a second series.  Continuing the example
above, the heart rate for the start of a lap can be retrieved using a single
call:

    scratch.rkt﻿> (df-lookup df "timestamp" "hr" 1480802896)
    157.0
    scratch.rkt﻿> (df-lookup df "timestamp" '("lat" "lon") 1480802896)
    '#(-22.475329376757145 118.56146103702486)
    
Multiple values can be looked up using `df-lookup*`, which is analogous to
`df-index-of*`:

    scratch.rkt﻿> (df-lookup* df "timestamp" "hr" 1480802896 1480803311)
    '(157.0 133.0)

## Iterating over values using `df-map`, `df-for-each` and `df-fold`

    (df-map df base-series fn #:start (start 0) #:stop (stop (get-row-count)))
    (df-for-each df base-series fn #:start (start 0) #:stop (stop (get-row-count)))
    (df-fold df base-series init-val fn #:start (start 0) #:stop (stop (get-row-count)))

The `df-map`, `df-for-each` and `df-fold` functions are similar to the
corresponding Racket built-in variants, but operate on the values of series.
They take the following parameters:

* `base-series` is either a series name or a list of series names.  The
  iteration will happen over values in these series
* `init-val` (used for `fold` only) is the initial value passed in
* `fn` is a function called on each value.
* `#:start` and `#:stop` allow specifying start and end positions for elements
  that are iterated.
  
The call back function can have one or two arguments for `df-map` and
`df-for-each` and two or three arguments for `df-fold`.

To iterate over a single value at a time, use a function like `(lambda (VAL)
...)`, it will be passed in values from the series packed in a vector.  To
iterate over adjacent pairs of values, specify `(lambda (PREV-VAL VAL) ...)`,
it will be passed in the current and previous set of values.  The variants
used for `fold` use the accumulator as a first argument: `(lambda (ACCUM VAL)
...)`, or `(lambda (ACCUM PREV-VAL VAL) ...)`.

For example, the following function can be used to calculate the work (in
Joules) from the time and the power series.  The function receives pairs of
data points and determines the amount of work (power * delta-time) and adds it
to the accumulated value:

    (define (accum-work prev-work prev-val val)
      ;; for the first element, there will be no previous value
      (if prev-val
          (match-let (((vector time1 power1) prev-val)
                      ((vector time2 power2) val))
            (if (and time1 power1 time2 power2) ; all values are valid
                (+ prev-work (* (* 0.5 (+ power1 power2)) (- time2 time1)))
                prev-work))
          prev-work))
      
    scratch.rkt> (df-fold df '("timer" "pwr") 0 accum-work)
    796091.0

## Adding new series using `df-add-derived` and `df-add-lazy`

    (df-add-derived name base-series value-fn)
    (df-add-lazy name base-series value-fn)

The `df-add-derived` function can be used to add new series to the data frame,
as computations from other series.  It is used in session-df for example to
create a "distance" series (which is either in KM or Miles") from the "dst"
series which is in meters.

The function takes the following parameters:

* `name` -- name of the new data series
* `base-series` -- is either a series name or a list of series names.  The
  iteration will happen over values in these series
* `value-fn` -- function to produce values for the new series it has the same
  signature as the function passed to `map` or `for-each`

The example below, adds the accumulated work at each point in the bike ride:

    (define current-work 0)
    (define (add-work prev-val val)
      ;; for the first element, there will be no previous value
      (when prev-val
        (match-let (((vector time1 power1) prev-val)
                    ((vector time2 power2) val))
          (when (and time1 power1 time2 power2) ; all values are valid
            (set! current-work (+ current-work
                                  (* (* 0.5 (+ power1 power2)) (- time2 time1)))))))
      current-work)

    scratch.rkt> (df-add-derived df "work" '("timer" "pwr") add-work)
    scratch.rkt> (df-select df "work")
    '#(0 0.0 0.0 0.0 50.0 241.5 553.0 891.5 1208.0 1439.0 ... 796091.0)

The `df-add-lazy` is the lazy version of the function: it adds a closure to
the data frame and the data series will be created the first time it is
referenced.  Special care needs to be used with this function, especially if
it captures local variables, as the environment in which the function runs
might not be the same as the non-lazy version.

## Other useful functions

    (df-write/csv outp df . series)
    
    (df-histogram df series
        #:weight-series [weight (send df get-default-weight-series)]
        #:bucket-width [bwidth 1]
        #:trim-outliers [trim #f]
        #:include-zeroes? [zeroes? #t]
        #:as-percentage? [as-pct? #f])

    (df-statistics df series
        #:weight-series [weight (send df get-default-weight-series)]
        #:start (start 0)
        #:end (end (send df get-row-count)))

    (df-quantile df series
        #:weight-series [weight (send df get-default-weight-series)]
        #:less-than (lt <)
        . quantiles)

    (df-mean-max df series
        #:inverted? (inverted? #f)
        #:weight-series [weight "elapsed"]
        #:durations [durations default-best-avg-durations])

    (df-mean-max-aux df series best-avg-data
        #:weight-series [weight "elapsed"])
