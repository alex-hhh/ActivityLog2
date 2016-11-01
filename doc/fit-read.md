The "fit-file.rkt" library provides convenient infrastructure for parsing FIT
files.  It also provides an activity builder which allows reading activities
from FIT files into a assoc list representation.

## Reading Activities (the easy way) ##

To load an activity from a file:

    (require "fit-file.rkt")
    (define activity (read-activity-from-file "activity.fit"))

The returned ACTIVITY contains a nested ALIST which can be explored using
`assq'.

Some terminology:

* An ACTIVITY contains a number of SESSIONS.  Most of the time, there will be
  only one session, but for multi-sport activities, there will be multiple
  sessions, one for each leg of the sport.
* A SESSION has a number of LAPS.  This corresponds to each lap recorded by
  the device.
* A LAP has a number of LENGTHS.  Most of the time, there will be only one
  length, but swim activities have several pool lengths for a LAP
* A LENGTH has a number of TRACKPOINTS.  Each track point represents a
  recording of the monitored parameters (speed, heart rate, cadence, etc) at a
  moment in time.

Since the activity object is hierarchical, it can be difficult to access the
entire data series for an activity (individual track points).  Helper functions
are provided for iterating over the track points in an activity and session:

    (require "activity-util.rkt")

    ;; Obtain the heart rate data series from an activity
    (define hr-series
      (map-activity-trackpoints activity
         (lambda (prev next)
           (vector (assq1 'timestamp next)
                   (assq1 'heart-rate next)))))

The `map-activity-trackpoints` function applies a function to each (PREV,
NEXT) pair of trackpoints in an activity.  This allows processing points
individually, by looking just at the NEXT track point or in pairs, for
computing deltas.

Other useful helpers are:

* for-each-activity-trackpoint
* map-activity-trackpoints
* for-each-session-trackpoint
* map-session-trackpoints
* for-each-lap-trackpoint
* map-lap-trackpoints
* for-each-session-length
* map-session-lengths

## Reading other FIT files ##

The general mechanism for reading FIT files requires the following:

* Define a "builder/consumer" object by deriving from fit-event-dispatcher%
* Create a fit stream
* Use `read-fit-records` to process the FIT file and dispatch records to the
builder object.

For example, to read a workout file, we can define the following builder
object:

    (define workout-builder% 
      (class fit-event-dispatcher% (init) (super-new)

        (define/override (on-file-id file-id) (printf "file-id: ~a~%" file-id))
        (define/override (on-file-creator creator) (printf "file-creator: ~a~%" creator))
        (define/override (on-device-info device-info) (printf "device-info: ~a~%" device-info))
        (define/override (on-workout workout) (printf "workout: ~a~%" workout))
        (define/override (on-workout-step workout-step) (printf "workout-step: ~a~%" workout-step))

        (define/override (on-event event) (printf "event: ~a~%" event))
        (define/override (on-other type data) (printf "other: ~a ~a~%" type data))))

    (define (read-wk-from-file file)
      (let ((stream (make-fit-data-stream-from-file file))
            (builder (new workout-builder%)))
        (read-fit-records stream builder)))

Note that `read-fit-records` does not return anything.  It is up to the
"builder" object to construct the object.  The above example will just print
out the workout steps.

See `fit-event-dispatcher%` for all the message types that can be handled.
For unknown messages, on-other is invoked.  A familiarity with the FIT file
format is useful when trying to parse FIT files.
