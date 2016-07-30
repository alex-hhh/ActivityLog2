This document gives an overview on how HRV data is recoded by a Garmin device
and how it can be used by ActivityLog2.

## Recording HRV data ##

Garmin devices can record HRV data in the activity FIT files.  This needs to
be explicitely enabled on the device, but usually there is no option you can
set on the device itself.  ActivityLog2 allows creating a settings file that
can be exported to the device and this can be used to enable HRV recording.
Alternatively, there are various web sites describing how to do this for each
device.

## Reading HRV data ##

Since the device records the time interval between every heart beat, it can
contain a lot of samples, so HRV data is not read during normal FIT file
parsing and only summary HRV metrics are put into the database.  

The actual HRV data can be read in a data-frame% by two functions (defined in
"hrv.rkt"):

* make-hrv-data-frame/file -- will read a FIT file
* make-hrv-data-frame/db -- will read the FIT data stored in the database

Both functions will return #f if the FIT file does not contain HRV data.  If
you are using "al-interactive.rkt", the ``hrv-df`` provides a convenient
shortcut.  Evaluating the following program:

    #lang racket
    (require "al-interactive.rkt")
    
Will give an interactive prompt which allows loading, inspecting and saving
HRV data into CSV files:

    > 
    Opened C:\Users\alexh\AppData\Local\ActivityLog\Alex-AL.db
    scratch.rkt> ;; HINT: Use "Activity/Copy sesion id to clipboard" menu
    scratch.rkt> (define df (hrv-df 1717))
    scratch.rkt> (df-describe df)
    data-frame: 4 series, 6223 items
    properties:
    series:
                  NAs           min           max          mean        stddev
      bpm           0            85           141        113.55         11.44
      delta-hrv     0             0         15527         34.75        272.65
      hrv           0           141         17040        607.45         348.7
      timestamp     0    1468032094    1468035492  1468033833.8       1107.83
    scratch.rkt> (df->csv df "hrv.csv")
    scratch.rkt> 

The HRV data-frame% contains 4 series:

* "timestamp" is a UNIX timestamp on when the recording was made.  This series
  is ordered. Since there are several HRV values recorded every second, and
  the timestamp resolution is 1 second, there will be duplicates in this
  series.
* "hrv" represents the time in milliseconds between heart beats, as recorded
  by the device.  The series is stored as read from the FIT file and might
  contain bad values (e.g. missing heart beats are recorded as unusually large
  HRV values).
* "delta-hrv" represents the difference between adjacent HRV values.
* "bpm" is the HR data read from the FIT file.  Can be used to correlate HRV
  values with the HR data recorded by the device.

The function ``compute-hrv-metrics`` in hrv.rkt can be used to compute some
common HRV metrics, the results of this computation is stored in the database.
The database stores metrics for the entire activity, but they can be computed
for subsets.  For example, to compute the metrics for the first 5 minutes of
the activity:

    scratch.rkt> ;; HINT: 1468032094 is the min value of "timestamp", see df-describe
    scratch.rkt> (match-define (list start end) 
                   (send df get-index* "timestamp" 1468032094 (+ 1468032094 300)))
    scratch.rkt> ;; HINT: see hrv-metrics struct definition (hrv.rkt) for the fields
    scratch.rkt> (compute-hrv-metrics df #:start start #:end end)
    (hrv-metrics
     63
     121
     69
     126
     0.12340842311459353
     298
     0.29187071498530853
     1014
     5209)
    scratch.rkt> 
