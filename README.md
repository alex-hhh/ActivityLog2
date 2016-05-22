# This is ActivityLog2, a fitness activity tracker. #

The application can read sport activities from .FIT files (as produced by
Garmin and some other devices) and display various graphs and allow reportings
on them.

## Features ##

* Can import FIT activity files (Run, Bike, Swim activities).  Other
  activities such as Hiking are also supported.

* Can import multisport activitites

* Supports the usual data series (speed, heart-rate, cadence, power), also
  supports advanced run metrics (Vertical Oscilation, Ground Contact Time,
  etc) and bike metrics (Torque Efficiency, Pedal Smoothness, Power Phase,
  etc).
  
* Several plots for the data series: time/distance, scatter, histograms,
  best-avg
  
* Sports maps (map tiles from openstreet map) and weather data

* Equipment tracking, both manual and automatic.  ALso supports service
  reminders.
  
* Reports and trends charts
  
* Can generate FIT settings files for HR, speed and power zones.  These can be
  sent to the device directly.
  
### Developer features ###

* Data is stored in a SQLite database, allows accessing the activity data from
  any language.  Most report views allow exporting the SQL query to give a
  starting point for writing your own reports/data processing.
  
* FIT file reader is easy to use to read any FIT file type (e.g. workout,
  settings or monitoring files).
  
* FIT file writer for various file types.  A writer for FIT workouts is also
  available.
  
* Session data can be easily loaded into a data frame to be manipulated inside
  Racket programs.
  
## Building and running the application ##

To build the application, you will need to install Racket from
http://www.racket-lang.org.  Once Racket is installed, open the file "run.rkt"
and select "Racket/Run" (Ctrl-R).  You will be prompted to open an existing
database or create a new one.  Once you have a database, you can import FIT
files from Garmin (and other devices).

You can also build a stand-alone distribution.  To do that, open the file
"build.rkt", evaluate it (Ctrl-R), than run the commands:

    (build-app)
    (mkdist)

This will create a stand-alone application in a folder ../ActivityLog2-dist.
This application will run even on machines that don't have Racket installed.
