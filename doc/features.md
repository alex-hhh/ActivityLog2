## Features ##

* Can import FIT activity files (Run, Bike, Swim).  Other activities such as
  Hiking and Skiing are also supported. Can import multisport activities
  (triathlons, duathlons)

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
