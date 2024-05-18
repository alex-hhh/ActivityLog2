# ActivityLog2 - analyze data from swim, bike and run activities

    https://github.com/alex-hhh/ActivityLog2

Import swim, bike and run and other activities from .FIT files.  Display
plots, scatter plots, histograms, maps and other views for activities.  Show
reports and trends from activity data and track equipment usage.

This file contains a high level summary of changes in each released version of
the application.

# Release 2024.05 (May 2024)

* Show traffic information (other vehicles overtaking the rider) on the map.
  This feature uses data collected by the MyBikeTraffic Garmin IQ application,
  see https://www.mybiketraffic.com/about/ from a Garmin Varia bike radar.
  Data is overlaid on the map for each session where it is available.

* Allow multiple instances of the ActivityLog2 and AL2-Climb-Analysis
  applications to run simultaneously.  This allows databases for two or more
  athletes to be opened at the same time.

* Application settings and preferences are now stored in the database,
  allowing separate settings for charts and displayed fields for different
  user databases.

* Update FIT file reader to handle lap records at the start. New Garmin
  firmware generates FIT files which have out-of-time-order lap messages
  before the track points.  This update will correctly assign track points to
  the correct lap, instead of creating an additional lap to hold these
  points. (AB#57)

* Show equipment names from fit-product-defs file, so product names are
  up-to-date when fit-product-defs itself is updated.  If the user names their
  devices explicitly, they will still show with the user-specified name.  Also
  updated fit-product-defs to include latest devices.

* Graph for "Left-Right Balance" series uses 50% when values are missing from
  the data -- if a value is missing, it is better to assume neutral balance
  instead of an "all the way to the left", which 0% would produce.

* Use interpolated lookup in GPS Segments view.  Usually there is a large
  distance between adjacent data points in GPX segment files, and the point
  might show up in the wrong place on the plot if we always use an exact data
  point.

## Bugfixes

* Fix segment matching so a single match is found on a particular route
  segment.  If there is a U-Turn on a route, the segment could be matched
  twice: both before and after the U turn.  (AB#61)

* In the session view, a previously highlighted lap is removed from the graphs
  and map view when switching segment types, to avoid showing misleading data
  in these views.  (AB#60)

* Make al2-climb-analysis tool more resilient to printing bad values.  Wrap ~r
  so it does not throw an exception if it has to print a non-rational number,
  such as +inf.0

* Ensure the calculated HRV metrics have valid values and return #f if they
  don't.  This avoids returning +nan.0 or other values which throw exceptions
  later on. (#98)

* Workaround incorrect device serial produced by ELEMNT Bolt which uses signed
  32 bit numbers for device serials, causing truncation for some serial
  numbers, so they were not found in the database -- this caused creation of
  duplicate entries.  (AB#56)

* Fix W'Bal series going above max value.  Maximum value should be WPrime, but
  on longer stops the calculation could produce a value larger than
  that. (AB#55)

* Make data for AE trend chart more resilient to bad values.  Some devices
  import speed, power or HR as 0 when they don't have data, instead of using
  the "not available" flag

# Release 2024.01 (January 2024)

* More improvements to PMC trend chart: legend entry now shows data for today,
  if today's date is covered by the PMC chart range; added Ramping Rate, the
  average increase in CTL over the past 4 weeks; Hovering over TSS "dots" on
  the plot shows the activity, or activities, on that day.

* New al2-activity-import command line import utility allows setting up
  scripts to automatically import activities.

* The ActivityLog2 application now accepts the database on the command line
  and AL2-Climb-Analysis to accept the GPX route file on the command line.
  This allows setting up scripts to open separate activity databases.

* A map can be added to the charts view, this allows showing the map next to
  all the other data series (#66)

* Update FTHR settings for percentage and duration, after re-reading Friel's
  book.  Threshold Heart Rate is 95% of the highest average 20 minute HR.  For
  running use 100% of the 30 minute highest HR.

* Update Aerolab parameter estimation: use an exponential "temperature" drop
  instead of a linear one and also transition all parameters between states
  instead of just one of them.  This seems to find better matches for CdA and
  Crr.

* Fix a bug with reading FIT files containing extra data after the FIT file
  information (#96), as well as FIT files which have bad developer field
  definitions.

* Fix a bug that where very long device names were created on activity import,
  in some degenerate cases, causing the application to fail to start.

* Fix a bug that prevented the user from saving CP2 parameters in the
  "Athlete/Edit Critical Power..."  dialog. (AB#53)

* Fix a bug preventing saving of sport zones in the "Athlete/Edit Sport
  Zones..." dialog (AB#52)

* "Sensor Battery low" don't include voltage -- some sensors don't report the
  voltage when reporting battery low condition and this caused an application
  crash.

* Add extra checks to prevent crashes and errors when data is missing...
  graphs used to expect that data such as heart rate or power is always
  available, or that sport zones always exist for an activity.  This caused
  errors and crashes when such data was missing.  Attempted to fix the cases I
  found.

# Release 2023.09 (September 2023)

* Fix a bug where session inspector would fail to load when there were no
  previous UI settings saved -- this is the case in new installations, and
  this prevented the use of ActivityLog2 in such cases

* Wattbike generated FIT files can now be imported without breaking the lap
  selector in the session inspector.  These FIT files contain unusual encoding
  for timestamps, which broke the laps selector.

* "Performance Management Chart" (PMC) had some visual improvements and some
  internal refactoring of the code.  Has nicer colors and hovering over
  individual TSS points shows the corresponding activity, or activities.

* "Combined Pedal Smoothness" data series is now imported from FIT files --
  this data is generated by some power meters.

# Release 2023.07 (July 2023)

* Add Aerolab analysis for sessions (AB#41). This implements estimating the
  Coefficient of Drag Area (CdA) and Coefficient of Rolling Resistance (Crr)
  using the method described in "estimating CdA with a power meter" by
  R. Chung.

  This is the initial implementation, which has been used for a limited amount
  of testing.

  For more details about this feature, see
  [Aerolab](https://github.com/alex-hhh/ActivityLog2/wiki/Aerolab).

* Add climb rate as a metric for laps and GPS segments, this measures the
  vertical climb speed for a segment (most useful for climb sections) and it
  is a good indicator for how hard you are climbing.  It ISO Power (normalized
  power) is a better metric, but that one requires a power meter.

* Improvements to Critical Power model fitting to produce more realistic
  estimations when there is poor data at shorter time intervals. Also avoid
  crashing the application when the CP parameters cannot be determined.

* Fix Swimming MMAX plot range by converting between a m/s value into an
  appropriate min/km, min/100m, etc values, so the numeric limits for the plot
  are appropriate.

* New graphs for the session inspector: heart rate, power/speed reserve and
  aerobic decoupling graphs.  The "reserve" graphs show a 30second rolling
  average, of heart rate reserve (percentage between min and max heart rate),
  or power/speed threshold (CP, CV).  Aerobic decoupling shows the ratio
  between heart rate reserve and power or speed reserve.

* New sport types: Kayak and SUP. Hiking and Sailing sport IDs were re-mapped,
  so they are correctly labeled when importing FIT files with these
  activities.

* Improvements to AL2-Climb-Analysis tool: added ability to read binary FIT
  course files, fixed a bug for course files with points in the same location,
  and added a summary (distance, ascent, descent) to the header

* Add option to track/not track location on the map in the session inspector,
  since scrolling the map each time the mouse goes over the elevation plot can
  sometimes be annoying.

* Update activity summaries in the segment match view when they change
  (AB#50).  Changes to activities, like changing the headline or removing an
  activity are detected by the segment match list and they are updated there
  as well.  This avoids potential bugs where a deleted activity is still shown
  in the view and the user attempts to select it.

* Fix bug preventing saving updated Sport Zones in the sport zone editor

* graphs in the graph selector dialog for the session inspector are now sorted
  alphabetically and easier to find

* graphs in the session inspector are now sub-sport specific, allowing, for
  example to have different graphs for Road Cycling vs Indoor Cycling.

* Changed the "Average Line" to "Redline" in the graphs to indicate that we
  don't always display the actual average for that setting.

* Changed default missing value to #f instead of 0 for some graphs, so the
  graph does not drop to 0 for a missing value.

# Release 2023.03 (March 2023)

* Removed automatic download of weather data (#46).  The weather data provider
  DarkSky will discontinue their service, and no suitable replacement was
  found. The linked issue contains more details, but currently weather data
  can be added manually or imported from FIT files, for devices that add this
  info to the activity.

* Fixed a bug with viewing Open Water Swim activities (#89)

* Changed pace for Open Water Swim activity graphs to min/100m or min/100yd

* The map view occupies the entire view when there is no elevation data, such
  as with Open Water Swim activities.

* Changed how RPE is shown, making it more easily editable from the session
  inspector.  Also added names to numeric values for RPE.

* Removed the drop-down selection for the activity time zone in the session
  inspector.  The time zone can still be edited from the "Activtiy/Edit..."
  dialog.

* Fixed a race condition in the FTHR analysis dashboard, which caused it to
  show an error.

* The Critical Power estimation chart is now significantly faster.

* Added an "Activity/Show session data frame summary..." menu item.  This
  shows a description of the data frame for the activity, and can be used do
  determine what data series are present and how many have NA values -- this
  is mostly a debugging tool for the data files.

* Fixed a bug that caused an error to be reported when activities are renamed
  (AB#47)

* Fixed a build issue that caused only Open Street Map tiles to be available
  in the map view.  Additional map tiles are now available from the
  Edit/Preferences dialog.

* Fixed a bug that caused corruption in the map tiles -- original issue in one
  of the packages used by the application:
  https://github.com/Bogdanp/racket-http-easy/issues/21

* Fixed a bug with grade series calculations when the route goes through a
  tunnel and GPS signal is lost.

* Lap views which show climbs (Hill Climbs, GPS Segments) now have a FIETS
  column, The FIETS score is a number indicating the difficulty of the climb.

* Altitude series will now correctly use the imperial system (feet), (#9)

* Updated list of device names, so more devices are recognized by name.

* More FIT devices are recognized in activities, so they can be tracked as
  equipment.

# Release 2022.09 (September 2022)

* Added a new application icon.

* Added AL2-Climb-Analysis tool to the installer, this was a separate project
  at https://github.com/alex-hhh/AL2-Climb-Analysis

* Timezone database updated to latest version.

* Fixed a bug where "Cycling" sport zones were not used for sub-activities
  such as "Road Cycling" or "Indoor Cycling"

* Fixed a bug that caused an error to be reported when trying to sort views on
  columns containing some empty fields.

* Fixed the CSV exporter to correctly quote values instead of creating
  potentially broken CSV files.

* Fixed a bug that could cause an error to be reported when deleting
  activities.

* Fixed a bug that caused caused an error to be reported when switching to
  Trends view when no trends charts are available (this was fixed by updating
  to Racket version 8.6)

# Release 2022.05 (May 2022)

* improvements to the "smoothing" step of the elevation correction algorithm
  -- the result is much smoother elevation plots, without loosing the peaks
  and valleys.

* limit the range of the elevation plot in the GPS segments view, plots look
  better when the segment does not start at or near zero elevation.

* weather data records can now be read from activity files -- this is some
  work towards addressing #46, however, except for a few sample files, I could
  not get my Garmin to record this data...

* improve responsiveness for the heat map trend chart, while the chart is
  loading.  Also improved the color range, so more frequent routes are better
  highlighted.

# Release 2022.01 (January 2022)

* Temperature data is now read from FIT files and shows up in summary data as
  well as graphs for a session.

* Various fixes to the Lap Swim Editor, to work with Lap Swims which have HR
  data.

* Varius fixes to importing Lap Swim activities from recent Garmin Watches,
  including recording Heart Rate data from wrist based sensors.

* Various fixes to GPS segments: show both elapsed and moving time for a
  segment match, make the code more resilient to GPX files form different
  sources, fixes a bug where the grade was incorretly colored for a segment
  and added FIETS score to the data about a segment.

* The Chez Scheme based Racket implementation is now used for the build
  (currently Racket 8.3 CS)

# Release 2021.10 (October 2021)

* Changed release versioning scheme to use YEAR.MONTH -- this is more meaning
  full to the end user and should be more obvious if they use an old version.

* Fixed issue #73 where an activity would fail to import when the user stopped
  running while keeping the watch recording.


* Fixed issue #74 where heard rate samples would be associated with the wrong
  pool length in lap swimming activities.

* Heart Rate data is now shown for lap swimming activities -- currently this
  only works for activities recorded with a device that measures heart rate at
  the wrist.   See issue #4

* Updated the tab bar in the Trends view to allow reordering tabs by dragging
  them.

* The default set of columns visible in various views has been restricted to a
  smaller subset to make the activities and lap views easier to follow.  This
  only affects new users of the application and the visible columns can be
  edited using the "View/Setup Columns..." menu item.

* Added support for GPS segments -- these allow defining sections of a route
  (e.g. a hill climb) and inspecting all activities that traverse that
  section, allowing comparing time, power, speed, etc. on the given segment.

# Release 1.12.0 (June 2021)

* Added 100 meter splits as custom intervals for sessions (this is useful for
  open water swims (#67)

* GPX export will now also export heart rate, cadence, speed, power and
  distance data when this is available.

* For some trend charts, the user can choose session labels to show on the
  chart and timestamps of the sessions containing the selected labels will be
  marked on the plot.  Charts supporting this feature are the body weight,
  aerobic decoupling, PMC, time in zone, training volume and triathlon
  training volume.

  This allows, for example, to mark races on the performance management chart,
  by marking the race sesssion with th "race" label and selecting the "race"
  label on the PMC chart.  Given that "note" activities with future dates can
  be created and assigned labels, this feature also allows marking future
  races or other evens on the chart.

* New plot type for elevation, shading the area under the plot with colors
  that depend on the grade (more intense colors for higher grades) (AB#37)

* fix a bug which used too many network resources when made map tile were
  loaded (#68)

* fixed a bug which caused an error to be reported when the user deleted an
  activity (#70, AB#39)


# Release 1.11.0 (April 2021)

* Heat maps in trend charts can now render a large number of data points
  (tested with 4 million data points, or about 2500 activities, but should be
  able to handle larger numbers)

* Improvements to elevation correction algorithm, will work better for flat
  routes and the algorithm will handle bad data points better.

* Also added an preference option to avoid calculating corrected elevation on
  import.  Corrected elevation can still be done on individual activities.

* Session labels can now be selected as a column in the activities view (#62,
  #61)

* Added tool to clear spikes from power data series, the tool is available
  from the "Activity/Clear Power Spikes..." menu option (AB#31)

* Aerobic Decoupling and ISO Power are now calculated for custom lap types
  (such as Hill Climbs) and can be viewed in the "Laps" tab of the session
  inspector.

There are various other bug fixes and small usability improvements.  For a
detailed log, see the git log between the v1.10.2 and v.11 tags.

# Release 1.10.2

Bugfix release, fixing the following bugs:

* Fix importing FIT files which contains 64bit data fields, see also #50

* Fix importing FIT files produced by the Coros2 watch

# Release 1.10.1

Bugfix release, fixing the following bugs:

* Fix workout import for workouts containing "open" steps (until lap button
  press), see also #49

* Fix sorting by battery voltage in the equipment view, see also AB#25

* Speed-up rendering of charts which are colored by zone, see also AB#24

# Release 1.10.0

New release containing assorted new features and bug fixes, the most important
of them are:

* New look for the time/distance charts for the session inspector.  Plots are
  now shown in multiple columns and dual series plots are available (for
  example Power + W'Bal or Elevation + Grade).

* Add support for the 3 parameter Critical Power model, both in estimating it
  in the MMAX Trends Chart and when setting up athlete CP values for display
  in the session inspector (the Athlete / Edit Critical Power... menu)

* Elevation correction fixes: when averaging elevation, only different passes
  over the area are considered, avoiding averaging against nearby points from
  the same run.  This results in better corrected elevation values.

* Avoid spikes in grade (slope) series: fixed a bug which caused unrealistic
  spikes in the grade series

* Fixed a bug in the FTHR Estimation dialog where the HR zones were showed
  instead of the pace or power ones.

* Improve readability of "hover labels" which displayed in plots: more
  information is added to the labels and they are less transparent.

# Release 1.9.0

New release containing assorted new features and bug fixes, the most important
of them are:

* ActivityLog2 will display the local time where the activity took place.
  Timezones are detected from GPS coordinates and can be set manually for
  other activities.  See #11 and the [blog
  post](https://alex-hhh.github.io/2019/10/local-time.html) about this
  feature.

* A new "Functional Threshold Analysis Dashboard" for determining sport zones.
  For more details, see [this blog
  post](https://alex-hhh.github.io/2020/05/threshold-analysis-in-activitylog2.html)

* New "Injury Risk" Trend Chart, based on [this blog
  post](https://alex-hhh.github.io/2019/02/data-visualization-dashboard.html)

* Scatter plots in trend charts now support dual series such as Torque
  Effectiveness and Pedal Smoothness.  These are power-related series,
  recorded for the left and right pedal separately.  When such a series is
  selected, two plots will be displayed side-by-side, one for the left pedal,
  one for the right pedal.  See also #3.

* Fix a bug which caused incorrect segment to be highlighted when selecting
  laps in the Graphs View.  See also AB#3.

* Map view will be correctly zoomed when the first activity is opened.  See
  also AB#7.

* Fix an error being displayed when trying to fetch weather data when the
  weather data download was disabled in the "Preferences" dialog.  See also
  AB#18.


# Release 1.8.2

This is a bug fix release containing the following fixes:

* Correctly assign XDATA series to applications when activities are recorded
  using more than one Connect IQ application.

* Fix exception when trying to sort a view using a column which contained
  empty cells.

# Release 1.8.1

This release updates the Windows installer to allow installing the application
for the current user only.  This will avoid the intimidating Windows UAC
prompt which warns the user that the application is trying to modify the
computer.

**NOTE** If a previous version if ActivityLog2 was already installed, it is
recommended to un-install it first before installing this version.

# Release 1.8.0

* add support for enhanced altitude and speed records in FIT files -- newer
  Garmin devices create FIT files with altitude and speed stored in these
  types of records.

* add support for reading compressed FIT files which some devices produce.

* the application now contains a large database of device names and there's a
  greater chance that the device will be correctly named when it is seen the
  first time.  Existing devices will not be renamed, but the user can rename
  these to anything they wish.

* A new [HeatMap Trends
  chart](https://alex-hhh.github.io/2019/09/interactive-heat-maps.html), this
  fixes issue #7.

* A new layout for the Model Parameters inspector page, ranges for sport zones
  are more clearly identified

* (Bug Fix) display a "No Data to Plot" message when attempting to display a
  plot for a data series which does not exist in the activity.

* More accurate detection of hill climb and descent sections in an activity.

## Build Improvements

Continuous Integration Build improvements -- this is more of a "behind the
scenes" change, but has the following user visible changes:

* A windows installer is now built for every changes pushed to the master
  branch.  This is not however published as a release.

* The version number produced by an Azure DevOps build now contains the build
  number as the fourth value (e.g. 1.8.0.200).  The build number should always
  change even if the same version is built a second time.

* The windows installer is now signed with GnuPG.  This is an experimental
  feature, but if you wish to verify the signature see [this wiki
  page](https://github.com/alex-hhh/ActivityLog2/wiki/Signing-Releases).  Note
  that Windows will still warn when attempting to install the application, as
  I don't want to spend money purchasing a signing certificate for a hobby
  project, sorry.

* On windows, the application executable now contains a proper name and the
  version number of the application.  This makes it easier to identify the
  application in process explorer.

# Release 1.7.1

This is a bug fix release, fixing the following bugs:

* Fix bug which prevented editing effort based on Heart Rate zones, see issue
  #45

* Fix bug which prevented entering athlete metrics (bodyweight, etc) on an
  empty database, see issue #44

* Fix bug which prevented entering manual weather information from an
  activity.

* Fix bug which caused Time-In-Zone data to be duplicated when the sport zones
  for an activity changed.

# Release 1.7.0

* Weather data is now fetched from DarkSky.net, as the previous provider,
  Wunderground is retiring their service.  See issue #33.

* Fix bug which prevented import of activities containing Garmin Run Power
  data field, see issue #42.  Also fixed import for some other activities that
  contain Garmin IQ data.

* Add support for named sport zones.  Each sport zone now can have a name
  defined, and this name is used in all places where sport zones are used
  (e.g. as values for histogram plots and as hover tags for plots)

* Fixed bug which displayed mouse hover information over plot legends, causing
  incorrect image re-draw.

# Release 1.6.0

This release introduces support for XDATA, which is data recorded by third
party applications and sensors, such as running power.  For more details on
the implementation see [docs/xdata.md](docs/xdata.md).

The code base was also reorganized, and many tests and improvements were added
to the code, these should have no user visible changes.  In addition to this,
the following minor fixes were added:

* The activity view panel will display a log message when the current filter
  does not select any activities.
* Better handling of gaps in GPS recording
* Add support for trend-lines for bodyweight chart
* New trends chart: Aerobic Efficiency
* Fix issue #25: the histogram plot will not forget its settings

# Release 1.5.2

This release contains the following bug fixes and improvements:

* "Lengths For Lap" label for Swim sessions is now vertical, it looks much
  nicer.

* improvements to map drawing and fixing an issue where maps would not display
  correctly on high DPI displays (issue #29)

* reduce flicker when graphs view is opened for the first time for a session.

* fixed a bug where histogram trends could not be computed for the "Grade
  Adjusted Pace" series.

# Release 1.5.1

This is a bug-fix release, addressing issue #34, which prevented creation of
new trend charts.

# Release 1.5.0

* A new [workout
  editor](https://alex-hhh.github.io/2018/05/running-and-cycling-workout-editor.html)
* General bug-fixes

# Release 1.4.0

In addition to general bug-fixes, the following improvements were made in this
release:

* Moving the mouse over a plot will show relevant information at the current
  mouse position, this works on all the plots, both for an individual session
  and the trend plots.  See [this blog
  post](https://alex-hhh.github.io/2018/02/interactive-overlays-with-the-racket-plot-package.html)
  for some screen shots.

* Data between views is synchronized in more cases (e.g. the activity list
  view is updated when he session weather changes), reducing the need to
  refresh views.  In particular, reports and trend charts are refreshed
  automatically when sessions are added, removed or are updated.

* Implement Grade Adjusted Pace for running sessions (see Issue #15).  W'Bal
  for running sessions is computed using the GAP series to model more
  accurately the W' depletion while running uphill.

* Only show "Model Parameters" page for Swim-Bike-Run activities

* When no Tau is defined for the critical power parameters, the "Model
  Parameters" page will display the "implicit tau" which is W' / CP.

# Release 1.3.0

In addition to general bug-fixes, the following improvements were made in this
release

## Synchronize data between views such as activity list and calendar

Changes to sessions and athlete metrics are now synchronized between the
various views, so they don't need to be manually refreshed when things change.
For example, editing the session headline in the session view, will
automatically update the headline in the activity list and calendar view.

Session Views are also updated with Critical Power values are changed via the
"Athlete/Edit Critical Power..." menu.

Currently, reports and trends are not updated via this mechanism and they need
to be manually refreshed with Ctrl-R.

## Activity view improvements

The activity view filter settings are now remembered between application
restarts.

Label and equipment filters change meaning from ANY to ALL.  For example,
adding two labels to the filter will now display sessions that have *both*
labels assigned.  Previously it would display sessions that have either one of
the labels assigned.  The same change was made for equipment filters.

## Sessions for trend charts can be filtered by labels and equipment

The settings dialogs for the BAVG, HISTOGRAM and SCATTER trend charts have
been updated to allow filtering sessions by labels and equipment in addition
to sport and date.  This allows more refined selections of sessions to include
in a trend chart.

## Session view improvements

Histogram plots format time as "hh:mm:ss" instead of displaying time in
seconds only (potentially showing values like 10000 seconds)

Issue #17, fix Best-Avg calculations for running and swimming activities

Issue #12, the session view has a new "Model Parameters" tab which shows the
sport zones and critical power parameters used for that session

Issue #20, the Best Avg plot is now shown for lap swimming activities

Issue #13, the Critical Power curve is shown on the Best-Avg plot for a
session. The best efforts for the activity and the theoretical best efforts
from the CP curve are also shown.  The Critical Power parameters used are the
ones that are applicable to the session, and are set in the "Athlete/Edit
Critical Power..." dialog box.  This dialog box has been updated to
create/edit CP parameters for swimming -- this allows the CP curve to be
displayed for swimming activities as well

## Other improvements

* The GPS track data for a session can be exported to a GPX file

* Plots can be exported as images in more image than just PNG.  In particular,
  they can be exported as SVG files making them scale nicely.

# Release 1.2.0

## Critical Power Model Estimation

Added support for Critical Power estimation and model W' depletion and
reconstitution during an activity.  CP estimation works for Cycling (using
power) and Running and Swimming (using pace).

For more details, see the "doc/critical-power.md" file.

## Updated sport zone editor

Sport zones can now be defined for a specific time range and each session will
use the sport zones for that time range.

## Support for trend scatter plots

Scatter plots can be defined for several activities at once via the Trends
pane.  This allows to plot, for example, pace vs cadence for all runs in a
training season.

## GUI improvements

* add a sample image to the trends chart selection dialog to make it easier to
  determine what type of chart is being selected
* the "Select Data Series" dialog box in the Chart View allows re-ordering the
  plots
* update the zone colors used by the application, these are now defined in a
  separate file, color-theme.rktd
* updates to the visual layout and colors of scatter plots
* the map view can show only the selected lap -- this can be used, for
  example, for sking sessions where the GPS track overlaps as

## Other Bug-fixes and small improvements

* map tile count did not drop to 0 when all tiles were downloaded
* fix bug preventing the deletion of a session in certain cases
* pace labels are correctly formatted for run and swim histogram plots
* improvements to the corrected elevation algorithm, corrected elevation data
  can now also be deleted.
* various performance improvements related to session loading and plotting
* updates to climb / descent detection code

# Release 1.1

* Lap views can show laps as they were recorded by the device and also splits
  by km, miles, hill climbs and descends plus best pace (for running) and best
  power (for bike)
* Most plots can display colors by the respective sport zone
* Histogram trend plots
* Multiple map tile sources
* Lots of small fixes and improvements

# Release 1.0

Initial built version.
