This file contains a high level summary of changes in each released version of
the application.  Each release is tagged, you can check the tag names using
the command `git tag -l` in a checked out source and use `git log
tag-1..tag-2` to find the detailed changes between two tags.  For example to
find the changes between version 1.7.1 and 1.8.0 you can run:

```
git log v1.7.1..v1.8.0
```

Or, if you want to see what has changed in each submodule as well:

```
git log --submodule=log --patch v1.10.2..v1.11 \
    | awk '/^commit/,/^diff/ { if ($1 != "diff") { print; } }'`
```

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
