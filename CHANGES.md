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
