# Notes on elevation correction

The elevation data collected by the GPS devices is not accurate enough to
provide nice plots and accurate grade calculations.  This is true for both GPS
based elevation and barometric devices.  In particular, barometric devices are
affected by atmospheric pressure changes.

To provide improved elevation data, ActivityLog2 will apply an elevation
correction algorithm to each GPS based activity.  Both the original and
corrected elevations can be plotted in the Graph view, but grade calculations
and summary altitude changes (for laps and the entire activity) are displayed
from the corrected elevation data, if this is available.

This elevation correction algorithm works on the assumption that a certain
route is traversed many times (in different sessions or as laps of the same
session), therefore multiple elevation readings are available for the same
positions at different times.  The code works by averaging the reported
elevation of all the track points in the database recorded around certain
position.

If you train over the same set of routes, the elevation information will be
quite good, even for small elevation changes on mostly flat routes.

It will not work well in the following cases:

* When the route is traversed only once.  In such a case, the algorithm just
  provides some smoothing of the data, which may or may not look better than
  the original.
  
* When the a point is traversed at different levels: for example running over
  an overpass or bridge, than running under it.  In such cases, different
  elevations are recorded for points that are close-by and the algorithm just
  averages them out.
  
The elevation correction is computed automatically when a session is imported,
for every session.  After that, you can:

* Clear the elevation using the "Activity/Clear Corrected Elevation..." menu.
  Do this if the corrected elevation or grade information looks bad for the
  activity.

* Refresh the corrected elevation by selecting the "Activity/Fixup
  Elevation..." menu.  This is useful to re-create the previously cleared
  corrected elevation data.  It is also useful if you started using a new
  training route and older sessions on that route did not originally have
  enough points to provide a good corrected data.
  
* Refresh the elevation for all sessions from the "Tools/Fixup Elevation..."
  menu.  This will take a very long time of you have a lot of activities in
  the database.
