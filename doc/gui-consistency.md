# Notes on maintaining GUI consistency

The application implements a mechanism to allow updating different parts of
the GUI when data in the database changes.  For example, when new sessions are
imported, the reports and trend charts should refresh automatically.  This
document contains the technical description of that mechanism.

Any part of the application code can signal when and what things have changed
using the `log-event` call (defined in "../rkt/utilities.rkt").  The function
takes two arguments: a symbol representing a generic type of change and some
data corresponding to that change.  Currently, the following events are
logged:

* `session-created` SID -- logged when a new session is created.  The data
  part is the session ID
* `session-updated` SID -- logged when the summary data of a session is
  updated.  The data part is the session ID.
* `session-updated-data` SID the -- logged when the track data for a session
  has changed (a corresponding `session-updated` event is also logged in this
  case).  The data part is the session ID. Currently it is logged when
  corrected elevation is changed or when laps are edited for a lap swim
  session.
* `session-deleted` SID -- logged with a session is deleted.  The data part is
  the session ID.
* `critical-power-parameters-changed` #f -- logged when critical power
  parameters have changed via the "Athlete/Edit Critical Power..." menu item.
  The data part is always #f
* `weather-data-changed` SID -- logged when weather data associated with a
  session has been added or updated.  The data part is the session ID.
* `measurement-system-changed` NEW-VAL -- logged when the measurement system
  has changed, the data part is the new measurement system type.
* `athlete-metrics-created` ID, SID ... -- logged when athlete metrics are
  created.  The data part is a list: the first ID is the ID of the athlete
  metrics entry, the remaining IDs are session ids which might be affected by
  this change.)
* `athlete-metrics-deleted` ID, SID ... -- logged when athlete metrics are
  deleted.  The data part is a list: the first ID is the ID of the athlete
  metrics entry, the remaining IDs are session ids which might be affected by
  this change.)
* `athlete-metrics-updated` ID, SID ... -- logged when athlete metrics are
  updated.  The data part is a list: the first ID is the ID of the athlete
  metrics entry, the remaining IDs are session ids which might be affected by
  this change.)

A GUI element (such as a trends chart) can subscribe to notifications by
creating a change notification source using `make-log-event-source` (defined
in "../rkt/utilities.rkt").  This is an async channel, so it can obtain events
using `async-channel-get` and `async-channel-try-get` calls, but a convenient
utility function, `collect-events`, is defined as well.  Normally, when a view
becomes visible, it will query what events have happened since it was last
visible and update itself if needed.  See examples of `collect-events` usage
throughout the code base.

This mechanism is not perfectly implemented , so the GUI views can always be
refreshed by selecting the "View/Refresh..." from the menu.
