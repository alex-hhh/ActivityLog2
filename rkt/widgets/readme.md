This directory contains GUI widgets built on top of the racket GUI library.

* `map-widget%` -- implements a widget that displays a map, based on Open
  Street Map tiles.  It can also display a track on this map, plus markers.
  Supports normal panning and zooming functionality.  This widget is also
  available as a separate package on [pkgs.racket-lang.org]
  (https://pkgs.racket-lang.org/package/data-frame).
  
* `notes-input-field%` -- implements a widget that allows editing nodes or a
  description text.  The widget displays "Save"/"Revert" buttons only when the
  text is actually modified, making the GUI less cluttered.

* `validating-input-field%` a text input field that allows specifying a
  validation function to restrict the text that can be entered.  This can be
  used to restrict the input to only numeric values, for example.  Invalid
  inputs are highlighted in red.  Several subclasses are available for common
  input types:

    * `date-input-field%` -- an input text field based on, that allows
      entering date values in the format dd/mm/yyyy

    * `duration-input-field%` -- an input text field for entering duration
      values in the format hh:mm::ss

    * `number-input-field%` -- an input text field for entering numbers

    * `pace-input-field%`, `swim-pace-input-field%` -- input text fields for
      entering pace values (min/km)

* `number-range-selector%` -- a widget for selecting a numeric range (low and
  high numbers)

* `date-range-selector%` -- a widget that allows selecting a date range

* `edit-dialog-base%` -- base class to assist writing dialog boxes

* `grid-pane%` -- a `pane%` object that places its children in a 2D aligned
  grid.

* `notification-banner%` -- a widget for displaying messages on the top of the
  application

* `progress-dialog%` -- implement a progress dialog for a long running task.

* `qresults-list%` -- a list view with sortable columns and other
  enhancements.

* `tab-selector%` -- a widget displaying a set of labels (tabs) and allowing
  to select one of them.

While they are used by the ActivityLog2 application, these widgets might be
generally useful, and could technically be moved into one separate package, or
a separate package per widget.  Some of the widgets have dependencies on files
in the main ActivityLog2 application, these fall under these categories:

* `dbglog` and `thread/dbglog` (from "utilities.rkt") are used to log messages
  and exceptions to a log file.  These could be easily replaced with "printf"
  and "thread" if files are used outside of the ActivityLog2 application.

* `put-pref` and `get-pref` (from "utilities.rkt") are used to store
  preferences in an ActivityLog2 specific file.  These calls could be easily
  replaced with `put-preferences` and `get-preference`, which are part of
  Racket, if the files are used outside of the ActivityLog2 application.

* `pace-input-field%` has a dependency on "fmt-util.rkt" for formatting pace
  values based on the measurement system (minutes/km if metric, minutes/mile
  otherwise).

* "icon-resources.rkt" reference the icons of the ActivityLog2 application,
  and it is used by the ActivityLog2 application dialog boxes.  Only one icon
  is used by the `qresults-list%` and icon can be easily removed, so
  "qresults-list.rk
