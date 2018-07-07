# Data-Frame branch

[![Build Status](https://travis-ci.org/alex-hhh/ActivityLog2.svg?branch=data-frame)](https://travis-ci.org/alex-hhh/ActivityLog2) (this branch)

This branch contains a reimplementation of the data-frame module in
preparation for releasing it as a separate package.  The data-frame
implementation is in the "rkt/data-frame" folder, and should not have any
dependencies on the other files in the project.  There is a unit-test file in
"test/df-tests.rkt".  The documentation in "doc/data-frame.md" is not
outdated.

This branch is a work in progress and currently, the ActivityLog2 application
might not run correctly, although it is unlikely that it will corrupt the
data.  If you want a working application, use the master branch.

## Implementation Status

[x] The data-frame package is feature complete (more or less)

[ ] Application does not work with the new data-frame package

[ ] Documentation for the data-frame package is missing(replaces
    "doc/data-frame.md")

[ ] Data-frame performance is not validated yet (in particular it will copy
    data where the previous implementation didn't (df-select)

----

![Logo](img/logo/ActivityLog2.png)

# ActivityLog2 - analyze data from swim, bike and run activities

[![Build Status](https://travis-ci.org/alex-hhh/ActivityLog2.svg?branch=master)](https://travis-ci.org/alex-hhh/ActivityLog2) (master branch)

Import swim, bike and run and other activities from .FIT files.  Display
plots, scatter plots, histograms, maps and other views for activities.  Show
reports and trends from activity data and track equipment usage.

Download pre-built releases from
the [releases](https://github.com/alex-hhh/ActivityLog2/releases) page, or
see [doc/building.md](doc/building.md) for how to build your own.

<p align="center">
<img align="center" width="800" 
     alt="session view" 
     src="https://drive.google.com/uc?export=download&id=1sktF8A_3CCe-9KYdc7YHJWLsDTs4ffy8" />
</p>

## Features

* Import Run, Bike and Swim activities from FIT files (as generated by Garmin
  devices, and others)
* Show data charts, histograms, scatter plots, best-average plots
* Show GPS track on a map, with several map tile styles
* Show hill climbs and best sections (e.g. best 5k, best 20 min power)
* Elevation correction (see [Notes](./doc/elevation-correction.md))
* Show weather information for sessions with GPS data
* Fix recording errors in swim activities (merge or split lengths, fix stroke
  type)
* Export data as CSV for analyzing it with other tools
* Export settings sport zones to the device
* Show trend plots and reports
* Track equipment usage and service reminders (e.g. replace running shoes or
  service the bike).
* Shows firmware version and battery voltage for all sensors (e.g HRM or Bike
  Cadence sensor), displays a warning on low battery.
* Track body weight, and other metrics
* Estimate [Critical Power](./doc/critical-power.md)
* [Generate workout files](https://alex-hhh.github.io/2018/05/running-and-cycling-workout-editor.html)
  for downloading to the Garmin device.

## License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.
