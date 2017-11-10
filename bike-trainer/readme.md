# Bike Trainer Prototype

This is a prototype bike trainer application, which allows to ride a virtual
GPS route using a bike and a ANT+ FE-C capable bike-trainer (all recent
trainers are FE-C capable.)

<p align="center">
<img align="center" width="800" 
     alt="session view" 
     src="https://drive.google.com/uc?export=download&id=1bygxcA9U7Qzkx4YZfhY7AkW4F_NR-Tmy" />
</p>

The application can load a GPX file containing GPS track points, and display
it on a map along with a "current location".  It can read telemetry data
(speed, power, cadence) from an ANT+ FE-C capable bike trainer and heart rate
from an ANT+ capable heart rate monitor.  It will display all these telemetry
values and will update the current location on the map based on the speed
provided by the trainer.  It can than determine the current slope from the GPX
data and adjust the resistance of the trainer to account for uphill and
downhill portions of the ride.

## Running the application

To run the application, follow the instructions below.  The application can
also run in demo mode, where fake telemetry data is sent to it, there is a
section about that at the end.

### Things you need

You need the following equipment to run the application:

* An ANT+ FE-C capable bike trainer (all recent trainers should work)
* A bike to put on the trainer
* An ANT+ Heart Rate Monitor (optional)
* An ANT+ USB dongle

You will also need a GPX file, containing the GPS track to ride.  If you don't
have any, here are two sample ones:

* [Mt Adelaide and Clarence](https://drive.google.com/uc?export=download&id=1_EvQnSEBy6fcjYKqcENYJKf1wWaojWyZ)
* [Guilford Hills](https://drive.google.com/uc?export=download&id=1jlOLyG0MXTezoSwCdSGWDiaFO9MolRj8)

### Install, build and run the telemetry server

Communicating with the application is done using a separate application,
a [TelemetryServer](https://github.com/alex-hhh/TrainerControl), which you can
find here:

    https://github.com/alex-hhh/TrainerControl

Follow the instructions in that project README file to build and run it.

### Running the application

To run the application, first start the one of the telemetry server, than open
the `bike-trainer.rkt` file in DrRacket and click "Run" (or Ctrl-R).  You will
need to connect to the telemetry server by typing the following in the Racket
interaction window:

    (connect 7500)

You can load a GPX track, click "Start" and start pedaling.

## Running the application in demo mode

The `demo-telemetry-server.rkt` utility can be used to supply pre-defined
telemetry values to the bike trainer application.  This can be used to test
the application without a bike trainer, or to simply run the application in
demo mode.

To run the server, you will need a CSV file with telemetry data.  A CSV file
in the required format can be exported by selecting an existing activity and
using the "Activity / Export track data (CSV)...".

**NOTE** You can also download a sample CSV file
[here](https://drive.google.com/uc?export=download&id=1Zgzrj5IqER7Qf4AzrMc3i3gVa5ePqg_W)

The server can be run from the command line as:

    racket demo-telemetry-server.rkt -f telemetry-data.csv

Alternatively, the server can send telemetry directly from an existing session
from the ActivityLog2 database.  You will need the session id, which you can
find by opening a session and using the "Activity/Copy session id to
clipboard..." menu item.  The server can be run from the command line as:

    racket demo-telemetry-server.rkt -s SESSION-ID
    
Once started, the bike-trainer application will connect to this server to
receive telemetry values.

# Technical Notes

## Code changes to the map widget

A "current location" concept was added to the `map-widget%`: when a current
location is set using `set-current-location`, a red circle will be drawn at
that location.  Also the current location is "tracked", meaning that the map
is slowly panned so that the current location is always displayed on the
screen.  This is currently non-configurable, see `track-current-location`.

**TODO** make `track-current-location` configurable.

The `map-widget%` was updated to act as a `pasteboard%`.  The map is drawn as
the pasteboard background, and the pasteboard can be accessed using
`get-pasteboard` and snips can be inserted.  Mouse handling has changes: click
and drag events are used to pan the map around and mouse events with the
"Control" key pressed are passed to the pasteboard%.  This means that
selecting and moving snips around is done while the "control" key is pressed.

**TODO** make the mouse interaction behave in a more reasonable way.

**TODO** there is a separate issue
[#7](https://github.com/alex-hhh/ActivityLog2/issues/7) to add the map-widget%
as a trends chart, this would require it to be a snip%, we need to make the
map-widget% draw to a normal `canvas%`, to a `pasteboard%` or a `snip%`.

Some other minor updates:

* a `map-point->lat-lon` function was added which is the inverse of
  `lat-lon->map-point`.
* default map center was changed to Perth, Western Australia.  This is only
  used when no track is added to the map-widget%.
* `map-widget%/resize-to-fit` can handle the case when there is no track
* `map-widget%/get-zoom-level` method added

## Controlling the trainer resistance

The trainer can change resistance of the rear flywheel by sending it special
ANT+ commands.  The resistance can be controlled directly via a
`set-resistance` command, by setting a power value required to turn the
flywheel via the `set-power` command or by simulating a slope via the
`set-slope` command.

*NOTE* currently only `set-slope` is implemented.

When controlling the resistance, the following need to be taken into account:

* *Abrupt changes*: when the trainer receives a new command to change the
  resistance, it will instantly change to the new value.  If the change is
  small, say from 0 % to 1 % slope, this is not very noticeable, however when
  changing from 0% to 5%, the trainer will pretty much brake and stop the rear
  wheel.

* *Ignoring speed and cadence*: when the trainer applies a high resistance
  value, it will do so regardless of the speed at which the flywheel is
  turning.  When high resistance is applied at low speeds and cadences, the
  bike wheel will just start sliding on the flywheel.

* *Slow reaction time*: the trainer has a slow reaction time (up to a few
  seconds) to any changes in resistance.  That is, the time between sending a
  SET-SLOPE command and the trainer resistance being updated is several
  seconds.  It is unclear what happens if several different set resistance
  commands are sent in quick succession.

----

* send resistance commands slowly: at most one every 2 seconds

* change the resistance slowly, in 0.5% increments (or slower?)

* monitor speed and cadence, and reduce resistance when they drop below
  thresholds.
  
