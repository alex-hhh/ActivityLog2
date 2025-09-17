-- SPDX-License-Identifier: GPL-3.0-or-later
-- wke-db-schema.sql -- database schema for al2-workout-editor
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
-- more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program.  If not, see <http://www.gnu.org/licenses/>.

create table SCHEMA_VERSION(version integer);
insert into SCHEMA_VERSION(version) values(52);

-- A workout library is a convenient way to group workouts together.  It only
-- has a name.
create table WORKOUT_LIBRARY (
  id integer not null primary key autoincrement,
  name text unique not null
);

insert into WORKOUT_LIBRARY(name) values('Default');

-- Store a workout.  A workout is a sequence of steps with a specified
-- duration and intensity.  Workouts can be exported to FIT files and
-- downloaded onto a Garmin device and the device will guide the user through
-- the workout
--
-- This table only holds workout metadata (e.g. name, sport, etc), the actual
-- workout data is stored in the WORKOUT_VERSION table.
create table WORKOUT (
  id integer not null primary key autoincrement,
  library_id integer not null,
  name text not null,
  sport_id integer not null,
  sub_sport_id integer,
  serial integer unique not null,
  foreign key (library_id) references WORKOUT_LIBRARY(id),
  foreign key (sport_id) references E_SPORT(id),
  foreign key (sub_sport_id) references E_SUB_SPORT(id)
);

create index IX0_WORKOUT on WORKOUT(serial);

-- Hold data about a workout version.  Each time a workout is saved, a new
-- entry is created in this table, but some of the previous versions are also
-- kept.  Previous versions which were exported as FIT files are kept, because
-- activities will contain "training file" messages which are identified by
-- the serial number and the timestamp.  This will allow us to match a workout
-- referenced in the activity file to a workout version.
--
-- Also note that the actual workout steps are stored as a compressed JSON
-- string in the data field, and are not accessible via SQL commands -- the
-- format is hierarchical and there is little use for having it in a
-- relational schema.
create table WORKOUT_VERSION (
  id integer not null primary key autoincrement,
  workout_id integer not null,
  timestamp integer not null,
  -- if this is not 0, this workout version was exported (e.g as a FIT file)
  -- and as such we keep it, as we might import activities against this
  -- version and we want to link it up in V_SESSION_WORKOUT.
  is_exported integer not null default 0,
  data blob not null,                   -- compressed using gzip
  foreign key (workout_id) references WORKOUT(id)
);

create index IX0_WORKOUT_VERSION on WORKOUT_VERSION(workout_id, timestamp);
