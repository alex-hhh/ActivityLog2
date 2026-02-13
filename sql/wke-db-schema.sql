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
insert into SCHEMA_VERSION(version) values(53);

-- Store ActivityLog2 application preferences in the database.  preferences
-- have a "tag" (a name) and a value which is a serialized SEXPR.  This table
-- is meant for application data only (e.g. layout of various GUI views).  It
-- is stored in the database, so each database can have its own associated GUI
-- data.
create table SEXPR_PREFERENCES (
  id integer not null primary key autoincrement,
  name text not null,
  value blob not null);

create unique index IX0_SEXPR_PREFERENCES on SEXPR_PREFERENCES(name);


create table E_SPORT (
  id integer not null primary key autoincrement,
  name text unique not null,
  color integer not null, -- not used by the application anymore
  icon string not null    -- a predefined icon name
  );

insert into E_SPORT(id, name, color, icon) values(0, 'Generic', -1, '');
insert into E_SPORT(id, name, color, icon) values(1, 'Running', 1, 'run');
insert into E_SPORT(id, name, color, icon) values(2, 'Cycling', 9, 'bike');
insert into E_SPORT(id, name, color, icon) values(3, 'Transition', 8, 'walk');
insert into E_SPORT(id, name, color, icon) values(4, 'Fitness Equipment', 11, 'weight-lift');
insert into E_SPORT(id, name, color, icon) values(5, 'Swimming', 5, 'swim');
insert into E_SPORT(id, name, color, icon) values(6, 'Basketball', -1, '');
insert into E_SPORT(id, name, color, icon) values(7, 'Soccer', -1, '');
insert into E_SPORT(id, name, color, icon) values(8, 'Tennis', -1, '');
insert into E_SPORT(id, name, color, icon) values(9, 'American Football', -1, '');
insert into E_SPORT(id, name, color, icon) values(10, 'Training', -1, '');
insert into E_SPORT(id, name, color, icon) values(12, 'Cross Country Skiing', -1, '');
insert into E_SPORT(id, name, color, icon) values(13, 'Alpine Skiing', 19, 'ski');
insert into E_SPORT(id, name, color, icon) values(14, 'Snowboarding', -1, '');
insert into E_SPORT(id, name, color, icon) values(15, 'Rowing', -1, '');
insert into E_SPORT(id, name, color, icon) values(17, 'Hiking', -1, 'hike');
insert into E_SPORT(id, name, color, icon) values(32, 'Sailing', -1, 'sail');
insert into E_SPORT(id, name, color, icon) values(37, 'Stand Up Paddleboarding', -1, 'sup');
insert into E_SPORT(id, name, color, icon) values(41, 'Kayaking', -1, 'kayak');
insert into E_SPORT(id, name, color, icon) values(53, 'Diving', -1, 'diving');
insert into E_SPORT(id, name, color, icon) values(254, 'All', -1, '');

create table E_SUB_SPORT(
  id integer not null primary key autoincrement,
  sport_id integer not null,
  name text unique not null,
  color integer not null,               -- not used by the application anymore
  icon string not null,
  foreign key (sport_id) references E_SPORT(id));

insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(0, 0, 'Generic', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(1, 1, 'Treadmill Running', 1, 'run');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(2, 1, 'Street Running', 1, 'run');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(3, 1, 'Trail Running', 1, 'run');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(4, 1, 'Track Running', 1, 'run');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(5, 2, 'Spin Cycling', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(6, 2, 'Indoor Cycling', 9, 'bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(7, 2, 'Road Cycling', 9, 'bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(8, 2, 'Mountain Biking', 9, 'mountain-bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(9, 2, 'Downhill Biking', 9, 'mountain-bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(10, 2, 'Recumbent Cycling', 9, 'bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(11, 2, 'Cyclocross', 9, 'bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(12, 2, 'Hand Cycling', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(13, 2, 'Track Cycling', 9, 'bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(14, 4, 'Indoor Rowing', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(15, 4, 'Elliptical', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(16, 4, 'Stair Climbing', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(17, 5, 'Lap Swimming', 5, 'swim');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(18, 5, 'Open Water Swimming', 5, 'swim');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(19, 4, 'Flexibility Training', 10, 'yoga');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(20, 4, 'Strength Training', 11, 'weight-lift');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(21, 0, 'Warm Up', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(22, 0, 'Match', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(23, 0, 'Exercise', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(24, 0, 'Challenge', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(25, 0, 'Indoor Skiing', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(254, 254, 'All', -1, '');

create table E_SWIM_STROKE(
  id integer not null primary key autoincrement,
  name text unique not null);

insert into E_SWIM_STROKE(id, name) values (0, 'Freestyle');
insert into E_SWIM_STROKE(id, name) values (1, 'Backstroke');
insert into E_SWIM_STROKE(id, name) values (2, 'Breaststroke');
insert into E_SWIM_STROKE(id, name) values (3, 'Butterfly');
insert into E_SWIM_STROKE(id, name) values (4, 'Drill');
insert into E_SWIM_STROKE(id, name) values (5, 'Mixed');
insert into E_SWIM_STROKE(id, name) values (6, 'IM');

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
