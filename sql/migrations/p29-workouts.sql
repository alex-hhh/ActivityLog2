-- p29-workouts.sql -- add DB tables to support the workout editor
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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

drop table if exists WORKOUT_VERSION;
drop table if exists WORKOUT;
drop table if exists WORKOUT_LIBRARY;

create table WORKOUT_LIBRARY (
  id integer not null primary key autoincrement,
  name text unique not null
);

insert into WORKOUT_LIBRARY(name) values('Default');

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

create table WORKOUT_VERSION (
  id integer not null primary key autoincrement,
  workout_id integer not null,
  timestamp integer not null,
  is_exported integer not null default 0,
  data blob not null,                   -- compressed using gzip
  foreign key (workout_id) references WORKOUT(id)
);

create index IX0_WORKOUT_VERSION on WORKOUT_VERSION(workout_id, timestamp);

update SCHEMA_VERSION set version = 29;
