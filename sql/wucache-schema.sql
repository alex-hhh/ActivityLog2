-- Database schema for caching Wunderground weather data
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

create table SCHEMA_VERSION(version integer);
insert into SCHEMA_VERSION(version) values(1);

create table WU_WSTATION (
  id integer not null primary key autoincrement,
  name text not null,                   -- user friendly name
  ident text not null,                  -- e.g. YPPH, IWAKARRI2
  type text not null,                   -- e.g. pws, icao
  position_lat real not null,
  position_lon real not null,
  -- timestamp when station became active.  This is an estimate.  Weather
  -- history requests will not be made for this station before this
  -- timestamp. Also if a history request returns no data, this field will be
  -- updated.
  active_since integer
  );

-- type and ident are used to query data from this weather station, as such,
-- they must be unique.
create unique index IX0_WU_STATION on WU_WSTATION(type, ident);

create table WU_OBSERVATION (
  id integer not null primary key autoincrement,
  wstation_id integer not null,
  timestamp integer not null,

  temperature real,                     -- celsius
  dew_point real,                       -- celsius
  humidity real,                        -- percentage: 0 .. 100
  wind_speed real,                      -- km/h
  wind_gusts real,                      -- km/h
  wind_direction real,                  -- degrees 0 - N, 90 - E, 180 - S, 270 - W
  pressure real,

  foreign key (wstation_id) references WU_WSTATION(id)
  );

create unique index IX0_WU_OBSERVATION on WU_OBSERVATION(wstation_id, timestamp);
