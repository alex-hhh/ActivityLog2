-- p46-aerolab.sql -- add SESSION_AEROLAB table to the database
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

create table E_AIR_DENSITY_CALCULATION_METHOD(
  id integer not null primary key autoincrement,
  name text unique not null);

insert into E_AIR_DENSITY_CALCULATION_METHOD(id, name)
values (0, 'Dew Point'),
       (1, 'Relative Humidity');

create table SESSION_AEROLAB(
  id integer not null primary key autoincrement,
  session_id integer not null,  -- session for which this aerolab data belongs
  crr real,                     -- coefficient of rolling resistance
  cda real,                     -- coefficient of drag area

  air_density real,
  wind_speed real,                    -- speed in m/s
  wind_direction real,                -- degrees 0-360, 0 = North, 180 = South
  should_use_wind boolean,            -- if true, use wind data in simulation and estimates

  total_weight real,                    -- athlete + bike weight, in kg
  lap_count integer,
  trim_start real,                      -- km
  trim_end real,                        -- km
  altitude_offset real,                 -- meters

  -- These fields determine how the air density is to be calculated from
  -- weather data.
  air_density_calculation_method integer,
  temperature real,                     -- degrees Celsius
  dew_point real,                       -- degrees Celsius
  pressure real,                        -- in hPa
  humidity real,                        -- 0 - 100

  foreign key (session_id) references A_SESSION(id) on delete cascade,
  foreign key (air_density_calculation_method) references E_AIR_DENSITY_CALCULATION_METHOD(id)
  );

create index IX0_SESSION_AEROLAB on SESSION_AEROLAB(session_id);

update SCHEMA_VERSION set version = 46;
