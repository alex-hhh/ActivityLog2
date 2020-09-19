-- p36-geoids.sql -- add geoids to the A_TRACKPOINT table
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

alter table A_TRACKPOINT
  add column geoid integer;

-- NOTE: the latitude, longitude can be recovered from geoid, so we don't need
-- to index those to make it a covering index for elevation correction, saving
-- some space.
create index IX2_A_TRACKPOINT
  on A_TRACKPOINT(geoid, timestamp, altitude);

-- Improve speed of heat map queries
create index IX3_A_TRACKPOINT
  on A_TRACKPOINT(length_id, geoid);

update SCHEMA_VERSION set version = 36;
