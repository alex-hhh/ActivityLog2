-- p34-clusters.sql -- update IX1_A_TRACKPOINT to support clusters for
-- elevation correction
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

drop index if exists IX1_A_TRACKPOINT;

create index IX1_A_TRACKPOINT
  on A_TRACKPOINT(tile_code, position_lat, position_long, altitude, timestamp);

update SCHEMA_VERSION set version = 34;
