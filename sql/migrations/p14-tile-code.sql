-- p14-tile-code.sql -- upgrade an existing database
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2016 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

.bail on

alter table A_TRACKPOINT add column tile_code integer;

create index IX1_A_TRACKPOINT on A_TRACKPOINT(tile_code);

update SCHEMA_VERSION set version = 14;
