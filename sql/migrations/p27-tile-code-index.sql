-- p27-tile-code-index.sql -- update the A_TRACKPOINT tile code index
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2018 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

-- Change the indices on the BAVG_CACHE, HIST_CACHE and SCATTER_CACHE so they
-- can be used for both "series" only lookups and "session_id" + "series"
-- lookups.

drop index if exists IX1_A_TRACKPOINT;

-- This is a good covering index for both updating the TILE_CODE query and for
-- retrieving lat/lon coordinates for a TILE_CODE, TILE code queries will only
-- need to scan this index, speeding the lookup.  We pay for this by having a
-- larger index size.
create index IX1_A_TRACKPOINT
  on A_TRACKPOINT(tile_code, position_lat, position_long, altitude);

update SCHEMA_VERSION set version = 27;
