-- p25-index.sql -- update indices for cache tables
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

drop index if exists IX0_BAVG_CACHE;
create unique index IX0_BAVG_CACHE on BAVG_CACHE(series, session_id);

drop index if exists IX0_HIST_CACHE;
create unique index IX0_HIST_CACHE on HIST_CACHE(series, session_id);

drop index if exists IX0_SCATTER_CACHE;
create unique index IX0_SCATTER_CACHE on SCATTER_CACHE(series1, series2, session_id);

drop index if exists IX1_SCATTER_CACHE;
create index IX1_SCATTER_CACHE on SCATTER_CACHE(series2);

update SCHEMA_VERSION set version = 25;

