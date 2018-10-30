-- p21-smetrics.sql -- add metrics storage cache for scatter plots
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

drop index if exists IX0_SCATTER_CACHE;
drop table if exists SCATTER_CACHE;

create table SCATTER_CACHE (
  id integer not null primary key autoincrement,
  session_id integer not null,
  series1 text not null,
  series2 text not null,
  data blob not null,
  foreign key(session_id) references A_SESSION(id)
  );

create unique index IX0_SCATTER_CACHE on SCATTER_CACHE(session_id, series1, series2);

update SCHEMA_VERSION set version = 22;
