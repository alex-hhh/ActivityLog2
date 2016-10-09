-- p19-metrics.sql -- add metrics storage cache
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

create table BAVG_CACHE (
  id integer not null primary key autoincrement,
  session_id integer not null,
  series text not null,
  data blob not null,
  foreign key(session_id) references A_SESSION(id)
  );

create unique index IX0_BAVG_CACHE on BAVG_CACHE(session_id, series);

update SCHEMA_VERSION set version = 19;

