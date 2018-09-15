-- p30-xdata.sql -- add XDATA series
-- 
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (C) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

create table if not exists XDATA_APP (
  id integer not null primary key autoincrement,
  app_guid text unique not null,
  dev_guid text
);

create index if not exists IX0_XDATA_APP on XDATA_APP(app_guid);

create table XDATA_FIELD (
  id integer not null primary key autoincrement,
  app_id integer not null,
  name text not null,
  unit_name text,
  native_message integer,
  native_field integer,
  foreign key (app_id) references XDATA_APP(id)
);

create unique index if not exists IX0_XDATA_FIELD
  on XDATA_FIELD(app_id, name);

create table if not exists XDATA_VALUE (
  id integer not null primary key autoincrement,
  trackpoint_id integer not null,
  field_id integer not null,
  val real,
  foreign key (field_id) references XDATA_FIELD(id),
  foreign key (trackpoint_id) references A_TRACKPOINT(id) on delete cascade
);

create index if not exists IX0_XDATA_VALUE
  on XDATA_VALUE(trackpoint_id, field_id, val);

create table if not exists XDATA_SUMMARY_VALUE (
  id integer not null primary key autoincrement,
  summary_id integer not null,
  field_id integer not null,
  val real,
  foreign key (field_id) references XDATA_FIELD(id),
  foreign key (summary_id) references SECTION_SUMMARY(id) on delete cascade
);

create index if not exists IX0_XDATA_SUMMARY_VALUE
  on XDATA_SUMMARY_VALUE(summary_id, field_id);

update SCHEMA_VERSION set version = 30;
