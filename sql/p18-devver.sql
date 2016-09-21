-- p18-devver.sql -- add device version information to the database
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

create table E_BATTERY_STATUS(
  id integer not null primary key autoincrement,
  name text unique not null);

insert into E_BATTERY_STATUS(id, name) values(1, 'new');
insert into E_BATTERY_STATUS(id, name) values(2, 'good');
insert into E_BATTERY_STATUS(id, name) values(3, 'ok');
insert into E_BATTERY_STATUS(id, name) values(4, 'low');
insert into E_BATTERY_STATUS(id, name) values(5, 'critical');
insert into E_BATTERY_STATUS(id, name) values(6, 'charging');
insert into E_BATTERY_STATUS(id, name) values(7, 'unknown');

create table EQUIPMENT_VER (
  id integer not null primary key autoincrement,
  equipment_id integer not null,
  timestamp integer not null,
  software_version text,
  hardware_version text,
  battery_voltage real,
  battery_status integer,
  foreign key (equipment_id) references EQUIPMENT(id) on delete cascade,
  foreign key (battery_status) references E_BATTERY_STATUS(id)
  );

update SCHEMA_VERSION set version = 18;
