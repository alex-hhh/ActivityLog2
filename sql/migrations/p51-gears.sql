-- SPDX-License-Identifier: GPL-3.0-or-later
-- p50-prefs.sql -- add the GEAR_CHANGE table
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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

create table GEAR_CHANGE(
  id integer not null primary key autoincrement,
  session_id integer not null,
  timestamp integer not null,
  front_gear_index integer,             -- 1 is innermost gear
  front_gear_teeth integer,
  rear_gear_index integer,              -- 1 is innermost gear
  rear_gear_teeth integer,
  foreign key (session_id) references A_SESSION(id) on delete cascade
);

create index IX1_GEAR_CHANGE
  on GEAR_CHANGE(session_id);

update SCHEMA_VERSION set version = 51;
