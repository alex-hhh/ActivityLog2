-- SPDX-License-Identifier: GPL-3.0-or-later
-- p50-prefs.sql -- add the SEXPR_PREFERENCES table
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

create table SEXPR_PREFERENCES (
  id integer not null primary key autoincrement,
  name text not null,
  value blob not null);

create unique index IX0_SEXPR_PREFERENCES on SEXPR_PREFERENCES(name);

update SCHEMA_VERSION set version = 50;
