-- SPDX-License-Identifier: GPL-3.0-or-later
-- p52-similar.sql -- add SIMILAR_SESSION_CACHE table
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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

create table SIMILAR_SESSION_CACHE(
  id integer not null primary key autoincrement,
  first_session_id integer not null,
  second_session_id integer not null,
  are_similar integer not null, -- 1 if sessions are similar, 0 if they are not
  foreign key (first_session_id) references A_SESSION(id),
  foreign key (second_session_id) references A_SESSION(id),
  check (first_session_id < second_session_id));

create unique index IX0_SIMILAR_SESSION_CACHE
  on SIMILAR_SESSION_CACHE(first_session_id, second_session_id, are_similar);

create index IX1_SIMILAR_SESSION_CACHE
  on SIMILAR_SESSION_CACHE(second_session_id);

update SCHEMA_VERSION set version = 52;
