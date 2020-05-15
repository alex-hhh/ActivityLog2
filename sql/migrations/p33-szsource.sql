-- p33-szsource.sql -- sport_zone_source table
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

drop table if exists SPORT_ZONE_SOURCE;

create table SPORT_ZONE_SOURCE (
  zone_id integer not null,
  session_id integer not null,

  foreign key (zone_id) references SPORT_ZONE(id) on delete cascade,
  foreign key (session_id) references A_SESSION(id) on delete cascade
);

update SCHEMA_VERSION set version = 33;
