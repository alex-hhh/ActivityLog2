-- SPDX-License-Identifier: GPL-3.0-or-later
-- p45-time-zone.sql -- update time zone names to new definitions
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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

-- Latest version of the
-- https://github.com/evansiroky/timezone-boundary-builder database (2023c),
-- bought in via tzgeolookup package has the following changes:

-- America/Nipigon -- REMOVED, Replaced with America/Toronto
-- America/Pangnirtung -- REMOVED -- Merged into America/Iqaluit
-- America/Rainy_River -- REMOVED, Replaced with America/Winnipeg
-- America/Thunder_Bay -- REMOVED, Replaced with America/Toronto
-- America/Yellowknife -- REMOVED, Replaced with America/Edmonton (unit tests indicate this)
-- Europe/Uzhgorod -- REMOVED, Replaced with Europe/Kyiv
-- Europe/Zaporozhye -- REMOVED, Replaced with Europe/Kyiv
-- America/Ciudad_Juarez  -- NEW, taking area from America/Ojinaga
-- Europe/Kyiv -- RENAMED FROM Europe/Kiev

update A_SESSION
   set time_zone_id = (select id from E_TIME_ZONE where name = 'America/Toronto')
 where time_zone_id = (select id from E_TIME_ZONE where name = 'America/Nipigon');

update A_SESSION
   set time_zone_id = (select id from E_TIME_ZONE where name = 'America/Iqaluit')
 where time_zone_id = (select id from E_TIME_ZONE where name = 'America/Pangnirtung');

update A_SESSION
   set time_zone_id = (select id from E_TIME_ZONE where name = 'America/Winnipeg')
 where time_zone_id = (select id from E_TIME_ZONE where name = 'America/Rainy_River');

update A_SESSION
   set time_zone_id = (select id from E_TIME_ZONE where name = 'America/Toronto')
 where time_zone_id = (select id from E_TIME_ZONE where name = 'America/Thunder_Bay');

update A_SESSION
   set time_zone_id = (select id from E_TIME_ZONE where name = 'America/Edmonton')
 where time_zone_id = (select id from E_TIME_ZONE where name = 'America/Yellowknife');

update A_SESSION
   set time_zone_id = (select id from E_TIME_ZONE where name = 'Europe/Kiev')
 where time_zone_id = (select id from E_TIME_ZONE where name = 'Europe/Uzhgorod');

update A_SESSION
   set time_zone_id = (select id from E_TIME_ZONE where name = 'Europe/Kiev')
 where time_zone_id = (select id from E_TIME_ZONE where name = 'Europe/Zaporozhye');

update E_TIME_ZONE
   set name = 'Europe/Kyiv'
 where name = 'Europe/Kiev';

delete from E_TIME_ZONE where name in (
  'America/Nipigon',
  'America/Pangnirtung',
  'America/Thunder_Bay',
  'America/Yellowknife',
  'Europe/Uzhgorod',
  'Europe/Zaporozhye');

insert into E_TIME_ZONE(name)
values ('America/Ciudad_Juarez');

update SCHEMA_VERSION set version = 49;
