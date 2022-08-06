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
-- https://github.com/evansiroky/timezone-boundary-builder database (2021c),
-- bought in via tzgeolookup package has the following changes:
--
-- * America/Godthab renamed to America/Nuuk
-- * Pacific/Enderbury renamed to Pacific/Kanton
-- * Australia/Currie merged into Australia/Hobart
--
-- We update our own list of time zones to match

update E_TIME_ZONE set name = 'America/Nuuk' where name = 'America/Godthab';
update E_TIME_ZONE set name = 'Pacific/Kanton' where name = 'Pacific/Enderbury';

update A_SESSION
   set time_zone_id = (select id from E_TIME_ZONE where name = 'Australia/Hobart')
 where time_zone_id = (select id from E_TIME_ZONE where name = 'Australia/Currie');

delete from E_TIME_ZONE where name = 'Australia/Currie';

update SCHEMA_VERSION set version = 45;
