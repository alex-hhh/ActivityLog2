-- p31-named-zones.sql -- add names to the sport zones
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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

alter table SPORT_ZONE_ITEM
  add zone_name text;

-- since existing zones will not have names, and the GUI will use these as
-- labels, put some default names on zones.
update SPORT_ZONE_ITEM
   set zone_name = 'Zone ' || zone_number
 where zone_name is null;

update SCHEMA_VERSION set version = 31;
