-- p46-aerolab.sql -- add/update some sport types
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

-- Move sailing into its own sport, corresponding to the FIT file format
delete from E_SPORT where id = 32;
insert into E_SPORT(id, name, color, icon) values(32, 'Sailing', -1, 'sail');

update A_SESSION
   set sport_id = 32,
       sub_sport_id = null
 where sport_id = 0
   and sub_sport_id = 257;

delete from E_SUB_SPORT where id = 257 and sport_id = 0;

-- Move hiking into its own sport, corresponding to the FIT file format

delete from E_SPORT where id = 17;
insert into E_SPORT(id, name, color, icon) values(17, 'Hiking', -1, 'hike');

update A_SESSION
   set sport_id = 17,
       sub_sport_id = null
 where sport_id = 0
   and sub_sport_id = 256;

delete from E_SUB_SPORT where id = 256 and sport_id = 0;

delete from E_SPORT where id = 37;
insert into E_SPORT(id, name, color, icon) values(37, 'Stand Up Paddleboarding', -1, 'sup');

delete from E_SPORT where id = 41;
insert into E_SPORT(id, name, color, icon) values(41, 'Kayaking', -1, 'kayak');

delete from E_SPORT where id = 53;
insert into E_SPORT(id, name, color, icon) values(53, 'Diving', -1, 'diving');

update SCHEMA_VERSION set version = 47;
