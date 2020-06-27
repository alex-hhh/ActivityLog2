-- p35-cp3.sql -- update CRITICAL_POWER table for CP3 model
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

alter table CRITICAL_POWER add column pmax real;

drop view if exists V_CRITICAL_POWER;

create view V_CRITICAL_POWER as
  select CP.id as cp_id,
         CP.sport_id as sport_id,
         CP.sub_sport_id as sub_sport_id,
         CP.cp as cp,
         CP.wprime as wprime,
         CP.tau as tau,
         CP.pmax as pmax,
         CP.valid_from as valid_from,
         (select ifnull(min(CP1.valid_from - 1), strftime('%s', datetime('now', '+1 day')) + 0)
            from CRITICAL_POWER CP1
           where CP1.sport_id = CP.sport_id
             and ((CP1.sub_sport_id is null and CP.sub_sport_id is null)
                  or CP1.sub_sport_id = CP.sub_sport_id)
             and CP1.valid_from > CP.valid_from) as valid_until
    from CRITICAL_POWER CP;

drop view if exists MV_CRITICAL_POWER;

create view MV_CRITICAL_POWER as
  select VCP.cp_id as cp_id,
         (select ES.name from E_SPORT ES where ES.id = VCP.sport_id) as sport,
         (select ESS.name from E_SUB_SPORT ESS where ESS.id = VCP.sub_sport_id) as sub_sport,
         VCP.cp as cp,
         VCP.wprime as wprime,
         VCP.tau as tau,
         VCP.pmax as pmax,
         datetime(VCP.valid_from, 'unixepoch', 'localtime') as valid_from,
         datetime(VCP.valid_until, 'unixepoch', 'localtime') as valid_until,
         (select count(VCPFS.session_id)
            from V_CRITICAL_POWER_FOR_SESSION VCPFS
           where VCPFS.cp_id = VCP.cp_id) as session_count
    from V_CRITICAL_POWER VCP;

update SCHEMA_VERSION set version = 35;
