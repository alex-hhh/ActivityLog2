-- p22-cp.sql -- add critical power parameters storage
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

drop view if exists MV_CRITICAL_POWER;
drop view if exists V_CRITICAL_POWER_FOR_SESSION;
drop view if exists V_CRITICAL_POWER;
drop index if exists IX0_CRITICAL_POWER;
drop table if exists CRITICAL_POWER;

-- Store critical power parameters for a sport (running or cycling).  The
-- parameters have a validity time stamp, and are valid after that timestamp
-- untill a newer set of parameters are available. The V_CRITICAL_POWER view
-- mimics this table but contains an automatically updated valid_until field.
create table CRITICAL_POWER (
  id integer not null primary key autoincrement,
  valid_from integer not null,
  sport_id integer not null,
  sub_sport_id integer,
  cp real not null, -- critical power (watts), or cricital velocity (m/s)
  wprime real not null, -- anaerobic work capacity (W' in joules, or D' in meters)
  tau real   -- wprime reconstitution time constant, see doc/critical-power.md
  );

create unique index IX0_CRITICAL_POWER
  on SPORT_ZONE(sport_id, sub_sport_id, valid_from);

-- Mimic the CRICITAL_POWER table, but add a valid_until field, so that the
-- critical power for a timestamp can be located using a timestamp and a
-- "between" SQL operator.  A critical power setting is valid until a newer CP
-- for the same sport is defined.  The last zone in the field has the
-- valid_until set to the current seconds + 1 day in the future.
--
-- NOTE: the "+ 0" syntax in the strftime() call is needed to coerce the value
-- from a string to a number.
create view V_CRITICAL_POWER as
  select CP.id as cp_id,
         CP.sport_id as sport_id,
         CP.sub_sport_id as sub_sport_id,
         CP.cp as cp,
         CP.wprime as wprime,
         CP.tau as tau,
         CP.valid_from as valid_from,
         (select ifnull(min(CP1.valid_from - 1), strftime('%s', datetime('now', '+1 day')) + 0)
            from CRITICAL_POWER CP1
           where CP1.sport_id = CP.sport_id
             and ((CP1.sub_sport_id is null and CP.sub_sport_id is null)
                  or CP1.sub_sport_id = CP.sub_sport_id)
             and CP1.valid_from > CP.valid_from) as valid_until
    from CRITICAL_POWER CP;

-- Associate critical power settings with sessions using the start time of the
-- session.  This view can be used to easily determine the critical power
-- setting for a specific session.
create view V_CRITICAL_POWER_FOR_SESSION as
select S.id as session_id,
       VCP.cp_id as cp_id
  from A_SESSION S, V_CRITICAL_POWER VCP
 where S.sport_id = VCP.sport_id
   and (((S.sub_sport_id is null or S.sub_sport_id = 0)
         and (VCP.sub_sport_id is null or VCP.sub_sport_id = 0))
         or S.sub_sport_id = VCP.sub_sport_id)
   and S.start_time between VCP.valid_from and VCP.valid_until;

-- List the sport zones in a user friendly format (dereferencing sport names
-- and metric names and adding the number of sessions that use each zone
-- definition.  This is intended db management purposes. MV == management view
create view MV_CRITICAL_POWER as
  select VCP.cp_id as cp_id,
         (select ES.name from E_SPORT ES where ES.id = VCP.sport_id) as sport,
         (select ESS.name from E_SUB_SPORT ESS where ESS.id = VCP.sub_sport_id) as sub_sport,
         VCP.cp as cp,
         VCP.wprime as wprime,
         VCP.tau as tau,
         datetime(VCP.valid_from, 'unixepoch', 'localtime') as valid_from,
         datetime(VCP.valid_until, 'unixepoch', 'localtime') as valid_until,
         (select count(VCPFS.session_id)
            from V_CRITICAL_POWER_FOR_SESSION VCPFS
           where VCPFS.cp_id = VCP.cp_id) as session_count
    from V_CRITICAL_POWER VCP;

update SCHEMA_VERSION set version = 23;
