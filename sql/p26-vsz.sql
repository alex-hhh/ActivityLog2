-- p26-vsz.sql -- fixup V_SPORT_ZONE_FOR_SESSION, V_CRITICAL_POWER_FOR_SESSION
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

-- fix V_SPORT_ZONE_FOR_SESSION and V_CRITICAL_POWER_FOR_SESSION to correctly
-- assign zones and CP settings to sports which have a non-null sub_sport_id
-- e.g Cycling zones (2, NULL) are assigned to Indoor Cycling (2, 6) sports,
-- if there is no Indoor Cycling specific zone defined.

drop view if exists V_SPORT_ZONE_FOR_SESSION;

create view V_SPORT_ZONE_FOR_SESSION as
  select S.id as session_id,
         VSZ.zone_id as zone_id,
         VSZ.zone_metric_id as zone_metric_id
    from A_SESSION S, V_SPORT_ZONE VSZ
   where S.sport_id = VSZ.sport_id
     and S.start_time between VSZ.valid_from and VSZ.valid_until
     and (S.sub_sport_id = VSZ.sub_sport_id
          -- sub_sport_id is NULL for both session and sport zone
          or ((S.sub_sport_id is null or S.sub_sport_id = 0)
              and (VSZ.sub_sport_id is null or VSZ.sub_sport_id = 0))
          -- sub_sport_id is NOT NULL for the session, but there is no
          -- specific sub_sport_id zone, 
          or (S.sub_sport_id > 0
              and (VSZ.sub_sport_id is null or VSZ.sub_sport_id = 0)
              and not exists (
                select * from V_SPORT_ZONE VSZ2
                 where S.sport_id = VSZ2.sport_id
                   and S.sub_sport_id = VSZ2.sub_sport_id
                   and S.start_time between VSZ2.valid_from and VSZ2.valid_until)));

drop view if exists V_CRITICAL_POWER_FOR_SESSION;

create view V_CRITICAL_POWER_FOR_SESSION as
  select S.id as session_id,
         VCP.cp_id as cp_id
    from A_SESSION S, V_CRITICAL_POWER VCP
   where S.sport_id = VCP.sport_id
     and S.start_time between VCP.valid_from and VCP.valid_until
     and (S.sub_sport_id = VCP.sub_sport_id
          -- sub_sport_id is NULL for both session and sport zone
          or ((S.sub_sport_id is null or S.sub_sport_id = 0)
              and (VCP.sub_sport_id is null or VCP.sub_sport_id = 0))
          -- sub_sport_id is NOT NULL for the session, but there is no
          -- specific sub_sport_id zone, so use the sport zone with a NULL
          -- sub_sport_id
          or (S.sub_sport_id > 0
              and (VCP.sub_sport_id is null or VCP.sub_sport_id = 0)
              and not exists (
                select * from V_SPORT_ZONE VCP2
                 where S.sport_id = VCP2.sport_id
                   and S.sub_sport_id = VCP2.sub_sport_id
                   and S.start_time between VCP2.valid_from and VCP2.valid_until)));

update SCHEMA_VERSION set version = 26;
