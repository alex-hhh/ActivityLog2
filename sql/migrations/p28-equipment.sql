-- p28-equipment.sql -- speed up V_EQUIPMENT_USE and V_EQUIPMENT_SLOG_CURRENT
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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

drop index if exists IX0_EQIPMENT_USE;
drop view if exists V_EQUIPMENT_USE;
drop view if exists V_EQUIPMENT_SLOG_CURRENT;

-- make it a covering index, so we don't have to read the table pages in at
-- all.
create index IX0_EQIPMENT_USE on EQUIPMENT_USE(equipment_id, session_id);


-- no need to scan the EQUIPMENT table
create view V_EQUIPMENT_USE as
select EU.equipment_id as equipment_id,
       ifnull(count(EU.session_id), 0) as use_count,
       ifnull(sum(SS.total_timer_time), 0) as hours_used,
       ifnull(sum(SS.total_distance), 0) as kms_used,
       ifnull(min(S.start_time), 0) as first_use,
       ifnull(max(S.start_time), 0) as last_use
  from EQUIPMENT_USE EU,
       A_SESSION S,
       SECTION_SUMMARY SS
 where EU.session_id = S.id
   and S.summary_id = SS.id
 group by EU.equipment_id;

-- no need to scan the equipment table
create view V_EQUIPMENT_SLOG_CURRENT as
select ESL.id as service_log_id,
       round(case ESL.service_type
       when 0 then (select total(SS1.total_timer_time)
                      from EQUIPMENT_USE EU1, A_SESSION S1, SECTION_SUMMARY SS1
                     where ESL.equipment_id = EU1.equipment_id
                       and EU1.session_id = S1.id
                       and S1.summary_id = SS1.id
                       and S1.start_time > ESL.start_date
                       and S1.start_time < ifnull(ESL.end_date, 3600 + strftime('%s','now')))
       when 1 then (select total(SS2.total_distance)
                      from EQUIPMENT_USE EU2, A_SESSION S2, SECTION_SUMMARY SS2
                     where ESL.equipment_id = EU2.equipment_id
                       and EU2.session_id = S2.id
                       and S2.summary_id = SS2.id
                       and S2.start_time > ESL.start_date
                       and S2.start_time < ifnull(ESL.end_date, 3600 + strftime('%s','now')))
       when 2 then ((ifnull(ESL.end_date, strftime('%s','now')) - ESL.start_date) / (24 * 3600))
       else null end) as current
  from EQUIPMENT_SERVICE_LOG ESL, E_SERVICE_TYPE EST
 where ESL.service_type = EST.id;

update SCHEMA_VERSION set version = 28;

