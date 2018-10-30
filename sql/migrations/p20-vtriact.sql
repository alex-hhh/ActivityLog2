-- p20-vtriact.sql -- update V_TRIATHLON_SESSIONS to include effort
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2016 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

drop view V_TRIATHLON_SESSIONS;

create view V_TRIATHLON_SESSIONS as
select S.id as session_id,
       S.start_time as start_time,
       S.sport_id as sport_id,
       S.sub_sport_id as sub_sport_id,
       (case S.sport_id when 1 then 1 else 0 end) as run_count,
       (case S.sport_id when 1 then SS.total_distance else 0 end) as run_distance,
       (case S.sport_id when 1 then SS.total_timer_time else 0 end) as run_time,
       (case S.sport_id when 1 then S.training_stress_score else 0 end) as run_effort,
       (case S.sport_id when 2 then 1 else 0 end) as bike_count,
       (case S.sport_id when 2 then SS.total_distance else 0 end) as bike_distance,
       (case S.sport_id when 2 then SS.total_timer_time else 0 end) as bike_time,
       (case S.sport_id when 2 then S.training_stress_score else 0 end) as bike_effort,
       (case S.sport_id when 5 then 1 else 0 end) as swim_count,
       (case S.sport_id when 5 then SS.total_distance else 0 end) as swim_distance,
       (case S.sport_id when 5 then SS.total_timer_time else 0 end) as swim_time,
       (case S.sport_id when 5 then S.training_stress_score else 0 end) as swim_effort,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then 1 else 0 end) else 0 end) as strength_count,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then SS.total_distance else 0 end) else 0 end) as strength_distance,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then SS.total_timer_time else 0 end) else 0 end) as strength_time,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then S.training_stress_score else 0 end) else 0 end) as strength_effort,
       1 as sport_count,
       SS.total_distance as sport_distance,
       SS.total_timer_time as sport_time,
       S.training_stress_score as effort
  from A_SESSION S, SECTION_SUMMARY SS
 where S.summary_id = SS.id
   and (S.sport_id in (1, 2, 5) or (S.sport_id = 4 and S.sub_sport_id = 20));

update SCHEMA_VERSION set version = 20;
