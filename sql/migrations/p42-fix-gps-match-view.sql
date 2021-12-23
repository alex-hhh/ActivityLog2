-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- p42-fix-gps-match-view.sql -- fix V_GPS_SEGMENT_MATCH_LIST to use
-- total_timer_time for duration.
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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

drop view if exists V_GPS_SEGMENT_MATCH_LIST;

create view V_GPS_SEGMENT_MATCH_LIST as
  select S.id as session_id,
         GS.id as segment_id,
         GSM.id as match_id,
         GSM.match_cost as match_cost,
         S.name as headline,
         S.start_time as session_start_time,
         T_START.timestamp as segment_start_time,
         T_END.timestamp as segment_end_time,
         (select name from E_TIME_ZONE ETZ where ETZ.id = S.time_zone_id) as time_zone,
         S.sport_id as sport,
         S.sub_sport_id as sub_sport,
         -- NOTE: elapsed time is used as "Duration" for laps in the rest of
         -- the application
         SS.total_timer_time as duration,
         SS.total_elapsed_time as elapsed,
         SS.total_distance as distance,
         SS.avg_speed as speed,
         SS.max_speed as max_speed,
         SS.avg_heart_rate as hr,
         SS.max_heart_rate as max_hr,
         SS.avg_temperature as avg_temperature,
         SS.max_temperature as max_temperature,
         SS.avg_cadence as cadence,
         SS.max_cadence as max_cadence,
         SS.total_distance / (2 * SS.total_cycles) as stride,
         ifnull(SS.total_corrected_ascent, SS.total_ascent) as ascent,
         ifnull(SS.total_corrected_descent, SS.total_descent) as descent,
         SS.avg_vertical_oscillation as vosc,
         SS.avg_stance_time as gct,
         SS.avg_stance_time_percent as gct_pct,
         SS.avg_power as power,
         SS.max_power as max_power,
         SS.normalized_power as np,
         SS.left_right_balance as lrbal,
         SS.avg_left_torque_effectiveness as ltorqeff,
         SS.avg_right_torque_effectiveness as rtorqeff,
         SS.avg_left_pedal_smoothness as lpdlsmth,
         SS.avg_right_pedal_smoothness as rpdlsmth,
         SS.avg_left_pco as lpco,
         SS.avg_right_pco as rpco,
         SS.avg_left_pp_start as lppstart,
         SS.avg_left_pp_end as lppend,
         SS.avg_right_pp_start as rppstart,
         SS.avg_right_pp_end as rppend,
         SS.avg_left_ppp_start as lpppstart,
         SS.avg_left_ppp_end as lpppend,
         SS.avg_right_ppp_start as rpppstart,
         SS.avg_right_ppp_end as rpppend,
         SS.aerobic_decoupling as adecl
    from GPS_SEGMENT_MATCH GSM,
         A_SESSION S,
         GPS_SEGMENT GS,
         SECTION_SUMMARY SS,
         A_TRACKPOINT T_START,
         A_TRACKPOINT T_END
   where GSM.summary_id = SS.id
     and GSM.session_id = S.id
     and GSM.segment_id = GS.id
     and GSM.start_trackpoint_id = T_START.id
     and GSM.end_trackpoint_id = T_END.id;

update SCHEMA_VERSION set version = 42;
