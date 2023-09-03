-- SPDX-License-Identifier: GPL-3.0-or-later

-- db-session-matches.sql -- extract summary data for all GPS segment matches
-- for a session.
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

select GSM.id,
       GS.name,
       GS.fiets_score,
       T_START.timestamp as start_time,
       SS.total_timer_time,
       SS.total_elapsed_time,
       SS.total_distance,
       SS.total_calories,
       SS.avg_speed,
       SS.max_speed,
       SS.avg_heart_rate,
       SS.max_heart_rate,
       SS.avg_cadence,
       SS.max_cadence,
       SS.total_cycles,
       SS.avg_cycle_distance,
       SS.total_ascent,
       SS.total_descent,
       SS.total_corrected_ascent,
       SS.total_corrected_descent,
       SS.swim_stroke_id,
       SS.avg_vertical_oscillation,
       SS.avg_stance_time,
       SS.avg_stance_time_percent,
       SS.avg_power,
       SS.max_power,
       SS.normalized_power,
       SS.left_right_balance,
       SS.avg_left_torque_effectiveness,
       SS.avg_right_torque_effectiveness,
       SS.avg_left_pedal_smoothness,
       SS.avg_right_pedal_smoothness,
       SS.avg_combined_pedal_smoothness,
       SS.avg_left_pco,
       SS.avg_right_pco,
       SS.avg_left_pp_start,
       SS.avg_left_pp_end,
       SS.avg_right_pp_start,
       SS.avg_right_pp_end,
       SS.avg_left_ppp_start,
       SS.avg_left_ppp_end,
       SS.avg_right_ppp_start,
       SS.avg_right_ppp_end,
       SS.aerobic_decoupling
  from GPS_SEGMENT GS,
       GPS_SEGMENT_MATCH GSM,
       A_TRACKPOINT T_START,
       SECTION_SUMMARY SS
 where GSM.summary_id = SS.id
   and GSM.segment_id = GS.id
   and GSM.start_trackpoint_id = T_START.id
   and GSM.session_id = ?
 order by T_START.timestamp;
