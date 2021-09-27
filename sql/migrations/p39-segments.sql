-- SPDX-License-Identifier: GPL-3.0-or-later
-- p39-segments.sql -- add GPS segments related tables to the database
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

create table GPS_SEGMENT (
  id integer not null primary key autoincrement,
  name text not null,
  segment_length real not null,
  segment_height real,
  segment_grade real,
  total_ascent real,
  total_descent real,
  max_grade real,
  min_elevation real,
  max_elevation real
);

-- The way-points that define a segment.  Within a segment, way-points are
-- ordered by the POS column, which otherwise has no meaning.

create table GPS_SEGMENT_WAYPOINT (
  id integer not null primary key autoincrement,
  segment_id integer not null,
  pos integer not null, -- an incrementing number used for ordering
  latitude real not null,
  longitude real not null,
  distance real not null,
  geoid integer not null,               -- geoid based on latitude and longitude
  altitude real,                        -- altitude in meters, may be NULL
  grade real,                           -- segment grade, if altitude is not NULL
  foreign key (segment_id) references GPS_SEGMENT(id) on delete cascade
);

create index IX0_GPS_SEGMENT_WAYPOINT on GPS_SEGMENT_WAYPOINT(segment_id, pos);

-- A segment match represents the portion of a session which traverses a given
-- segment.  We store references to the segment and session, as well as
-- pointers to the trackpoints representing the start and end of the match.
-- Note that a segment might have multiple matches within a session, e.g when
-- the session traverses the same route more than once, and, of course, a
-- segment might have matches in multiple sessions.

create table GPS_SEGMENT_MATCH (
  id integer not null primary key autoincrement,
  segment_id integer not null,
  session_id integer not null,
  summary_id integer not null,          -- summary data for this segment
  start_trackpoint_id integer not null,
  end_trackpoint_id integer not null,
  match_cost real not null,             -- DTW cost for the match
  foreign key (segment_id) references GPS_SEGMENT(id) on delete cascade,
  foreign key (session_id) references A_SESSION(id) on delete cascade,
  foreign key (summary_id) references SECTION_SUMMARY(id) on delete set null,
  foreign key (start_trackpoint_id) references A_TRACKPOINT(id) on delete cascade,
  foreign key (end_trackpoint_id) references A_TRACKPOINT(id) on delete cascade
);

create index IX0_GPS_SEGMENT_MATCH on GPS_SEGMENT_MATCH(segment_id);
create index IX1_GPS_SEGMENT_MATCH on GPS_SEGMENT_MATCH(session_id);

-- "Exploded view" of a GPS segment match, bringing in the summary values.
-- This is used to populate the match view in the activity, similar to
-- V_ACTIVITY_LIST.
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
         SS.total_elapsed_time as duration,
         SS.total_distance as distance,
         SS.avg_speed as speed,
         SS.max_speed as max_speed,
         SS.avg_heart_rate as hr,
         SS.max_heart_rate as max_hr,
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

update SCHEMA_VERSION set version = 39;
