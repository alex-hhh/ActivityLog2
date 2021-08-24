-- fetch-swim-trackpoints.sql -- fetch trackpoints for a swim activtiy
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2018 Alex Hars�nyi <AlexHarsanyi@gmail.com>
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

-- This SQL query is used by session-df.rkt to fetch trackpoints for a lap
-- swimming session from the database.  We calculate the cadence as length
-- time / total strokes, as this produces a fractional cadence which looks
-- nicer on the various graphs.

select L.start_time as timestamp,
       (select max(T.distance) from A_TRACKPOINT T where T.length_id = L.id) as dst,
       (select round(avg(T.heart_rate)) from A_TRACKPOINT T where T.length_id = L.id) as hr,
       ifnull(SS.total_timer_time, 0) as duration,
       SS.avg_speed as spd,
       round(60.0 * SS.total_cycles / SS.total_timer_time, 1) as cad,
       SS.swim_stroke_id as swim_stroke,
       SS.total_cycles as strokes
  from A_LENGTH L, A_LAP P, SECTION_SUMMARY SS
 where L.lap_id = P.id
   and L.summary_id = SS.id
   and P.session_id = ?
 order by L.start_time;
