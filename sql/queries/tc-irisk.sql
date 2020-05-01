-- tc-irisk.sql -- Fetch Training Volume data for the IRisk trends chart
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

with recursive
  TS(week) as (
    select date(?, 'unixepoch', 'localtime', 'weekday 1') as week
     union all
    select date(week, '+7 days', 'weekday 1') as week
      from TS
     where strftime('%s', week) < strftime('%s', date(?, 'unixepoch', 'localtime', '-6 days', 'weekday 1'))),
  SE(week, rDist, rDuration, rTss, bDist, bDuration, bTss, sDist, sDuration, sTss) as (
    select date(VTS.start_time, 'unixepoch', 'localtime', '-6 days', 'weekday 1') as week,
           total(VTS.run_distance) / 1000.0 as rDist,
           total(VTS.run_time) / 3600.0 as rDuration,
           total(VTS.run_effort) as rTss,
           total(VTS.bike_distance) / 1000.0 as bDist,
           total(VTS.bike_time) / 3600.0 as bDuration,
           total(VTS.bike_effort) as bTss,
           total(VTS.swim_distance) / 1000.0 as sDist,
           total(VTS.swim_time) / 3600.0 as sDuration,
           total(VTS.swim_effort) as sTss
      from V_TRIATHLON_SESSIONS VTS
     where VTS.start_time between (select min(strftime('%s', TS.week)) from TS)
           and (select max(strftime('%s', TS.week)) from TS)
     group by week)
select (strftime('%s', TS.week) + 0) as timestamp, -- +0 forces the column to be an int
       TS.week as week,
       coalesce(SE.sDist, 0) as sDist,
       coalesce(SE.sDuration, 0) as sDuration,
       coalesce(SE.sTss, 0) as sTss,
       coalesce(SE.bDist, 0) as bDist,
       coalesce(SE.bDuration, 0) as bDuration,
       coalesce(SE.bTss, 0) as bTss,
       coalesce(SE.rDist, 0) as rDist,
       coalesce(SE.rDuration, 0) as rDuration,
       coalesce(SE.rTss, 0) as rTss
  from TS left join SE on TS.week = SE.week;
