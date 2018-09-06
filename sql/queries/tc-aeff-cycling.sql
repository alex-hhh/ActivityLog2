-- tc-aeff-cycling.sql -- cycling aerobic efficiency SQL query
-- 
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

select datetime(VAL.start_time, 'unixepoch', 'localtime') as date,
       VAL.session_id as id,
       VAL.start_time as timestamp,
       VAL.headline as headline,
       VAL.duration as duration,
       VAL.distance as distance,
       VAL.power as power,
       VAL.np as npower,
       VAL.hr as heart_rate,
       round(coalesce(VAL.np, VAL.power) / VAL.hr, 3) as ae
  from V_ACTIVITY_LIST VAL
 where VAL.start_time between ? and ?
   and VAL.sport = 2                    -- cycling
   and VAL.power is not null
   and VAL.hr is not null
 order by timestamp;
