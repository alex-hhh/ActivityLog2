-- tiz-outdated.sql -- return the list of sessions which are missing TIZ data
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


-- this SQL query is used to determine which sessions are missing TIME_IN_ZONE
-- data (and therefore need to have it updated).  It is used when new sport
-- zones are added (or old ones deleted), to determine what needs to be
-- updated.
--
-- the DISTINCT keyword is needed because we might get duplicates when more
-- than one zone type changes (HR, pace and power).  Also, we only look for HR
-- and POWER zones (1 and 3), as these are the only ones for which we
-- calculate TIME_IN_ZONE data.  Since the V_SPORT_ZONE_FOR_SESSION lists
-- sessions based on their start data, it might contain sessions which, while
-- they have a sport zone allocated, they don't have the necessary data
-- (E.g. a session might have a valid heard rate zone, but no heart rate
-- data). To filter these out, we look at the average heart rate and power
-- (technically we should look if there is heart rate and power data in the
-- session's trackpoints)

select distinct VSZFS.session_id
  from V_SPORT_ZONE_FOR_SESSION VSZFS,
       A_SESSION S,
       SECTION_SUMMARY SS
 where VSZFS.session_id = S.id
   and S.summary_id = SS.id
   and ((VSZFS.zone_metric_id = 1 and SS.avg_heart_rate is not null)
        or
        (VSZFS.zone_metric_id = 3 and SS.avg_power is not null))
   and not exists(select * from TIME_IN_ZONE TIZ
                   where TIZ.session_id = VSZFS.session_id
                     and TIZ.sport_zone_id = VSZFS.zone_id);
