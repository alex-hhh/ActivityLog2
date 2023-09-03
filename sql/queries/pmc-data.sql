-- SPDX-License-Identifier: GPL-3.0-or-later
-- pmc-data.sql -- fetch data for the Performance Management Chart
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

-- Unix timestamp for day start in local time
-- strftime("%s", datetime('2020-01-01', 'utc'))

with
  first_day(day)
    as (select ?),
  last_day(day)
    as (select ?),
  session_tss(day, tss)
    as (select date(VAL.start_time + VAL.duration, 'unixepoch', 'localtime') as day,
               VAL.tss as tss
          from V_ACTIVITY_LIST VAL
         where (VAL.start_time
                between strftime('%s', datetime((select day from first_day), 'utc'))
                and strftime('%s', datetime((select day from last_day), 'utc')))
           and VAL.tss > 0
         order by day),
  daily_tss(day, tss)
    as (select day, round(sum(tss))
          from session_tss
         group by day),
  day_stream(day)
    as (select day from first_day
         union all
        select date(day, '+1 day') as day
          from day_stream
         where day < (select day from last_day))
select cast(strftime('%s', datetime(day_stream.day, 'utc')) as decimal) as timestamp,
       cast(strftime('%s', datetime(day_stream.day, '+12 hours', 'utc')) as decimal) as tsmidday,
       day_stream.day as day,
       coalesce(daily_tss.tss, 0) as tss
  from day_stream
       left join daily_tss
           on day_stream.day = daily_tss.day;
