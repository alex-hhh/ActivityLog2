-- SPDX-License-Identifier: GPL-3.0-or-later
-- pmc-data.sql -- fetch data for the Performance Management Chart
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2023 Alex Hars�nyi <AlexHarsanyi@gmail.com>
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

select
  date(VAL.start_time + VAL.duration, 'unixepoch', 'localtime') as day,
  VAL.start_time as start_time,
  VAL.headline,
  VAL.sport,
  VAL.sub_sport,
  VAL.tss as tss
  from V_ACTIVITY_LIST VAL
 where VAL.start_time between ? and ?
   and VAL.tss > 0
 order by day;
