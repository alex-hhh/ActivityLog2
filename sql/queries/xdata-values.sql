-- fetch-xdata-values.sql -- fetch XDATA values for a session
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


select T.timestamp, XV.field_id, XV.val
  from XDATA_VALUE XV,
       A_TRACKPOINT T,
       A_LENGTH L,
       A_LAP P
 where XV.trackpoint_id = T.id
   and T.length_id = L.id
   and L.lap_id = P.id
  and P.session_id = ?
order by T.timestamp;

