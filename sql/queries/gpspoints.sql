-- gpspoints.sql -- fetch GPS data for a session
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

-- this SQL query is used by trends-heatmap.rkt to fetch GPS points for a
-- session from the database

select T.position_lat as lat,
       T.position_long as lon
  from A_TRACKPOINT T, A_LENGTH L, A_LAP P
 where T.length_id = L.id
   and L.lap_id = P.id
   and P.session_id = ?
   and T.position_lat is not null
   and T.position_long is not null
 order by T.timestamp;
