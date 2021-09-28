-- SPDX-License-Identifier: GPL-3.0-or-later
-- ec-update-segment.sql -- update summary elevation data for a segment
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

update GPS_SEGMENT
   set segment_height = ?,
       segment_grade = ?,
       total_ascent = ?,
       total_descent = ?,
       max_grade = ?,
       min_elevation = ?,
       max_elevation = ?
 where id = ?;
