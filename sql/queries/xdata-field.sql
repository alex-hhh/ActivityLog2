-- fetch-xdata-field.sql -- fetch information about an XDATA field
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

select XA.app_guid as guid,
       coalesce(XF.native_message, -1) as native_message,
       coalesce(XF.native_field, -1) as native_field,
       coalesce(XF.name, 'Unnamed') as name,
       coalesce(XF.unit_name, 'Unlabeled') as units
  from XDATA_FIELD XF, XDATA_APP XA
 where XF.id = ? and XF.app_id = XA.id;
