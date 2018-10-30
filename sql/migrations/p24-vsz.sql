-- p24-cp.sql -- update the V_SPORT_ZONE view
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

drop view if exists V_SPORT_ZONE;

create view V_SPORT_ZONE as
  select SZ.id as zone_id,
         SZ.sport_id as sport_id,
         SZ.sub_sport_id as sub_sport_id,
         SZ.zone_metric_id as zone_metric_id,
         SZ.valid_from as valid_from,
         (select ifnull(min(SZ1.valid_from - 1), strftime('%s', datetime('now', '+1 day')) + 0)
            from SPORT_ZONE SZ1
           where SZ1.sport_id = SZ.sport_id
             and ((SZ1.sub_sport_id is null and SZ.sub_sport_id is null)
                  or SZ1.sub_sport_id = SZ.sub_sport_id)
             and SZ1.zone_metric_id = SZ.zone_metric_id
             and SZ1.valid_from > SZ.valid_from) as valid_until
    from SPORT_ZONE SZ;

update SCHEMA_VERSION set version = 24;

