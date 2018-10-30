-- p13-athlete-info.sql -- upgrade an existing database
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2016 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

.bail on

create table ATHLETE (
  -- trick to ensure there is only one entry in this table, every insert will
  -- need to *explicitely* specify an ID of 0.
  id integer primary key check (id = 0),
  name text not null,
  gender integer check (gender >= 0 and gender <= 1), -- 0 female, 1 male
  dob text not null,                                  -- YYYY-MM-DD format
  height integer check (height > 0),                  -- meters

  -- NOTE: the parameters below are changing over time, but we don't record
  -- that.  We only record the 'current' value.

  ftp integer,                          -- Functional Threshol Power (watts)
  swim_tpace integer                    -- Swim Threshold Pace (meters / sec)
  );

-- Insert the only row in the ATHLETE table now.  It will only need to be
-- udpated from now on.
insert into ATHLETE (id, name, gender, dob, height)
values (0, 'Athlete', 1, '2000-01-01', 1.80);

/*
-- NOTE: not supported in SQLLite from Racket 6.1.1
create view V_ATHLETE as
with
  hpw(h) as (
  select ((sum(SS.total_timer_time) / 3600.0) / 6) as h
    from A_SESSION S, SECTION_SUMMARY SS
   where S.start_time > strftime('%s', datetime('now', '-42 days'))
     and S.summary_id = SS.id),
  aclass(a) as (
    select (case
            when (select h from hpw) < 0.25 then 1
            when (select h from hpw) < 0.5 then 2
            when (select h from hpw) = 0.5 then 3 -- will probably never be hit
            when (select h from hpw) < 0.75 then 4
            when (select h from hpw) < 1 then 5
            when (select h from hpw) < 3 then 6
            when (select h from hpw) < 7 then 7
            when (select h from hpw) < 11 then 8
            when (select h from hpw) < 15 then 9
            else 10 end) as activity_class),
  age(ag) as (
    select cast(round((strftime('%s', 'now') - strftime('%s', dob))
                      / (365 * 24 * 3600.0)) as INTEGER)
             as age
      from ATHLETE)
select A.name as name,
       A.gender as gender,
       (case A.gender when 0 then 'female' when 1 then 'male' else 'unknown' end) as gname,
       A.dob as dob,
       AGE.ag as age,
       A.height as height,
       ACLASS.a as activity_class,
       A.ftp as ftp,
       A.swim_tpace as swim_tpace
  from ATHLETE A, ACLASS, AGE;
*/

update SCHEMA_VERSION set version = 13;

-- Local Variables:
-- sql-product: sqlite
-- End:
