-- db-schema.sql -- database schema for ActivityLog2
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

create table SCHEMA_VERSION(version integer);
insert into SCHEMA_VERSION(version) values(14);


--........................................................ Enumerations ....

create table E_SPORT (
  id integer not null primary key autoincrement,
  name text unique not null,
  color integer not null, -- an index into the aplication's list of predefined colors
  icon string not null    -- a predefined icon name
  );

insert into E_SPORT(id, name, color, icon) values(0, 'Generic', -1, '');
insert into E_SPORT(id, name, color, icon) values(1, 'Running', 1, 'run');
insert into E_SPORT(id, name, color, icon) values(2, 'Cycling', 9, 'bike');
insert into E_SPORT(id, name, color, icon) values(3, 'Transition', 8, 'walk');
insert into E_SPORT(id, name, color, icon) values(4, 'Fitness Equipment', 11, 'weight-lift');
insert into E_SPORT(id, name, color, icon) values(5, 'Swimming', 5, 'swim');
insert into E_SPORT(id, name, color, icon) values(6, 'Basketball', -1, '');
insert into E_SPORT(id, name, color, icon) values(7, 'Soccer', -1, '');
insert into E_SPORT(id, name, color, icon) values(8, 'Tennis', -1, '');
insert into E_SPORT(id, name, color, icon) values(9, 'American Football', -1, '');
insert into E_SPORT(id, name, color, icon) values(10, 'Training', -1, '');
insert into E_SPORT(id, name, color, icon) values(12, 'Cross Country Skiing', -1, '');
insert into E_SPORT(id, name, color, icon) values(13, 'Alpine Skiing', -1, '');
insert into E_SPORT(id, name, color, icon) values(14, 'Snowboarding', -1, '');
insert into E_SPORT(id, name, color, icon) values(15, 'Rowing', -1, '');
insert into E_SPORT(id, name, color, icon) values(254, 'All', -1, '');

create table E_SUB_SPORT(
  id integer not null primary key autoincrement,
  sport_id integer not null,
  name text unique not null,
  color integer not null,
  icon string not null,
  foreign key (sport_id) references E_SPORT(id));

insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(0, 0, 'Generic', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(1, 1, 'Treadmill Running', 1, 'run');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(2, 1, 'Street Running', 1, 'run');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(3, 1, 'Trail Running', 1, 'run');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(4, 1, 'Track Running', 1, 'run');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(5, 2, 'Spin Cycling', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(6, 2, 'Indoor Cycling', 9, 'bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(7, 2, 'Road Cycling', 9, 'bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(8, 2, 'Mountain Biking', 9, 'mountain-bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(9, 2, 'Downhill Biking', 9, 'mountain-bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(10, 2, 'Recumbent Cycling', 9, 'bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(11, 2, 'Cyclocross', 9, 'bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(12, 2, 'Hand Cycling', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(13, 2, 'Track Cycling', 9, 'bike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(14, 4, 'Indoor Rowing', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(15, 4, 'Elliptical', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(16, 4, 'Stair Climbing', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(17, 5, 'Lap Swimming', 5, 'swim');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(18, 5, 'Open Water Swimming', 5, 'swim');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(19, 4, 'Flexibility Training', 10, 'yoga');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(20, 4, 'Strength Training', 11, 'weight-lift');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(21, 0, 'Warm Up', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(22, 0, 'Match', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(23, 0, 'Exercise', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(24, 0, 'Challenge', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(25, 0, 'Indoor Skiing', -1, '');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values(254, 254, 'All', -1, '');

-- other sport ID's, not defined by the FIT file format, we use identifiers
-- greater than 255, so we won't clash with the default sport names.

insert into E_SUB_SPORT(id, sport_id, name, color, icon) values (256, 0, 'Hiking', 3, 'hike');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values (257, 0, 'Sailing', 3, 'sail');
insert into E_SUB_SPORT(id, sport_id, name, color, icon) values (258, 0, 'Note', 3, 'note');

create table E_SWIM_STROKE(
  id integer not null primary key autoincrement,
  name text unique not null);

insert into E_SWIM_STROKE(id, name) values (0, 'Freestyle');
insert into E_SWIM_STROKE(id, name) values (1, 'Backstroke');
insert into E_SWIM_STROKE(id, name) values (2, 'Breaststroke');
insert into E_SWIM_STROKE(id, name) values (3, 'Butterfly');
insert into E_SWIM_STROKE(id, name) values (4, 'Drill');
insert into E_SWIM_STROKE(id, name) values (5, 'Mixed');
insert into E_SWIM_STROKE(id, name) values (6, 'IM');



--.......................................................... Activities ....

create table ACTIVITY (
  id integer not null primary key autoincrement,
  start_time integer not null,
  name text,
  description text,

  -- a unique identifier for the activity.  For FIT files, this is set to the
  -- creators serial number + creation time.  This allows us to avoid
  -- re-importing activities.
  guid text
  );

create index IX0_ACTIVITY on ACTIVITY(start_time);
create unique index IX1_ACTIVITY on ACTIVITY(guid);

-- We store files from which we import activities (.FIT and .TCX) in this
-- table, so we can restore them and maybe re-import them when we want to
-- analyze data that is currently being discarded by the importer.
create table ACTIVITY_RAW_DATA (
  id integer not null primary key autoincrement,
  activity_id integer not null,
  file_name text not null,
  data blob not null,                   -- compressed using gzip

  foreign key (activity_id) references ACTIVITY(id)
);

create index IX0_ACTIVITY_RAW_DATA on ACTIVITY_RAW_DATA(activity_id);

-- Summary data about a section (length, lap or session) in a workout session
create table SECTION_SUMMARY (
  id integer not null primary key autoincrement,
  total_timer_time real,
  total_elapsed_time real,
  total_distance real,
  total_calories integer,

  avg_speed real,
  max_speed real,

  avg_heart_rate integer,
  max_heart_rate integer,

  -- For bike, this is cadence, for running we store here steps and stride
  -- length, for swimming we store strokes and stroke length
  avg_cadence integer,                  -- cycles / min
  max_cadence integer,
  total_cycles integer,
  avg_cycle_distance real,

  avg_power real,
  max_power real,
  normalized_power real,

  total_ascent integer,
  total_descent integer,

  total_corrected_ascent integer,
  total_corrected_descent integer,

  swim_stroke_id integer,

  -- Running dynamics fields

  avg_vertical_oscillation real,
  avg_stance_time real,
  avg_stance_time_percent real,

  -- Cycling dynamics fields

  left_right_balance real,
  avg_left_torque_effectiveness real,
  avg_right_torque_effectiveness real,
  avg_left_pedal_smoothness real,
  avg_right_pedal_smoothness real,

  avg_left_pco real,                    -- PCO = platform centre offset, mm
  avg_right_pco real,

  avg_left_pp_start real,               -- PP = power phase
  avg_left_pp_end real,
  avg_right_pp_start real,
  avg_right_pp_end real,

  avg_left_ppp_start real,              -- PPP = peak power phase
  avg_left_ppp_end real,
  avg_right_ppp_start real,
  avg_right_ppp_end real,

  foreign key (swim_stroke_id) references E_SWIM_STROKE(id)
  );

create index IX0_SECTION_SUMMARY on SECTION_SUMMARY(id);

-- an activity can have one or more sessions.  Most of the time there will be
-- only one session, except for multi-sport activitites e.g. Bike + Transition
-- + Run.
create table A_SESSION (
  id integer not null primary key autoincrement,
  name text,
  description text,
  activity_id integer,
  start_time integer,
  sport_id integer,
  sub_sport_id integer,
  pool_length integer,
  pool_length_unit integer,
  training_effect real,
  training_stress_score real,
  intensity_factor real,

  rpe_scale integer check (rpe_scale >= 1 and rpe_scale <= 10), -- Rating of Perceived Extertion

  summary_id integer,

  foreign key (activity_id) references ACTIVITY(id),
  foreign key (sport_id) references E_SPORT(id),
  foreign key (sub_sport_id) references E_SUB_SPORT(id),
  foreign key (summary_id) references SECTION_SUMMARY(id) on delete set null
  );

create index IX0_A_SESSION on A_SESSION(activity_id);
create index IX1_A_SESSION on A_SESSION(start_time);
create index IX2_A_SESSION on A_SESSION(sport_id, sub_sport_id);

-- a session can have one or more laps which divide the session into
-- meaningfull sections (e.g every 1km or every workout step is a lap).
create table A_LAP (
  id integer not null primary key autoincrement,
  session_id integer,
  start_time integer,
  summary_id integer,

  foreign key (session_id) references A_SESSION(id),
  foreign key (summary_id) references SECTION_SUMMARY(id) on delete set null
  );

create index IX0_A_LAP on A_LAP(session_id);

-- A lap can have one or more lenghts representing logical sub-divisions of a
-- lap.  Currently the Garmim Swim generats a length record for every pool
-- length (a lap is created when "pause" is hit).  Our laps will always have
-- at lease one length (which might be generated implicitely at import).
create table A_LENGTH (
  id integer not null primary key autoincrement,
  lap_id integer,                       -- the lap this length is part of
  start_time integer,
  summary_id integer,

  foreign key (lap_id) references A_LAP(id),
  foreign key (summary_id) references SECTION_SUMMARY(id) on delete set null
  );

create index IX0_A_LENGTH on A_LENGTH(lap_id);

create table A_TRACKPOINT (
  id integer not null primary key autoincrement,
  length_id integer not null,
  timestamp integer not null,           -- trackpoints are ordered by their timestamp

  position_lat real,
  position_long real,
  altitude real,
  corrected_altitude real,              -- filled in by elevation-correction.rkt
  distance real,                        -- from previous trackpoint
  cadence real,
  speed real,                           -- comes from a sesor, like footpod or bike cadence
  heart_rate real,
  power real,
  accumulated_power real,

  -- Running dynamics fields

  vertical_oscillation real,            -- cm
  stance_time real,                     -- (a.k.a ground contact time), milliseconds
  stance_time_percent real,

  -- Cycling dynamics fields

  left_right_balance real,

  left_torque_effectiveness real,
  right_torque_effectiveness real,
  left_pedal_smoothness real,
  right_pedal_smoothness real,

  left_pco real,                        -- PCO = platform centre offset (mm)
  right_pco real,

  left_pp_start real,                   -- PP = power phase
  left_pp_end real,                     -- degrees, clockwise, 0 at the top
  right_pp_start real,
  right_pp_end real,

  left_ppp_start real,                  -- PPP = peak power phase
  left_ppp_end real,                    -- degrees, clockwise, 0 at the top
  right_ppp_start real,
  right_ppp_end real,
  tile_code integer,                    -- see elevation-correction.rkt

  foreign key (length_id) references A_LENGTH(id)
  );

create index IX0_A_TRACKPOINT on A_TRACKPOINT(length_id);

create index IX1_A_TRACKPOINT on A_TRACKPOINT(tile_code);



--......................................................... Sport Zones ....

create table E_ZONE_METRIC (
  id integer not null primary key autoincrement,
  name text unique not null);

insert into E_ZONE_METRIC(id, name) values (1, 'Heart Rate');
insert into E_ZONE_METRIC(id, name) values (2, 'Speed/Pace');
insert into E_ZONE_METRIC(id, name) values (3, 'Power');

-- Sport zone definitions are per sport and per zone metric.  This allows to
-- define, for example, separate HR and pace zone for running, cycling and
-- swimming.  By convention, if sub_sport_id is NULL, this zone definitions
-- applies to all sub-sports of the sport_id.
--
-- The valid_from field is a UNIX timestamp from which this sport zone
-- definition is valid.  It will be valid until a newer sport zone is defined.
-- The V_SPORT_ZONE view mimics this table but contains an automatically
-- updated valid_until field.
create table SPORT_ZONE (
  id integer not null primary key autoincrement,
  valid_from integer not null,
  sport_id integer not null,
  sub_sport_id integer,
  zone_metric_id integer not null,
  foreign key (sport_id) references E_SPORT(id),
  foreign key (sub_sport_id) references E_SUB_SPORT(id),
  foreign key (zone_metric_id) references E_ZONE_METRIC(id)
  );

create unique index IX0_SPORT_ZONE
  on SPORT_ZONE(sport_id, sub_sport_id, zone_metric_id, valid_from);

-- The zone_number 0 is by convention the minimum value and the last zone ID
-- would be the max value.  The value in zone_value defines the start of the
-- range and the end of the rage coincides with the start of the next zones.
--
-- For example, a 5 zone HR system will have 7 entries, where value 0 will be
-- the minimum heart rate and value 6 will be the max heart rate.  Values 1 to
-- 5 will define the actual zones.
create table SPORT_ZONE_ITEM (
  sport_zone_id integer not null,
  zone_number integer not null,
  zone_value real not null,
  foreign key (sport_zone_id) references SPORT_ZONE(id) on delete cascade);

-- Hold the time spent in each sport zone for a session.
create table TIME_IN_ZONE (
  session_id integer not null,
  sport_zone_id integer not null,
  zone_id integer not null,
  duration integer not null default 0,  -- seconds
  foreign key (session_id) references A_SESSION(id),
  foreign key (sport_zone_id) references SPORT_ZONE(id));

create index IX0_TIME_IN_ZONE on TIME_IN_ZONE(session_id);

-- Mimic the SPORT_ZONE table, but add a valid_until field, so that the sport
-- zone for a timestamp can be located using a timestamp and a "between" SQL
-- operator.  A sport zone is valid until a newer zone of the same type for
-- the same sport is defined.  The last zone in the field has the valid_until
-- set to the current seconds + 1 day in the future.
create view V_SPORT_ZONE as
  select SZ.id as zone_id,
         SZ.sport_id as sport_id,
         SZ.sub_sport_id as sub_sport_id,
         SZ.zone_metric_id as zone_metric_id,
         SZ.valid_from as valid_from,
         (select ifnull(min(SZ1.valid_from), strftime('%s', datetime('now', '+1 day')))
            from SPORT_ZONE SZ1
           where SZ1.sport_id = SZ.sport_id
             and ((SZ1.sub_sport_id is null and SZ.sub_sport_id is null)
                  or SZ1.sub_sport_id = SZ.sub_sport_id)
             and SZ1.zone_metric_id = SZ.zone_metric_id
             and SZ1.valid_from > SZ.valid_from) as valid_until
    from SPORT_ZONE SZ;

-- Associate sport zones with sessions using the start time of the session.
-- This view can be used to easily determine the sport zone for a specific
-- session.
create view V_SPORT_ZONE_FOR_SESSION as
select S.id as session_id,
       VSZ.zone_id as zone_id,
       VSZ.zone_metric_id as zone_metric_id
  from A_SESSION S, V_SPORT_ZONE VSZ
 where S.sport_id = VSZ.sport_id
   and (((S.sub_sport_id is null or S.sub_sport_id = 0)
         and (VSZ.sub_sport_id is null or VSZ.sub_sport_id = 0))
         or S.sub_sport_id = VSZ.sub_sport_id)
   and S.start_time between VSZ.valid_from and VSZ.valid_until;

-- List the sport zones in a user friendly format (dereferencing sport names
-- and metric names and adding the number of sessions that use each zone
-- definition).  This is intended db management purposes. MV == management
-- view
create view MV_SPORT_ZONE as
  select VSZ.zone_id as zone_id,
         (select ES.name from E_SPORT ES where ES.id = VSZ.sport_id) as sport,
         (select ESS.name from E_SUB_SPORT ESS where ESS.id = VSZ.sub_sport_id) as sub_sport,
         EZM.name as zone_metric,
         datetime(VSZ.valid_from, 'unixepoch', 'localtime') as valid_from,
         datetime(VSZ.valid_until, 'unixepoch', 'localtime') as valid_until,
         (select count(VSZFS.session_id)
            from V_SPORT_ZONE_FOR_SESSION VSZFS
           where VSZFS.zone_id = VSZ.zone_id) as session_count
    from V_SPORT_ZONE VSZ,
         E_ZONE_METRIC EZM
   where VSZ.zone_metric_id = EZM.id;


--............................................................. Weather ....

create table SESSION_WEATHER (
  id integer not null primary key autoincrement,
  session_id not null,
  wstation text not null,           -- weather station identification
  timestamp not null,               -- when the observation was taken
  temperature real,                 -- celsius
  dew_point real,                   -- celsius
  humidity real,                    -- percentage: 0 .. 100
  wind_speed real,                  -- km/h
  wind_gusts real,                  -- km/h
  wind_direction real,              -- degrees 0 - N, 90 - E, 180 - S, 270 - W
  pressure real,
  foreign key (session_id) references A_SESSION(id) on delete cascade
  );

create index IX0_SESSION_WEATHER on SESSION_WEATHER(session_id);


--........................................................... Equipment ....

create table EQUIPMENT (
  id integer not null primary key autoincrement,
  name text,        -- e.g. "My Garmin 310", set by the user
  device_name text, -- e.g. "Garmin Forerunner 310XT", "Garmin Swim", "Footpod", etc
  description text,
  retired integer default 0,  -- when non 0, this euqipment is no longer in use
  manufacturer_id integer,
  device_id integer,
  serial_number integer,
  -- another equipment this one is part of, for example a bike cadence sensor
  -- can be attached to a bike.  Using this field, the import mechanism can
  -- assign a bike to an activity based on the presence of the part (cadence
  -- sensor).
  part_of integer,
  foreign key (part_of) references EQUIPMENT(id) on delete set null);

create table EQUIPMENT_USE (
  equipment_id integer not null,
  session_id integer not null,
  foreign key (equipment_id) references EQUIPMENT(id),
  foreign key (session_id) references A_SESSION(id)
  );

create index IX0_EQIPMENT_USE on EQUIPMENT_USE(equipment_id);
create unique index IX1_EQIPMENT_USE on EQUIPMENT_USE(session_id, equipment_id);

create table E_SERVICE_TYPE (
  id integer not null primary key autoincrement,
  name text unique not null);

insert into E_SERVICE_TYPE(id, name) values(0, 'Time');
insert into E_SERVICE_TYPE(id, name) values(1, 'Mileage');
insert into E_SERVICE_TYPE(id, name) values(2, 'Calendar time');

-- Record servicing reminders for equipment.  A servicing reminder has a start
-- date and a target (hours of use, mileage or calendar days), see also
-- V_EQUIPMENT_SLOG_CURRENT.
create table EQUIPMENT_SERVICE_LOG (
  id integer not null primary key autoincrement,
  equipment_id integer not null,
  name text not null,
  start_date integer not null,
  end_date integer,                     -- if not null, this service log is complete
  service_type integer not null,
  -- if service_type is 0 (Time), target is hours; if it is 1 (Mileage),
  -- target is km, if it is 2 (Calendar time), target is days.
  target integer not null,
  foreign key (equipment_id) references EQUIPMENT(id) on delete cascade,
  foreign key (service_type) references E_SERVICE_TYPE(id)
  );

create index IX0_EQIPMENT_SERVICE_LOG on EQUIPMENT_SERVICE_LOG(equipment_id);

-- View that determines usage level for each piece of equipment.  Note that
-- equipment which has no sessions referenced will not be listed here.
create view V_EQUIPMENT_USE as
select EQ.id as equipment_id,
       ifnull(count(EU.session_id), 0) as use_count,
       ifnull(sum(SS.total_timer_time), 0) as hours_used,
       ifnull(sum(SS.total_distance), 0) as kms_used,
       ifnull(min(S.start_time), 0) as first_use,
       ifnull(max(S.start_time), 0) as last_use
  from EQUIPMENT EQ,
       EQUIPMENT_USE EU,
       A_SESSION S,
       SECTION_SUMMARY SS
 where EQ.id = EU.equipment_id
   and EU.session_id = S.id
   and S.summary_id = SS.id
 group by EQ.id;

-- View that contains the current usage count for service log entries.  Note
-- that completed entries (the ones that have a non-null end_date) only count
-- up to that date.
create view V_EQUIPMENT_SLOG_CURRENT as
select ESL.id as service_log_id,
       round(case ESL.service_type
       when 0 then (select total(SS1.total_timer_time)
                      from EQUIPMENT EQ1, EQUIPMENT_USE EU1, A_SESSION S1, SECTION_SUMMARY SS1
                     where EQ1.id = EU1.equipment_id
                       and EU1.session_id = S1.id
                       and S1.summary_id = SS1.id
                       and EQ1.id = ESL.equipment_id
                       and S1.start_time > ESL.start_date
                       and S1.start_time < ifnull(ESL.end_date, 3600 + strftime('%s','now')))
       when 1 then (select total(SS2.total_distance)
                      from EQUIPMENT EQ2, EQUIPMENT_USE EU2, A_SESSION S2, SECTION_SUMMARY SS2
                     where EQ2.id = EU2.equipment_id
                       and EU2.session_id = S2.id
                       and S2.summary_id = SS2.id
                       and EQ2.id = ESL.equipment_id
                       and S2.start_time > ESL.start_date
                       and S2.start_time < ifnull(ESL.end_date, 3600 + strftime('%s','now')))
       when 2 then ((ifnull(ESL.end_date, strftime('%s','now')) - ESL.start_date) / (24 * 3600))
       else null end) as current
  from EQUIPMENT_SERVICE_LOG ESL, E_SERVICE_TYPE EST
 where ESL.service_type = EST.id;


--..................................................... Athlete Metrics ....

create table E_SLEEP_QUALITY (
  id integer not null primary key autoincrement,
  name text unique not null
  );

insert into E_SLEEP_QUALITY(id, name) values(0, 'Bad');
insert into E_SLEEP_QUALITY(id, name) values(1, 'Average');
insert into E_SLEEP_QUALITY(id, name) values(2, 'Good');

create table E_OVERALL_FEELING (
  id integer not null primary key autoincrement,
  name text unique not null
  );

insert into E_OVERALL_FEELING(id, name) values(0, 'Bad');
insert into E_OVERALL_FEELING(id, name) values(1, 'Below average');
insert into E_OVERALL_FEELING(id, name) values(2, 'Above average');
insert into E_OVERALL_FEELING(id, name) values(3, 'Good');
insert into E_OVERALL_FEELING(id, name) values(4, 'Best');

create table ATHLETE_METRICS (
  id integer not null primary key autoincrement,
  timestamp integer not null,           -- See note below
  body_weight real,                     -- in kilograms
  sleep_time integer,                   -- in seconds
  sleep_quality integer,
  overall_feeling integer,
  description text,
  foreign key (sleep_quality) references E_SLEEP_QUALITY(id),
  foreign key (overall_feeling) references E_OVERALL_FEELING(id)
  );

create index IX0_ATHLETE_METRICS on ATHLETE_METRICS(timestamp);

-- On ATHLETE_METRICS.timestamp: this timestamp can include both the date and
-- the time when the recording was made (thus allowing for multiple recordings
-- per day).
--
-- To find the metrics for a particular date (may return multiple entries):
--
-- SELECT * FROM ATHLETE_METRICS
-- WHERE date(timestamp, 'unixepoch', 'localtime') = '2015-04-08';


--.............................................................. Labels ....

create table LABEL (
  id integer not null primary key autoincrement,
  name text not null,
  description text);

create table SESSION_LABEL(
  label_id integer not null,
  session_id integer not null,
  foreign key (label_id) references LABEL(id),
  foreign key (session_id) references A_SESSION(id)
);

create unique index IX0_SESSION_LABEL on SESSION_LABEL(session_id, label_id);
create index IX1_SESSION_LABEL on SESSION_LABEL(label_id);


--............................................................. Seasons ....

-- A season is a date range definition, useful for selecting activities.
create table SEASON (
  id integer not null primary key autoincrement,
  name text unique not null,
  description text,
  start_date integer not null,
  end_date integer not null,
  check (start_date < end_date));


--............................................................. Athlete ....

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


--......................................................... Last import ....

-- when an activity is imported its session_id is stored in this table.  The
-- import process can clear this table just before the import, so this table
-- contains just activities from the last import.
create table LAST_IMPORT(
  activity_id integer not null,
  foreign key (activity_id) references ACTIVITY(id));

-- Temporary table for storing a list of session IDs
create table TMP_SESSION_LIST(
  session_id integer primary key
);


--......................................................... Other views ....

-- View that expands a session summary data (total distance and total time)
-- into swim/bike/run/strength training colums.  This view is used by the
-- "Triathlon Trainig Volume" reports.
create view V_TRIATHLON_SESSIONS as
select S.id as session_id,
       S.start_time as start_time,
       S.sport_id as sport_id,
       S.sub_sport_id as sub_sport_id,
       (case S.sport_id when 1 then 1 else 0 end) as run_count,
       (case S.sport_id when 1 then SS.total_distance else 0 end) as run_distance,
       (case S.sport_id when 1 then SS.total_timer_time else 0 end) as run_time,
       (case S.sport_id when 2 then 1 else 0 end) as bike_count,
       (case S.sport_id when 2 then SS.total_distance else 0 end) as bike_distance,
       (case S.sport_id when 2 then SS.total_timer_time else 0 end) as bike_time,
       (case S.sport_id when 5 then 1 else 0 end) as swim_count,
       (case S.sport_id when 5 then SS.total_distance else 0 end) as swim_distance,
       (case S.sport_id when 5 then SS.total_timer_time else 0 end) as swim_time,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then 1 else 0 end) else 0 end) as strength_count,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then SS.total_distance else 0 end) else 0 end) as strength_distance,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then SS.total_timer_time else 0 end) else 0 end) as strength_time,
       1 as sport_count,
       SS.total_distance as sport_distance,
       SS.total_timer_time as sport_time
  from A_SESSION S, SECTION_SUMMARY SS
 where S.summary_id = SS.id
   and (S.sport_id in (1, 2, 5) or (S.sport_id = 4 and S.sub_sport_id = 20));


-- Expands all summary data about a every session (duration, time, max speed,
-- etc).  This can be used as a convenience view without having to remember
-- all the "right" joins to get info about a session.  It is used by the
-- view-activities.rkt

create view V_ACTIVITY_LIST as
  select S.id as session_id,
         A.guid as activity_guid,
         S.name as headline,
         S.start_time as start_time,
         S.sport_id as sport,
         S.sub_sport_id as sub_sport,
         S.rpe_scale as rpe,
         S.training_effect as te,
         S.training_stress_score as tss,
         S.intensity_factor as ifact,
         SS.total_timer_time as duration,
         SS.total_distance as distance,
         SS.total_calories as calories,
         SS.avg_speed as speed,
         SS.max_speed as max_speed,
         SS.avg_heart_rate as hr,
         SS.max_heart_rate as max_hr,
         SS.avg_cadence as cadence,
         SS.max_cadence as max_cadence,
         SS.total_distance / (2 * SS.total_cycles) as stride,
         ifnull(SS.total_corrected_ascent, SS.total_ascent) as ascent,
         ifnull(SS.total_corrected_descent, SS.total_descent) as descent,
         SS.avg_vertical_oscillation as vosc,
         SS.avg_stance_time as gct,
         SS.avg_stance_time_percent as gct_pct,
         SS.avg_power as power,
         SS.max_power as max_power,
         SS.normalized_power as np,
         SS.left_right_balance as lrbal,
         SS.avg_left_torque_effectiveness as ltorqeff,
         SS.avg_right_torque_effectiveness as rtorqeff,
         SS.avg_left_pedal_smoothness as lpdlsmth,
         SS.avg_right_pedal_smoothness as rpdlsmth,
         SS.avg_left_pco as lpco,
         SS.avg_right_pco as rpco,
         SS.avg_left_pp_start as lppstart,
         SS.avg_left_pp_end as lppend,
         SS.avg_right_pp_start as rppstart,
         SS.avg_right_pp_end as rppend,
         SS.avg_left_ppp_start as lpppstart,
         SS.avg_left_ppp_end as lpppend,
         SS.avg_right_ppp_start as rpppstart,
         SS.avg_right_ppp_end as rpppend,
         (select AM1.body_weight
            from ATHLETE_METRICS AM1
           where AM1.timestamp = (
             select max(AM.timestamp)
               from ATHLETE_METRICS AM
              where AM.timestamp between S.start_time - 84600 and S.start_time)) as body_weight,
         (select temperature from SESSION_WEATHER SW1 where SW1.session_id = S.id) as temperature,
         (select humidity from SESSION_WEATHER SW2 where SW2.session_id = S.id) as humidity,
         (select wind_speed from SESSION_WEATHER SW3 where SW3.session_id = S.id) as wind_speed,
         (select wind_direction from SESSION_WEATHER SW4 where SW4.session_id = S.id) as wind_direction,
         (select dew_point from SESSION_WEATHER SW1 where SW1.session_id = S.id) as dew_point
    from A_SESSION S, SECTION_SUMMARY SS, ACTIVITY A
   where S.summary_id = SS.id
     and S.activity_id = A.id;

-- Local Variables:
-- sql-product: sqlite
-- compile-command: "rm al.db;  sqlite3 -batch al.db < db-schema.sql"
-- End:
