-- SPDX-License-Identifier: GPL-3.0-or-later
-- p44-weather.sql -- update db to store weather data from FIT records
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2022 Alex Hars�nyi <AlexHarsanyi@gmail.com>
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

create table E_WEATHER_STATUS(
  id integer not null primary key autoincrement,
  name text unique not null);

insert into E_WEATHER_STATUS(id, name)
values (0, 'Clear'),
       (1, 'Partly Cloudy'),
       (2, 'Mostly Cloudy'),
       (3, 'Rain'),
       (4, 'Snow'),
       (5, 'Windy'),
       (6, 'Thunderstorms'),
       (7, 'Wintry Mix'),
       (8, 'Fog'),
       (11, 'Hazy'),
       (12, 'Hail'),
       (13, 'Scattered Showers'),
       (14, 'Scattered Thunderstorms'),
       (15, 'Unknown Precipitation'),
       (16, 'Light Rain'),
       (17, 'Heavy Rain'),
       (18, 'Light Snow'),
       (19, 'Heavy Snow'),
       (20, 'Light Rain Snow'),
       (21, 'Heavy Rain Snow'),
       (22, 'Cloudy');

alter table SESSION_WEATHER add weather_status_id integer references E_WEATHER_STATUS(id);
alter table SESSION_WEATHER add precipitation_probability real;
alter table SESSION_WEATHER add feels_like real;
alter table SESSION_WEATHER add position_lat real;
alter table SESSION_WEATHER add position_long real;

drop view if exists V_ACTIVITY_LIST;

create view V_ACTIVITY_LIST as
  select S.id as session_id,
         A.guid as activity_guid,
         S.name as headline,
         (select  group_concat(l.name, ' / ')
            from SESSION_LABEL SL, LABEL L
           where L.id == SL.label_id and SL.session_id = S.id ) as labels,
         S.start_time as start_time,
         (select name from E_TIME_ZONE ETZ where ETZ.id = S.time_zone_id) as time_zone,
         S.sport_id as sport,
         S.sub_sport_id as sub_sport,
         S.rpe_scale as rpe,
         S.training_effect as te,
         S.training_stress_score as tss,
         S.pool_length as pl,
         S.pool_length_unit as plu,
         S.intensity_factor as ifact,
         SS.total_timer_time as duration,
         SS.total_distance as distance,
         SS.total_calories as calories,
         SS.avg_speed as speed,
         SS.max_speed as max_speed,
         SS.avg_heart_rate as hr,
         SS.max_heart_rate as max_hr,
         SS.avg_temperature as avg_temperature,
         SS.max_temperature as max_temperature,
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
         SS.aerobic_decoupling as adecl,
         (select AM1.body_weight
            from ATHLETE_METRICS AM1
           where AM1.timestamp = (
             select max(AM.timestamp)
               from ATHLETE_METRICS AM
              where AM.timestamp between S.start_time - 84600 and S.start_time)) as body_weight,
         (select SH.sdnn
            from SESSION_HRV SH
           where SH.session_id = S.id) as hrv,
         SW.temperature as temperature,
         SW.humidity as humidity,
         SW.wind_speed as wind_speed,
         SW.wind_direction as wind_direction,
         SW.dew_point as dew_point,
         SW.precipitation_probability as precipitation_probability,
         SW.feels_like as feels_like,
         (select name from E_WEATHER_STATUS EWS where EWS.id = SW.weather_status_id) as weather_status
    from A_SESSION S
         left join SESSION_WEATHER SW
             on (SW.session_id = S.id
                 and SW.timestamp = (select min(timestamp)
                                       from SESSION_WEATHER
                                      where session_id = S.id)),
         SECTION_SUMMARY SS,
         ACTIVITY A
   where S.summary_id = SS.id
     and S.activity_id = A.id;

update SCHEMA_VERSION set version = 44;
