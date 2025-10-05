-- db-schema.sql -- database schema for ActivityLog2
--
-- This file is part of ActivityLog2, an fitness activity tracker
-- Copyright (C) 2015 - 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
insert into SCHEMA_VERSION(version) values(53);


--........................................................ Enumerations ....

create table E_SPORT (
  id integer not null primary key autoincrement,
  name text unique not null,
  color integer not null, -- not used by the application anymore
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
insert into E_SPORT(id, name, color, icon) values(13, 'Alpine Skiing', 19, 'ski');
insert into E_SPORT(id, name, color, icon) values(14, 'Snowboarding', -1, '');
insert into E_SPORT(id, name, color, icon) values(15, 'Rowing', -1, '');
insert into E_SPORT(id, name, color, icon) values(17, 'Hiking', -1, 'hike');
insert into E_SPORT(id, name, color, icon) values(32, 'Sailing', -1, 'sail');
insert into E_SPORT(id, name, color, icon) values(37, 'Stand Up Paddleboarding', -1, 'sup');
insert into E_SPORT(id, name, color, icon) values(41, 'Kayaking', -1, 'kayak');
insert into E_SPORT(id, name, color, icon) values(53, 'Diving', -1, 'diving');
insert into E_SPORT(id, name, color, icon) values(254, 'All', -1, '');

create table E_SUB_SPORT(
  id integer not null primary key autoincrement,
  sport_id integer not null,
  name text unique not null,
  color integer not null,               -- not used by the application anymore
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

create table E_TIME_ZONE(
  id integer not null primary key autoincrement,
  name text unique not null);

create unique index IX0_E_TIME_ZONE on E_TIME_ZONE(name);

-- These are all the time zones supported by the tzgeolookup package, which in
-- turn are based on the data from
-- https://github.com/evansiroky/timezone-boundary-builder, if new time zones
-- appear, a new migration will have to be written to update this table, but
-- we hope this does not happen too often.
insert into E_TIME_ZONE(name)
values ('Africa/Abidjan'), ('Africa/Accra'), ('Africa/Addis_Ababa'), ('Africa/Algiers'), ('Africa/Asmara'),
       ('Africa/Bamako'), ('Africa/Bangui'), ('Africa/Banjul'), ('Africa/Bissau'), ('Africa/Blantyre'),
       ('Africa/Brazzaville'), ('Africa/Bujumbura'), ('Africa/Cairo'), ('Africa/Casablanca'), ('Africa/Ceuta'),
       ('Africa/Conakry'), ('Africa/Dakar'), ('Africa/Dar_es_Salaam'), ('Africa/Djibouti'), ('Africa/Douala'),
       ('Africa/El_Aaiun'), ('Africa/Freetown'), ('Africa/Gaborone'), ('Africa/Harare'), ('Africa/Johannesburg'),
       ('Africa/Juba'), ('Africa/Kampala'), ('Africa/Khartoum'), ('Africa/Kigali'), ('Africa/Kinshasa'),
       ('Africa/Lagos'), ('Africa/Libreville'), ('Africa/Lome'), ('Africa/Luanda'), ('Africa/Lubumbashi'),
       ('Africa/Lusaka'), ('Africa/Malabo'), ('Africa/Maputo'), ('Africa/Maseru'), ('Africa/Mbabane'),
       ('Africa/Mogadishu'), ('Africa/Monrovia'), ('Africa/Nairobi'), ('Africa/Ndjamena'), ('Africa/Niamey'),
       ('Africa/Nouakchott'), ('Africa/Ouagadougou'), ('Africa/Porto-Novo'), ('Africa/Sao_Tome'),
       ('Africa/Tripoli'), ('Africa/Tunis'), ('Africa/Windhoek'), ('America/Adak'), ('America/Anchorage'),
       ('America/Anguilla'), ('America/Antigua'), ('America/Araguaina'), ('America/Argentina/Buenos_Aires'),
       ('America/Argentina/Catamarca'), ('America/Argentina/Cordoba'), ('America/Argentina/Jujuy'),
       ('America/Argentina/La_Rioja'), ('America/Argentina/Mendoza'), ('America/Argentina/Rio_Gallegos'),
       ('America/Argentina/Salta'), ('America/Argentina/San_Juan'), ('America/Argentina/San_Luis'),
       ('America/Argentina/Tucuman'), ('America/Argentina/Ushuaia'), ('America/Aruba'), ('America/Asuncion'),
       ('America/Atikokan'), ('America/Bahia'), ('America/Bahia_Banderas'), ('America/Barbados'), ('America/Belem'),
       ('America/Belize'), ('America/Blanc-Sablon'), ('America/Boa_Vista'), ('America/Bogota'), ('America/Boise'),
       ('America/Cambridge_Bay'), ('America/Campo_Grande'), ('America/Cancun'), ('America/Caracas'),
       ('America/Cayenne'), ('America/Cayman'), ('America/Ciudad_Juarez'), ('America/Chicago'), ('America/Chihuahua'), ('America/Costa_Rica'),
       ('America/Creston'), ('America/Cuiaba'), ('America/Curacao'), ('America/Danmarkshavn'), ('America/Dawson'),
       ('America/Dawson_Creek'), ('America/Denver'), ('America/Detroit'), ('America/Dominica'), ('America/Edmonton'),
       ('America/Eirunepe'), ('America/El_Salvador'), ('America/Fort_Nelson'), ('America/Fortaleza'),
       ('America/Glace_Bay'), ('America/Nuuk'), ('America/Goose_Bay'), ('America/Grand_Turk'),
       ('America/Grenada'), ('America/Guadeloupe'), ('America/Guatemala'), ('America/Guayaquil'), ('America/Guyana'),
       ('America/Halifax'), ('America/Havana'), ('America/Hermosillo'), ('America/Indiana/Indianapolis'),
       ('America/Indiana/Knox'), ('America/Indiana/Marengo'), ('America/Indiana/Petersburg'),
       ('America/Indiana/Tell_City'), ('America/Indiana/Vevay'), ('America/Indiana/Vincennes'),
       ('America/Indiana/Winamac'), ('America/Inuvik'), ('America/Iqaluit'), ('America/Jamaica'),
       ('America/Juneau'), ('America/Kentucky/Louisville'), ('America/Kentucky/Monticello'), ('America/Kralendijk'),
       ('America/La_Paz'), ('America/Lima'), ('America/Los_Angeles'), ('America/Lower_Princes'), ('America/Maceio'),
       ('America/Managua'), ('America/Manaus'), ('America/Marigot'), ('America/Martinique'), ('America/Matamoros'),
       ('America/Mazatlan'), ('America/Menominee'), ('America/Merida'), ('America/Metlakatla'),
       ('America/Mexico_City'), ('America/Miquelon'), ('America/Moncton'), ('America/Monterrey'),
       ('America/Montevideo'), ('America/Montserrat'), ('America/Nassau'), ('America/New_York'),
       ('America/Nome'), ('America/Noronha'), ('America/North_Dakota/Beulah'),
       ('America/North_Dakota/Center'), ('America/North_Dakota/New_Salem'), ('America/Ojinaga'),
       ('America/Panama'), ('America/Paramaribo'), ('America/Phoenix'),
       ('America/Port-au-Prince'), ('America/Port_of_Spain'), ('America/Porto_Velho'), ('America/Puerto_Rico'),
       ('America/Punta_Arenas'), ('America/Rankin_Inlet'), ('America/Recife'),
       ('America/Regina'), ('America/Resolute'), ('America/Rio_Branco'), ('America/Santarem'),
       ('America/Santiago'), ('America/Santo_Domingo'), ('America/Sao_Paulo'), ('America/Scoresbysund'),
       ('America/Sitka'), ('America/St_Barthelemy'), ('America/St_Johns'), ('America/St_Kitts'),
       ('America/St_Lucia'), ('America/St_Thomas'), ('America/St_Vincent'), ('America/Swift_Current'),
       ('America/Tegucigalpa'), ('America/Thule'), ('America/Tijuana'),
       ('America/Toronto'), ('America/Tortola'), ('America/Vancouver'), ('America/Whitehorse'),
       ('America/Winnipeg'), ('America/Yakutat'), ('Antarctica/Casey'),
       ('Antarctica/Davis'), ('Antarctica/DumontDUrville'), ('Antarctica/Macquarie'), ('Antarctica/Mawson'),
       ('Antarctica/McMurdo'), ('Antarctica/Palmer'), ('Antarctica/Rothera'), ('Antarctica/Syowa'),
       ('Antarctica/Troll'), ('Antarctica/Vostok'), ('Arctic/Longyearbyen'), ('Asia/Aden'),
       ('Asia/Almaty'), ('Asia/Amman'), ('Asia/Anadyr'), ('Asia/Aqtau'), ('Asia/Aqtobe'), ('Asia/Ashgabat'),
       ('Asia/Atyrau'), ('Asia/Baghdad'), ('Asia/Bahrain'), ('Asia/Baku'), ('Asia/Bangkok'), ('Asia/Barnaul'),
       ('Asia/Beirut'), ('Asia/Bishkek'), ('Asia/Brunei'), ('Asia/Chita'), ('Asia/Choibalsan'), ('Asia/Colombo'),
       ('Asia/Damascus'), ('Asia/Dhaka'), ('Asia/Dili'), ('Asia/Dubai'), ('Asia/Dushanbe'), ('Asia/Famagusta'),
       ('Asia/Gaza'), ('Asia/Hebron'), ('Asia/Ho_Chi_Minh'), ('Asia/Hong_Kong'), ('Asia/Hovd'), ('Asia/Irkutsk'),
       ('Asia/Jakarta'), ('Asia/Jayapura'), ('Asia/Jerusalem'), ('Asia/Kabul'), ('Asia/Kamchatka'),
       ('Asia/Karachi'), ('Asia/Kathmandu'), ('Asia/Khandyga'), ('Asia/Kolkata'), ('Asia/Krasnoyarsk'),
       ('Asia/Kuala_Lumpur'), ('Asia/Kuching'), ('Asia/Kuwait'), ('Asia/Macau'), ('Asia/Magadan'),
       ('Asia/Makassar'), ('Asia/Manila'), ('Asia/Muscat'), ('Asia/Nicosia'), ('Asia/Novokuznetsk'),
       ('Asia/Novosibirsk'), ('Asia/Omsk'), ('Asia/Oral'), ('Asia/Phnom_Penh'), ('Asia/Pontianak'),
       ('Asia/Pyongyang'), ('Asia/Qatar'), ('Asia/Qostanay'), ('Asia/Qyzylorda'), ('Asia/Riyadh'),
       ('Asia/Sakhalin'), ('Asia/Samarkand'), ('Asia/Seoul'), ('Asia/Shanghai'), ('Asia/Singapore'),
       ('Asia/Srednekolymsk'), ('Asia/Taipei'), ('Asia/Tashkent'), ('Asia/Tbilisi'), ('Asia/Tehran'),
       ('Asia/Thimphu'), ('Asia/Tokyo'), ('Asia/Tomsk'), ('Asia/Ulaanbaatar'), ('Asia/Urumqi'),
       ('Asia/Ust-Nera'), ('Asia/Vientiane'), ('Asia/Vladivostok'), ('Asia/Yakutsk'), ('Asia/Yangon'),
       ('Asia/Yekaterinburg'), ('Asia/Yerevan'), ('Atlantic/Azores'), ('Atlantic/Bermuda'),
       ('Atlantic/Canary'), ('Atlantic/Cape_Verde'), ('Atlantic/Faroe'), ('Atlantic/Madeira'),
       ('Atlantic/Reykjavik'), ('Atlantic/South_Georgia'), ('Atlantic/St_Helena'), ('Atlantic/Stanley'),
       ('Australia/Adelaide'), ('Australia/Brisbane'), ('Australia/Broken_Hill'),
       ('Australia/Darwin'), ('Australia/Eucla'), ('Australia/Hobart'), ('Australia/Lindeman'),
       ('Australia/Lord_Howe'), ('Australia/Melbourne'), ('Australia/Perth'), ('Australia/Sydney'),
       ('Etc/GMT'), ('Etc/GMT+1'), ('Etc/GMT+10'), ('Etc/GMT+11'), ('Etc/GMT+12'), ('Etc/GMT+2'),
       ('Etc/GMT+3'), ('Etc/GMT+4'), ('Etc/GMT+5'), ('Etc/GMT+6'), ('Etc/GMT+7'), ('Etc/GMT+8'),
       ('Etc/GMT+9'), ('Etc/GMT-1'), ('Etc/GMT-10'), ('Etc/GMT-11'), ('Etc/GMT-12'), ('Etc/GMT-2'),
       ('Etc/GMT-3'), ('Etc/GMT-4'), ('Etc/GMT-5'), ('Etc/GMT-6'), ('Etc/GMT-7'), ('Etc/GMT-8'),
       ('Etc/GMT-9'), ('Etc/UTC'), ('Europe/Amsterdam'), ('Europe/Andorra'), ('Europe/Astrakhan'),
       ('Europe/Athens'), ('Europe/Belgrade'), ('Europe/Berlin'), ('Europe/Bratislava'),
       ('Europe/Brussels'), ('Europe/Bucharest'), ('Europe/Budapest'), ('Europe/Busingen'),
       ('Europe/Chisinau'), ('Europe/Copenhagen'), ('Europe/Dublin'), ('Europe/Gibraltar'),
       ('Europe/Guernsey'), ('Europe/Helsinki'), ('Europe/Isle_of_Man'), ('Europe/Istanbul'),
       ('Europe/Jersey'), ('Europe/Kaliningrad'), ('Europe/Kyiv'), ('Europe/Kirov'), ('Europe/Lisbon'),
       ('Europe/Ljubljana'), ('Europe/London'), ('Europe/Luxembourg'), ('Europe/Madrid'),
       ('Europe/Malta'), ('Europe/Mariehamn'), ('Europe/Minsk'), ('Europe/Monaco'),
       ('Europe/Moscow'), ('Europe/Oslo'), ('Europe/Paris'), ('Europe/Podgorica'), ('Europe/Prague'),
       ('Europe/Riga'), ('Europe/Rome'), ('Europe/Samara'), ('Europe/San_Marino'), ('Europe/Sarajevo'),
       ('Europe/Saratov'), ('Europe/Simferopol'), ('Europe/Skopje'), ('Europe/Sofia'), ('Europe/Stockholm'),
       ('Europe/Tallinn'), ('Europe/Tirane'), ('Europe/Ulyanovsk'), ('Europe/Vaduz'),
       ('Europe/Vatican'), ('Europe/Vienna'), ('Europe/Vilnius'), ('Europe/Volgograd'), ('Europe/Warsaw'),
       ('Europe/Zagreb'), ('Europe/Zurich'), ('Indian/Antananarivo'),
       ('Indian/Chagos'), ('Indian/Christmas'), ('Indian/Cocos'), ('Indian/Comoro'), ('Indian/Kerguelen'),
       ('Indian/Mahe'), ('Indian/Maldives'), ('Indian/Mauritius'), ('Indian/Mayotte'), ('Indian/Reunion'),
       ('Pacific/Apia'), ('Pacific/Auckland'), ('Pacific/Bougainville'), ('Pacific/Chatham'),
       ('Pacific/Chuuk'), ('Pacific/Easter'), ('Pacific/Efate'), ('Pacific/Kanton'), ('Pacific/Fakaofo'),
       ('Pacific/Fiji'), ('Pacific/Funafuti'), ('Pacific/Galapagos'), ('Pacific/Gambier'),
       ('Pacific/Guadalcanal'), ('Pacific/Guam'), ('Pacific/Honolulu'), ('Pacific/Kiritimati'),
       ('Pacific/Kosrae'), ('Pacific/Kwajalein'), ('Pacific/Majuro'), ('Pacific/Marquesas'),
       ('Pacific/Midway'), ('Pacific/Nauru'), ('Pacific/Niue'), ('Pacific/Norfolk'), ('Pacific/Noumea'),
       ('Pacific/Pago_Pago'), ('Pacific/Palau'), ('Pacific/Pitcairn'), ('Pacific/Pohnpei'),
       ('Pacific/Port_Moresby'), ('Pacific/Rarotonga'), ('Pacific/Saipan'), ('Pacific/Tahiti'),
       ('Pacific/Tarawa'), ('Pacific/Tongatapu'), ('Pacific/Wake'), ('Pacific/Wallis');

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
  aerobic_decoupling real,

  avg_temperature real,
  max_temperature real,

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
  avg_combined_pedal_smoothness real, -- some power meters provide a single combined value

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
  start_time integer,           -- always in UTC
  time_zone_id integer,         -- can be NULL, indicating timezone is unknown
  sport_id integer,
  sub_sport_id integer,
  pool_length integer,
  pool_length_unit integer,
  training_effect real,
  training_stress_score real,
  intensity_factor real,

  rpe_scale integer check (rpe_scale >= 1 and rpe_scale <= 10), -- Rating of Perceived Exertion
  feel_scale real check (feel_scale >= 0 and feel_scale <= 10),

  summary_id integer,

  foreign key (activity_id) references ACTIVITY(id),
  foreign key (sport_id) references E_SPORT(id),
  foreign key (sub_sport_id) references E_SUB_SPORT(id),
  foreign key (summary_id) references SECTION_SUMMARY(id) on delete set null,
  foreign key (time_zone_id) references E_TIME_ZONE(id)
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
  temperature real,

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
  combined_pedal_smoothness real, -- some power meters provide a single, combined value

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

  -- no longer used, should be null for all new track points, kept only to
  -- make the database backwards compatible.
  tile_code integer,

  -- a 64 bit integer representing the geographic location of this point Note
  -- that geoids are stored using an 2^63 offset to account for SQLite storing
  -- 64 bit values as signed.  See notes on the documentation for the geoid
  -- package.
  geoid integer,

  foreign key (length_id) references A_LENGTH(id)
  );

create index IX0_A_TRACKPOINT on A_TRACKPOINT(length_id);

-- NOTE: the latitude, longitude can be recovered from geoid, so we don't need
-- to index those to make it a covering index for elevation correction, saving
-- some space.  This is a covering index for the elevation correction query.
create index IX2_A_TRACKPOINT
  on A_TRACKPOINT(geoid, timestamp, altitude);

-- Improve speed of heat map queries
create index IX3_A_TRACKPOINT
  on A_TRACKPOINT(length_id, geoid);


--........................................................ gear changes ....

-- Stores gear change events that happened during a session (as recorded in
-- the FIT file).  Note that the "teeth" information, in front_gear_teeth and
-- rear_gear_teeth, comes from the device configuration itself and the user
-- must configure their gears on the device that recorded the session.
--
-- The `update-gear-rations-for-session!` function is provided to update gear
-- teeth counts, but currently this must be called from a Racket script.
--
-- Unlike the A_TRACKPOINT table, this table records "events", that is moments
-- in time when a gear change happened.  Each entry contains the gearing after
-- the change and must be compared with the previous entry (ordered by
-- timestamp), to determine which gear has changed.
create table GEAR_CHANGE(
  id integer not null primary key autoincrement,
  session_id integer not null,
  timestamp integer not null,
  front_gear_index integer,             -- 1 is innermost gear
  front_gear_teeth integer,
  rear_gear_index integer,              -- 1 is innermost gear
  rear_gear_teeth integer,
  foreign key (session_id) references A_SESSION(id) on delete cascade
);

create index IX1_GEAR_CHANGE
  on GEAR_CHANGE(session_id);


--............................................................... Xdata ....

-- Garmin allows third party applications to run on their devices and record
-- extra bits of data.  Each application will have an entry in this table,
-- created when we first encounter an application in a FIT file.  Technically,
-- a third party application is identified by a 16 byte developer id and a 16
-- byte application id, but in all the files that I have seen, the developer
-- id is set to FF, so the application id uniquely identifies a third party
-- application in the Garmin world.
--
-- The application name is not recorded in the FIT file, so it will have to be
-- set by the user.
create table XDATA_APP (
  id integer not null primary key autoincrement,
  app_guid text unique not null,
  dev_guid text
);

create index IX0_XDATA_APP on XDATA_APP(app_guid);

-- Each XDATA application can define one or more fields in which it records
-- data.  We list them here.  Fields have an unique name withing an
-- application GUID, but not necessarily with other applications.
create table XDATA_FIELD (
  id integer not null primary key autoincrement,
  app_id integer not null,
  name text not null,
  unit_name text,
  -- the native message where this field can appear.  We are mostly interested
  -- here in message 20, which is a data record.
  native_message integer,
  -- the native field for which this field corresponds, for example a running
  -- power application might indicate that this is a "power"(7) field. This
  -- can be NULL, indicating that the developer field does not correspond to
  -- any native field.
  native_field integer,
  foreign key (app_id) references XDATA_APP(id)
);

create unique index IX0_XDATA_FIELD on XDATA_FIELD(app_id, name);

-- An XDATA value attached to a track point record.
create table XDATA_VALUE (
  id integer not null primary key autoincrement,
  trackpoint_id integer not null,
  field_id integer not null,
  val real,
  foreign key (field_id) references XDATA_FIELD(id),
  foreign key (trackpoint_id) references A_TRACKPOINT(id) on delete cascade
);

-- Add the 'val' option to this index, to make it a covering index when we use
-- the query to fetch values for a sessions track points.  This will
-- effectively double the data storage needed for the XDATA_VALUE table, but
-- it is already almost double even if we leave 'val' out.  Also, I am happy
-- to trade off disk space for speed.
create index IX0_XDATA_VALUE on XDATA_VALUE(trackpoint_id, field_id, val);

-- A summary XDATA value (representing XDATA values added to sessions, laps
-- and lengths).  It is attached to a SECTION_SUMMARY row, which in turn is
-- referenced by a A_SESSION, A_LAP or A_LENGTH row.
create table XDATA_SUMMARY_VALUE (
  id integer not null primary key autoincrement,
  summary_id integer not null,
  field_id integer not null,
  val real,
  foreign key (field_id) references XDATA_FIELD(id),
  -- note: ON DELETE CASCADE is correct here.  We want this row gone when the
  -- corresponding SECTION_SUMMARY row is gone.
  foreign key (summary_id) references SECTION_SUMMARY(id) on delete cascade
);

create index IX0_XDATA_SUMMARY_VALUE on XDATA_SUMMARY_VALUE(summary_id, field_id);


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
--
-- Different names have been used for sport zones by different coaches (E.g
-- zone 1 is named either "Endurance" or "Fat Burning Zone" depending on who
-- you ask).  The zone_name field allows naming zones as the user pleases and
-- these names will be used as labels in the GUI.
create table SPORT_ZONE_ITEM (
  sport_zone_id integer not null,
  zone_number integer not null,
  zone_name text,
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
--
-- NOTE: the "+ 0" syntax in the strftime() call is needed to coerce the value
-- from a string to a number.
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

-- Associate sport zones with sessions using the start time of the session.
-- This view can be used to easily determine the sport zone for a specific
-- session.
create view V_SPORT_ZONE_FOR_SESSION as
  select S.id as session_id,
         VSZ.zone_id as zone_id,
         VSZ.zone_metric_id as zone_metric_id
    from A_SESSION S, V_SPORT_ZONE VSZ
   where S.sport_id = VSZ.sport_id
     and S.start_time between VSZ.valid_from and VSZ.valid_until
     and (S.sub_sport_id = VSZ.sub_sport_id
          -- sub_sport_id is NULL for both session and sport zone
          or ((S.sub_sport_id is null or S.sub_sport_id = 0)
              and (VSZ.sub_sport_id is null or VSZ.sub_sport_id = 0))
          -- sub_sport_id is NOT NULL for the session, but there is no
          -- specific sub_sport_id zone, so use the sport zone with a NULL
          -- sub_sport_id
          or (S.sub_sport_id > 0
              and (VSZ.sub_sport_id is null or VSZ.sub_sport_id = 0)
              and not exists (
                select * from V_SPORT_ZONE VSZ2
                 where S.sport_id = VSZ2.sport_id
                   and S.sub_sport_id = VSZ2.sub_sport_id
                   and S.start_time between VSZ2.valid_from and VSZ2.valid_until)));

-- Stores the session which was used to determine a sport zone.  E.g. a time
-- trial, FTP or FTHR session can be used to determine pace, heart rate or
-- power zones.  That session can be linked to the zones it determined using
-- this table.  This is used for code which automatically determines sport
-- zones, currently FTHR analysis does that and this table is used to
-- determine if the sport zones from a FTHR session have already been set.
--
-- WARNING: this table does not hold the sport zone which applies to a
-- session, see V_SPORT_ZONE_FOR_SESSION instead.
create table SPORT_ZONE_SOURCE (
  zone_id integer not null,
  session_id integer not null,

  foreign key (zone_id) references SPORT_ZONE(id) on delete cascade,
  foreign key (session_id) references A_SESSION(id) on delete cascade
);


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


--...................................................... Critical Power ....

-- Store critical power parameters for a sport (running or cycling).  The
-- parameters have a validity time stamp, and are valid after that timestamp
-- untill a newer set of parameters are available. The V_CRITICAL_POWER view
-- mimics this table but contains an automatically updated valid_until field.
create table CRITICAL_POWER (
  id integer not null primary key autoincrement,
  valid_from integer not null,
  sport_id integer not null,
  sub_sport_id integer,
  cp real not null, -- critical power (watts), or cricital velocity (m/s)
  wprime real not null, -- anaerobic work capacity (W' in joules, or D' in meters)

  -- DEPRECATED: wprime reconstitution time constant, see
  -- doc/critical-power.md.  We keep it here, but it is no longer used by the
  -- application.
  tau real,
  -- maximum power or velocity according to the CP3 model, or null for the CP2
  -- model.  Technically, the CP3 model uses a `k` constant, but that has
  -- little practical meaning, so we store Pmax.  `k` can be determine from
  -- cp, wprime and pmax.
  pmax real);

create unique index IX0_CRITICAL_POWER
  on SPORT_ZONE(sport_id, sub_sport_id, valid_from);

-- Mimic the CRICITAL_POWER table, but add a valid_until field, so that the
-- critical power for a timestamp can be located using a timestamp and a
-- "between" SQL operator.  A critical power setting is valid until a newer CP
-- for the same sport is defined.  The last zone in the field has the
-- valid_until set to the current seconds + 1 day in the future.
--
-- NOTE: the "+ 0" syntax in the strftime() call is needed to coerce the value
-- from a string to a number.
create view V_CRITICAL_POWER as
  select CP.id as cp_id,
         CP.sport_id as sport_id,
         CP.sub_sport_id as sub_sport_id,
         CP.cp as cp,
         CP.wprime as wprime,
         CP.tau as tau,
         CP.pmax as pmax,
         CP.valid_from as valid_from,
         (select ifnull(min(CP1.valid_from - 1), strftime('%s', datetime('now', '+1 day')) + 0)
            from CRITICAL_POWER CP1
           where CP1.sport_id = CP.sport_id
             and ((CP1.sub_sport_id is null and CP.sub_sport_id is null)
                  or CP1.sub_sport_id = CP.sub_sport_id)
             and CP1.valid_from > CP.valid_from) as valid_until
    from CRITICAL_POWER CP;

-- Associate critical power settings with sessions using the start time of the
-- session.  This view can be used to easily determine the critical power
-- setting for a specific session.
create view V_CRITICAL_POWER_FOR_SESSION as
  select S.id as session_id,
         VCP.cp_id as cp_id
    from A_SESSION S, V_CRITICAL_POWER VCP
   where S.sport_id = VCP.sport_id
     and S.start_time between VCP.valid_from and VCP.valid_until
     and (S.sub_sport_id = VCP.sub_sport_id
          -- sub_sport_id is NULL for both session and sport zone
          or ((S.sub_sport_id is null or S.sub_sport_id = 0)
              and (VCP.sub_sport_id is null or VCP.sub_sport_id = 0))
          -- sub_sport_id is NOT NULL for the session, but there is no
          -- specific sub_sport_id zone, so use the sport zone with a NULL
          -- sub_sport_id
          or (S.sub_sport_id > 0
              and (VCP.sub_sport_id is null or VCP.sub_sport_id = 0)
              and not exists (
                select * from V_SPORT_ZONE VCP2
                 where S.sport_id = VCP2.sport_id
                   and S.sub_sport_id = VCP2.sub_sport_id
                   and S.start_time between VCP2.valid_from and VCP2.valid_until)));

-- List the sport zones in a user friendly format (dereferencing sport names
-- and metric names and adding the number of sessions that use each zone
-- definition.  This is intended db management purposes. MV == management view
create view MV_CRITICAL_POWER as
  select VCP.cp_id as cp_id,
         (select ES.name from E_SPORT ES where ES.id = VCP.sport_id) as sport,
         (select ESS.name from E_SUB_SPORT ESS where ESS.id = VCP.sub_sport_id) as sub_sport,
         VCP.cp as cp,
         VCP.wprime as wprime,
         VCP.tau as tau,
         VCP.pmax as pmax,
         datetime(VCP.valid_from, 'unixepoch', 'localtime') as valid_from,
         datetime(VCP.valid_until, 'unixepoch', 'localtime') as valid_until,
         (select count(VCPFS.session_id)
            from V_CRITICAL_POWER_FOR_SESSION VCPFS
           where VCPFS.cp_id = VCP.cp_id) as session_count
    from V_CRITICAL_POWER VCP;


--................................................................. HRV ....

-- Store HRV (Heart Rate Variability) data for the session (if this is
-- recorded in the import file).  HRV data stored here is based on the entire
-- session.  See also rkt/hrv.rkt.
create table SESSION_HRV (
  id integer not null primary key,
  session_id integer not null,
  sdnn integer not null,                -- STDDEV of NN intervals (hrv samples)
  rmssd integer not null, -- root mean square of successive differences (delta-hrv)
  sdsd integer not null, -- stddev of successive differences (delta-hrv)
  nn50 integer not null, -- # of successive pairs that differ by more than 50ms (delta-hrv)
  nn20 integer not null, -- # of successive pairs that differ by more than 20ms (delta-hrv)
  good_samples integer not null,        -- # of samples where good-hrv? is #t
  bad_samples integer not null,         -- # of samples where good-hrv? is #f
  foreign key (session_id) references A_SESSION(id) on delete cascade
  );

create unique index IX0_SESSION_HRV on SESSION_HRV(session_id);


--............................................................. Weather ....

create table SESSION_WEATHER (
  id integer not null primary key autoincrement,
  session_id not null,
  wstation text not null,           -- weather station identification
  weather_status_id integer,
  timestamp not null,               -- when the observation was taken
  temperature real,                 -- celsius
  feels_like real,                  -- celsius, heat index or wind chill
  dew_point real,                   -- celsius
  humidity real,                    -- percentage: 0 .. 100
  precipitation_probability real,
  wind_speed real,                  -- m/s
  wind_gusts real,                  -- m/s
  wind_direction real,              -- degrees 0 - N, 90 - E, 180 - S, 270 - W
  pressure real,
  position_lat real,                    -- location of the weather station
  position_long real,
  foreign key (session_id) references A_SESSION(id) on delete cascade,
  foreign key (weather_status_id) references E_WEATHER_STATUS(id)
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

create table E_BATTERY_STATUS(
  id integer not null primary key autoincrement,
  name text unique not null);

insert into E_BATTERY_STATUS(id, name) values(1, 'new');
insert into E_BATTERY_STATUS(id, name) values(2, 'good');
insert into E_BATTERY_STATUS(id, name) values(3, 'ok');
insert into E_BATTERY_STATUS(id, name) values(4, 'low');
insert into E_BATTERY_STATUS(id, name) values(5, 'critical');
insert into E_BATTERY_STATUS(id, name) values(6, 'charging');
insert into E_BATTERY_STATUS(id, name) values(7, 'unknown');

-- Hold firmware version and battery status for a piece of equipment.  FIT
-- files contain device-info records with this information, and we populate
-- entries based off that info.
create table EQUIPMENT_VER (
  id integer not null primary key autoincrement,
  equipment_id integer not null,
  -- This is the timestamp when this entry was last updated.  This is used to
  -- avoid storing old information when an old activity is imported.
  timestamp integer not null,
  software_version text,
  hardware_version text,
  battery_voltage real,
  battery_status integer,
  foreign key (equipment_id) references EQUIPMENT(id) on delete cascade,
  foreign key (battery_status) references E_BATTERY_STATUS(id)
  );

create table EQUIPMENT_USE (
  equipment_id integer not null,
  session_id integer not null,
  foreign key (equipment_id) references EQUIPMENT(id),
  foreign key (session_id) references A_SESSION(id)
  );

create index IX0_EQIPMENT_USE on EQUIPMENT_USE(equipment_id, session_id);
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
select EU.equipment_id as equipment_id,
       ifnull(count(EU.session_id), 0) as use_count,
       ifnull(sum(SS.total_timer_time), 0) as hours_used,
       ifnull(sum(SS.total_distance), 0) as kms_used,
       ifnull(min(S.start_time), 0) as first_use,
       ifnull(max(S.start_time), 0) as last_use
  from EQUIPMENT_USE EU,
       A_SESSION S,
       SECTION_SUMMARY SS
 where EU.session_id = S.id
   and S.summary_id = SS.id
 group by EU.equipment_id;


-- View that contains the current usage count for service log entries.  Note
-- that completed entries (the ones that have a non-null end_date) only count
-- up to that date.
create view V_EQUIPMENT_SLOG_CURRENT as
select ESL.id as service_log_id,
       round(case ESL.service_type
       when 0 then (select total(SS1.total_timer_time)
                      from EQUIPMENT_USE EU1, A_SESSION S1, SECTION_SUMMARY SS1
                     where ESL.equipment_id = EU1.equipment_id
                       and EU1.session_id = S1.id
                       and S1.summary_id = SS1.id
                       and S1.start_time > ESL.start_date
                       and S1.start_time < ifnull(ESL.end_date, 3600 + strftime('%s','now')))
       when 1 then (select total(SS2.total_distance)
                      from EQUIPMENT_USE EU2, A_SESSION S2, SECTION_SUMMARY SS2
                     where ESL.equipment_id = EU2.equipment_id
                       and EU2.session_id = S2.id
                       and S2.summary_id = SS2.id
                       and S2.start_time > ESL.start_date
                       and S2.start_time < ifnull(ESL.end_date, 3600 + strftime('%s','now')))
       when 2 then ((ifnull(ESL.end_date, strftime('%s','now')) - ESL.start_date) / (24 * 3600))
       else null end) as current
  from EQUIPMENT_SERVICE_LOG ESL, E_SERVICE_TYPE EST
 where ESL.service_type = EST.id;


--..................................................... Aerolab ....

-- Method to calculate air density for Aerolab estimations.  Can be dew-point
-- or humidity based, depending on what data we have.  Assuming dew-point is
-- available, and accurate, the dew-point method will produce more accurate
-- results.

create table E_AIR_DENSITY_CALCULATION_METHOD(
  id integer not null primary key autoincrement,
  name text unique not null);

insert into E_AIR_DENSITY_CALCULATION_METHOD(id, name)
values (0, 'Dew Point'),
       (1, 'Relative Humidity');

-- Holds Aerolab parameters for a session.  Crr and CdA are the important
-- ones, the rest are used to estimate these.  Note that this table stores
-- whatever the user set for these parameters in the Aerolab panel of the
-- session inspector, so it might not contain actual correct values, also all
-- the values can be null, if the user did not supply any value for those.

create table SESSION_AEROLAB(
  id integer not null primary key autoincrement,
  session_id integer not null,  -- session for which this aerolab data belongs

  crr real,                     -- coefficient of rolling resistance
  cda real,                    -- coefficient of drag area (in meters squared)

  air_density real,        -- in kg/m^3
  wind_speed real,                    -- speed in m/s
  wind_direction real,                -- degrees 0-360, 0 = North, 180 = South
  should_use_wind boolean, -- if true, use wind data in simulation and estimates

  total_weight real,                    -- athlete + bike weight, in kg

  lap_count integer, -- number of laps in the session (for vizualisation only)
  trim_start real,   -- distance to trim from the start, in km
  trim_end real,     -- distance to trim from the end, in km

  -- initial offset for virtual altitude, in meters, used to line up virtual
  -- altitude with the actual altitude data.
  altitude_offset real,

  -- These fields determine how the air density is to be calculated from
  -- weather data.
  air_density_calculation_method integer,
  temperature real,                     -- degrees Celsius
  dew_point real,                       -- degrees Celsius
  pressure real,                        -- in hPa
  humidity real,                        -- 0 - 100

  foreign key (session_id)
  references A_SESSION(id)
  on delete cascade,

  foreign key (air_density_calculation_method)
  references E_AIR_DENSITY_CALCULATION_METHOD(id)
  );

create index IX0_SESSION_AEROLAB on SESSION_AEROLAB(session_id);


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


--............................................................ Workouts ....

-- A workout library is a convenient way to group workouts together.  It only
-- has a name.
create table WORKOUT_LIBRARY (
  id integer not null primary key autoincrement,
  name text unique not null
);

insert into WORKOUT_LIBRARY(name) values('Default');

-- Store a workout.  A workout is a sequence of steps with a specified
-- duration and intensity.  Workouts can be exported to FIT files and
-- downloaded onto a Garmin device and the device will guide the user through
-- the workout
--
-- This table only holds workout metadata (e.g. name, sport, etc), the actual
-- workout data is stored in the WORKOUT_VERSION table.
create table WORKOUT (
  id integer not null primary key autoincrement,
  library_id integer not null,
  name text not null,
  sport_id integer not null,
  sub_sport_id integer,
  serial integer unique not null,
  foreign key (library_id) references WORKOUT_LIBRARY(id),
  foreign key (sport_id) references E_SPORT(id),
  foreign key (sub_sport_id) references E_SUB_SPORT(id)
);

create index IX0_WORKOUT on WORKOUT(serial);

-- Hold data about a workout version.  Each time a workout is saved, a new
-- entry is created in this table, but some of the previous versions are also
-- kept.  Previous versions which were exported as FIT files are kept, because
-- activities will contain "training file" messages which are identified by
-- the serial number and the timestamp.  This will allow us to match a workout
-- referenced in the activity file to a workout version.
--
-- Also note that the actual workout steps are stored as a compressed JSON
-- string in the data field, and are not accessible via SQL commands -- the
-- format is hierarchical and there is little use for having it in a
-- relational schema.
create table WORKOUT_VERSION (
  id integer not null primary key autoincrement,
  workout_id integer not null,
  timestamp integer not null,
  -- if this is not 0, this workout version was exported (e.g as a FIT file)
  -- and as such we keep it, as we might import activities against this
  -- version and we want to link it up in V_SESSION_WORKOUT.
  is_exported integer not null default 0,
  data blob not null,                   -- compressed using gzip
  foreign key (workout_id) references WORKOUT(id)
);

create index IX0_WORKOUT_VERSION on WORKOUT_VERSION(workout_id, timestamp);


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


--........................................................ GPS Segments ....

-- A GPS segment is a sequence of way-points defining a route.  The
-- application can detect if an activity traverses this way-point sequence and
-- store such matches in GPS_SEGMENT_MATCH -- this can be used, for example,
-- to identify a local hill and compare the hill climbing portion of all
-- cycling activities.

-- GPS segment name and summary information about the segment, such as height,
-- grade, etc.  Note that a segment must only have latitude and longitude
-- data, but elevation is optional, so all elevation related values can be
-- NULL.

create table GPS_SEGMENT (
  id integer not null primary key autoincrement,
  name text not null,
  segment_length real not null,
  segment_height real,
  segment_grade real,
  total_ascent real,
  total_descent real,
  max_grade real,
  min_elevation real,
  max_elevation real,
  fiets_score real                      -- see docs/climbs.md
);

-- The way-points that define a segment.  Within a segment, way-points are
-- ordered by the POS column, which otherwise has no meaning.

create table GPS_SEGMENT_WAYPOINT (
  id integer not null primary key autoincrement,
  segment_id integer not null,
  pos integer not null, -- an incrementing number used for ordering
  latitude real not null,
  longitude real not null,
  distance real not null,
  geoid integer not null,               -- geoid based on latitude and longitude
  altitude real,                        -- altitude in meters, may be NULL
  grade real,                           -- segment grade, if altitude is not NULL
  foreign key (segment_id) references GPS_SEGMENT(id) on delete cascade
);

create index IX0_GPS_SEGMENT_WAYPOINT on GPS_SEGMENT_WAYPOINT(segment_id, pos);

-- A segment match represents the portion of a session which traverses a given
-- segment.  We store references to the segment and session, as well as
-- pointers to the trackpoints representing the start and end of the match.
-- Note that a segment might have multiple matches within a session, e.g when
-- the session traverses the same route more than once, and, of course, a
-- segment might have matches in multiple sessions.

create table GPS_SEGMENT_MATCH (
  id integer not null primary key autoincrement,
  segment_id integer not null,
  session_id integer not null,
  summary_id integer not null,          -- summary data for this segment
  start_trackpoint_id integer not null,
  end_trackpoint_id integer not null,
  match_cost real not null,             -- DTW cost for the match
  foreign key (segment_id) references GPS_SEGMENT(id) on delete cascade,
  foreign key (session_id) references A_SESSION(id) on delete cascade,
  foreign key (summary_id) references SECTION_SUMMARY(id) on delete set null,
  foreign key (start_trackpoint_id) references A_TRACKPOINT(id) on delete cascade,
  foreign key (end_trackpoint_id) references A_TRACKPOINT(id) on delete cascade
);

create index IX0_GPS_SEGMENT_MATCH on GPS_SEGMENT_MATCH(segment_id);
create index IX1_GPS_SEGMENT_MATCH on GPS_SEGMENT_MATCH(session_id);

-- "Exploded view" of a GPS segment match, bringing in the summary values.
-- This is used to populate the match view in the activity, similar to
-- V_ACTIVITY_LIST.
create view V_GPS_SEGMENT_MATCH_LIST as
  select S.id as session_id,
         GS.id as segment_id,
         GSM.id as match_id,
         GSM.match_cost as match_cost,
         S.name as headline,
         S.start_time as session_start_time,
         T_START.timestamp as segment_start_time,
         T_END.timestamp as segment_end_time,
         (select name from E_TIME_ZONE ETZ where ETZ.id = S.time_zone_id) as time_zone,
         S.sport_id as sport,
         S.sub_sport_id as sub_sport,
         -- NOTE: elapsed time is used as "Duration" for laps in the rest of
         -- the application
         SS.total_timer_time as duration, -- moving time
         SS.total_elapsed_time as elapsed,
         SS.total_distance as distance,
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
         SS.avg_combined_pedal_smoothness as cpdlsmth,
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
         SS.aerobic_decoupling as adecl
    from GPS_SEGMENT_MATCH GSM,
         A_SESSION S,
         GPS_SEGMENT GS,
         SECTION_SUMMARY SS,
         A_TRACKPOINT T_START,
         A_TRACKPOINT T_END
   where GSM.summary_id = SS.id
     and GSM.session_id = S.id
     and GSM.segment_id = GS.id
     and GSM.start_trackpoint_id = T_START.id
     and GSM.end_trackpoint_id = T_END.id;


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


--....................................................... Metrics Cache ....

-- Store Best Avg data for sessions and series.  This is a cache storage, not
-- really ment for user level access.  Entries in this table can be deleted
-- any time and will be automatically regenerated (however, this may take a
-- long time). See rkt/metrics.rkt, where this is used.

create table BAVG_CACHE (
  id integer not null primary key autoincrement,
  session_id integer not null,
  series text not null,           -- data series name from session data-frame%
  data blob not null,             -- GZIP-ped JSON object containing the data
  foreign key(session_id) references A_SESSION(id)
  );

-- this index is written such that it can be used to look up entries both by
-- series only and by session id + series.
create unique index IX0_BAVG_CACHE on BAVG_CACHE(series, session_id);

-- Store Histogram data for sessions and series.  See notes for BAVG_CACHE
create table HIST_CACHE (
  id integer not null primary key autoincrement,
  session_id integer not null,
  series text not null,
  data blob not null,
  foreign key(session_id) references A_SESSION(id)
  );

-- this index is written such that it can be used to look up entries both by
-- series only and by session id + series.
create unique index IX0_HIST_CACHE on HIST_CACHE(series, session_id);

-- Store scatter plot data for sessions and series.  See notes for BAVG_CACHE
create table SCATTER_CACHE (
  id integer not null primary key autoincrement,
  session_id integer not null,
  series1 text not null,
  series2 text not null,
  data blob not null,
  foreign key(session_id) references A_SESSION(id)
  );

-- this index is written such that it can be used to look up entries both by
-- series1 only and by session id + series1 + series2.
create unique index IX0_SCATTER_CACHE on SCATTER_CACHE(series1, series2, session_id);

create index IX1_SCATTER_CACHE on SCATTER_CACHE(series2);

-- Store a cache of sessions we have determined to be similar.  Note that we
-- store the session with the smallest ID first, to avoid having to store
-- multiple entries or create ambiguities.

create table SIMILAR_SESSION_CACHE(
  id integer not null primary key autoincrement,
  first_session_id integer not null,
  second_session_id integer not null,
  are_similar integer not null, -- 1 if sessions are similar, 0 if they are not
  foreign key (first_session_id) references A_SESSION(id),
  foreign key (second_session_id) references A_SESSION(id),
  check (first_session_id < second_session_id));

-- this is a covering index, to find if two sessions are similar without
-- consulting the actual table.
create unique index IX0_SIMILAR_SESSION_CACHE
  on SIMILAR_SESSION_CACHE(first_session_id, second_session_id, are_similar);

create index IX1_SIMILAR_SESSION_CACHE
  on SIMILAR_SESSION_CACHE(second_session_id);


--............................................. application preferences ....

-- Store ActivityLog2 application preferences in the database.  preferences
-- have a "tag" (a name) and a value which is a serialized SEXPR.  This table
-- is meant for application data only (e.g. layout of various GUI views).  It
-- is stored in the database, so each database can have its own associated GUI
-- data.
create table SEXPR_PREFERENCES (
  id integer not null primary key autoincrement,
  name text not null,
  value blob not null);

create unique index IX0_SEXPR_PREFERENCES on SEXPR_PREFERENCES(name);


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
       (case S.sport_id when 1 then S.training_stress_score else 0 end) as run_effort,
       (case S.sport_id when 2 then 1 else 0 end) as bike_count,
       (case S.sport_id when 2 then SS.total_distance else 0 end) as bike_distance,
       (case S.sport_id when 2 then SS.total_timer_time else 0 end) as bike_time,
       (case S.sport_id when 2 then S.training_stress_score else 0 end) as bike_effort,
       (case S.sport_id when 5 then 1 else 0 end) as swim_count,
       (case S.sport_id when 5 then SS.total_distance else 0 end) as swim_distance,
       (case S.sport_id when 5 then SS.total_timer_time else 0 end) as swim_time,
       (case S.sport_id when 5 then S.training_stress_score else 0 end) as swim_effort,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then 1 else 0 end) else 0 end) as strength_count,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then SS.total_distance else 0 end) else 0 end) as strength_distance,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then SS.total_timer_time else 0 end) else 0 end) as strength_time,
       (case S.sport_id when 4 then (case S.sub_sport_id when 20 then S.training_stress_score else 0 end) else 0 end) as strength_effort,
       1 as sport_count,
       SS.total_distance as sport_distance,
       SS.total_timer_time as sport_time,
       S.training_stress_score as effort
  from A_SESSION S, SECTION_SUMMARY SS
 where S.summary_id = SS.id
   and (S.sport_id in (1, 2, 5) or (S.sport_id = 4 and S.sub_sport_id = 20));

-- Expands all summary data about a every session: duration, time, max speed,
-- etc.  This can be used as a convenience view without having to remember all
-- the "right" joins to get info about a session.  It is used by the
-- view-activities.rkt

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
         S.feel_scale as feel,
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
         SS.avg_combined_pedal_smoothness as cpdlsmth,
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

-- Local Variables:
-- sql-product: sqlite
-- compile-command: "rm al.db;  sqlite3 -batch al.db < db-schema.sql"
-- End:
