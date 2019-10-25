-- p32-time-zone.sql -- record timezones for sessions
--
-- This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
-- Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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


--......................................................... E_TIME_ZONE ....

drop table if exists E_TIME_ZONE;

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
       ('America/Cayenne'), ('America/Cayman'), ('America/Chicago'), ('America/Chihuahua'), ('America/Costa_Rica'),
       ('America/Creston'), ('America/Cuiaba'), ('America/Curacao'), ('America/Danmarkshavn'), ('America/Dawson'),
       ('America/Dawson_Creek'), ('America/Denver'), ('America/Detroit'), ('America/Dominica'), ('America/Edmonton'),
       ('America/Eirunepe'), ('America/El_Salvador'), ('America/Fort_Nelson'), ('America/Fortaleza'),
       ('America/Glace_Bay'), ('America/Godthab'), ('America/Goose_Bay'), ('America/Grand_Turk'),
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
       ('America/Nipigon'), ('America/Nome'), ('America/Noronha'), ('America/North_Dakota/Beulah'),
       ('America/North_Dakota/Center'), ('America/North_Dakota/New_Salem'), ('America/Ojinaga'),
       ('America/Panama'), ('America/Pangnirtung'), ('America/Paramaribo'), ('America/Phoenix'),
       ('America/Port-au-Prince'), ('America/Port_of_Spain'), ('America/Porto_Velho'), ('America/Puerto_Rico'),
       ('America/Punta_Arenas'), ('America/Rainy_River'), ('America/Rankin_Inlet'), ('America/Recife'),
       ('America/Regina'), ('America/Resolute'), ('America/Rio_Branco'), ('America/Santarem'),
       ('America/Santiago'), ('America/Santo_Domingo'), ('America/Sao_Paulo'), ('America/Scoresbysund'),
       ('America/Sitka'), ('America/St_Barthelemy'), ('America/St_Johns'), ('America/St_Kitts'),
       ('America/St_Lucia'), ('America/St_Thomas'), ('America/St_Vincent'), ('America/Swift_Current'),
       ('America/Tegucigalpa'), ('America/Thule'), ('America/Thunder_Bay'), ('America/Tijuana'),
       ('America/Toronto'), ('America/Tortola'), ('America/Vancouver'), ('America/Whitehorse'),
       ('America/Winnipeg'), ('America/Yakutat'), ('America/Yellowknife'), ('Antarctica/Casey'),
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
       ('Australia/Adelaide'), ('Australia/Brisbane'), ('Australia/Broken_Hill'), ('Australia/Currie'),
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
       ('Europe/Jersey'), ('Europe/Kaliningrad'), ('Europe/Kiev'), ('Europe/Kirov'), ('Europe/Lisbon'),
       ('Europe/Ljubljana'), ('Europe/London'), ('Europe/Luxembourg'), ('Europe/Madrid'),
       ('Europe/Malta'), ('Europe/Mariehamn'), ('Europe/Minsk'), ('Europe/Monaco'),
       ('Europe/Moscow'), ('Europe/Oslo'), ('Europe/Paris'), ('Europe/Podgorica'), ('Europe/Prague'),
       ('Europe/Riga'), ('Europe/Rome'), ('Europe/Samara'), ('Europe/San_Marino'), ('Europe/Sarajevo'),
       ('Europe/Saratov'), ('Europe/Simferopol'), ('Europe/Skopje'), ('Europe/Sofia'), ('Europe/Stockholm'),
       ('Europe/Tallinn'), ('Europe/Tirane'), ('Europe/Ulyanovsk'), ('Europe/Uzhgorod'), ('Europe/Vaduz'),
       ('Europe/Vatican'), ('Europe/Vienna'), ('Europe/Vilnius'), ('Europe/Volgograd'), ('Europe/Warsaw'),
       ('Europe/Zagreb'), ('Europe/Zaporozhye'), ('Europe/Zurich'), ('Indian/Antananarivo'),
       ('Indian/Chagos'), ('Indian/Christmas'), ('Indian/Cocos'), ('Indian/Comoro'), ('Indian/Kerguelen'),
       ('Indian/Mahe'), ('Indian/Maldives'), ('Indian/Mauritius'), ('Indian/Mayotte'), ('Indian/Reunion'),
       ('Pacific/Apia'), ('Pacific/Auckland'), ('Pacific/Bougainville'), ('Pacific/Chatham'),
       ('Pacific/Chuuk'), ('Pacific/Easter'), ('Pacific/Efate'), ('Pacific/Enderbury'), ('Pacific/Fakaofo'),
       ('Pacific/Fiji'), ('Pacific/Funafuti'), ('Pacific/Galapagos'), ('Pacific/Gambier'),
       ('Pacific/Guadalcanal'), ('Pacific/Guam'), ('Pacific/Honolulu'), ('Pacific/Kiritimati'),
       ('Pacific/Kosrae'), ('Pacific/Kwajalein'), ('Pacific/Majuro'), ('Pacific/Marquesas'),
       ('Pacific/Midway'), ('Pacific/Nauru'), ('Pacific/Niue'), ('Pacific/Norfolk'), ('Pacific/Noumea'),
       ('Pacific/Pago_Pago'), ('Pacific/Palau'), ('Pacific/Pitcairn'), ('Pacific/Pohnpei'),
       ('Pacific/Port_Moresby'), ('Pacific/Rarotonga'), ('Pacific/Saipan'), ('Pacific/Tahiti'),
       ('Pacific/Tarawa'), ('Pacific/Tongatapu'), ('Pacific/Wake'), ('Pacific/Wallis');


--........................................................... A_SESSION ....

alter table A_SESSION add time_zone_id integer references E_TIME_ZONE(id);


--..................................................... V_ACTIVITY_LIST ....

-- re-build V_ACTIVITY_LIST to include the session time zone

drop view if exists V_ACTIVITY_LIST;

create view V_ACTIVITY_LIST as
  select S.id as session_id,
         A.guid as activity_guid,
         S.name as headline,
         S.start_time as start_time,
         (select name from E_TIME_ZONE ETZ where ETZ.id = S.time_zone_id) as time_zone,
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
         (select temperature from SESSION_WEATHER SW1 where SW1.session_id = S.id) as temperature,
         (select humidity from SESSION_WEATHER SW2 where SW2.session_id = S.id) as humidity,
         (select wind_speed from SESSION_WEATHER SW3 where SW3.session_id = S.id) as wind_speed,
         (select wind_direction from SESSION_WEATHER SW4 where SW4.session_id = S.id) as wind_direction,
         (select dew_point from SESSION_WEATHER SW1 where SW1.session_id = S.id) as dew_point
    from A_SESSION S, SECTION_SUMMARY SS, ACTIVITY A
   where S.summary_id = SS.id
     and S.activity_id = A.id;

update SCHEMA_VERSION set version = 32;
