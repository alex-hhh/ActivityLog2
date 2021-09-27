-- find orphan section summary rows -- these are rows that are not referenced
-- from any other tables...
select SS.id
  from SECTION_SUMMARY SS
 where SS.id not in (select summary_id from A_SESSION)
   and SS.id not in (select summary_id from A_LAP)
   and SS.id not in (select summary_id from A_LENGTH)
   and SS.id not in (select summary_id from XDATA_SUMMARY_VALUE)
   and SS.id not in (select summary_id from GPS_SEGMENT_MATCH);
