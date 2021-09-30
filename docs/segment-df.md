# GPS Segment data-frame objects

GPS Segments are represented as data frame objects containing the waypoints of
the segment and additional properties about the segment.  This document
describes the contents of these data frame objects.

## Data series

A GPS Segment data frame will contain the following series (note that some of
them might not be present).  Other parts of the application might add
additional data series.

* **lat**, **lon** -- latitude and longitude for each point in the segment
* **geoid** -- the geoid (can be computed from lat, lon) -- this is used for
  matching segments on sessions, among other things.
* **dst** -- distance along the segment, in meters. First position has a
  distance of 0, last position is the total length of the segment.
* **alt** -- (OPTIONAL) altitude for each data point, in meters.
* **grade** -- (OPTIONAL) the grade, or slope, of the current point as a
  percentage.  Only present if the `alt` series is present.

To check if a data frame contains some series, use the `df-contains?`
function:

    (df-contains? df "lat" "lon" "alt")
    => #t

## Properties

The data frame object can store a set of properties, which are key - value
mappings. The following properties are present in the gps-segment data frame,
while other parts of the application can attach additional properties:

* **total-ascent**, **total-descent** -- total amount of climb and descent
  along the segment, in meters.  Only present if the **alt** series is
  present.
* **min-elevation**, **max-elevation** -- lowest and highest altitude point,
  in meters.  Only present if the **alt** series is present.
* **segment-length** -- the total length of the segment, in meters
* **segment-height** -- the altitude difference between the start and end of
  the segment, in meters.  Only present if the **alt** series is present.
* **segment-grade** -- the average grade of the segment, calculated as
  **segment-height** / **segment-length**.  Only present if the **alt** series
  is present.
* **max-grade** -- maximum grade in the **grade** series, if that series is
  present.

Properties can be accessed using the `df-get-property` function.  For example,
this will return the segment height, or `#f` if the property is not present:

    (df-get-property df 'segment-height)


