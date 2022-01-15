**TLDR** Climbs in this program are scored using the FIETS index, but
calculated over each pairs of data-points the climb and the scores added
together.  This produces higher climb scores than simply using the average
grade of the entire segment.

## The FIETS Score

One way to determine the difficulty of a climb is to use the FIETS formula to
calculate a numeric value for the climb.  This forumula was developed by the
Dutch cycling magazine Fiets. The formula is shown below:

```
FIETS Score = (H * H / D * 10) + (T - 1000) / 1000
```

Where:

* **H** is the height of the climb (in meters), i.e. difference between the
  top and bottom elevations,
* **D** is the climb length, or distance (in meters),
* **T** is the altitude at the top (in meters),

The second term in the formula is only added when it is positive, thay is, for
climbs whose top is above 1000m.

**NOTE** In this program, the "(T - 1000)/1000" term of the FIETS formula is
not added to the climb segments, so climbs can be joined together.

The FIETS formula can also be expressed in terms of Grade, and some literature
shows it as such.  The formula variations are shown below.  Here **Grade** is
represented as the ratio of height to length of the climb, for example, a
grade of 3% is represented as 0.03.  **Distance** is represented in meters.

```
FIETS Score =  Grade * Grade * Distance(m) / 10
```

Different ways of representing the grade results in slight differences in the
scaling factors of the forumla.  For example, if grade is represented as a
percent, and the distance in km, the forumla becomes:

```
FIETS Score = Grade(%) * Grade(%) * Distance(km) / 100
```

Or, when distance is in meters:

```
FIETS Score = Grade(%) * Grade(%) * Distance(m) / 100000
```

## Implementation Challenges

The FIETS formula can be used in two ways:

* (A) considering the entire climb and calculating it based on the bottom +
  top elevations only or
* (B) calculating scores for individual sections and adding them together.

There is a difference between the results of each calculation, usually method
B will produce higher scores.  For example, consider a climb that has three
sections each 1000 meters long:

| Section | Length | Grade | Elevation | FIETS Score |
|:--------|:-------|:------|:----------|:------------|
| 1       | 1000m  | 3%    | 30m       | 0.09        |
| 2       | 1000m  | 5%    | 50m       | 0.25        |
| 3       | 1000m  | 8%    | 80m       | 0.64        |

Since this is a 3000 meter climb, we can add the FIETS Score of each section,
obtaining a total of 0.98.  We could also consider the entirety of the climb,
160 meters climbed over 3000 meter distances, making it a FIETS score of 0.85
and an average grade of 5.33%.

Using Method B, calculating the FIETS score over individual sections, will
usually produce a higher score, and, subjectively, it is perhaps easier to
climb 3000m at a constant 5.33 %, than having a variable climb with the last
1000m at 8%.

Using Method B has some implementation problems: some climb sections will have
0% grade or even descents, and applying the formula without discrimination,
will result in a positive climb score even if the section is a descent (since
the height is raised to the power of 2).  It is unclear how to handle this
case in general, but this program will use a score of 0 for each section which
is a descent, which is somewhat incorrect (this is done implicitly when two
nearby climb scores are added together, without considering the score if the
"in-between" section).

## Climb Categories

**Tour de France** climbs are categorized using only guidelines and presumably
the organizers publish the official category of each climb in that race,
without using a numerical formula (see also
https://velovation.co.uk/climb-categorisation/).

**Runalize** (see https://runalyze.com/help/article/climb-score) uses the
following scores to categorize climbs:

| Category             | FIETS Score |
|:---------------------|:------------|
| HC (Hors Categories) | >= 6.5      |
| 1st                  | >= 5.0      |
| 2nd                  | >= 3.5      |
| 3rd                  | >= 2.0      |
| 4th                  | >= 0.5      |
| 5th                  | >= 0.25     |

**Climbfinder** (https://climbfinder.com/en/climb-categories/) uses the
following scores to categorize climbs.

| Category             | FIETS Score * 100 |
|:---------------------|:------------------|
| HC (Hors Categories) | >= 1200           |
| 1st                  | >= 800            |
| 2nd                  | >= 500            |
| 3rd                  | >= 300            |
| 4th                  | >= 100            |

The Climbfinder formula (https://climbfinder.com/en/difficulty-points/, see
second comment) is shown below, and it is calculated for each 100m section of
a climb.  Climbfinder will also multiply the score by 1.7 if the climb is on
cobbles, but that is not shown here.

```
FIETS Score for each 100m = Grade(%) * Grade(%) * 0.1
```

This is simply the FIETS Score expressed in therms of grade and distance,
without the division by 100. Note that 0.1 represents 100m when expressed as
kilometers:

```
FIETS Score = Grade(%) * Grade(%) * Distance(km) / 100
```

Their conversion of climb scores into climb categories is higher than that of
Runalize, possibly because their climb scores are higher for the same climb.

**MapMyRide** probably uses a variant of the FIETS score, as they use the
basic ingredients (see https://www.mapmyride.com/routes/climb_information/):

> All climb scores are based on distance, grade/elevation change, and maximum
> elevation.

**Strava** climb information is confusing (see
https://support.strava.com/hc/en-us/articles/216917057-Climb-Categorization):

> To decide the category of a climb Strava multiplies the length of the climb
> (in meters) with the grade of the climb.

This does not make much sense.  The grade of the climb is defined as the total
ascent of the climb divided by the length of the climb, so if they multiply
that value by the length of the climb, they will simply get the height of the
climb (plus some constant factor):

```
STRAVA Climb Score = Length (m) * Grade (%)
                   = Length (m) * (Ascent (m) / Length (m)) * 100
                   = Ascent (m) * 100

Note that Grade (%) is defined as (Ascent / Length) * 100
```

So basically, the Strava forumla can be simply stated as "the height of the
climb multiplied by 100".
