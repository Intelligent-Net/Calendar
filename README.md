# Calendar

Universal Calendar

This library is intended to be a reference implementaation of a universal calendar library.

Mankind is on the brink of colonising other worlds, and of creating artificial habitats. We (in the west anyway) have been using a similar calendar for 2000 years (The Julian calendar which then became the Gregorian caalendar). It shows it's age, the year starts on a day that has no astronomical significaance, months have no relationship to the orbital period of Luna, days of the week vary from year to year within a week and all sorts of other problems.

A calendar that, in so far as is possible, removes the anomalies is required, and provides a framework for other planets. This library is an attempt to achieve that.

Note : This library is for the calculation of planetary dates give only 2 assumptions. The length of a year and the length of a day (relative to the earth). For this reason real number arithmetic must be used and then when the date is converted into unit's like YY+DOY rounding/truncation to integers done. This is incompatable with Gregorian dates, which are integer based with a rule for 'leap' years. To keep this consistent, in so much as tyhat is possible, two sets of dates are calculated internally - real numbered universal dates and integer based Gregorian dates. These will become slightly misaligned over time, then realign etc. This is unavoidable.

To install install sbt from https://www.scala-sbt.org/release/docs/Setup.html

Testing is 'inline'. 'sbt run' will run tests.

The libray should work with any Java runtime language, providing of course the scala runline is available to Java or similar.

TODO :

1. Add in Era start offset, which is some number between -1.0 and 1.0 which is the drift at the start of the Era in days from the 'true' time.
2. Introduce 'aggregation' of units, for example a year is approximately 13 Lunar months on earth, so a year in a lunar calendar will vary in length (but always be 12 or 13 months), but average be the same length as a solar year.
3. Possible other aggregations for example for quarters/seasons (2 soltices and 2 equinoxes, approximately 3 months apart).
4. Add example Calendars, for example sidereal/Galactic Centre/Sagatarius Astar. Need data.
5. Local time of day support (Sun time). In addition to prime meridean time.
6. Examples of mixed calendar use, for example tides and solar calendar to predict spring tides. 
7. Support for named months and days of the week.
8. Declarative formats.
9. A web API
10. A web GUI
N. Add to this list...
