# Calendar
Universal Calendar

This library is intended to be a reference implementaation of a universal calendar library.

Mankind is on the brink of colonising other worlds, and of creating artificial habitats. We (in the west anyway) have been using a similar calendar for 2000 years (The Julian calendar which then became the Gregorian caalendar). It shows it's age, the year starts on a day that has no astronomical significaance, months have no relationship to the orbital period of Luna, days of the week vary from year to year within a week and all sorts of other problems.

A calendar that, in so far as is possible, removes the anomalies is required, and provides a framework for other planets. This library is an attempt to achieve that.

Note : This library is for the calculation of planetary dates give only 2 assumptions. The length of a year and the length of a day (relative to the earth). For this reason real number arithmetic must be used and then when the date is converted into unit's like YY+DOY rounding/truncation to integers done. This is incompatable with Gregorian dates, which are integer based with a rule for 'leap' years. To keep this consistent, in so much as tyhat is possible, two sets of dates are calculated internally - real numbered universal dates and integer based Gregorian dates. These will become slightly misaligned over time, then realign etc. This is unavoidable.
