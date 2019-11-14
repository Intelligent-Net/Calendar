package space.asgardia.calendar

object EarthCalendar extends Calendar

// Length of Mars second : 1.027491251701389 
// Length of Martian solar year : 668.5991
object MarsCalendar extends Calendar("Mars", startOfEra=2016, daysInYear=668.5991, hoursInDay=24.6597900408, daysOffset=33)

// Offset is relative to last new moon at UTC meridian prior to start of era
object LunarCycleEarth extends Calendar("LunarCycle", daysInYear=29.530589, daysOffset=0.00003472222222222222)

object LunarCalendar extends Calendar("Luna", daysInYear=530589, startOfEra=2016)

// Sidereal Calendars should be relative to the galactic centre (Sagittarius A*)
// http://alignment2012.com/whatisGA.htm
object EarthSiderealCalendar extends Calendar("EarthSiderial", daysInYear=EarthCalendar.siderealFromSolar(EarthCalendar.daysInYear), startOfEra=1998, hoursInDay=23.999997666666665)

// Not sure of daysOffset or best startOfEra for facing Galactic Centre
// Without accurate data don't use this for now
object MarsSiderealCalendar extends Calendar("MarsSidereal", daysInYear=MarsCalendar.siderealFromSolar(MarsCalendar.daysInYear), hoursInDay=24.6597900408, daysOffset=MarsCalendar.yearDayLen(EarthSiderealCalendar.startOfEra - 1) - MarsCalendar.conv(EarthCalendar.gregorianToAsgardian(EarthSiderealCalendar.startOfEra - 1,12,21)).doy, startOfEra=EarthSiderealCalendar.startOfEra)

