package space.asgardia.calendar

object Test {
  def main(args: Array[String]): Unit = {
    test(args)
  }

  def testAsgardianToGregorian(cal: Calendar) = {
    for (y <- -3000 to 5000) {
      for (yd <- 0 until (if (cal.gregorianIsLeap(y + (if (y <= -cal.startOfEra) cal.startOfEra - 1 else cal.startOfEra))) cal.daysInYear.ceil.toInt else cal.daysInYear.toInt)) {
        val g = cal.asgardianToGregorian(y, yd)
        val fmt = if (y < 0) f"${y}%05d+${yd}%03d" else f"${y}%04d+${yd}%03d"
        val a = cal.gregorianToAsgardian(g)

        assert(fmt == a.toString)
      }
    }
  }

  def testGregorianToAsgardian(cal: Calendar) = {
    for (y <- -3000 to 5000) {
      if (y != 0) { // No year zero in Gregorian calendar!!!
        for (m <- 1 to 12) {
          for (d <- 1 to cal.getMonthLens(y)(m - 1)) {
            val fmt = if (y < 0) f"${y}%05d:${m}%02d:${d}%02d" else f"${y}%04d:${m}%02d:${d}%02d"
            val a = cal.gregorianToAsgardian(y, m, d)
            val g = cal.asgardianToGregorian(a)

            //if (fmt != g)
            //  println(fmt + " -> " + a + " => " + g)
            assert(fmt == g)
          }
        }
      }
    }
  }

  def testIncDays(cal: Calendar) = {
    var y = -5000
    var doy = 0.0
    var day = Date("-5000+000")
    do {
      day = cal.incDays(day,1)
      assert(day.y == y && day.doy == doy + 1 || day.y == y + 1 && day.doy == 0)
      y = day.y
      doy = day.doy
    } while (day.y < 5000)
  }

  def testDecDays(cal: Calendar) = {
    var y = 5000
    var doy = 0.0
    var day = Date("5000+000")
    do {
      day = cal.decDays(day,1)
      assert(day.y == y && day.doy == doy - 1 || day.y == y - 1 && day.doy == (if (cal.isLeap(day.y)) cal.daysInYear.ceil - 1 else cal.daysInYear.floor - 1))
      y = day.y
      doy = day.doy
    } while (day.y > -5000)
  }

  def testPlanetEraConversion() = {
    val earth = Date(EarthCalendar.cal)
    val mars = Date(MarsCalendar.cal)

    assert(earth.epoch == earth.fromEra(earth.toEra).epoch)
    assert(earth.epoch == mars.epoch)
    assert(earth.epoch == mars.fromEra(mars.toEra).epoch)
    assert(earth == earth.convert(mars))
    assert(mars == mars.convert(earth))
    assert(earth.fromEra(earth.toEra).epoch == mars.fromEra(mars.toEra).epoch)
  }

  def testPlanetConversion() = {
    val earth = EarthCalendar.cal
    val mars = MarsCalendar.cal

    //println(earth.shortToLong(0,0) + " : " + mars.shortToLong(0,0))
    //println(earth.shortToLong(0,1) + " : " + mars.shortToLong(0,1))
    //println(earth.shortToLong(1,100) + " : " + mars.shortToLong(1,600))

    /*
    val marsDay = Date(0,668,mars)
    println(marsDay)
    val earthDay = earth.convert(marsDay)
    println(earthDay)
    println(marsDay.convert(marsDay))
    println(marsDay.convert(earthDay))
    */
    
    // CANNOT complete this accuarately until time taken into account cleanly

    assert(true)
    //System.exit(0)
  }

  def testShortToLong(cal: Calendar) = {
    //println(cal.isLeap(cal.shortToLong(-1900,360)) + " : " + cal.shortToLong(-1900,360))
    assert(cal.shortToLong(-4,366) == "-0004-12-30") // -04 is a leap year
    assert(cal.shortToLong(0,363) == "0000-12-27")
    assert(cal.shortToLong(0,364) == "0000-12-28")
    assert(cal.shortToLong(0,365) == "0000-12-29")
    assert(cal.shortToLong(0,366) == "0000-12-30") // 00 is a leap year
    assert(cal.shortToLong(4,366) == "0004-12-30") // 04 is a leap year
    assert(cal.shortToLong(5,0) == "0005-00-00")
  }

  def testLongToShort(cal: Calendar) = {
    assert(cal.longToShort(-4,12,30) == "-0004+366") // -04 is a leap year
    assert(cal.longToShort(0,12,27) == "0000+363")
    assert(cal.longToShort(0,12,28) == "0000+364")
    assert(cal.longToShort(0,12,29) == "0000+365")
    assert(cal.longToShort(0,12,30) == "0000+366") // 00 is a leap year
    assert(cal.longToShort(4,12,30) == "0004+366") // 04 is a leap year
    assert(cal.longToShort(5,0,0) == "0005+000")
  }

  def testLeapYears(cal: Calendar) = {
    val leaps = (0 until 100).map(y => cal.isLeap(y))
    val expected = Vector(true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, false, true, false)

    assert(leaps.zip(expected).filter(p => p._1 == p._2).length == 100)
  }

  def testAsgardianDates(cal: Calendar) = {
    val asgardianDatesPrev = (0 to 13).map(m => cal.asgardianToGregorian(-1,m * 28))
    val expectedPrev = Vector("2014:12:21", "2015:01:18", "2015:02:15", "2015:03:15", "2015:04:12", "2015:05:10", "2015:06:07", "2015:07:05", "2015:08:02", "2015:08:30", "2015:09:27", "2015:10:25", "2015:11:22", "2015:12:20")
    assert(asgardianDatesPrev.toString == expectedPrev.toString)
    val asgardianDates = (0 to 13).map(m => cal.asgardianToGregorian(0,m * 28))
    val expected = Vector("2015:12:21", "2016:01:18", "2016:02:15", "2016:03:14", "2016:04:11", "2016:05:09", "2016:06:06", "2016:07:04", "2016:08:01", "2016:08:29", "2016:09:26", "2016:10:24", "2016:11:21", "2016:12:19")
    assert(asgardianDates.toString == expected.toString)
    val asgardianDatesNext = (0 to 13).map(m => cal.asgardianToGregorian(1,m * 28))
    val expectedNext = Vector("2016:12:21", "2017:01:18", "2017:02:15", "2017:03:15", "2017:04:12", "2017:05:10", "2017:06:07", "2017:07:05", "2017:08:02", "2017:08:30", "2017:09:27", "2017:10:25", "2017:11:22", "2017:12:20")
    assert(asgardianDatesNext.toString == expectedNext.toString)
  }

  def testGregorianDates(cal: Calendar) = {
    val gregorianDatesPrev = (1 to 12).map(m => cal.gregorianToAsgardian(2015,m,28))
    val expectedPrev = Vector("-0001+038", "-0001+069", "-0001+097", "-0001+128", "-0001+158", "-0001+189", "-0001+219", "-0001+250", "-0001+281", "-0001+311", "-0001+342", "0000+007")
    assert(gregorianDatesPrev.toString == expectedPrev.toString)
    val gregorianDates = (1 to 12).map(m => cal.gregorianToAsgardian(2016,m,28))
    val expected = Vector("0000+038", "0000+069", "0000+098", "0000+129", "0000+159", "0000+190", "0000+220", "0000+251", "0000+282", "0000+312", "0000+343", "0001+007")
    assert(gregorianDates.toString == expected.toString)
    val gregorianDatesNext = (1 to 12).map(m => cal.gregorianToAsgardian(2017,m,28))
    val expectedNext = Vector("0001+038", "0001+069", "0001+097", "0001+128", "0001+158", "0001+189", "0001+219", "0001+250", "0001+281", "0001+311", "0001+342", "0002+007")
    assert(gregorianDatesNext.toString == expectedNext.toString)
  }

  def testToLongDates(cal: Calendar) = {
    val longDates = (0 to 13).map(m => cal.shortToLong(0,m * 28))
    val expected = Vector("0000-00-00", "0000-01-00", "0000-02-00", "0000-03-00", "0000-04-00", "0000-05-00", "0000-06-00", "0000-07-00", "0000-08-00", "0000-09-00", "0000-10-00", "0000-11-00", "0000-12-00", "0000-12-28")
    assert(longDates == expected)
  }

  def test(args: Array[String]): Unit = {
    val earth = EarthCalendar.cal

    testLeapYears(earth)
    testShortToLong(earth)
    testLongToShort(earth)
    testToLongDates(earth)
    testAsgardianDates(earth)
    testGregorianDates(earth)
    testPlanetEraConversion
    testPlanetConversion

    // Stress tests
    testIncDays(earth)
    testDecDays(earth)
    testAsgardianToGregorian(earth)
    testGregorianToAsgardian(earth)
  }
}
