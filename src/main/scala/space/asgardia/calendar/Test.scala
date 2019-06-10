package space.asgardia.calendar

object Test {
  def main(args: Array[String]): Unit = {
    test(args)
  }

  def testAsgardianToGregorian(cal: Calendar) = {
    for (y <- -3000 to 1000) {
      for (yd <- 0 until (if (cal.gregorianIsLeap(y + (if (y < -cal.startOfEra) cal.startOfEra else cal.startOfEra + 1))) cal.daysInYear.ceil.toInt else cal.daysInYear.toInt)) {
        val g = cal.asgardianToGregorian(y, yd)
        val fmt = if (y < 0) f"${y}%05d+${yd}%03d" else f"${y}%04d+${yd}%03d"
        val a = cal.gregorianToAsgardian(g)

        //if (fmt != a.toString)
        //  println(fmt + " : " + g + " -> " + a + " # " + cal.isLeap(y) + " / " + Date(y, yd))
        assert(fmt == a.toString)
      }
    }
  }

  def testGregorianToAsgardian(cal: Calendar) = {
    for (y <- -5000 to 5000) {
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

  def testPlanetConversion(cal: Calendar) = {
    val earth = Date(EarthCalendar.cal)
    val mars = Date(MarsCalendar.cal)

    assert(earth.epoch == earth.fromEra(earth.toEra).epoch)
    assert(earth.epoch == mars.epoch)
    assert(earth.epoch == mars.fromEra(mars.toEra).epoch)
    assert(earth == earth.convert(mars))
    assert(mars == mars.convert(earth))
    assert(earth.fromEra(earth.toEra).epoch == mars.fromEra(mars.toEra).epoch)
  }

  def testShortToLong(cal: Calendar) = {
    assert(cal.shortToLong(0,363) == "0000-12-27")
    assert(cal.shortToLong(0,364) == "0000-12-28")
    assert(cal.shortToLong(0,365) == "0000-12-29")
    assert(cal.shortToLong(3,366) == "0003-12-30") // 03 is a leap year
    assert(cal.shortToLong(4,0) == "0004-00-00")
  }

  def testLongToShort(cal: Calendar) = {
    assert(cal.longToShort(0,12,27) == "0000+363")
    assert(cal.longToShort(0,12,28) == "0000+364")
    assert(cal.longToShort(0,12,29) == "0000+365")
    assert(cal.longToShort(3,12,30) == "0003+366") // 03 is a leap year
    assert(cal.longToShort(4,0,0) == "0004+000")
  }

  def testLeapYears(cal: Calendar) = {
    val leaps = (0 until 100).map(y => cal.isLeap(y))
    val expected = Vector(false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, true, false, false, false, false, true, false)

    assert(leaps.zip(expected).filter(p => p._1 == p._2).length == 100)
  }

  def testAsgardianDates(cal: Calendar) = {
    val asgardianDates = (0 to 13).map(m => cal.asgardianToGregorian(0,m * 28))
    val expected = Vector("2016:12:21", "2017:01:18", "2017:02:15", "2017:03:15", "2017:04:12", "2017:05:10", "2017:06:07", "2017:07:05", "2017:08:02", "2017:08:30", "2017:09:27", "2017:10:25", "2017:11:22", "2017:12:20")
    assert(asgardianDates.toString == expected.toString)
  }

  def testGregorianDates(cal: Calendar) = {
    val gregorianDates = (1 to 12).map(m => cal.gregorianToAsgardian(2017,m,28))
    val expected = Vector("0000+038", "0000+069", "0000+097", "0000+128", "0000+158", "0000+189", "0000+219", "0000+250", "0000+281", "0000+311", "0000+342", "0001+007")
    assert(gregorianDates.toString == expected.toString)
  }

  def testToLongDates(cal: Calendar) = {
    val longDates = (0 to 13).map(m => cal.shortToLong(0,m * 28))
    val expected = Vector("0000-00-00", "0000-01-00", "0000-02-00", "0000-03-00", "0000-04-00", "0000-05-00", "0000-06-00", "0000-07-00", "0000-08-00", "0000-09-00", "0000-10-00", "0000-11-00", "0000-12-00", "0000-12-28")
    assert(longDates == expected)
  }

  def test(args: Array[String]): Unit = {
    val cal = EarthCalendar.cal

    testLeapYears(cal)
    testShortToLong(cal)
    testLongToShort(cal)
    testToLongDates(cal)
    testAsgardianDates(cal)
    testGregorianDates(cal)

    // Stress tests
    testIncDays(cal)
    testDecDays(cal)
    testPlanetConversion(cal)
    testAsgardianToGregorian(cal)
    testGregorianToAsgardian(cal)
  }
}
