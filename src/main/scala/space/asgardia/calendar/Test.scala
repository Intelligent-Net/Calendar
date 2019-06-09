package space.asgardia.calendar

object TestCalendar {
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

  def testLeapYears(cal: Calendar) = {
    val leaps = (0 until 10).map(y => cal.isLeap(y))
  }

  def testGregorianDates(cal: Calendar) = {
    val gregorianDates = (1 to 12).map(m => cal.gregorianToAsgardian(2017,m,10))
  }

  def testAsgardianDates(cal: Calendar) = {
    val asgardianDates = (0 until 13).map(m => cal.shortToLong(0,m * 28))
  }

  def test(args: Array[String]): Unit = {
    val cal = EarthCalendar.cal

    testAsgardianToGregorian(cal)
    testPlanetConversion(cal)
    testGregorianToAsgardian(cal)
    testIncDays(cal)
    testDecDays(cal)
    testShortToLong(cal)
    testLeapYears(cal)
    testGregorianDates(cal)
    testAsgardianDates(cal)
  }
}
