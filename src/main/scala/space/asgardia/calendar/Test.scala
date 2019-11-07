package space.asgardia.calendar

object Test {
  def main(args: Array[String]): Unit = {
    test(args)
  }

  private def rnd(d: Double) = 
    EarthCalendar.round(d, 100000.0)

  def testAsgardianToGregorian(cal: Calendar) = {
    // NOTE: does not work properly before start of Gregorian Era
    for (yy <- - cal.startOfEra + 1 to 3000) {
      for (yd <- 0 until (if (cal.gregorianIsLeap(yy + (if (yy <= -cal.startOfEra) cal.startOfEra - 1 else cal.startOfEra))) cal.daysInYear.ceil.toInt else cal.daysInYear.toInt)) {
        val g = cal.asgardianToGregorian(yy, yd)
        val a = cal.gregorianToAsgardian(g)
        val fmt = if (yy < 0) f"${yy}%05d+${yd}%03d" else f"${yy}%04d+${yd}%03d"

        if (fmt != a.toDate)
          println(fmt + " == " + a.toDate)
        assert(fmt == a.toDate)
      }
    }
  }

  def testGregorianToAsgardian(cal: Calendar) = {
    // NOTE: does not work properly before start of Gregorian Era
    for (y <- 0 to 5000 if y != 0) {
      for (m <- 1 to 12) {
        for (d <- 1 to cal.getMonthLens(y)(m - 1)) {
          val a = cal.gregorianToAsgardian(y, m, d)
          val g = cal.asgardianToGregorian(a)
          val fmt = if (y < 0) f"${y}%05d:${m}%02d:${d}%02d" else f"${y}%04d:${m}%02d:${d}%02d"

          if (fmt != g)
            println(fmt + " == " + g)
          assert(fmt == g)
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
    val present = EarthCalendar.now.era

    assert(Date(present,EarthCalendar).era == present)
    assert(Date(Date(present,MarsCalendar).era).era == present)
    assert(EarthCalendar.asgardianToGregorian(Date(present)) == EarthCalendar.asgardianToGregorian(present))
    assert(rnd(Date().era) == rnd(Date(MarsCalendar).era))

    val c1 = Calendar("c1", daysInYear=10, hoursInDay=24.0, daysOffset=0, startOfEra=2016)

    assert(Date(10.0).era == 10.0)
    assert(Date(Date(10.0).era).era == 10.0)
    assert(Date(Date(10.0).era,MarsCalendar).era == 10.0)
    assert(Date(10.0,MarsCalendar).era == 10.0)
    assert(Date(Date(10.0,MarsCalendar).era).era == 10.0)
    assert(Date(0,1,c1).era == 1.0)
    assert(Date(0,1.5,c1).era == 1.5)
    assert(Date(0,1.5).era == 1.5)
    assert(Date(0,0,c1).era == 0.0)
    assert(Date(0,0).era == 0.0)
    assert(Date(1,1,c1).era == 11.0)
    assert(Date(1,1).era == 367.0)
    assert(Date(-1,1,c1).era == -9.0)
    assert(Date(-1,1).era == -365.0)
    assert(Date(3,299).era == 1395.0)
    assert(Date(3,30.53).era == 1126.53)
  }

  def testPlanetConversion() = {
    {
      val marsDay = Date(9998,667,MarsCalendar)
      val earthDay = Date(marsDay.y, marsDay.doy, marsDay.cal)

      assert(marsDay.conv(marsDay) == marsDay.conv(earthDay))
      assert(earthDay.conv(marsDay) == earthDay.conv(earthDay))
    }
    {
      val marsDay = Date(9999,0,MarsCalendar)
      val earthDay = EarthCalendar.conv(marsDay)

      assert(marsDay.conv(marsDay) == marsDay.conv(earthDay))
      assert(earthDay.conv(marsDay) == earthDay.conv(earthDay))
    }
    {
      val marsDay = Date(10000,0,MarsCalendar)
      val earthDay = EarthCalendar.conv(marsDay)

      assert(marsDay.conv(marsDay) == marsDay.conv(earthDay))
      assert(earthDay.conv(marsDay) == earthDay.conv(earthDay))
    }
    
    {
      val c1 = Calendar("c1", daysInYear=10, hoursInDay=10.0, daysOffset=0, startOfEra=0)
      val c2 = Calendar("c2", daysInYear=10, hoursInDay=20.0, daysOffset=0, startOfEra=0)

      assert(Date(0,1,c1).diff(Date(0,1,c2)) == Date(1,1,c2).diff(Date(1,1,c1)))
      assert(Date(0,1,c1).diff(Date(0,1,c2)) == 0.0)
    }
    {
      val c1 = Calendar("c1", daysInYear=10, hoursInDay=10.0, daysOffset=0, startOfEra=0)
      val c2 = Calendar("c2", daysInYear=20, hoursInDay=10.0, daysOffset=0, startOfEra=0)

      assert(Date(0,1,c1).diff(Date(0,1,c2)) < Date(1,1,c2).diff(Date(1,1,c1)))
      assert(Date(0,1,c1).diff(Date(0,1,c2)) == 0.0)
      assert(Date(1,1,c2).diff(Date(1,1,c1)) == 10.0)

      assert(Date(0,1,c2).diff(Date(0,1,c1)) > Date(1,1,c1).diff(Date(1,1,c2)))
      assert(Date(0,1,c2).diff(Date(0,1,c1)) == 0.0)
      assert(Date(1,1,c1).diff(Date(1,1,c2)) == -10.0)
    }
    {
      val c1 = Calendar("c1", daysInYear=10, hoursInDay=10.0, daysOffset=0, startOfEra=0)
      val c2 = Calendar("c2", daysInYear=10, hoursInDay=10.0, daysOffset=0, startOfEra=1)

      assert(Date(0,1,c1).diff(Date(0,1,c2)) < Date(1,1,c2).diff(Date(1,1,c1)))
      assert(Date(0,1,c1).diff(Date(0,1,c2)) == -10.0)
      assert(Date(1,1,c2).diff(Date(1,1,c1)) == 10.0)

      assert(Date(0,1,c2).diff(Date(0,1,c1)) > Date(1,1,c1).diff(Date(1,1,c2)))
      assert(Date(0,1,c2).diff(Date(0,1,c1)) == 10.0)
      assert(Date(1,1,c1).diff(Date(1,1,c2)) == -10.0)
    }
    {
      val c1 = Calendar("c1", daysInYear=10, hoursInDay=24.0, daysOffset=0, startOfEra=0)
      val c2 = Calendar("c2", daysInYear=10, hoursInDay=24.0, daysOffset=1, startOfEra=0)

      assert(Date(0,1,c1).diff(Date(0,1,c2)) < Date(1,1,c2).diff(Date(1,1,c1)))
      assert(Date(0,1,c1).diff(Date(0,1,c2)) == -1.0)
      assert(Date(1,1,c2).diff(Date(1,1,c1)) == 1.0)

      assert(Date(0,1,c2).diff(Date(0,1,c1)) > Date(1,1,c1).diff(Date(1,1,c2)))
      assert(Date(0,1,c2).diff(Date(0,1,c1)) == 1.0)
      assert(Date(1,1,c1).diff(Date(1,1,c2)) == -1.0)
    }
  }

  def testPlanetTimeConversion() = {
    assert(Date(0,1.5).toDatetime == "0000+001 12:00:00")
    assert(Date(3,299).toString == "0003+299")
    assert(Date(3,299).toLong == "0003-10-19")
    assert(Date(3,30.53).toString == "0003+030")
    assert(Date(3,30.53).toLongDatetime == "0003-01-02 12:43:12")
    assert(Date(3,30.53).toLongDatetimeNano == "0003-01-02 12:43:12.000000")
    assert(Date(-50004,363).toString == "-50004+363")
    assert(Date(-50004,363).toLongDatetime == "-50004-12-27 00:00:00")
    assert(Date(-50004,364).toString == "-50004+364")
    assert(Date(-50004,364).toLongDatetime == "-50004-12-28 00:00:00")
    assert(Date(-50004,365).toString == "-50004+365")
    assert(Date(-50004,365).toLongDatetime == "-50004-12-29 00:00:00")
    assert(Date(-50005,1.5).toString == "-50005+001")
    assert(Date(-50005,1.5).toLongDatetime == "-50005-00-01 12:00:00")

    val present = EarthCalendar.now.era
    val asg = EarthCalendar.asgardianToGregorian(present)

    assert(EarthCalendar.asgardianToGregorian(EarthCalendar.now) == asg)
    assert(MarsCalendar.asgardianToGregorian(MarsCalendar.now) == asg)
    assert(EarthCalendar.asgardianToGregorian(MarsCalendar.now) == asg)
    assert(MarsCalendar.asgardianToGregorian(EarthCalendar.now) == asg)

    assert(EarthCalendar.asgardianToGregorian(3,314.0) == "2019:10:31")
    assert(EarthCalendar.asgardianToGregorian(3,315.0) == "2019:11:01")
    assert(EarthCalendar.asgardianToGregorian(3,316.0) == "2019:11:02")
    assert(EarthCalendar.asgardianToGregorian(3,317.0) == "2019:11:03")
    assert(EarthCalendar.asgardianToGregorian(3,364.0) == "2019:12:20")

    assert(MarsCalendar.asgardianToGregorian(3,314.0) == "2022:07:01")
    assert(MarsCalendar.asgardianToGregorian(3,315.0) == "2022:07:02")
    assert(MarsCalendar.asgardianToGregorian(3,316.0) == "2022:07:03")
    assert(MarsCalendar.asgardianToGregorian(3,317.0) == "2022:07:04")
    assert(MarsCalendar.asgardianToGregorian(3,364.0) == "2022:08:22")

    assert(EarthCalendar.asgardianToGregorian(4,0.0) == "2019:12:21")
    assert(EarthCalendar.asgardianToGregorian(4,10.0) == "2019:12:31")
    assert(EarthCalendar.asgardianToGregorian(4,11.0) == "2020:01:01")
    assert(EarthCalendar.asgardianToGregorian(3,69.0) == "2019:02:28")
    assert(EarthCalendar.asgardianToGregorian(3,70.0) == "2019:03:01")
    assert(EarthCalendar.asgardianToGregorian(4,69.0) == "2020:02:28")
    assert(EarthCalendar.asgardianToGregorian(4,70.0) == "2020:02:29")
    assert(EarthCalendar.asgardianToGregorian(4,364.0) == "2020:12:19")
    assert(EarthCalendar.asgardianToGregorian(4,365.0) == "2020:12:20")

    assert(MarsCalendar.asgardianToGregorian(4,0.0) == "2023:07:00")
    assert(MarsCalendar.asgardianToGregorian(4,10.0) == "2023:07:10")
    assert(MarsCalendar.asgardianToGregorian(4,11.0) == "2023:07:11")
    assert(MarsCalendar.asgardianToGregorian(3,69.0) == "2021:10:23")
    assert(MarsCalendar.asgardianToGregorian(3,70.0) == "2021:10:24")
    assert(MarsCalendar.asgardianToGregorian(4,69.0) == "2023:09:09")
    assert(MarsCalendar.asgardianToGregorian(4,70.0) == "2023:09:10")
    assert(MarsCalendar.asgardianToGregorian(4,364.0) == "2024:07:08")
    assert(MarsCalendar.asgardianToGregorian(4,365.0) == "2024:07:09")

    val earthNow = Date()
    val marsNow = Date(MarsCalendar)

    assert(earthNow.toLongDatetime == EarthCalendar.conv(marsNow).toLongDatetime)
    assert(earthNow.toLongDatetime == marsNow.conv(EarthCalendar).toLongDatetime)
    assert(marsNow.toLongDatetime == MarsCalendar.conv(earthNow).toLongDatetime)
    assert(marsNow.toLongDatetime == earthNow.conv(MarsCalendar).toLongDatetime)
  }

  def testShortToLong(cal: Calendar) = {
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
    assert(EarthCalendar.era(0,0) == EarthCalendar.gregorianToAsgardian(2015, 12, 21).era)
    assert(MarsCalendar.era(0,0) == MarsCalendar.gregorianToAsgardian(2015, 12, 21).era)
    assert(EarthCalendar.era(0,11) == EarthCalendar.gregorianToAsgardian(2016, 1, 1).era)
    assert(EarthCalendar.era(0,10) == EarthCalendar.gregorianToAsgardian(2015, 12, 31).era)

    assert(EarthCalendar.gregorianToAsgardian(2015, 1, 1).toString == "-0001+011")
    assert(EarthCalendar.gregorianToAsgardian(2015, 12, 20).toString == "-0001+364")
    assert(EarthCalendar.gregorianToAsgardian(2015, 12, 21).toString == "0000+000")
    assert(EarthCalendar.gregorianToAsgardian(2015, 12, 31).toString == "0000+010")
    assert(EarthCalendar.gregorianToAsgardian(2016, 1, 1).toString == "0000+011")
    assert(EarthCalendar.gregorianToAsgardian(2016, 12, 20).toString == "0000+365")
    assert(EarthCalendar.gregorianToAsgardian(2016, 12, 21).toString == "0001+000")
    assert(EarthCalendar.gregorianToAsgardian(2016, 12, 31).toString == "0001+010")
    assert(EarthCalendar.gregorianToAsgardian(2017, 1, 1).toString == "0001+011")
    assert(EarthCalendar.gregorianToAsgardian(2017, 12, 20).toString == "0001+364")
    assert(EarthCalendar.gregorianToAsgardian(2017, 12, 21).toString == "0002+000")
    assert(EarthCalendar.gregorianToAsgardian(2017, 12, 31).toString == "0002+010")
    assert(EarthCalendar.gregorianToAsgardian(2018, 1, 1).toString == "0002+011")

    assert(EarthCalendar.gregorianToAsgardian(-1, 1, 1).toString == "-2016+011")
    assert(EarthCalendar.gregorianToAsgardian(-1, 12, 20).toString == "-2016+365")
    assert(EarthCalendar.gregorianToAsgardian(-1, 12, 21).toString == "-2015+000")
    assert(EarthCalendar.gregorianToAsgardian(-1, 12, 31).toString == "-2015+010")

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

  def testEras() = {
    assert(Date(-1,0).era == Date(Date(-1,0).era).era)
    assert(Date(-1,0,MarsCalendar).era == Date(Date(-1,0,MarsCalendar).era, MarsCalendar).era)
    assert(EarthCalendar.round(EarthCalendar.now.era, 0.0000001) == EarthCalendar.round(MarsCalendar.now.era, 0.0000001))
    assert(EarthCalendar.era(0,0) == 0.0)
    assert(EarthCalendar.era(0,0) == Date(0,0).era)

    for (y <- Vector(-1,0,1)) {
      for (doy <- Vector(0,10,20)) {
        val ge = Date(y,doy, EarthCalendar).era

        val e = Date(ge, EarthCalendar)
        val m = Date(ge, MarsCalendar)

        assert(ge == e.era)
        assert(ge == m.era)
      }
    }

    for (e <- -5000 to 5000) {
      val md = Date(e, MarsCalendar)
      val ed = Date(md.era)

      if (e != md.era)
        println("Mars : " + e + " != " + md.era)
      assert(e == md.era)
      if (e != ed.era)
        println("Earth : " + e + " != " + ed.era)
      assert(e == ed.era)
    }

    for (y <- -1000 to 1000) {
      val ee = Date(y, 0, EarthCalendar).era
      val me = Date(y, 0, MarsCalendar).era
      val err = if (y < 0) -0.00000000001 else 0.00000000001

      assert(rnd(Date(ee, EarthCalendar).era + err) == rnd(ee))
      assert(rnd(Date(ee, MarsCalendar).era + err) == rnd(ee))
      assert(rnd(Date(Date(ee, MarsCalendar).era, EarthCalendar).era + err) == rnd(ee))
      assert(rnd(Date(me, EarthCalendar).era + err) == rnd(me))
      assert(rnd(Date(me, MarsCalendar).era + err) == rnd(me))
      assert(rnd(Date(Date(me, MarsCalendar).era, EarthCalendar).era + err) == rnd(me))
    }
  }

  def test(args: Array[String]): Unit = {
    val earth = EarthCalendar

    testLeapYears(earth)
    testShortToLong(earth)
    testLongToShort(earth)
    testToLongDates(earth)
    testAsgardianDates(earth)
    testGregorianDates(earth)
    testPlanetEraConversion
    testPlanetConversion
    testPlanetTimeConversion

    // Stress tests
    testEras
    testIncDays(earth)
    testDecDays(earth)
    testAsgardianToGregorian(earth)
    testGregorianToAsgardian(earth)
  }
}
