package space.asgardia.calendar

case class Calendar(locale: String = "Earth",
                    grDaysInYear: Double = 365.2422199,
                    grHoursInDay: Double = 24.0,
                    grDayOffset: Int = 10,
                    grStartOfEra: Int = 2016,
                    daysInMonth: Int = 28,
                    daysInWeek: Int = 7
                   ) {
  val monthsInYear: Int = (grDaysInYear / daysInMonth).round.toInt
  private val monthLens = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  private val leapMonthLens = monthLens.map(ml => if (ml == 28) 29 else ml)
  private val cumMonthLens = monthLens.scanLeft(0)((a,b) => a + b)
  private val cumLeapMonthLens = leapMonthLens.scanLeft(0)((a,b) => a + b)
  private val diff = grDaysInYear - grDaysInYear.floor
  private val leap = 1.0 / diff
  private val halfLeap = leap / 2

  def gregorianIsLeap(y: Int): Boolean =
    y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)

  def isLeap(y: Double): Boolean =
    (diff * (y + halfLeap - 1)).toInt < (diff * (y + halfLeap)).toInt 

  def getMonthLens(y: Int): List[Int] =
    if (gregorianIsLeap(y)) leapMonthLens else monthLens

  def getCumMonthLens(y: Int): List[Int] =
    if (gregorianIsLeap(y)) cumLeapMonthLens else cumMonthLens

  def dateToLong(dt: Date): String = {
    dt.toString
  }

  def shortToLong(y: Int, doy: Int): String = {
    assert(doy <= grDaysInYear.toInt + (if (isLeap(y)) 1 else 0))

    Date(y, doy).toLongString
  }

  def shortToLong(dt: String): String = {
    val flds = dt.split("\\+")

    shortToLong(flds(0).toInt, flds(1).toInt)
  }

  def shortToLong(dt: Date): String = {
    dt.toLongString
  }

  def decDays(dt: Date, i: Int): Date = {
    dt.decDays(i)
  }

  def decDays(y: Int, doy: Int, i: Int): Date = {
    Date(y, doy, this).decDays(i)
  }

  def decDays(dt: String, i: Int): Date = {
    val flds = dt.split("\\+")

    decDays(flds(0).toInt, flds(1).toInt, i)
  }

  def incDays(dt: Date, i: Int): Date = {
    dt.incDays(i)
  }

  def incDays(y: Int, doy: Int, i: Int): Date = {
    Date(y, doy, this).incDays(i)
  }

  def incDays(dt: String, i: Int): Date = {
    val flds = dt.split("\\+")

    incDays(flds(0).toInt, flds(1).toInt, i)
  }

  def decYears(dt: Date, i: Int): Date = {
    dt.decYears(i)
  }

  def incYears(dt: Date, i: Int): Date = {
    dt.decYears(-i)
  }

  def decYears(y: Int, doy: Int, i: Int): Date = {
    val diy = if (isLeap(y)) grDaysInYear.ceil.toInt else grDaysInYear.toInt

    if (i == 0) {
      var d = if (doy > diy - 1) doy else doy - 1

      Date(y, d)
    }
    else {
      decYears(y - 1, doy, i - 1)
    }
  }

  def decYears(dt: String, i: Int): Date = {
    val flds = dt.split("\\+")

    decYears(flds(0).toInt, flds(1).toInt, i)
  }

  def incYears(y: Int, doy: Int, i: Int): Date = {
    val diy = if (isLeap(y)) grDaysInYear.ceil.toInt else grDaysInYear.toInt

    if (i == 0) {
      var d = if (doy > diy - 1) doy else doy - 1

      Date(y, d)
    }
    else {
      incYears(y + 1, doy, i - 1)
    }
  }

  def incYears(dt: String, i: Int): Date = {
    val flds = dt.split("\\+")

    incYears(flds(0).toInt, flds(1).toInt, i)
  }

  def longToShort(y: Int, m: Int, d: Int): Date = {
    val doy = m * daysInMonth + d

    assert(doy <= grDaysInYear.toInt + (if (isLeap(y)) 1 else 0))

    Date(y, doy)
  }

  def longToShort(dt: String): Date = {
    val flds = dt.split("-")

    longToShort(flds(0).toInt, flds(1).toInt, flds(2).toInt)
  }

  /*
  private def altIsLeap(y: Int): Boolean = {
    val diff = grDaysInYear - grDaysInYear.floor

    (diff * (y - 1)).toInt < (diff * y).toInt 
  }
  */

  def gregorianToAsgardian(y: Int, m: Int, d: Int): Date = {
    if (locale != "Earth") {
      fromEra(EarthCalendar.cal.gregorianToAsgardian(y, m, d).toEra)
    }
    else {
      assert(m > 0 && m <= 12)
      assert(d > 0 && d <= monthLens.max)
      val cml = getCumMonthLens(y)
      val mm = m - 1
      val lastYear = cml(mm) + d < cml(12) - grDayOffset
      val dd = d - 1 + grDayOffset + (if (lastYear) 1 else 0)
      val yr = (if (lastYear) y - 1 else y) - grStartOfEra
      val yy = if (y <= 0) yr + 1 else yr // Adjust for no zero in Gregorian
      val yLen = (if (gregorianIsLeap(yr)) grDaysInYear.ceil.toInt else grDaysInYear.toInt) - 1
      val dayOfYear = cml(mm) + dd
      val doy = if (!lastYear) dayOfYear % yLen else dayOfYear

      Date(yy, doy, this)
    }
  }

  def gregorianToAsgardian(dt: String): Date = {
    val flds = dt.split(":")

    gregorianToAsgardian(flds(0).toInt, flds(1).toInt, flds(2).toInt)
  }

  def asgardianToGregorian(y: Int, doy: Int): String = {
    if (locale != "Earth") {
      EarthCalendar.cal.asgardianToGregorian(Date(y, doy, this).toEra)
    }
    else {
      //println("--- " + Date(y, doy, this).toEra)
//println(y + " - " + doy)
      assert(doy <= grDaysInYear.toInt)
      val yy = (if (doy > grDayOffset) y + 1 else y) + grStartOfEra
      val bce = if (y < -grStartOfEra) 0 else 1
      val yNorm = (y + bce + grStartOfEra).toInt
      val yLen = if (gregorianIsLeap(yNorm)) grDaysInYear.ceil.toInt else grDaysInYear.toInt
      val yyd = (if (doy >= grDayOffset) doy else doy + yLen) - grDayOffset
      val mtd = getCumMonthLens(yNorm).takeWhile(_ < yyd)
      val mm = if (mtd.length == 0) monthsInYear - 1 else mtd.length
      val dd = yyd - (if (mtd.length == 0) -31 else mtd.max)
//println(yyd + " ~ " + yy + " - " + mm + " - " + dd)

      if (yy <= 0) // Adjust for no zero in Gregorian
        f"${yy.toInt - 1}%05d:$mm%02d:${dd.toInt}%02d"
      else
        f"${yy.toInt}%04d:$mm%02d:${dd.toInt}%02d"
    }
  }

  def asgardianToGregorian(dt: String): String = {
    val flds = dt.split("\\+")

    asgardianToGregorian(flds(0).toInt, flds(1).toInt)
  }

  def asgardianToGregorian(dt: Date): String = {
    asgardianToGregorian((dt.era / grDaysInYear).toInt, (dt.era % grDaysInYear).toInt)
  }

  def asgardianToGregorian(era: Double): String = {
    asgardianToGregorian((era / grDaysInYear).toInt, (era % grDaysInYear).toInt)
  }

  def fromEra(e: Double): Date = {
    Date(e, this, true)
  }

  private val tz = java.util.TimeZone.getTimeZone("GMT")

  def now(): Date = {
    val now = java.util.Calendar.getInstance(tz).getTime()
    val format = new java.text.SimpleDateFormat("yyyy:MM:dd")
    val dt = format.format(now)

    val earth = EarthCalendar.cal.gregorianToAsgardian(dt)

    if ("Earth" == locale)
      earth
    else {
      fromEra(earth.toEra)
    }
  }
}

object EarthCalendar {
  val cal = Calendar()
}

object MarsCalendar {
  val cal = Calendar("Mars", grDaysInYear = 668.5968606, grHoursInDay=24.6588056, grDayOffset=33)
}

object TestCalendar {
  def main(args: Array[String]): Unit = {
    val cal = EarthCalendar.cal
    //val leaps = (0 until 10).map(y => cal.isLeap(y))
    //println(leaps)
    //val gregorianDates = (1 to 12).map(m => cal.gregorianToAsgardian(2017,m,10))
    //println(gregorianDates)
    //val asgardianDates = (0 until 13).map(m => cal.shortToLong(0,m * 28))
    //println(asgardianDates)
    /*
    println(cal.shortToLong(0,363))
    println(cal.shortToLong(0,364))
    println(cal.shortToLong(0,365))
    println(cal.shortToLong(3,366))
    */
    /*
    println("------------")
    for (y <- -7000 to 3000) {
      for (yd <- 0 until (if (cal.gregorianIsLeap(y + (if (y < -2016) 2016 else 2017))) 366 else 365)) {
        val g = cal.asgardianToGregorian(y, yd)
        val fmt = if (y < 0) f"${y}%05d+${yd}%03d" else f"${y}%04d+${yd}%03d"

        if (fmt != cal.gregorianToAsgardian(g).toString)
          println(fmt + " : " + g + " -> " + cal.gregorianToAsgardian(g))
      }
    }
    println("------------")
    */
//    for (y <- -5000 to 5000) {
    for (y <- 2022 to 2024) {
      if (y != 0) { // No year zero in Gregorian calendar!!!
        for (m <- 1 to 12) {
          for (d <- 1 to cal.getMonthLens(y)(m - 1)) {
            val g = cal.gregorianToAsgardian(y, m, d)
            val a = cal.asgardianToGregorian(g)
//println(g + " - " + a)

            if (g != cal.gregorianToAsgardian(a))
              println(g + " : " + a + " -> " + cal.gregorianToAsgardian(a))
          }
        }
      }
    }
    println("------------")
    /*
    var day = "-5000+000"
    do {
      day = cal.incDays(day,1)
      //println(day)
    } while (day != "5000+000")
    println("------------")
    day = "5000+000"
    do {
      day = cal.decDays(day,1)
      //println(day)
    } while (day != "-5000+000")
    */
    val now = Date()
    //println(now + " : " + cal.shortToLong(now) + " -> " + cal.asgardianToGregorian(now))
    //println(now + " : " + now.toLongString + " -> " + now.asgardianToGregorian + " => " + now.gregorianToAsgardian(now.asgardianToGregorian) + " : " + now.gregorianToAsgardian(now.asgardianToGregorian))

    val marsNow = Date(MarsCalendar.cal)

    //println(marsNow + " : " + marsNow.toLongString + " -> " + marsNow.asgardianToGregorian + " => " + marsNow.gregorianToAsgardian(marsNow.asgardianToGregorian) + " : " + marsNow.gregorianToAsgardian(now.asgardianToGregorian) + " : " + now.gregorianToAsgardian(marsNow.asgardianToGregorian))
 
    var earth = now
    var mars = marsNow
    //var mars = earth.convert(MarsCalendar.cal)
    println("Earth: " + earth.era + " / " + earth.toEra + " - " + earth.fromEra(earth.toEra).era + ", Mars: " + mars.era + " / " + mars.toEra + " - " + mars.fromEra(mars.toEra).era) 

    /*
    println(earth.toEra)// + " != " + mars.toEra + " - " + mars.convert(earth.cal).toEra)
    earth = earth.dec
    mars = mars.dec
    println(earth.toEra)// + " != " + mars.toEra + " - " + mars.convert(earth.cal).toEra)
    */

    /*
    for (_ <- 0 until 10) {
      //println(marsNow + " : " + marsNow.toLongString + " -> " + marsNow.asgardianToGregorian + " => " + marsNow.gregorianToAsgardian(marsNow.asgardianToGregorian) + " : " + marsNow.gregorianToAsgardian(now.asgardianToGregorian) + " : " + now.gregorianToAsgardian(marsNow.asgardianToGregorian))
      //if (earth != earth.gregorianToAsgardian(mars.asgardianToGregorian))
      if (earth.toEra != mars.convert(earth.cal).toEra)
        //println(marsNow + " : " + marsNow.toLongString + " -> " + marsNow.asgardianToGregorian + " => " + marsNow.gregorianToAsgardian(marsNow.asgardianToGregorian) + " : " + marsNow.gregorianToAsgardian(now.asgardianToGregorian) + " : " + now.gregorianToAsgardian(marsNow.asgardianToGregorian))
        //println(earth + " != " + earth.gregorianToAsgardian(mars.asgardianToGregorian))
        //println(earth.asgardianToGregorian + " != " + mars.asgardianToGregorian)
        println(earth.toEra + " != " + mars.toEra + " - " + mars.convert(earth.cal).toEra)

      earth = earth.dec
      //mars = earth.convert(mars.cal)
      mars = mars.dec
    }
    */

    //println(marsNow.toEra + " -> " + now.toEra)
    //println(marsNow.convert(EarthCalendar.cal) + " : " + now.convert(MarsCalendar.cal))
    //println(marsNow.inc.convert(EarthCalendar.cal) + " : " + now.inc.convert(MarsCalendar.cal))
  }
}
