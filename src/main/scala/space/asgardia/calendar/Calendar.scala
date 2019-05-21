package space.asgardia.calendar

case class Calendar(locale: String = "Earth",
                    grDaysInYear: Double = 365.2422,
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

  def isLeap(y: Int): Boolean =
    (diff * (y + halfLeap - 1)).toInt < (diff * (y + halfLeap)).toInt 

  def getMonthLens(y: Int): List[Int] =
    if (gregorianIsLeap(y)) leapMonthLens else monthLens

  def getCumMonthLens(y: Int): List[Int] =
    if (gregorianIsLeap(y)) cumLeapMonthLens else cumMonthLens

  def dateToLong(dt: Date): String = {
    assert(dt.doy <= grDaysInYear.toInt + (if (isLeap(dt.y)) 1 else 0))

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
    shortToLong(dt.y, dt.doy)
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

  def gregorianToAsgardian(y: Int, m: Int, d: Int): Date = {
    if (locale != "Earth") {
      fromEra(EarthCalendar.cal.gregorianToAsgardian(y, m, d).toEra)
    }
    else {
      assert(m > 0 && m <= 12)
      assert(d > 0 && d <= monthLens.max)
      val isLeap = gregorianIsLeap(y)
      val cml = getCumMonthLens(y)
      val mm = m - 1
      val lastYear = cml(mm) + d < cml(12) - grDayOffset
      val dd = d - 1 + grDayOffset + (if (lastYear) 1 else 0)
      val yr = (if (lastYear) y - 1 else y) - grStartOfEra.toInt 
      val yy = if (y <= 0) yr + 1 else yr // Adjust for no zero in Gregorian
      val yLen = (if (isLeap) grDaysInYear.ceil.toInt else grDaysInYear.toInt) - 1
      val dayOfYear = cml(mm) + dd
      val doy = if (!lastYear) dayOfYear % yLen else dayOfYear

      Date(yy, doy, this)
    }
  }

  def gregorianToAsgardian(dt: String): Date = {
    val flds = dt.split(":")

    gregorianToAsgardian(flds(0).toInt, flds(1).toInt, flds(2).toInt)
  }

  def asgardianToGregorian(y: Int, yd: Int): String = {
    if (locale != "Earth") {
      //println(locale)
      //println(Date(y, yd, this))
      //println(Date(y, yd, this).toEra)
      //println(EarthCalendar.cal.fromEra(Date(y, yd, this).toEra))
      EarthCalendar.cal.asgardianToGregorian(EarthCalendar.cal.fromEra(Date(y, yd, this).toEra))
    }
    else {
      //println("--- " + Date(y, yd, this).toEra)
      assert(yd <= grDaysInYear.toInt)
      val yy = (if (yd > grDayOffset) y + 1 else y) + grStartOfEra
      val bce = if (y < -grStartOfEra) 0 else 1
      val isLeap = gregorianIsLeap(y + bce + grStartOfEra)
      val yLen = if (isLeap) grDaysInYear.ceil.toInt else grDaysInYear.toInt
      val yyd = (if (yd >= grDayOffset) yd else yd + yLen) - grDayOffset
      val mtd = getCumMonthLens(y + bce + grStartOfEra).takeWhile(_ < yyd)
      val mm = if (mtd.length == 0) monthsInYear - 1 else mtd.length
      val dd = yyd - (if (mtd.length == 0) -31 else mtd.max)

      if (yy <= 0) // Adjust for no zero in Gregorian
        f"${yy - 1}%05d:$mm%02d:${dd}%02d"
      else
        f"${yy}%04d:$mm%02d:${dd}%02d"
    }
  }

  def asgardianToGregorian(dt: String): String = {
    val flds = dt.split("\\+")

    asgardianToGregorian(flds(0).toInt, flds(1).toInt)
  }

  def asgardianToGregorian(dt: Date): String = {
    asgardianToGregorian(dt.y, dt.doy)
  }

  def fromEra(e: Double): Date = {
    val days = ((e - grDayOffset) * EarthCalendar.cal.grHoursInDay) / grHoursInDay

    val yr = (days / grDaysInYear).round.toInt
    val doy = (days % grDaysInYear).round.toInt

    Date(yr,doy,this)
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
  val cal = Calendar("Mars", grDaysInYear = 668.5910, grHoursInDay=24.65979, grDayOffset=33)
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
    for (y <- -5000 to 5000) {
      if (y != 0) { // No year zero in Gregorian calendar!!!
        for (m <- 1 to 12) {
          for (d <- 1 to cal.getMonthLens(y)(m - 1)) {
            val g = cal.gregorianToAsgardian(y, m, d)
            val a = cal.asgardianToGregorian(g)

            if (g != cal.gregorianToAsgardian(a))
              println(g + " : " + a + " -> " + cal.gregorianToAsgardian(a))
          }
        }
      }
    }
    */
    /*
    println("------------")
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
    println(now + " : " + now.toLongString + " -> " + now.asgardianToGregorian + " => " + now.gregorianToAsgardian(now.asgardianToGregorian) + " : " + now.gregorianToAsgardian(now.asgardianToGregorian))

    val marsNow = Date(MarsCalendar.cal)

    println(marsNow + " : " + marsNow.toLongString + " -> " + marsNow.asgardianToGregorian + " => " + marsNow.gregorianToAsgardian(marsNow.asgardianToGregorian) + " : " + marsNow.gregorianToAsgardian(now.asgardianToGregorian))

    println(marsNow.toEra + " -> " + now.toEra)
  }
}
