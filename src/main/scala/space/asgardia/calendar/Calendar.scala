package space.asgardia.calendar

case class Calendar(locale: String = "Earth",
                    grDaysInYear: Double = 365.2422,
                    grHoursInDay: Double = 24.0,
                    grDayOffset: Int = 10,
                    grStartOfEra: Int = 2016,
                    daysInMonth: Int = 28,
                    daysInWeek: Int = 7,
                    hoursInDay: Int = 24
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
    f"$y%04d-${if (doy < grDaysInYear.toInt) doy / daysInMonth else monthsInYear - 1}%02d-${if (doy / daysInMonth < daysInMonth.toInt) doy % daysInMonth else daysInMonth + doy % daysInMonth}%02d"
  }

  def shortToLong(dt: String): String = {
    val flds = dt.split("\\+")

    shortToLong(flds(0).toInt, flds(1).toInt)
  }

  def decDays(dt: Date, i: Int): Date = {
    if (dt.doy >= i) {
      Date(dt.y, dt.doy - i, this)
    }
    else {
      val diy = if (isLeap(dt.y - 1)) grDaysInYear.ceil.toInt else grDaysInYear.toInt

      decDays(Date(dt.y - 1, dt.doy, this), i - diy)
    }
  }

  def incDays(dt: Date, i: Int): Date = {
    val diy = if (isLeap(dt.y)) grDaysInYear.ceil.toInt else grDaysInYear.toInt

    if (dt.doy + i < diy) {
      Date(dt.y, dt.doy + i, this)
    }
    else {
      incDays(Date(dt.y + 1, dt.doy, this), i - diy)
    }
  }

  def decDays(y: Int, doy: Int, i: Int): String = {
    if (doy >= i) {
      if (y < 0)
        f"$y%05d+${doy - i}%03d"
      else
        f"$y%04d+${doy - i}%03d"
    }
    else {
      val diy = if (isLeap(y - 1)) grDaysInYear.ceil.toInt else grDaysInYear.toInt

      decDays(y - 1, doy, i - diy)
    }
  }

  def decDays(dt: String, i: Int): String = {
    val flds = dt.split("\\+")

    decDays(flds(0).toInt, flds(1).toInt, i)
  }

  def incDays(y: Int, doy: Int, i: Int): String = {
    val diy = if (isLeap(y)) grDaysInYear.ceil.toInt else grDaysInYear.toInt

    if (doy + i < diy) {
      if (y < 0)
        f"$y%05d+${doy + i}%03d"
      else
        f"$y%04d+${doy + i}%03d"
    }
    else {
      incDays(y + 1, doy, i - diy)
    }
  }

  def incDays(dt: String, i: Int): String = {
    val flds = dt.split("\\+")

    incDays(flds(0).toInt, flds(1).toInt, i)
  }

  def decYears(dt: Date, i: Int): Date = {
    var yy = dt.y - i
    val diy = if (isLeap(dt.y)) grDaysInYear.ceil.toInt else grDaysInYear.toInt
    var d = if (dt.doy > diy - 1) dt.doy else dt.doy - 1

    Date(yy, d, this)
  }

  def incYears(dt: Date, i: Int): Date = {
    decYears(dt, -i)
  }

  def decYears(y: Int, doy: Int, i: Int): String = {
    val diy = if (isLeap(y)) grDaysInYear.ceil.toInt else grDaysInYear.toInt

    if (i == 0) {
      var d = if (doy > diy - 1) doy else doy - 1

      if (y < 0)
        f"$y%05d+$d%03d"
      else
        f"$y%04d+$d%03d"
    }
    else {
      decYears(y - 1, doy, i - 1)
    }
  }

  def decYears(dt: String, i: Int): String = {
    val flds = dt.split("\\+")

    decYears(flds(0).toInt, flds(1).toInt, i)
  }

  def incYears(y: Int, doy: Int, i: Int): String = {
    val diy = if (isLeap(y)) grDaysInYear.ceil.toInt else grDaysInYear.toInt

    if (i == 0) {
      var d = if (doy > diy - 1) doy else doy - 1

      if (y < 0)
        f"$y%05d+$d%03d"
      else
        f"$y%04d+$d%03d"
    }
    else {
      incYears(y + 1, doy, i - 1)
    }
  }

  def incYears(dt: String, i: Int): String = {
    val flds = dt.split("\\+")

    incYears(flds(0).toInt, flds(1).toInt, i)
  }

  def longToDate(y: Int, m: Int, d: Int): Date = {
    val doy = m * daysInMonth + d

    assert(doy <= grDaysInYear.toInt + (if (isLeap(y)) 1 else 0))

    Date(y, doy)
  }

  def longToShort(y: Int, m: Int, d: Int): String = {
    val doy = m * daysInMonth + d
    assert(doy <= grDaysInYear.toInt + (if (isLeap(y)) 1 else 0))
    f"$y%04d+${doy}%03d"
  }

  def longToShort(dt: String): String = {
    val flds = dt.split("-")

    longToShort(flds(0).toInt, flds(1).toInt, flds(2).toInt)
  }

  def gregorianToAsgardian(y: Int, m: Int, d: Int): String = {
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

    if (yy < 0)
      f"${yy}%05d+${doy}%03d"
    else
      f"${yy}%04d+${doy}%03d"
  }

  def gregorianToAsgardian(dt: String): String = {
    val flds = dt.split(":")

    gregorianToAsgardian(flds(0).toInt, flds(1).toInt, flds(2).toInt)
  }

  def asgardianToGregorian(y: Int, yd: Int): String = {
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

  def asgardianToGregorian(dt: String): String = {
    val flds = dt.split("\\+")

    asgardianToGregorian(flds(0).toInt, flds(1).toInt)
  }

  def fromEra(e: Double): Date = {
    val eoff = e - grStartOfEra * EarthCalendar.cal.grDaysInYear
    val days = ((eoff - grDayOffset) * EarthCalendar.cal.grHoursInDay) / grHoursInDay

    val yr = (days / grDaysInYear).round.toInt
    val doy = (days % grDaysInYear).round.toInt

    Date(yr,doy,this)
  }

  private val tz = java.util.TimeZone.getTimeZone("GMT")

  def now(): Date = {
    val now = java.util.Calendar.getInstance(tz).getTimeInMillis()
    var epoch = 1970 * EarthCalendar.cal.grDaysInYear
    var start = grStartOfEra * EarthCalendar.cal.grDaysInYear
    var diff = start - epoch
    var days = start - diff + now / 1000 / 60 / 60 / 24

    fromEra(days)
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
    val gregorianDates = (1 to 12).map(m => cal.gregorianToAsgardian(2017,m,10))
    println(gregorianDates)
    val asgardianDates = (0 until 13).map(m => cal.shortToLong(0,m * 28))
    println(asgardianDates)
    /*
    println(cal.shortToLong(0,363))
    println(cal.shortToLong(0,364))
    println(cal.shortToLong(0,365))
    println(cal.shortToLong(3,366))
    */
    /*
    println("------------")
    for (y <- 0 until 200) {
      println(y + " : " + cal.isLeap(y) + " --> " + cal.gregorianIsLeap(y))
    }
    */
    println("------------")
    for (y <- -7000 to 3000) {
      for (yd <- 0 until (if (cal.gregorianIsLeap(y + (if (y < -2016) 2016 else 2017))) 366 else 365)) {
        val g = cal.asgardianToGregorian(y, yd)
        val fmt = if (y < 0) f"${y}%05d+${yd}%03d" else f"${y}%04d+${yd}%03d"

        if (fmt != cal.gregorianToAsgardian(g))
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
    val now = cal.now.toString
    println(now + " : " + cal.shortToLong(now) + " -> " + cal.asgardianToGregorian(now))

    val mars = MarsCalendar.cal

    val marsNow = mars.now.toString
    println(marsNow + " : " + mars.shortToLong(marsNow) + " -> " + mars.asgardianToGregorian(marsNow))
  }
}
