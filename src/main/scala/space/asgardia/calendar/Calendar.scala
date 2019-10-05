package space.asgardia.calendar

case class Calendar(locale: String = "Earth",
                    daysInYear: Double = 365.24219878,
                    hoursInDay: Double = 24.0,
                    dayOffset: Int = 10,
                    startOfEra: Int = 2016,
                    daysInMonth: Int = 28,
                    daysInWeek: Int = 7
                   ) {
  val earthDaysInYear = 365.2422199
  val earthHoursInDay = 24.0
  val monthsInYear: Int = (daysInYear / daysInMonth).round.toInt
  private val gMonthLens = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  private val gLeapMonthLens = gMonthLens.map(ml => if (ml == 28) 29 else ml)
  private val gCumMonthLens = gMonthLens.scanLeft(0)((a,b) => a + b)
  private val gCumLeapMonthLens = gLeapMonthLens.scanLeft(0)((a,b) => a + b)
  private val diff = daysInYear - daysInYear.floor
  private val leap = 1.0 / diff
  private val halfLeap = leap / 2
  // Probably good enough for 5000 years
  private val cumYearLens = (0 to 5000).scanLeft(0)((cy,y) => cy + (if (isLeap(y)) daysInYear.ceil.toInt else daysInYear.floor.toInt))
  private val baseCumMonthLens = (0 until (daysInYear / daysInMonth).ceil.toInt).scanLeft(0)((cm,m) => cm + daysInMonth)
  private val cumMonthLens = baseCumMonthLens.map(m => if (m == baseCumMonthLens.max) daysInYear.floor.toInt else m).filter(m => m < daysInYear.ceil.toInt)
  private val cumLeapMonthLens = gCumMonthLens.map(m => if (m == gCumMonthLens.max) daysInYear.ceil.toInt else m)

  def yearFromEra(e: Double) = {
    ((e / daysInYear.ceil).toInt to (e / daysInYear.floor).toInt).filter(m => cumYearLens(m) <= e).max
  }

  def doyFromEra(e: Double, y: Int = -2000000000) = {
    val yr = if (y == -2000000000) yearFromEra(e) else y

    e - cumYearLens(yr)
  }

  private val tz = java.util.TimeZone.getTimeZone("UTC")
  private val libEpoch = {
    val cal = java.util.Calendar.getInstance(tz)

    cal.set(startOfEra, 11, 31)

    cal.getTime().getTime() / 1000 / 60 / 60 / 24
  }


  def toEra(dt: Date) = {
    cumYearLens(dt.y) + dt.doy
  }

  def toEra(y: Int, doy: Double) = {
    cumYearLens(y) + doy
  }

  def fromEra(era: Double): Date = {
    val yr = yearFromEra(era)
    val dy = doyFromEra(era, yr)

    Date(yr, dy, this)
  }

  def convert(dt: Date): Date = {
    fromEra((dt.cal.epoch(dt) + dayOffset) * earthHoursInDay / hoursInDay)
  }

  def convert(dt: String): Date = {
    convert(Date(dt))
  }

  def epoch(dt: Date): Double = {
    (cumYearLens(dt.y) + dt.doy) * hoursInDay / earthHoursInDay - dayOffset
  }

  def fromEpoch(epoch: Double) = {
    val era = ((epoch - libEpoch) + dayOffset) * earthHoursInDay / hoursInDay

    fromEra(era)
  }

  def now(): Date = {
    val cal = java.util.Calendar.getInstance(tz)
    val epoch = cal.getTime().getTime() / 1000 / 60 / 60 / 24

    fromEpoch(epoch)
  }

  def gregorianIsLeap(y: Int): Boolean =
    y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)

  def isLeap(y: Int): Boolean =
    (diff * (y + halfLeap - 1)).toInt < (diff * (y + halfLeap)).toInt 

  def getMonthLens(y: Int): List[Int] =
    if (gregorianIsLeap(y)) gLeapMonthLens else gMonthLens

  def getCumMonthLens(y: Int): List[Int] =
    if (gregorianIsLeap(y)) gCumLeapMonthLens else gCumMonthLens

  def dateToLong(dt: Date): String = {
    assert(dt.doy <= daysInYear.toInt + (if (isLeap(dt.y)) 1 else 0))

    dt.toString
  }

  def shortToLong(y: Int, doy: Double): String = {
    assert(doy <= daysInYear.toInt + (if (isLeap(y)) 1 else 0))
    f"$y%04d-${(if (doy < daysInYear.toInt - 1) doy / daysInMonth else monthsInYear - 1).toInt}%02.0f-${if (doy / daysInMonth < monthsInYear) doy % daysInMonth else daysInMonth + doy % daysInMonth}%02.0f"
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
      val diy = if (isLeap(dt.y - 1)) daysInYear.ceil.toInt else daysInYear.toInt

      decDays(Date(dt.y - 1, dt.doy, this), i - diy)
    }
  }

  def incDays(dt: Date, i: Int): Date = {
    val diy = if (isLeap(dt.y)) daysInYear.ceil.toInt else daysInYear.toInt

    if (dt.doy + i < diy) {
      Date(dt.y, dt.doy + i, this)
    }
    else {
      incDays(Date(dt.y + 1, dt.doy, this), i - diy)
    }
  }

  def decDays(y: Int, doy: Double, i: Int): Date = {
    if (doy >= i) {
      Date(y, doy - i, this)
    }
    else {
      val diy = if (isLeap(y - 1)) daysInYear.ceil.toInt else daysInYear.toInt

      decDays(y - 1, doy, i - diy)
    }
  }

  def decDays(dt: String, i: Int): Date = {
    val flds = dt.split("\\+")

    decDays(flds(0).toInt, flds(1).toInt, i)
  }

  def incDays(y: Int, doy: Double, i: Int): Date = {
    val diy = if (isLeap(y)) daysInYear.ceil.toInt else daysInYear.toInt

    if (doy + i < diy) {
      Date(y, doy - i, this)
    }
    else {
      incDays(y + 1, doy, i - diy)
    }
  }

  def incDays(dt: String, i: Int): Date = {
    val flds = dt.split("\\+")

    incDays(flds(0).toInt, flds(1).toInt, i)
  }

  def decYears(dt: Date, i: Int): Date = {
    var yy = dt.y - i
    val diy = if (isLeap(dt.y)) daysInYear.ceil.toInt else daysInYear.toInt
    var d = if (dt.doy > diy - 1) dt.doy else dt.doy - 1

    Date(yy, d, this)
  }

  def incYears(dt: Date, i: Int): Date = {
    decYears(dt, -i)
  }

  def decYears(y: Int, doy: Double, i: Int): Date = {
    val diy = if (isLeap(y)) daysInYear.ceil.toInt else daysInYear.toInt

    if (i == 0) {
      var d = if (doy > diy - 1) doy else doy - 1

      Date(y, d, this)
    }
    else {
      decYears(y - 1, doy, i - 1)
    }
  }

  def decYears(dt: String, i: Int): Date = {
    val flds = dt.split("\\+")

    decYears(flds(0).toInt, flds(1).toInt, i)
  }

  def incYears(y: Int, doy: Double, i: Int): Date = {
    val diy = if (isLeap(y)) daysInYear.ceil.toInt else daysInYear.toInt

    if (i == 0) {
      var d = if (doy > diy - 1) doy else doy - 1

      Date(y, d, this)
    }
    else {
      incYears(y + 1, doy, i - 1)
    }
  }

  def incYears(dt: String, i: Int): Date = {
    val flds = dt.split("\\+")

    incYears(flds(0).toInt, flds(1).toInt, i)
  }

  def longToDate(y: Int, m: Int, d: Int): Date = {
    val doy = m * daysInMonth + d

    assert(doy <= daysInYear.toInt + (if (isLeap(y)) 1 else 0))

    Date(y, doy)
  }

  def longToShort(y: Int, m: Int, d: Int): String = {
    val doy = m * daysInMonth + d

    assert(doy <= daysInYear.toInt + (if (isLeap(y)) 1 else 0))

    f"$y%04d+${doy}%03d"
  }

  def longToShort(dt: String): String = {
    val flds = dt.split("-")

    longToShort(flds(0).toInt, flds(1).toInt, flds(2).toInt)
  }

  def gregorianToAsgardian(y: Int, m: Int, d: Int): Date = {
    if ("Earth" != locale) {
      convert(EarthCalendar.cal.gregorianToAsgardian(y, m, d))
    }
    else {
      assert(m > 0 && m <= 12)
      assert(d > 0 && d <= gMonthLens.max)
      val cml = getCumMonthLens(y)
      val mm = m - 1
      val lastYear = cml(mm) + d < cml(12) - dayOffset
      val dd = d - 1 + dayOffset + (if (lastYear) 1 else 0)
      val yr = (if (lastYear) y - 1 else y) - startOfEra.toInt 
      val yy = if (y <= 0) yr + 1 else yr // Adjust for no zero in Gregorian
      val yLen = (if (gregorianIsLeap(y)) daysInYear.ceil.toInt else daysInYear.toInt) - 1
      val dayOfYear = cml(mm) + dd
      val doy = if (!lastYear) dayOfYear % yLen else dayOfYear

      Date(yy, doy, EarthCalendar.cal)
    }
  }
  def gregorianToAsgardian(dt: String): Date = {
    val flds = dt.split(":")

    gregorianToAsgardian(flds(0).toInt, flds(1).toInt, flds(2).toInt)
  }

  def asgardianToGregorian(y: Int, yd: Double): String = {
    if ("Earth" != locale) {
      EarthCalendar.cal.asgardianToGregorian(convert(Date(y, yd, this)))
    }
    else {
      assert(yd <= daysInYear.toInt)
      val yy = (if (yd > dayOffset) y + 1 else y) + startOfEra
      val bce = if (y < -startOfEra) 0 else 1
      val isLeap = gregorianIsLeap(y + bce + startOfEra)
      val yLen = if (isLeap) daysInYear.ceil.toInt else daysInYear.toInt
      val yyd = (if (yd >= dayOffset) yd else yd + yLen) - dayOffset
      val mtd = getCumMonthLens(y + bce + startOfEra).takeWhile(_ < yyd)
      val mm = if (mtd.length == 0) monthsInYear - 1 else mtd.length
      val dd = yyd - (if (mtd.length == 0) -31 else mtd.max)

      if (yy <= 0) // Adjust for no zero in Gregorian
        f"${yy - 1}%05d:$mm%02d:${dd.toInt}%02d"
      else
        f"${yy}%04d:$mm%02d:${dd.toInt}%02d"
    }
  }

  def asgardianToGregorian(dt: String): String = {
    val flds = dt.split("\\+")

    asgardianToGregorian(flds(0).toInt, flds(1).toInt)
  }

  def asgardianToGregorian(dt: Date): String = {
    asgardianToGregorian(dt.y, dt.doy)
  }
}

object EarthCalendar {
  val cal = Calendar()
}

object MarsCalendar {
  val cal = Calendar("Mars", daysInYear = 668.5910, hoursInDay=24.65979, dayOffset=33)
}
