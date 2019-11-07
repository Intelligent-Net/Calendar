package space.asgardia.calendar

case class Calendar(locale: String = "Earth",
                    daysInYear: Double = 365.24219878, // Local days in year
                    hoursInDay: Double = 24.0, // Earth hours in local day
                    daysOffset: Int = 11, // Local days of start of equinox before Gregorian start of year?
                    startOfEra: Int = 2016, // Gregorian start of Asgardian Era
                    localDaysInMonth: Int = 28,
                    localDaysInoweek: Int = 7,
                    localHoursInDay: Double = 24,
                    localMinutesInHour: Int = 60,
                    localSecondsInMinute: Int = 60,
                    yearZeroIsLeap: Boolean = true,
                    yearRange: Int = 100000
                   ) {
  val earthHoursInDay = if (locale == "Earth") hoursInDay else EarthCalendar.hoursInDay
  val earthDaylength = hoursInDay / earthHoursInDay
  val earthDayOffset = if (locale == "Earth") daysOffset else hoursInDay / EarthCalendar.hoursInDay * daysOffset
  val earthDaysInYear = daysInYear * hoursInDay / earthHoursInDay
  val earthStartOfEra = if (locale == "Earth") daysOffset else (EarthCalendar.startOfEra - startOfEra) * earthDaysInYear + earthDayOffset
  val secondsInDay = localHoursInDay * localMinutesInHour * localSecondsInMinute
  val monthsInYear: Int = (daysInYear / localDaysInMonth).round.toInt
  private val gMonthLens = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  private val gLeapMonthLens = gMonthLens.map(ml => if (ml == 28) 29 else ml)
  private val gCumMonthLens = gMonthLens.scanLeft(0)((a,b) => a + b)
  private val gCumLeapMonthLens = gLeapMonthLens.scanLeft(0)((a,b) => a + b)
  private val leapFrac = daysInYear - daysInYear.floor
  private val hlRecip = (1.0 / leapFrac) / 2.0

  val cumLocalYearLens = (0 to yearRange).scanLeft(0)((cy,y) => cy + (if (isLeap(y)) daysInYear.ceil.toInt else daysInYear.floor.toInt))

  def yearDayLen(y: Int) =
    if (isLeap(y)) daysInYear.ceil.toInt else daysInYear.toInt

  def dateFromEra(era: Double): (Int, Double) = {
    val ee = era //- earthDayOffset // earthDayOffset / earthDaylength
    val e = ee.abs
    val y = ((e / earthDaysInYear.ceil).toInt to (e / earthDaysInYear.floor).toInt).filter(yr => cumLocalYearLens(yr) <= e).max
    val doy = ee / earthDaylength + (if (ee >= 0.0) - cumLocalYearLens(y) else cumLocalYearLens(y))

    if (ee < 0)
      (- y - 1, yearDayLen(- y) + doy)
    else
      (y, doy)
  }

  private val tz = java.util.TimeZone.getTimeZone("UTC")

  def era(dt: Date): Double =
    dt.cal.era(dt.y, dt.doy)

  def era(y: Int, doy: Double): Double = {
    val e =
      if (y >= 0) {
        val offset = cumLocalYearLens(y)

        earthStartOfEra + (offset + doy) * earthDaylength - earthDayOffset
      }
      else {
        val offset = cumLocalYearLens(- y)

        earthStartOfEra - (offset - doy) * earthDaylength - earthDayOffset
      }

    // Rounding nasty but necessary
    round(e, 10000000000.0)
  }

  def round(n: Double, mul: Double) =
    math.round(n * mul) / mul

  def conv(dt: Date): Date = 
    Date(dt.era, this)

  def conv(y: Int, doy: Double): Date =
    Date(y, doy, this)

  def conv(e: Double): Date =
    Date(e, this)

  def now(): Date = {
    def nowInDays() = {
      val cal = java.util.Calendar.getInstance(tz)

      cal.getTime().getTime() / 1000.0 / 60.0 / 60.0 / 24.0
    }

    val d2y = EarthCalendar.cumLocalYearLens(EarthCalendar.startOfEra) - EarthCalendar.cumLocalYearLens(1970)
    val start = nowInDays - d2y + EarthCalendar.daysOffset * 2.0 + 1.0

    Date(start, this)
  }

  def gregorianIsLeap(y: Int): Boolean =
    y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)

  def isLeap(y: Int): Boolean = {
    if (y != 0) {
      // Leap years should be symmetric about the start of Era
      val yy = (if (y < 0) -y else y) - 1

      (leapFrac * (yy - 1 + hlRecip)).floor < (leapFrac * (yy + hlRecip)).floor
    }
    else // Year zero of Era may or may not be a Leap year
      yearZeroIsLeap
  }

  def getMonthLens(y: Int): List[Int] =
    if (gregorianIsLeap(y)) gLeapMonthLens else gMonthLens

  def getCumMonthLens(y: Int): List[Int] =
    if (gregorianIsLeap(y)) gCumLeapMonthLens else gCumMonthLens

  def dateToLong(dt: Date): String = {
    assert(dt.doy <= daysInYear.toInt + (if (isLeap(dt.y)) 1 else 0))

    shortToLong(dt.y, dt.doy)
  }

  def shortToLong(y: Int, doy: Double): String = {
    assert(doy <= daysInYear.toInt + (if (isLeap(y)) 1 else 0))
    val yy = if (y < 0) f"$y%05d" else f"$y%04d"
    f"$yy-${(if (doy < daysInYear.toInt - 1) doy / localDaysInMonth else monthsInYear - 1).toInt}%02.0f-${if (doy / localDaysInMonth < monthsInYear) doy % localDaysInMonth else localDaysInMonth + doy % localDaysInMonth}%02.0f"
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
    val doy = m * localDaysInMonth + d

    assert(doy <= daysInYear.toInt + (if (isLeap(y)) 1 else 0))

    Date(y, doy)
  }

  def longToShort(y: Int, m: Int, d: Int): String = {
    val doy = m * localDaysInMonth + d

    assert(doy <= daysInYear.toInt + (if (isLeap(y)) 1 else 0))

    if (y < 0) // Adjust precision for negative years
      f"$y%05d+${doy}%03d"
    else
      f"$y%04d+${doy}%03d"
  }

  def longToShort(dt: String): String = {
    val flds = dt.split("-")

    longToShort(flds(0).toInt, flds(1).toInt, flds(2).toInt)
  }

  def gregorianToAsgardian(y: Int, m: Int, d: Int): Date = {
    if ("Earth" != locale) {
      val dt = EarthCalendar.gregorianToAsgardian(y, m, d)

      Date(dt.era, this)
    }
    else {
      assert(y != 0)  // NOTE : Is this right? Must be historically!
      assert(m >= 1 && m <= 12)
      assert(d >= 1 && d <= gMonthLens.max)

      val yy = if (y < 0) y + 1 else y
      val cm = getCumMonthLens(yy)(m - 1) //- daysOffset
      //val dy = cm + d + daysOffset - 1
      val dy = cm + d + daysOffset - 1
      // NOTE: It is not clear when the leap year was at the start of era???
      val yl = (if (gregorianIsLeap(yy)) daysInYear.ceil.toInt else daysInYear.toInt) - 1
      
      if (dy > yl)
        Date(yy - startOfEra + 1, dy % yl - 1)
      else
        Date(yy - startOfEra, dy)
    }
  }

  def gregorianToAsgardian(dt: String): Date = {
    val flds = dt.split(":")

    gregorianToAsgardian(flds(0).toInt, flds(1).toInt, flds(2).toInt)
  }

  def asgardianToGregorian(y: Int, doy: Double): String = {
    if ("Earth" != locale) {
      EarthCalendar.asgardianToGregorian(EarthCalendar.conv(era(y, doy)))
    }
    else {
      assert(doy <= daysInYear.toInt)

      val yd = doy + 1
      val soe = startOfEra - 1
      val yy = (if (yd > earthDayOffset) y + 1 else y) + soe
      val bce = if (y < -soe) 0 else 1
      val isLeap = gregorianIsLeap(y + bce + soe)
      val yLen = if (isLeap) daysInYear.ceil.toInt else daysInYear.toInt
      val yyd = (if (yd >= earthDayOffset) yd else yd + yLen) - earthDayOffset
      val mtd = getCumMonthLens(y + bce + soe).takeWhile(_ < yyd)
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
    dt.cal.asgardianToGregorian(dt.y, dt.doy)
  }

  def asgardianToGregorian(e: Double): String = {
    asgardianToGregorian(Date(e))
  }

  def asgardianToGregorianDatetime(y: Int, yd: Double): String = {
    asgardianToGregorian(y, yd) + " " + toTime(yd)
  }

  def asgardianToGregorianDatetime(dt: Date): String = {
    asgardianToGregorian(dt.y, dt.doy) + " " + toTime(dt.doy)
  }

  def toTime(doy: Double): String = {
    val t = secondsInDay * (doy - doy.floor)
    val h = t / localMinutesInHour / localSecondsInMinute
    val m = h % 1.0 * localMinutesInHour
    val s = m % 1.0 * localSecondsInMinute + 0.0000000001

    f"${h.floor}%02.0f:${m.floor}%02.0f:${s.floor}%02.0f"
  }

  def toTimeNano(doy: Double): String = {
    val t = secondsInDay * (doy - doy.floor)
    val h = t / localMinutesInHour / localSecondsInMinute
    val m = h % 1.0 * localMinutesInHour
    val s = m % 1.0 * localSecondsInMinute + 0.0000000001
    val r = s % 1.0 * 10000

    f"${h.floor}%02.0f:${m.floor}%02.0f:${s.floor}%02.0f.$r%06.0f"
  }
}

object EarthCalendar extends Calendar
// Length of Mars second : 1.027491251701389 
// Length of Martian solar year : 668.5991
object MarsCalendar extends Calendar("Mars", daysInYear=668.5991, hoursInDay=24.6597900408, daysOffset=33)
//object Mars extends Calendar("Mars", daysInYear=668.5991, hoursInDay=24.6597900408, daysOffset=33)
//object Mars extends Calendar("Mars", daysInYear=1000.0, hoursInDay=12, daysOffset=20)
