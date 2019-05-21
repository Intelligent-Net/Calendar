package space.asgardia.calendar

case class Date(y: Int, doy: Int, cal: Calendar = EarthCalendar.cal) {
  def this(dt: String, c: Calendar) =
    this(dt.substring(0, dt.indexOf('+')).toInt, dt.substring(dt.indexOf('+')+1).toInt, c)
  def this(dt: String) =
    this(dt.substring(0, dt.indexOf('+')).toInt, dt.substring(dt.indexOf('+')+1).toInt, EarthCalendar.cal)
  def this(y:Int, m: Int, d: Int, c: Calendar) =
    this(y, m * c.daysInMonth + d, c)
  def this(y:Int, m: Int, d: Int) =
    this(y, m * EarthCalendar.cal.daysInMonth + d, EarthCalendar.cal)
  def this(c: Calendar) =
    this(c.now().toString, c)
  def this() =
    this(EarthCalendar.cal.now().toString, EarthCalendar.cal)

  override def toString(): String = 
    if (y < 0)
      f"$y%05d+$doy%03d"
    else
      f"$y%04d+$doy%03d"

  def toLongString(): String = {
    val yy = y
    val mm = if (doy < cal.grDaysInYear.toInt - 1) doy / cal.daysInMonth else cal.monthsInYear - 1
    val dd = if (doy / cal.daysInMonth < cal.monthsInYear.toInt) doy % cal.daysInMonth else cal.daysInMonth + doy % cal.daysInMonth

    if (yy < 0)
      f"$yy%05d-$mm%02d-$dd%02d"
    else
      f"$yy%04d-$mm%02d-$dd%02d"
  }

  private def toLong1BaseInternal(off: Int): String = {
    val yy = if (y > 0 || (off == 1 && y == 0)) y + off else y
    val mm = (if (doy < cal.grDaysInYear.toInt - 1) doy / cal.daysInMonth else cal.monthsInYear - 1) + off
    val dd = (if (doy / cal.daysInMonth < cal.monthsInYear.toInt) doy % cal.daysInMonth else cal.daysInMonth + doy % cal.daysInMonth) + off

    if (yy < 0)
      f"$yy%05d-$mm%02d-$dd%02d"
    else
      f"$yy%04d-$mm%02d-$dd%02d"
  }

  def toLong1Based(): String = {
    toLong1BaseInternal(1)
  }

  def fromLong1Based(): String = {
    toLong1BaseInternal(-1)
  }

  private def to1BasedInternal(off: Int): String = {
    val yy = if (y > 0 || (off == 1 && y == 0)) y + off else y
    val doy1 = doy + off

    if (y < 0)
      f"$yy%05d+$doy1%03d"
    else
      f"$yy%04d+$doy1%03d"
  }

  def to1Based(): String = {
    to1BasedInternal(1)
  }

  def from1Based(): String = {
    to1BasedInternal(-1)
  }

  def locale(): String = 
    cal.locale

  def diff(dt: Date): Double = {
    val hrs1 = ((cal.grStartOfEra + y) * cal.grDaysInYear + doy + cal.grDayOffset) * cal.grHoursInDay
    val hrs2 = ((dt.cal.grStartOfEra + dt.y) * dt.cal.grDaysInYear + dt.doy + dt.cal.grDayOffset) * dt.cal.grHoursInDay

    (hrs1 - hrs2) / cal.grHoursInDay // Note: normalised to lhs days
  }

  def before(dt: Date): Boolean = {
    val df = diff(dt)

    df < 0
  }

  def after(dt: Date): Boolean = {
    val df = diff(dt)

    df > 0
  }

  def toEra(): Double = {
    val days = ((y * cal.grDaysInYear + doy) * cal.grHoursInDay) / EarthCalendar.cal.grHoursInDay + cal.grDayOffset

    days
  }

  def fromEra(e: Double): Date = {
    cal.fromEra(e)
  }

  def decDays(i: Int): Date = {
    if (doy >= i) {
      Date(y, doy - i, cal)
    }
    else {
      val diy = if (cal.isLeap(y - 1)) cal.grDaysInYear.ceil.toInt else cal.grDaysInYear.toInt

      Date(y - 1, doy, cal).decDays(i - diy)
    }
  }

  def incDays(i: Int): Date = {
    val diy = if (cal.isLeap(y)) cal.grDaysInYear.ceil.toInt else cal.grDaysInYear.toInt

    if (doy + i < diy) {
      Date(y, doy + i, cal)
    }
    else {
      Date(y + 1, doy, cal).incDays(i - diy)
    }
  }

  def decYears(i: Int): Date = {
    var yy = y - i
    val diy = if (cal.isLeap(y)) cal.grDaysInYear.ceil.toInt else cal.grDaysInYear.toInt
    var d = if (doy > diy - 1) doy else doy - 1

    Date(yy, d, cal)
  }

  def incYears(i: Int): Date = {
    decYears(-i)
  }

  def asgardianToGregorian(): String = 
    cal.asgardianToGregorian(this)

  def gregorianToAsgardian(dt: String): Date =
    cal.gregorianToAsgardian(dt)
}

object Date {
  def apply(dt: String, c: Calendar) =
    new Date(dt.substring(0, 4).toInt, dt.substring(5).toInt, c)
  def apply(dt: String) =
    new Date(dt.substring(0, 4).toInt, dt.substring(5).toInt, EarthCalendar.cal)
  def apply(y:Int, m: Int, d: Int, c: Calendar) =
    new Date(y, m * c.daysInMonth + d, c)
  def apply(y:Int, m: Int, d: Int) =
    new Date(y, m * EarthCalendar.cal.daysInMonth + d, EarthCalendar.cal)
  def apply() =
    new Date()
  def apply(c: Calendar) =
    new Date(c)
  def now(): Date =
    new Date()
  def now(c: Calendar): Date =
    new Date(c)
}

object TestDate {
  def main(args: Array[String]): Unit = {
    val dt = Date(2, 100)
    val dt2 = Date(6, 100)
    val dt3 = Date(2, 100, MarsCalendar.cal)
    val dt4 = dt.fromEra(dt3.toEra)
    val dt5 = dt3.fromEra(dt4.toEra)

    println(dt + " -> " + dt2)
    println(dt3)

    println(dt.diff(dt2))
    println(dt.diff(dt))
    println(new Date)
    println(Date.now)
    //println(dt.toLocal(dt2))
    //println(dt.toLocal(dt3))
    //println(dt3.toLocal(Date(4,55)))
    println(dt.toEra + " : " + dt.fromEra(dt.toEra))
    println(dt2.toEra + " : " + dt2.fromEra(dt2.toEra))
    println(dt3.toEra + " : " + dt3.fromEra(dt3.toEra))
    println(dt3.toEra + " : " + dt.fromEra(dt3.toEra))
    println(dt.toEra + " : " + dt3.fromEra(dt.toEra))
    val edt3 = dt3.fromEra(dt.toEra)
    println(edt3.toEra + " : " + dt.fromEra(edt3.toEra))
    println("Earth: " + dt.toEra + ", Mars: " + dt3.toEra)
    println(dt4 + " -> " + dt5)
    println(Date())
    println(Date(MarsCalendar.cal))
  }
}
