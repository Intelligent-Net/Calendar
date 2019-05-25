package space.asgardia.calendar

case class Date(era: Double, cal: Calendar = EarthCalendar.cal) {
  def this(eEra: Double, c: Calendar, ge: Boolean) = {
    this(if (ge) ((eEra - c.grDayOffset) * EarthCalendar.cal.grHoursInDay) / c.grHoursInDay else eEra, c)
  }
  def this(y: Double, doy: Double, c: Calendar) = {
    this(y * c.grDaysInYear + doy, c)
  }
  def this(y: Double, doy: Double) =
    this(y, doy, EarthCalendar.cal)
  def this(dt: String, c: Calendar) = {
    this(dt.substring(0, dt.indexOf('+')).toInt * c.grDaysInYear + dt.substring(dt.indexOf('+')+1).toInt, c)
  }
  def this(dt: String) =
    this(dt, EarthCalendar.cal)
  def this(y:Int, m: Int, d: Int, c: Calendar) =
    this(y * c.grDaysInYear + m * c.daysInMonth + d, c)
  def this(y:Int, m: Int, d: Int) =
    this(y, m, d, EarthCalendar.cal)
  def this(c: Calendar) =
    this(c.now().era, c)
  def this() =
    this(EarthCalendar.cal.now().era, EarthCalendar.cal)

  def yDoy(): (Double, Double) = {
    cal.yDoy(era)
  }

  override def toString(): String = {
    val (y,doy) = yDoy
    //println(yDoy + " : " + era)

    if (y < 0)
      f"${y.toInt}%05d+${doy.round.toInt}%03d"
    else
      f"${y.toInt}%04d+${doy.round.toInt}%03d"
  }

  def toLongString(): String = {
    val (yy,doy) = yDoy

    val mm = if (doy < cal.grDaysInYear.toInt - 1) doy / cal.daysInMonth else cal.monthsInYear - 1
    val dd = if (doy / cal.daysInMonth < cal.monthsInYear.toInt) doy % cal.daysInMonth else cal.daysInMonth + doy % cal.daysInMonth

    if (yy < 0)
      f"${yy.toInt}%05d-${mm.toInt}%02d-${dd.toInt}%02d"
    else
      f"${yy.toInt}%04d-${mm.toInt}%02d-${dd.toInt}%02d"
  }

  private def toLong1BaseInternal(off: Int): String = {
    val (y,doy) = yDoy
    val yy = if (y > 0 || (off == 1 && y == 0)) y + off else y
    val mm = (if (doy < cal.grDaysInYear.toInt - 1) doy / cal.daysInMonth else cal.monthsInYear - 1) + off
    val dd = (if (doy / cal.daysInMonth < cal.monthsInYear.toInt) doy % cal.daysInMonth else cal.daysInMonth + doy % cal.daysInMonth) + off

    if (yy < 0)
      f"${yy.toInt}%05d-${mm.toInt}%02d-${dd.toInt}%02d"
    else
      f"${yy.toInt}%04d-${mm.toInt}%02d-${dd.toInt}%02d"
  }

  def toLong1Based(): String = {
    toLong1BaseInternal(1)
  }

  def fromLong1Based(): String = {
    toLong1BaseInternal(-1)
  }

  private def to1BasedInternal(off: Int): String = {
    val (y,doy) = yDoy

    val yy = if (y > 0 || (off == 1 && y == 0)) y + off else y
    val doy1 = doy + off

    if (y < 0)
      f"${yy.toInt}%05d+${doy1.toInt}%03d"
    else
      f"${yy.toInt}%04d+${doy1.toInt}%03d"
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
    val (y,doy) = yDoy
    val (dtY,dtDoy) = dt.yDoy
    val hrs1 = ((cal.grStartOfEra + y) * cal.grDaysInYear + doy + cal.grDayOffset) * cal.grHoursInDay
    val hrs2 = ((dt.cal.grStartOfEra + dtY) * dt.cal.grDaysInYear + dtDoy + dt.cal.grDayOffset) * dt.cal.grHoursInDay

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

  def equal(dt: Date): Boolean = {
    val df = diff(dt)

    df.round.toInt == 0
  }

  // This is Earth normalised Era
  def toEra(): Double = {
    (era * cal.grHoursInDay) / EarthCalendar.cal.grHoursInDay + cal.grDayOffset
  }

  // This is Calendar normalised Era
  def fromEra(e: Double, c: Calendar = EarthCalendar.cal): Date = {
    val calEra = ((e - cal.grDayOffset) * EarthCalendar.cal.grHoursInDay) / cal.grHoursInDay + EarthCalendar.cal.grDayOffset

    Date(calEra, c, true)
  }

  def convert(c: Calendar): Date = {
    c.fromEra(toEra)
  }

  def convertFrom(dt: Date): Date =
    fromEra(dt.toEra)

  def convertTo(dt: Date): Date =
    dt.fromEra(toEra)

  def dec(): Date =
    decDays(1)

  def inc(): Date =
    incDays(1)

  def decDays(i: Int): Date = {
    Date(era - i, cal)
  }

  def incDays(i: Int): Date = {
    Date(era + i, cal)
  }

  def decYears(i: Int): Date = {
    decYears(-i)
  }

  def incYears(i: Int): Date = {
    Date(era + i * cal.grDaysInYear, cal)
  }

  def asgardianToGregorian(): String = 
    cal.asgardianToGregorian(this)

  def gregorianToAsgardian(dt: String): Date =
    cal.gregorianToAsgardian(dt)
}

object Date {
  def apply(era: Double, c: Calendar, ae: Boolean) =
    new Date(era, c, ae)
  def apply(era: Double, c: Calendar) =
    new Date(era, c)
  def apply(era: Double, ae: Boolean) =
    new Date(era, EarthCalendar.cal, ae)
  def apply(era: Double) =
    new Date(era, EarthCalendar.cal, false)
  def apply(dt: String, c: Calendar) =
    new Date(dt, c)
  def apply(dt: String) =
    new Date(dt, EarthCalendar.cal)
  def apply(y: Double, doy: Double, c: Calendar) =
    new Date(y, doy, c)
  def apply(y: Double, doy: Double) =
    new Date(y, doy, EarthCalendar.cal)
  def apply(y: Int, m: Int, d: Int, c: Calendar) =
    new Date(y, m, d, c)
  def apply(y: Int, m: Int, d: Int) =
    new Date(y, m, d, EarthCalendar.cal)
  def apply(c: Calendar) =
    new Date(c)
  def apply() =
    new Date(EarthCalendar.cal)

  def now(c: Calendar = EarthCalendar.cal): Date =
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
    println(Date)
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
