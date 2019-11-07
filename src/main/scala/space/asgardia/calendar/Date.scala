package space.asgardia.calendar

case class Date(y: Int, doy: Double, cal: Calendar = EarthCalendar) {
  def this(dt: String, c: Calendar) =
    this(dt.substring(0, dt.indexOf('+')).toInt, dt.substring(dt.indexOf('+')+1).toInt, c)
  def this(dt: String) =
    this(dt, EarthCalendar)
  def this(y:Int, m: Int, d: Int, c: Calendar) =
    this(y, m * c.localDaysInMonth + d, c)
  def this(y:Int, m: Int, d: Int) =
    this(y, m, d, EarthCalendar)
  def this(dt: Date, c: Calendar) =
    this(dt.y, dt.doy, c)
  def this(dt: Date) =
    this(dt.y, dt.doy, EarthCalendar)
  def this(p: (Int, Double), c: Calendar) =
    this(p._1, p._2, c)
  def this(p: (Int, Double)) =
    this(p._1, p._2, EarthCalendar)
  def this(c: Calendar) =
    this(c.now(), c)
  def this(e: Double, c: Calendar) =
    this(c.dateFromEra(e), c)
  def this(e: Double) =
    this(e, EarthCalendar)
  def this() =
    this(EarthCalendar)

  override def toString(): String = 
    if (y < 0)
      f"$y%05d+${doy.floor}%03.0f"
    else
      f"$y%04d+${doy.floor}%03.0f"

  def toDate(): String = 
    toString

  def toDatetimeFraction(): String = 
    if (y < 0)
      f"$y%05d+${doy}%03.6f"
    else
      f"$y%04d+${doy}%03.6f"

  def toDatetime(): String = 
    toString + " " + toTime

  def toDatetimeNano(): String = 
    toString + " " + toTimeNano

  def toTime(): String =
    cal.toTime(doy)

  def toTimeNano(): String =
    cal.toTimeNano(doy)

  def toTime(doy: Double): String =
    cal.toTime(doy)

  def toTimeNano(doy: Double): String =
    cal.toTimeNano(doy)

  def toLongDatetime(): String = 
    toLong + " " + toTime

  def toLongDatetimeNano(): String = 
    toLong + " " + toTimeNano

  def toLong(): String = {
    (if (y < 0) f"$y%05.0f" else f"$y%04.0f") +
    (if ((doy < cal.daysInYear.toInt - 1))
       f"-${(doy / cal.localDaysInMonth).floor}%02.0f-${(doy % cal.localDaysInMonth).floor}%02.0f"
     else
       f"-${(doy / cal.localDaysInMonth - 1).floor}%02.0f-${(doy % cal.localDaysInMonth + cal.localDaysInMonth).floor}%02.0f")
  }

  def locale(): String = 
    cal.locale

  def diff(dt: Date): Double = {
    val hrs1 = ((cal.startOfEra + y) * cal.daysInYear + doy + cal.earthDayOffset) * cal.earthHoursInDay
    val hrs2 = ((dt.cal.startOfEra + dt.y) * dt.cal.daysInYear + dt.doy + dt.cal.earthDayOffset) * dt.cal.earthHoursInDay

    (hrs1 - hrs2) / cal.earthHoursInDay // Note: normalised to earth days
  }

  private val precision: Double = 0.00000001

  def ==(dt: Date): Boolean =
    //diff(dt) == 0.0
    diff(dt) < precision

  def <(dt: Date): Boolean =
    //diff(dt) < 0.0
    diff(dt) + precision < 0.0

  def >(dt: Date): Boolean =
    //diff(dt) > 0.0
    diff(dt) - precision < 0.0

  def <=(dt: Date): Boolean =
    diff(dt) + precision <= 0.0

  def >=(dt: Date): Boolean =
    diff(dt) - precision >= 0.0

  def era(): Double =
    cal.era(this)

  def era(c: Calendar): Double =
    c.era(this)

  def conv() =
    cal.conv(this)

  def conv(cal: Calendar) =
    cal.conv(this)

  def conv(dt: Date) =
    cal.conv(dt)

  def isLeap() =
    cal.isLeap(y)

  def asgardianToGregorian(): String =
    cal.asgardianToGregorian(this)
}

object Date {
  def apply(dt: String, c: Calendar) =
    new Date(dt, c)
  def apply(dt: String) =
    new Date(dt, EarthCalendar)
  def apply(y:Int, m: Int, d: Int, c: Calendar) =
    new Date(y, m * c.localDaysInMonth + d, c)
  def apply(y:Int, m: Int, d: Int) =
    new Date(y, m * EarthCalendar.localDaysInMonth + d, EarthCalendar)
  def apply() =
    new Date()
  def apply(c: Calendar) =
    new Date(c)
  def apply(dt: Date, c: Calendar) =
    new Date(dt.y, dt.doy, c)
  def apply(e: Double, c: Calendar) =
    new Date(e, c)
  def apply(e: Double) =
    new Date(e)
  def apply(dt: Date) =
    dt

  def now(): Date =
    EarthCalendar.now()
  def now(c: Calendar): Date =
    c.now()
}

