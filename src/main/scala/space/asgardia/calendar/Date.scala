package space.asgardia.calendar

case class Date(y: Int, doy: Int, cal: Calendar = EarthCalendar.cal) {
  def this(dt: String, c: Calendar) =
    this(dt.substring(0, 4).toInt, dt.substring(5).toInt, c)
  def this(dt: String) =
    this(dt.substring(0, 4).toInt, dt.substring(5).toInt, EarthCalendar.cal)
  def this(y:Int, m: Int, d: Int, c: Calendar) =
    this(y, m * c.daysInMonth + d, c)
  def this(y:Int, m: Int, d: Int) =
    this(y, m * EarthCalendar.cal.daysInMonth + d, EarthCalendar.cal)
  def this(c: Calendar) =
    this(c.now().toString, c)
  def this() =
    this(EarthCalendar.cal.now().toString)

  override def toString(): String = 
    if (y < 0)
      f"$y%05d+$doy%03d"
    else
      f"$y%04d+$doy%03d"

  def toLongString(): String = {
    f"$y%04d-${if (doy < cal.grDaysInYear.toInt - 1) doy / cal.daysInMonth else cal.monthsInYear - 1}%02d-${if (doy / cal.daysInMonth < cal.monthsInYear.toInt) doy % cal.daysInMonth else cal.daysInMonth + doy % cal.daysInMonth}%02d"
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

    cal.grStartOfEra * EarthCalendar.cal.grDaysInYear + days
  }

  def fromEra(e: Double): Date = {
    val eoff = e - cal.grStartOfEra * EarthCalendar.cal.grDaysInYear
    val days = ((eoff - cal.grDayOffset) * EarthCalendar.cal.grHoursInDay) / cal.grHoursInDay

    val yr = (days / cal.grDaysInYear).round.toInt
    val doy = (days % cal.grDaysInYear).round.toInt

    Date(yr,doy,cal)
  }
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
