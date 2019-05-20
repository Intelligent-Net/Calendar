package space.asgardia.calendar

case class Date(y: Int, doy: Int, cal: Calendar = Calendar.cal) {
  def this(dt: String, c: Calendar) =
    this(dt.substring(0, 4).toInt, dt.substring(5).toInt, c)
  def this(dt: String) =
    this(dt.substring(0, 4).toInt, dt.substring(5).toInt, Calendar.cal)
  def this(y:Int, m: Int, d: Int, c: Calendar) =
    this(y, m * c.daysInMonth + d, c)
  def this(y:Int, m: Int, d: Int) =
    this(y, m * Calendar.cal.daysInMonth + d, Calendar.cal)
  def this(c: Calendar) =
    this(c.now, c)
  def this() =
    this(Calendar.cal.now, Calendar.cal)

  override def toString(): String = 
    if (y < 0)
      f"$y%05d+$doy%03d"
    else
      f"$y%04d+$doy%03d"

  def toLongString(): String = {
    f"$y%04d-${if (doy < 364) doy / cal.daysInMonth else 12}%02d-${if (doy / cal.daysInMonth < 13) doy % cal.daysInMonth else cal.daysInMonth + doy % cal.daysInMonth}%02d"
  }

  def locale(): String = 
    cal.locale

  def diff(dt: Date): Double = {
    val hrs1 = ((cal.startOfEra + y) * cal.daysInYear + doy + cal.offset) * cal.hoursInDay
    val hrs2 = ((dt.cal.startOfEra + dt.y) * dt.cal.daysInYear + dt.doy + dt.cal.offset) * dt.cal.hoursInDay

    (hrs1 - hrs2) / cal.hoursInDay // Note: normalised to lhs days
  }

  def before(dt: Date): Boolean = {
    val df = diff(dt)

    df < 0
  }

  def after(dt: Date): Boolean = {
    val df = diff(dt)

    df > 0
  }
}

object Date {
  def now(): Date =
    new Date()
}

object TestDate {
  def main(args: Array[String]): Unit = {
    val dt = Date(2, 100)
    val dt2 = Date(6, 100)

    println(dt + " -> " + dt2)

    println(dt.diff(dt2))
    println(dt.diff(dt))
    println(new Date)
    println(Date.now)
  }
}
