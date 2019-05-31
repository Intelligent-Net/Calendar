package space.asgardia.calendar

case class Date(y: Int, doy: Double, cal: Calendar = EarthCalendar.cal) {
  //val era: Double = cal.toEra(y, doy)
  def this(dt: String, c: Calendar) =
    this(dt.substring(0, dt.indexOf('+')).toInt, dt.substring(dt.indexOf('+')+1).toInt, c)
  def this(dt: String) =
    this(dt, EarthCalendar.cal)
  def this(y:Int, m: Int, d: Int, c: Calendar) =
    this(y, m * c.daysInMonth + d, c)
  def this(y:Int, m: Int, d: Int) =
    this(y, m, d, EarthCalendar.cal)
  def this(dt: Date, c: Calendar) =
    this(dt.y, dt.doy, c)
  def this(dt: Date) =
    this(dt.y, dt.doy, EarthCalendar.cal)
  def this(c: Calendar) =
    this(c.now(), c)
  def this() =
    this(EarthCalendar.cal)

  override def toString(): String = 
    if (y < 0)
      f"$y%05d+${doy.toInt}%03d"
    else
      f"$y%04d+${doy.toInt}%03d"
      //f"$y%05d+${doy}%03.4f"

  def toLongString(): String = {
    f"$y%04.0f-${if (doy < cal.daysInYear.toInt - 1) doy / cal.daysInMonth else cal.monthsInYear - 1}%02.0f-${if (doy / cal.daysInMonth < cal.monthsInYear.toInt) doy % cal.daysInMonth else cal.daysInMonth + doy % cal.daysInMonth}%02.0f"
  }

  def locale(): String = 
    cal.locale

  def diff(dt: Date): Double = {
    val hrs1 = ((cal.startOfEra + y) * cal.daysInYear + doy + cal.dayOffset) * cal.hoursInDay
    val hrs2 = ((dt.cal.startOfEra + dt.y) * dt.cal.daysInYear + dt.doy + dt.cal.dayOffset) * dt.cal.hoursInDay

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

  def epoch(): Double = {
    cal.epoch(this)
  }

  def toEra(): Double = {
    cal.toEra(this)
  }

  def fromEra(e: Double): Date = {
    cal.fromEra(e)
  }

  def convert(dt: Date) = {
    cal.convert(dt)
  }
}

object Date {
  def apply(dt: String, c: Calendar) =
    new Date(dt, c)
  def apply(dt: String) =
    new Date(dt, EarthCalendar.cal)
  def apply(y:Int, m: Int, d: Int, c: Calendar) =
    new Date(y, m * c.daysInMonth + d, c)
  def apply(y:Int, m: Int, d: Int) =
    new Date(y, m * EarthCalendar.cal.daysInMonth + d, EarthCalendar.cal)
  def apply() =
    new Date()
  def apply(c: Calendar) =
    new Date(c)
  def apply(dt: Date, c: Calendar) =
    new Date(dt.y, dt.doy, c)
  def apply(dt: Date) =
    dt
  def now(): Date =
    EarthCalendar.cal.now()
  def now(c: Calendar): Date =
    c.now()
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
