object lectureNotes {



	val x = new Rational(1,3)                 //> x  : Rational = 1/3
	val y = new Rational(5,7)                 //> y  : Rational = 5/7
	val z = new Rational(3,2)                 //> z  : Rational = 3/2
	x.less(y)                                 //> res0: Boolean = true
	x.max(y)                                  //> res1: Rational = 5/7
	x - y                                     //> res2: Rational = 8/-21
	x + y                                     //> res3: Rational = 22/21
}

class Rational(x: Int, y:Int) {
  require (y != 0, "denominator must be nonzero")
  private def gcd(a: Int, b: Int):Int = if (b==0) a else gcd(b, a%b)
  //def numer = x / gcd(x, y)
  //def denom = y / gcd(x, y)
  def numer = x
  def denom = y
  def less(that: Rational) = this.numer * that.denom < that.numer * this.denom
  def max(that: Rational) = if (this.less(that)) that else this
  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
			denom * that.denom)
  def unary_- : Rational = new Rational(-numer, denom)
  def - (that: Rational) = this + -that
  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }
  
  
}