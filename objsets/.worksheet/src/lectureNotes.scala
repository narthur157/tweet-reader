object lectureNotes {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(51); 



	val x = new Rational(1,3);System.out.println("""x  : Rational = """ + $show(x ));$skip(27); 
	val y = new Rational(5,7);System.out.println("""y  : Rational = """ + $show(y ));$skip(27); 
	val z = new Rational(3,2);System.out.println("""z  : Rational = """ + $show(z ));$skip(11); val res$0 = 
	x.less(y);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(10); val res$1 = 
	x.max(y);System.out.println("""res1: Rational = """ + $show(res$1));$skip(7); val res$2 = 
	x - y;System.out.println("""res2: Rational = """ + $show(res$2));$skip(7); val res$3 = 
	x + y;System.out.println("""res3: Rational = """ + $show(res$3))}
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