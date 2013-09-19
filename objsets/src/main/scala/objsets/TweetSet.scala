package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {

  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

}

abstract class TweetSet {

  /** This method takes a predicate and returns a subset of all the elements
   *  in the original set for which the predicate is true.
   */
  def filter(p: Tweet => Boolean): TweetSet = filter0(p, new Empty)
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet = union0(that, new Empty)
  def union0(that: TweetSet, acc:TweetSet): TweetSet 
  // Hint: the method "remove" on TweetSet will be very useful.
  def ascendingByRetweet: Trending = ascendingByRetweet0(findMin, new EmptyTrending)
  def ascendingByRetweet0(cur:Tweet, accum:Trending): Trending
  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def incl(x: Tweet): TweetSet
  def contains(x: Tweet): Boolean
  def isEmpty: Boolean
  def head: Tweet
  def tail: TweetSet

  /** This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }

  def remove(tw: Tweet): TweetSet

  def findMin0(curr: Tweet): Tweet =
    if (this.isEmpty) curr
    else if (this.head.retweets < curr.retweets) this.tail.findMin0(this.head)
    else this.tail.findMin0(curr)

  def findMin: Tweet =
    this.tail.findMin0(this.head)
  // -------------------------------------------------------------------------
}

class Empty extends TweetSet {
  def ascendingByRetweet0(cur:Tweet, accum:Trending): Trending = accum
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = accu
  def union0(that:TweetSet, acc:TweetSet):TweetSet = if (that.isEmpty) acc else that.union0(this, acc)
  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean = false
  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)
  def isEmpty = true
  def head = throw new Exception("Empty.head")
  def tail = throw new Exception("Empty.tail")
  def remove(tw: Tweet): TweetSet = this
  // -------------------------------------------------------------------------
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
  def ascendingByRetweet0(cur:Tweet, accum:Trending): Trending = {
    if (!this.remove(cur).isEmpty) 
      this.remove(cur).ascendingByRetweet0(this.remove(cur).findMin, accum + cur) 
    else accum
  }
  def union0(that: TweetSet, acc:TweetSet):TweetSet = {
    if (that.isEmpty) {
      if (!acc.contains(this.head)) this.tail.union0(that, acc.incl(head))
      else this.tail.union0(that, acc)
    }
    else if (!acc.contains(this.head)) this.tail.union0(that, acc.incl(head))
    else if (!acc.contains(that.head)) union0(that.tail, acc.incl(that.head))
    else this.tail.union0(that.tail, acc)
  }
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = {
    if (p(elem)) {
      if (!right.isEmpty) right.filter0(p, accu.incl(elem))
      else if (!left.isEmpty) left.filter0(p, accu.incl(elem))
      else accu.incl(elem)
    }
    else {
      if (!right.isEmpty) right.filter0(p, accu)
      else if (!left.isEmpty) left.filter0(p, accu)
      else accu
    }
  }

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def isEmpty = false
  def head = if (left.isEmpty) elem else left.head
  def tail = if (left.isEmpty) right else new NonEmpty(elem, left.tail, right)

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)
  // -------------------------------------------------------------------------
}


/** This class provides a linear sequence of tweets.
 */
abstract class Trending {
  def + (tw: Tweet): Trending
  def head: Tweet
  def tail: Trending
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }
}

class EmptyTrending extends Trending {
  def + (tw: Tweet) = new NonEmptyTrending(tw, new EmptyTrending)
  def head: Tweet = throw new Exception
  def tail: Trending = throw new Exception
  def isEmpty: Boolean = true
  override def toString = "EmptyTrending"
}

class NonEmptyTrending(elem: Tweet, next: Trending) extends Trending {
  /** Appends tw to the end of this sequence.
   */
  def + (tw: Tweet): Trending =
    new NonEmptyTrending(elem, next + tw)
  def head: Tweet = elem
  def tail: Trending = next
  def isEmpty: Boolean = false
  override def toString =
    "NonEmptyTrending(" + elem.retweets + ", " + next + ")"
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val googleTweets: TweetSet = TweetReader.allTweets.filter((x:Tweet) => google.exists((p:String) => x.text.contains(p)))
  
  val appleTweets: TweetSet = TweetReader.allTweets.filter((x:Tweet) => apple.exists((p:String) => x.text.contains(p)))

  // Q: from both sets, what is the tweet with highest #retweets?
  val trending: Trending = googleTweets.union(appleTweets).ascendingByRetweet
}

object Main extends App {
  // Some help printing the results:
  //GoogleVsApple.googleTweets foreach println
  //GoogleVsApple.trending foreach println
  //GoogleVsApple.googleTweets foreach((x:Tweet)=> println(x.retweets))
  //GoogleVsApple.appleTweets foreach((x:Tweet)=> println(x.retweets))
  val set1 = new Empty
  val set2 = set1.incl(new Tweet("a", "a body", 20))
  val set3 = set2.incl(new Tweet("b", "b body", 20))
  val c = new Tweet("c", "c body", 7)
  val d = new Tweet("d", "d body", 9)
  val set4c = set3.incl(c)
  val set4d = set3.incl(d)
  val set5 = set4c.incl(d)
  val set6 = set5.incl(new Tweet("e", "derpalerp", 10))
  //set5.union(set3) foreach println
  //set5.filter((x:Tweet)=> x.text > "b") foreach println
  //set6.filter((x:Tweet)=> x.retweets < 21) foreach println
}
