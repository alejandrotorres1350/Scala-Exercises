package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 1
    * What will be the result of the following expression
    * R: 3
    */
  val x = List(1,2,3,4,5) match { // the answer is 3
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }


  /**
    * Exercise 2
    * Implement function tail, for removing the first element of the list
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,x) => x

  }

  /**
    * Exercise 3
    * Implement setHead
    */
  def setHead[A](l: List[A], h: A): List[A] = l match{
    case Nil => Cons(h, Nil)
    case Cons(_,x) => Cons(h,x)
  }

  /**
    * Exercise 4
    * Generalize tail by droping the n first elements from a list
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match{
        case Nil => sys.error("Error, lista vacia")
        case Cons(_,x) => drop(x, n - 1 )
    }
  }

  /**
    * Exercise 5
    * Drpowhile
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h,t) => if (f(h)) dropWhile(t, f) else l
   }

  /**
    * Exercise 6
    * Init: Remove the last element from the list
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Error, lista vacia")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  /**
    * Exercise 7
    * Product using foldRight
    */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  /**
    * Exercise 9
    * Lenght using foldRight
    */
  def length[A](l: List[A]): Int = foldRight(l,0)((_,acc) => acc + 1)

  /**
    * Exercise 10
    * foldLeft
    */
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match{
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
  }

  /**
    * Exercise 11
    * Product, sum and lenght using foldLeft
    */
  def sum3(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = List.foldLeft( l, 0 )((acc,_) => acc + 1)

  /**
    * Exercise 12
    * Reverse of a list
    */
  def reverse[A](l: List[A]): List[A] = foldLeft(l,List[A]())( ( a, b) => Cons( b, a))



  /**
    * Exercise 13 hard
    * FoldRight and foldLeft in terms of the other one
    */
  def foldRightLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z )( ( b, a) => f( a, b) )
  def foldLeftRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b: B) => b)(( a, g) => b => g(f(b,a)))(z)

  /*def sum4(ns: List[Int]) = foldLeftRight(ns, 0)((x,y) => x + y)

  def product4(ns: List[Double]) = foldLeftRight(ns, 1.0)(_ * _)

  def sum5(ns: List[Int]) =
    foldRightLeft(ns, 0)((x,y) => x + y)

  def product5(ns: List[Double]) =
    foldRightLeft(ns, 1.0)(_ * _)*/

  /**
    * Exercise 14
    * Append in terms of FoldRight
    */
  def appendFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))
  /**
    * Exercise 15
    * Concat function
    */
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(appendFoldRight)
  /**
    * Exercise 16
    * One_increment
    */
  def one_increment(l:List[Int]): List[Int] = foldRight(l, Nil:List[Int])( (a,b) => Cons(a+1,b))

  /**
    * Exercise 17
    * Double to string list
    */
  def double_string_list(l:List[Double]): List[String] = foldRight(l, Nil:List[String])( (a,b) => Cons(a.toString,b))

  /**
    * Exercise 18
    * funtion map maintainig the structure
    */
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((a,b) => Cons(f(a),b))
  /**
    * Exercise 19
    * filter and the odd numbers
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])( (a,b) => if (f(a)) Cons(a,b) else b)
  /**
    * Exercise 20
    * flat map with concat
    */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))
  /**
    * Exercise 20
    * flat map with foldRighy
    */
  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as,Nil:List[B])((a,b)=>append(f(a),b))
  /**
    * Exercise 21
    * filter with flatmap
    */
  def filter2[A,B](as: List[A])(f: A => Boolean): List[A] =
    flatMap2(as)(x =>  if(f(x)) List(x) else Nil)
  /**
    * Exercise 22
    * Adding two list into one
    */
  def addTwoList(list1:List[Int], list2:List[Int]): List[Int] = (list1, list2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(x,tx),Cons(y,ty)) => Cons( x + y , addTwoList(tx,ty))
  }
  /**
    * Exercise 23
    * Generalizing previous funtion, zipWith
    */
  def zipWith[A](list1: List[A], list2: List[A])(f: (A,A)=> A): List[A] = (list1, list2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(x,tx),Cons(y,ty)) => Cons( f(x,y) , zipWith(tx,ty)(f))
  }
  /**
    * Exercise 24 hard
    * Check if a list contains a given subsequence
    */

  @annotation.tailrec
  def startsWith[A](as: List[A], sub: List[A]): Boolean = (as, sub) match {
    case (_, Nil) => true
    case (Cons(x, tx), Cons(y, ty)) if x == y => startsWith(tx, ty)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = as match {
    case Nil => sub == Nil
    case _ if startsWith(as, sub) => true
    case Cons(x, tx) => hasSubsequence(tx, sub)
  }

  def main(args: Array[String]): Unit = {
    /*println(appendFoldRight(List(1,2,3,4,5), List(10,11,12,13,14)))

    println(append(List(1, 2, 3), List(1, 2)))

    println(append(List(1, 2, 3), Nil))

    println(append(Nil, List(1, 2)))

    println(append(Nil, Nil))

    println(concat(List(List(1,2,3), List(7,8,9))))

    println(one_increment(List(1,2,3,4)))

    println(double_string_list(List(1.0, 2.0, 3.0)))

    println(map(List(5,10,15,20,25))(x => x*5))

    println(filter2(List(1,2,3,4,5))(h => h%2==0))

    println(flatMap(List(1,2,3))(i=>List(i,i)))

    println(flatMap2(List(1,2,3))(i=>List(i,i)))

    println(addTwoList(List(1,2,3), List(4,5,6)))

    println(zipWith(List("a", "b", "c"), List("A", "B", "C"))(_ + _))

    println(zipWith(List(1, 2, 3), List(4, 5, 6))(_.toString() + _.toString()))

    println(hasSubsequence(List(1,2,3,4,5,6,7,8,9),List(10)))

    println(setHead(List(1, 2, 3), 3))

    println(drop(List(1, 2, 3), 1))*/

    /*println(dropWhile(List(1, 2, 3), (x: Int) => x < 2))
    println(dropWhile(List(1, 2, 3), (x: Int) => x > 2))
    println(dropWhile(List(1, 2, 3), (x: Int) => x > 0))
    println(dropWhile(Nil, (x: Int) => x > 0))*/
    println(length(List(1,2,3,4,10)))


  }

}
