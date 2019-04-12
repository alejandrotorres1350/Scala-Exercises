package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /**
    *Exercise 25
    *Size function for a tree
    */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }
  /**
    *Exercise 26
    *Maximum leaf in a tree
    */
  def maximum(tree:Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => maximum(left).max(maximum(right))
  }
  /**
    *Exercise 27
    *Depth function for a tree
    */
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }
  /**
    *Exercise 28
    *Map function for a tree
    */
  def map[A,B](tree: Tree[A])(f:A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
  /**
    *Exercise 29
    *Generalize size maximum depth and map with fold function
    */
  def fold[A,B](tree: Tree[A])(f: A => B)(b: (B,B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(left, right) => b(fold(left)(f)(b), fold(right)(f)(b))
  }

  def fold_size[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((left,right) => left + right + 1 )

  def fold_maximum[A](tree: Tree[Int]): Int =
    fold(tree)(x => x)((left,right) => left.max(right))

  def fold_depth[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((left,right) => left.max(right) + 1)

  def fold_map[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])((left, right) => Branch(left,right))

  def main(args: Array[String]): Unit = {
    //def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    def t2 = Branch(Branch(Branch(Leaf(5),Leaf(10)),Branch(Leaf(2),Leaf(4))),Branch(Branch(Leaf(30),Leaf(22)),Branch(Leaf(15),Leaf(1))))
    /*println(size(t2))
    println(maximum(t2))
    println(depth(t2))
    println(map(t2)(_ * 2))*/

    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    println(depth(t))
    println(fold_size(t)) //shouldBe 5
    println(fold_maximum(t)) //shouldBe 3
    println(fold_depth(t)) //shouldBe 2
    print(fold_map(t)(_ % 2 == 0))

  }

}

