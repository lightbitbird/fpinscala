package fpinscala.chapters.chapter3

/**
  * Created by seung on 2017/08/19.
  */
object DataConstructor {

  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(2, Cons(y, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(x)

    val ex1 = List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 4)
    println(ex1)
    val ex2 = List.dropWhile2(List(1, 2, 3, 4, 5))((x: Int) => x < 4)
    println(ex2)

    println(List.append(List(1, 2, 3), List(5, 7, 8)))

    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

  }

}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ls: List[A]): List[A] = {
    ls match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }
  }

  def setHead[A](ls: List[A], h: A): List[A] = ls match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](ls: List[A], n: Int): List[A] = {
    if (n <= 0) ls
    else ls match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  /**
    * remove elements until each one match f
    */
  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] =
    ls match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => ls
    }

  /**
    * remove elements until each one match f
    * 2 parameters into group
    */
  def dropWhile2[A](ls: List[A])(f: A => Boolean): List[A] =
    ls match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => ls
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](ls: List[A]): List[A] = {
    ls match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](ls: List[A], z: B)(f: (A, B) => B): B =
    ls match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def length[A](ls: List[A]): Int = foldRight(ls, 0)((_, z) => z + 1)

  @annotation.tailrec
  def foldLeft[A, B](ls: List[A], z: B)(f: (B, A) => B): B =
    ls match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)((z, h) => z + h)

  //  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)((z, h) => z * h)

  //  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def reverse[A](ls: List[A]): List[A] = foldLeft(ls, List[A]())((z, h) => Cons(h, z))

  def foldRightViaFoldLeft[A, B](ls: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(ls), z)((b, a) => f(a, b))

  /**
    * Another foldRight via foldLeft using curry instead of reverse
    */
  def foldRightViaFoldLeft_1[A, B](ls: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(ls, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a1, a2)((b, a) => Cons(a, b))

  def concat[A](ls: List[List[A]]): List[A] = foldRight(ls, Nil: List[A])(append)

  def add1(ls: List[Int]): List[Int] = foldLeft(ls, Nil: List[Int])((b, a) => Cons(a + 1, b))

  def doubleToString[A](ls: List[A]): List[String] =
    foldLeft(ls, Nil: List[String])((b, a) => Cons(a.toString, b))

  def map[A, B](ls: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(ls, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](ls: List[A])(f: A => Boolean): List[A] =
    foldLeft(ls, Nil: List[A])((t, h) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] =
  //  foldLeft(ls, Nil:List[B])((b, a) => f(a))(append)
    concat(map(ls)(f))

  def filterViaFlatMap[A](ls: List[A])(f: A => Boolean): List[A] =
    flatMap(ls)(a => if (f(a)) List(a) else Nil)

  def addPairWise(ls1: List[Int], ls2: List[Int]): List[Int] =
    (ls1, ls2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
    }

  def zipWith[A, B, C](ls1: List[A], ls2: List[B])(f: (A, B) => C): List[C] =
    (ls1, ls2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  @annotation.tailrec
  def startWith[A](ls: List[A], prefix: List[A]): Boolean = (ls, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startWith(t, t2)
    case _ => false

  }

  @annotation.tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startWith(sup, sub) => true
    case Cons(_, t) => hasSubSequence(t, sub)
  }

}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)((l, r) => l max r)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => l max r)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
