package fpinscala.chapters.chapter12

import java.util.Date

import fpinscala.chapters.chapter10.{Foldable, Monoid}
import fpinscala.chapters.chapter11.Functor
import fpinscala.chapters.chapter6._
import fpinscala.chapters.chapter6.State._

/**
  * Created by seung on 2017/10/01.
  */
object Applicative12 {

  def main(args: Array[String]): Unit = {
    println("Applicative12 --------")
    Validator.validWebForm("", "hogehoe", "2345") match {
      case Failure(h, t) => println(h); println(t)
      case Success(a) => println(a)
    }

    //    val t = Traverse.apply
    //    t.map(List(1,2,3))(a => println(a))

  }
}

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object Validator {
  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] = {
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case _ => Failure("Birthdate must be in the form yyyy-MM-dd")
    }
  }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
    Applicative.validationApplicative.map3(validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(
      WebForm(_, _, _))

}


trait _Applicative[F[_]] extends Functor[F] {
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  // `map2` is implemented by first currying `f` so we get a function
  // of type `A => B => C`. This is a function that takes `A` and returns
  // another function of type `B => C`. So if we map `f.curried` over an
  // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
  // `F[B]` will give us the desired `F[C]`.
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
  apply(map(fa)(f.curried))(fb)

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  //  def product[A, B](fa: F[A], fb: F[A]): F[(A, B)] =
  //    map2(fa, fb)((a, b) => (a, b))

  //  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
  //    flatMap(p)(a => map(p2)(b => (a,b)))

}

trait Applicative[F[_]] extends Functor[F] {
  // We simply use `map2` to lift a function into `F` so we can apply it
  // to both `fab` and `fa`. The function being lifted here is `_(_)`,
  // which is the same as the lambda notation `(f, x) => f(x)`. That is,
  // It's a function that takes two arguments:
  //   1. A function `f`
  //   2. An argument `x` to that function
  // and it simply applies `f` to `x`.
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_ (_))

  //    map2(fab, fa)((f, x) => f(x))

  // `map2` is implemented by first currying `f` so we get a function
  // of type `A => B => C`. This is a function that takes `A` and returns
  // another function of type `B => C`. So if we map `f.curried` over an
  // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
  // `F[B]` will give us the desired `F[C]`.
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  //  def product[A, B](fa: F[A], fb: F[A]): F[(A, B)] =
  //    map2(fa, fb)((a, b) => (a, b))

  //  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
  //    flatMap(p)(a => map(p2)(b => (a,b)))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
    (i, i2) => (f(i), g(i2))

  /**
    * Exercise 12.8
    * Important
    */
  def product[G[_]](G: Applicative[G]): Applicative[({type lamda[x] = (F[x], G[x])})#lamda] = {
    val self = this
    new Applicative[({type lamda[x] = (F[x], G[x])})#lamda] {
      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))

      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type lamda[x] = F[G[x]]})#lamda] = {
    val self = this
    new Applicative[({type lamda[x] = F[G[x]]})#lamda] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C) =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    (ofa foldLeft unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      def unit[A](a: => A) = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C) =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (e@Failure(_, _), _) => e
          case (_, e@Failure(_, _)) => e
        }

      override def map3[A, B, C, D](fa: Validation[E, A], fb: Validation[E, B],
                                    fc: Validation[E, C])(f: (A, B, C) => D): Validation[E, D] =
        super.map3(fa, fb, fc)(f)

    }

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type lamda[x] = Const[M, x]})#lamda] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  //  map2("1", "2")(productF((f, g))) == product(map(a)(f), map(b)(g))
  //  type Const[A, B] = A

}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))

  override def map[A, B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]) = ma match {
        case Right(a) => f(a)
        case Left(b) => Left(b)
      }
    }

  def stateMonad[S] = new Monad[({type lamda[x] = State[S, x]})#lamda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  // Monad composition
  def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]):
  Monad[({type f[x] = G[H[x]]})#f] = new Monad[({type f[x] = G[H[x]]})#f] {
    def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

    override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
      G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
  }

}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type lamda[x] = Const[B, x]})#lamda, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type lamda[x] = State[S, x]})#lamda, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex_[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => (
      for {
        i <- get[Int]
        _ <- set(i + 1)
      } yield (a, i))).run(0)._1

  def toList_[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => (for {
      as <- get[List[A]]
      _ <- set(a :: as)
    } yield ())).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    })._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    })._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    (mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    })._1

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type lamda[x] = (G[x], H[x])})#lamda, A, B](fa)(a => (f(a), g(a)))(G product H)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type lamda[x] = F[G[x]]})#lamda] =
    new Traverse[({type lamda[x] = F[G[x]]})#lamda] {
      override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }

}

case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
  def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
    OptionT(value flatMap {
      case None => M.unit(None)
      case Some(a) => f(a).value
    })
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_], A, B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      oa match {
        case Some(a) => M.map(f(a))(Some(_))
        case None => M.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[M[_], A, B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }

  //  def apply: Traverse[List] = new Traverse[List]{}

}

