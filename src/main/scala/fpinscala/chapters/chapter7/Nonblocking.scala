package fpinscala.chapters.chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}

/**
  * Created by seung on 2017/09/02.
  */
object Parallelism {
  def main(args: Array[String]): Unit = {

    val p = Nonblocking.Par.parMap(List.range(1, 100000))(math.sqrt(_))
    val x = Nonblocking.Par.run(Executors.newFixedThreadPool(2))(p)
    println(x)

  }
}

object Nonblocking {

  trait Future[+A] {
    private[chapter7] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) {
        a =>
          ref.set(a)
          latch.countDown
      }
      latch.await()
      ref.get
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    /**
      * Helper function, for evaluating an action
      * asynchronously, using the given `ExecutorService`.
      */
    def eval(es: ExecutorService)(r: Unit): Unit =
      es.submit(new Callable[Unit] {
        def call = r
      })

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    /**
      * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
      * This will come in handy in Chapter 13.
      */
    def async[A](f: (A => Unit) => Unit): Par[A] =
      es => new Future[A] {
        def apply(k: A => Unit) = f(k)
      }

    def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var lo: Option[A] = None
          var ro: Option[B] = None
          val combiner = Actor[Either[A, B]](es) {
            case Left(l) =>
              if (ro.isDefined) eval(es)(cb(f(l, ro.get)))
              else lo = Some(l)
            case Right(r) =>
              if (lo.isDefined) eval(es)(cb(f(lo.get, r)))
              else ro = Some(r)
          }
          p1(es)(l => combiner ! Left(l))
          p2(es)(r => combiner ! Right(r))
        }
      }

    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es)(cb(f(a))))
      }

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))
//      sequenceBalanced(as.map(asyncF(a => f(a))))

    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            if (b) eval(es) {
              t(es)(cb)
            }
            else eval(es) {
              f(es)(cb)
            }
          }
      }

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { ind =>
            eval(es) {
              ps(ind)(es)(cb)
            }
          }
      }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =
      es => new Future[V] {
        def apply(cb: V => Unit): Unit = p(es) { k => ps(k)(es)(cb) }
      }

    /* `chooser` is usually called `flatMap` or `bind`. */
    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      flatMap(p)(f)

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit = p(es)(a => f(a)(es)(cb))
      }

    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      flatMap(p) { b => if (b) t else f }

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(p) { i => choices(i) }

    def join[A](p: Par[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es)(p2 => eval(es) {
            p2(es)(cb)
          })
      }

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(x => x)

    def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      join(map(p)(f))


    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2` and `flatMap`
    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)

      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)

      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)

      def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
    }

  }


}
