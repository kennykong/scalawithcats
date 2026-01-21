package scalawithcats.ch11

import scalawithcats.Utils.p

object CRDTExample extends App {

  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int) = {
      val value = amount + counters.getOrElse(machine, 0)
      GCounter(counters + (machine -> value))
    }

    def merge(that: GCounter): GCounter =
      GCounter(that.counters ++ this.counters.map {
        case (k, v) =>
          k -> (v max that.counters.getOrElse(k, 0))
      })

    def total: Int = counters.values.sum
  }

  val g00 = Map("a" -> 7, "b" -> 3)
  val g01 = Map("a" -> 2, "b" -> 5)
  val counter00 = GCounter(g00)
  val counter01 = GCounter(g01)
  val merged0 = counter00.merge(counter01)
  p(merged0)
  // merged0: Map[String, Int] = Map("a" -> 7, "b" -> 5)
  val total0 = merged0.total
  p(total0)
  // total0: Int = 12

  // 11.3.1

  import cats.kernel.CommutativeMonoid

  object wrapper {
    trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
      def combine(a1: A, a2: A): A

      def empty: A
    }

    object BoundedSemiLattice {
      implicit val intInstance: BoundedSemiLattice[Int] =
        new BoundedSemiLattice[Int] {
          def combine(a1: Int, a2: Int): Int =
            a1 max a2

          val empty: Int =
            0
        }

      implicit def setInstance[A]: BoundedSemiLattice[Set[A]] =
        new BoundedSemiLattice[Set[A]] {
          def combine(a1: Set[A], a2: Set[A]): Set[A] =
            a1 union a2

          val empty: Set[A] =
            Set.empty[A]
        }
    }
  };

  import wrapper._

  import cats.instances.list._ // for Monoid
  import cats.instances.map._ // for Monoid
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.foldable._ // for combineAll

  final case class GCounter1[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)
                 (implicit m: CommutativeMonoid[A]): GCounter1[A] = {
      val value = amount |+| counters.getOrElse(machine, m.empty)
      GCounter1(counters + (machine -> value))
    }

    def merge(that: GCounter1[A])
             (implicit b: BoundedSemiLattice[A]): GCounter1[A] =
      GCounter1(this.counters |+| that.counters)

    def total(implicit m: CommutativeMonoid[A]): A =
      this.counters.values.toList.combineAll
  }


  //11.4
  trait GCounter2[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)
                 (implicit m: CommutativeMonoid[V]): F[K, V]

    def merge(f1: F[K, V], f2: F[K, V])
             (implicit b: BoundedSemiLattice[V]): F[K, V]

    def total(f: F[K, V])
             (implicit m: CommutativeMonoid[V]): V
  }

  object GCounter2 {
    def apply[F[_, _], K, V]
             (implicit counter: GCounter2[F, K, V]) =
      counter
  }

  import cats.instances.list._ // for Monoid
  import cats.instances.map._ // for Monoid
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.foldable._ // for combineAll

  implicit def mapGCounterInstance[K, V]: GCounter2[Map, K, V] =
    new GCounter2[Map, K, V] {
      def increment(map: Map[K, V])(key: K, value: V)
                   (implicit m: CommutativeMonoid[V]): Map[K, V] = {
        val total = map.getOrElse(key, m.empty) |+| value
        map + (key -> total)
      }

      def merge(map1: Map[K, V], map2: Map[K, V])
               (implicit b: BoundedSemiLattice[V]): Map[K, V] =
        map1 |+| map2

      def total(map: Map[K, V])
               (implicit m: CommutativeMonoid[V]): V =
        map.values.toList.combineAll
    }

  import cats.instances.int._ // for Monoid

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)
  val counter = GCounter2[Map, String, Int]
  val merged = counter.merge(g1, g2)
  p(merged)
  // merged: Map[String, Int] = Map("a" -> 7, "b" -> 5)
  val total = counter.total(merged)
  p(total)
  // total: Int = 12

  // 11.5
  trait KeyValueStore[F[_, _]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](f: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
      get(f)(k).getOrElse(default)

    def values[K, V](f: F[K, V]): List[V]
  }

  implicit val mapKeyValueStoreInstance: KeyValueStore[Map] =
    new KeyValueStore[Map] {
      def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
        f + (k -> v)

      def get[K, V](f: Map[K, V])(k: K): Option[V] =
        f.get(k)

      override def getOrElse[K, V](f: Map[K, V])
                                  (k: K, default: V): V =
        f.getOrElse(k, default)

      def values[K, V](f: Map[K, V]): List[V] =
        f.values.toList
    }

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)
           (implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(key)

    def getOrElse(key: K, default: V)
                 (implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] =
      kvs.values(f)
  }

  implicit def gcounterInstance[F[_, _], K, V]
                               (implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]) =
    new GCounter2[F, K, V] {
      def increment(f: F[K, V])(key: K, value: V)
                   (implicit m: CommutativeMonoid[V]): F[K, V] = {
        val total = f.getOrElse(key, m.empty) |+| value
        f.put(key, total)
      }

      def merge(f1: F[K, V], f2: F[K, V])
               (implicit b: BoundedSemiLattice[V]): F[K, V] =
        f1 |+| f2

      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.combineAll
    }

  val g3 = Map("a" -> 8, "b" -> 4)
  val g4 = Map("a" -> 3, "b" -> 6)
  val counter1 = GCounter2[Map, String, Int]
  val merged1 = counter1.merge(g3, g4)
  p(merged1)
  // merged1: Map[String, Int] = Map("a" -> 8, "b" -> 6)
  val total1 = counter1.total(merged1)
  p(total1)
  // total: Int = 14
}
