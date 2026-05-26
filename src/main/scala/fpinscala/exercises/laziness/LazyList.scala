package fpinscala.exercises.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    this match
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => empty

  def drop(n: Int): LazyList[A] =
    this match
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    // this match
    //   case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    //   case _ => empty
    foldRight(empty)((a, acc) => if p(a) then cons(a, acc) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, acc) => cons(f(a), acc))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty)((a, acc) => if f(a) then cons(a, acc) else acc)

  def append[B >: A](that: => LazyList[B]): LazyList[B] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)((a, acc) => f(a).append(acc))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this)(
      _ match
        case Empty => None
        case Cons(h, t) => Some((f(h()), t()))
    )

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold(this, n)((as, n) => as match
      case Cons(h, t) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    )

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this)(as => as match
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    )

  def zipWith[B,C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold(this, that)((as, bs) => (as, bs) match
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    )

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold(this, that)((as, bs) => (as, bs) match
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), empty)))
      case (Empty, Cons(h, t)) => Some(((None, Some(h())), (empty, t())))
      case (Empty, Empty) => None
    )

  def startsWith[B](s: LazyList[B]): Boolean =
    this.zipAll(s).takeWhile(_(1).isDefined).forAll(_ ==_)

  def tails: LazyList[LazyList[A]] =
    unfold(this)(state => state match
      case Empty => None
      case Cons(h, t) => Some((state, t()))
    ).append(LazyList(empty))

  def hasSubsequence[A](l: LazyList[A]): Boolean =
    tails.exists(_.startsWith(l))

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(init -> LazyList(init)) { (a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1(0))
      (b2, cons(b2, b1(1)))
    }.apply(1)

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    lazy val as: LazyList[A] = cons(a, as)
    as

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = 
    def go(current: Int, next: Int): LazyList[Int] =
      LazyList.cons(current, go(next, current + next))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None => empty
      case Some((value, nstate)) => cons(value, unfold(nstate)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))((current, next) => Some((current, (next, current + next))))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some((a, ())))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(())(_ => Some((1, ())))
