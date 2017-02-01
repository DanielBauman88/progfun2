package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    z <- arbitrary[Int]
    x <- arbitrary[Int]
    m <- oneOf(empty, insert(1, empty))
  } yield insert(x, insert(z, insert(k, m)))
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("insdelmin") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sortedwhenfindmin") = forAll { (h: H) =>
    def sortedFrom(h1: H, min: Int): Boolean = {
      if (h1 == empty) true else {
        val heapMin = findMin(h1)
        if (heapMin < min) false else sortedFrom(deleteMin(h1), heapMin)
      }
    }
    val min = findMin(h)
    sortedFrom(deleteMin(h), min)
  }

  property("minmeld") = forAll { (h1: H, h2: H) =>
    def m = meld(h1, h2)
    def minM = findMin(m)
    minM == findMin(h1) | minM == findMin(h2)
  }

  property("doubleinsert") = forAll { (a: Int) =>
    val a1 = if (a == Integer.MAX_VALUE) a - 1 else a
    val a2 = a1 + 1
    val heap = insert(a2, insert(a1, empty))
    findMin(heap) == a1 & findMin(deleteMin(heap)) == a2
  }

  property("simpledoubleinsert") = {
    val a1 = 5;
    val a2 = a1 + 1
    val heap = insert(a2, insert(a1, empty))
    findMin(heap) == a1 & findMin(deleteMin(heap)) == a2
  }

  property("simpledoubleinsert") = {
    val a = insert(1, empty)
    val b = insert(2, a)
    val c = insert(3, b)
    findMin(deleteMin((c))) == 2
  }
}
