package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  self =>
  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    h <- frequency((1, empty), (9, genHeap))
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { h: H =>
  val m = if (isEmpty(h)) 0 else findMin(h)
  findMin(insert(m, h)) == m
  }

  property("hint minimum") = forAll { (a: A, b: A) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == a.min(b)
  }

  property("melding on empty") = forAll { h: H =>
    val h2 = empty
    meld(h, h2) == h
    meld(h2, h) == h
  }

  property("hint: delete minimum on single element heap") = forAll { a: A =>
    deleteMin(insert(a, empty)) == empty
  }

  property("hint: finding a minimum of melding two Heaps") = forAll { (h1: H, h2: H) =>
    val m = findMin(meld(h1, h2))
    m == findMin(h1) || m == findMin(h2)
  }

  property("hint: given any heap, get a sorted heap when continually finding and deleting") = forAll{ h: H =>
    def isSorted(heap: H): Boolean =
      if(isEmpty(heap)) true
      else {
        val hDelete = deleteMin(heap)
        isEmpty(hDelete) || (findMin(heap) <= findMin(hDelete) && isSorted(hDelete))
      }
    isSorted(h)
  }

  property("find min of a melded heap and insert it to another lacking (bogus 3 and 4)") = forAll { (h1: H, h2: H) =>
    def eq(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else findMin(h1) == findMin(h2) && eq(deleteMin(h1), deleteMin(h2))
    eq(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
