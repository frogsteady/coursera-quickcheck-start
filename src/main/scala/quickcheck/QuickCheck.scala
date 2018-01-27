package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(genHeap, const(empty))
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>

    def createList(m: H):List[A] = if(isEmpty(m)) {List()} else {findMin(m)::createList(deleteMin(m))}

    val createList1 = createList(h)
    createList1 == createList1.sorted

  }

  property("gen3") = forAll { (h: H, w: H) =>

    val minH = findMin(h)
    val minW = findMin(w)
    val minHW = Math.min(minH, minW)

    val meld1 = meld(h, w)
    val minMeld = findMin(meld1)

    minHW == minMeld

  }

  property("gen5") = forAll { (h: H, w: H) =>

    def createList(m: H):H = if(isEmpty(m)) {empty} else {insert(findMin(m), createList(deleteMin(m)))}

    val meld1 = meld(h, w)

    val minH = findMin(h)
    val minW = findMin(w)
    val insert1 = insert(minW, deleteMin(h))
    val insert2 = insert(minH, deleteMin(w))
    val meld2 = meld(insert1, insert2)

    createList(meld1) == createList(meld2)
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val w = insert(b, h)
    val min = Math.min(a, b)
    findMin(w) == min
  }

  property("min3") = forAll { a: Int =>
    val h = insert(a, empty)
    val min = deleteMin(h)
    min == empty
  }


}
