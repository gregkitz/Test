package com.persist.uw.examples

import com.persist.uw.examples.FQueue.NonEmptyFQueue

import scala.annotation.tailrec

// make it similar to FQueue
// tests must pass
// use only immutable data
// if using recursion make sure its tail recursive
// use case classes and objects, not Scala collection types
// correctness not performance is the goal

object FSet {

  private case class NonEmptyFSet(currentVal:Int, restSet: FSet) extends FSet
  private case object EmptyFSet extends FSet
    
  def apply(): FSet = EmptyFSet
}

sealed trait FSet {

  import FSet._

  def contains(i: Int): Boolean = this match {
    case NonEmptyFSet(currentVal, restSet) => if (currentVal == i) true else restSet.contains(i)
    case EmptyFSet => false
  }

  def add(i: Int): FSet =
    if (this.contains(i)) this
    else NonEmptyFSet(i, this)


  def delete(i: Int): FSet = {
    @tailrec def delete1(items: FSet, accum: FSet): FSet =  {
      items match {
        case NonEmptyFSet(i1, next) => if (i1 == i) delete1(next, accum) else delete1(next, accum.add(i1))
        case EmptyFSet => accum
      }
    }
    delete1(this, FSet())
  }


  def union(set1: FSet): FSet = {
    @tailrec def union1(items: FSet, accu:FSet): FSet = {
      items match {
        case NonEmptyFSet(it, next) => if(!set1.contains(it)) union1(next, accu.add(it)) else union1(next,accu)
        case EmptyFSet => accu
      }
    }
    union1(this, set1)
  }

  def intersect(set1: FSet): FSet = {
    @tailrec def intersect1(items: FSet, accu : FSet): FSet = {
      items match {
        case NonEmptyFSet(it, next) => if (set1.contains(it)) intersect1(next, accu.add(it)) else intersect1(next, accu)
        case EmptyFSet => accu
      }
    }
    intersect1(this, FSet())
  }

  def subset(set1: FSet): Boolean = {
    @tailrec def subset1(items:FSet) : Boolean = {
      items match {
        case NonEmptyFSet(it, next) => if (set1.contains(it)) subset1(next) else false
        case EmptyFSet => true
      }
    }
    subset1(this)
  }

  def equals(set1: FSet): Boolean = {    //check everything in A in B and vice-versa
   this.subset(set1) && set1.subset(this)
  }

  def size: Int = {
    @tailrec def size1(items: FSet, accum: Int = 0): Int = {
      items match {
        case NonEmptyFSet(currentValue, next) => size1(next, accum + 1)
        case EmptyFSet => accum
      }
    }
      size1(this)
  }





}
