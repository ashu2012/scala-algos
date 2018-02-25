
sealed abstract class Tree[+A] {

  def value:A
  def left:Tree[A]

  def right:Tree[A]

  def isEmpty:Boolean

  def fail(str:String)={
    throw  new NoSuchElementException(str)
  }


  def size:Int


  def add[B >:A](item :B)(implicit ordering: Ordering[B]):Tree[B]={
    import ordering._
    if (isEmpty ){
      Tree.make(item,Leaf, Leaf)
    }
    else if(item < value){
      Tree.make(value, left.add(item), right)
    }
    else if(item>= value){
      Tree.make(value, left, right.add(item))
    }
    else this
  }


  /**
    * Creates a new tree by mapping this tree to the 'f' function.
    *
    * Time - O(n)
    * Space - O(log n)
    */
  def map[B ](f: (A) => B): Tree[B] = {
    if (isEmpty) Tree.Empty
    else Tree.make(f(value), left.map(f), right.map(f))
  }



}


case object Leaf extends Tree[Nothing]{
  def value : Nothing = fail("An empty tree.")
  def left():Tree[Nothing]={
    fail("empty branch")
  }

  def right():Tree[Nothing]={
    fail("empty branch")
  }

  override def isEmpty(): Boolean = {
    true
  }

  override def size: Int = {
    return 0
  }
}


case class Branch[A](value :A , left:Tree[A] , right:Tree[A] , size:Int) extends Tree[A]{
  override def isEmpty(): Boolean = false

}


object Tree{

  def Empty[A]:Tree[A]=Leaf

  def apply[A](xs:A*)(implicit ordering :Ordering[A])={
    import ordering._
    var mytree:Tree[A]= Tree.Empty
    for(x<-xs){
      mytree=mytree.add(x)
    }
    mytree
  }

  def make[A](item:A, leftBranch:Tree[A], rightBranch:Tree[A]):Tree[A]={
      Branch(item,leftBranch,rightBranch,leftBranch.size+ rightBranch.size  +1 )
  }


}



object basicTree {
  def main(args: Array[String]) {
    var a =Tree(5,2,8,9,1)
    a.map(println)
    println(a)
  }
}
