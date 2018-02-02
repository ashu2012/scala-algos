sealed class myList[A] {

private var elements: List[A] = Nil

 
  def apply(x:A):List[A]={
    elements:::List(x)
  }

  def head():A = this.elements.head

  def tail():List[A] = this.elements.tail

  def isEmpty():Boolean ={
    if (elements.length==0) true
    else false
  }

  def append(x: A) ={
    if (isEmpty)  this.elements=this.elements::: List(x)
    else  this.elements= List(head):::tail :::List(x)
  }
 
  def print()={
      this.elements.map(x=> println(x))
   }

}


case class truck(
      numberPlate:String
      
  )


object Main {
  def main(args: Array[String]) {
      val a= new  truck("1233bsd")
      val b = new  truck("dsads334")

      val c =   new myList[truck]
      c.append(a)
      c.print()
      c.append(b)
      c.print()

  }
}

 
