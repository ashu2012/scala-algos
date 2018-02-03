import scala.annotation.tailrec
       

        sealed abstract class myList[+A] {
         

          def head():A 

          def tail():myList[A] 

          def apply[A](as: A*): myList[A] ={

            if (as.isEmpty) Nil

            else Cons(as.head, apply(as.tail: _*))
          }

         

          def isEmpty():Boolean ={
            this match {
              case Nil => true
              case  _: myList[A] => false
            }
          }

          def preappend[B>:A](x: B): myList[B] ={
            if (isEmpty)  make(x)
            else  Cons(x, this)
          }

          //@tailrec
          final def append[B>:A](x: B): myList[B] ={

            def appendrecc(lst:myList[B], a:B):myList[B]={
              lst match  {
                case Nil =>Cons(a, Nil)
                case x:myList[A]=> appendrecc(x,a) 
              }
            }
            Console.println(this.toString , x.toString)
            if (this.isEmpty) make(x)
            else  Cons(this.head, this.tail.append(x))
          }
         
          def print()={
              this.map(println)
           }

          def make[B>:A](x:B): myList[B] ={
            
              this match {
                case Cons(xh:B, xs:Cons[B]) => Cons(xh, xs.make(x)) 
                case Cons(xh : B, Nil)=>Cons(xh,Cons(x, Nil))
                case _=>Cons(x, Nil)
              }
          }
          

          def map[A,B](f: (A) => B): myList[B] = { 
            
              this match {
                case Cons(xh : A, Nil) => Cons(f(xh),Nil) 
                case Cons(xh:A, xs:Cons[A]) => Cons(f(xh),xs.map(f ))
                case a:myList[A] =>Cons(f(a.head),a.tail.map(f ))
              }
          }
          /**
           * Combines all elements of this list into value.
           *
           * Time - O(n)
           * Space - O(n)
           */
          def fold[B](n: B)(op: (B, A) => B): B = {
            def loop(l: myList[A], a: B): B =
              if (l.isEmpty) a
              else loop(l.tail, op(a, l.head))

            loop(this, n)
          }

          def foldLeft[B](z: B)(f: (B, A) => B): B = {
              var acc = z
              var these = this
              while (!these.isEmpty) {
                acc = f(acc, these.head)
                these = these.tail
              }
              acc
            }

          def foldRight[B,A](z: B)(f: (A, B) => B): B = 
          this match {
            case  Cons(x:A,xs:Cons[A])=>f(x, xs.foldRight(z)(f))
            case Cons(x:A, Nil) =>f(x,z)
              

            }


          def length[B>:A]():Int={ 
            this match {
              case _: myList[B] => foldRight(0) {(y:A,x:Int ) =>x+1
                          //Console.println(this)
                          /*
                          this match{
                                 case  y: A => x+1

                                }
                          */
                }
              }

          }


          def appendprint[B>:A]():String={ 
            this match {
              case z: myList[B] => foldRight("") {(y:A, x:String ) =>
                          //Console.println(this)
                          y.toString+" --->"+x
                          /*
                          y match{
                                 case  z: A => x+" ---"+y.toString

                                }
                          */
                }         
              }

          }


          def fail(m: String) = throw new NoSuchElementException(m)

        }


        case object Nil extends myList[Nothing]{
        override  def head: Nothing = fail("An empty list.")
        override  def tail: myList[Nothing] = fail("An empty list.")

        override  def isEmpty: Boolean = true
        }

        case class Cons[+A](headc: A,  tailc: myList[A]) extends myList[A] {


        override def isEmpty: Boolean = false
        override def head():A ={
            headc
          }
        override def tail():myList[A] ={
            tailc
          }

        }

        case class truck(
              numberPlate:String
              
          )


        object Main {
          def main(args: Array[String]) {
              var a= new  truck("1233bsd")
              var b = new  truck("dsads334")
              var c = Cons(a, Nil)
              c.print()
              Console.println("appending b")
              var d = c.append(b)
              Console.println(d.toString)
              Console.println("printing using map and fold ")
              d.print()
              Console.println("printing length of list ")
              Console.println(d.length())
              Console.println("printing list content using fold ")
              Console.println(d.appendprint())


          }
        }
