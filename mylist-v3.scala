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

          def foldRight[D,C](z: D)(f: (C, D) => D): D = 
          try { 
            // ...
            this match {
            case Cons(x:C,xs:Cons[C])=>{println("1case->"+this.toString);f(x, xs.foldRight(z)(f))}
            case Cons(x:C, Nil) =>{println("2case->"+this.toString+"x->"+x.toString+"z-> "+z.toString);f(x,Nil.foldRight(z)(f))}
            case y:C=>f(y,z)
            }
          } catch {
            case e: Exception =>{ println(this.toString);e.printStackTrace(); sys.exit;}
          }
          


          def length[B>:A]():Int={ 
            this match {
              case _: myList[B] => foldRight(0) {(y:A,x:Int ) =>x+1
                }
              }

          }


          def appendprint[B>:A]():String={ 
            this match {
              case z: myList[B] => foldRight("") {(y:A, x:String ) =>
                          //Console.println(this)
                          y.toString+" --->"+x
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


      object myList {

        /**
         * An empty list.
         */
        def empty[A]: myList[A] = Nil

        /**
         * A smart constructor for list's cons.
         */
        def make[A](x: A, t: myList[A] = Nil): myList[A] = Cons(x, t)

        /**
         * Creates a new list from given 'xs' sequence.
         *
         * Time - O(n)
         * Space - O(1)
         */
        def apply[A](xs: A*): myList[A] = {
          var r: myList[A] = myList.empty
          for (x <- xs.reverse) r = r.append(x)
          r
        }

      }

        object Main {
          def main(args: Array[String]) {
              var a= new  truck("1233bsd")
              var b = new  truck("dsads334")
              var lst = List(a,b)
              var e = myList(a,b)
              Console.println(e)
              Console.println("printing length of list ")
              Console.println(e.length())
              Console.println("copy list using fold operations------>")
              var f=e.foldRight(Nil: myList[truck])((next, acc:myList[truck]) =>{println("fold function->"+next.toString); Cons[truck](next, acc)})
              Console.println(f)
          }
        }
