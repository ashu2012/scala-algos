
sealed abstract class vehicle{
  def tyres :Int
  def runOn:String
}

class truck(numtyres:Int , surface:String) extends vehicle{
  val tyres = numtyres
  def runOn={
    surface
  }
}



class boat(numtyres:Int , surface:String) extends vehicle{
  val tyres = numtyres
  def runOn={
    surface
  }
}


class fleat[A]{

}

object Main extends App{

  override def main(args: Array[String]): Unit = {
    println("Hello, world!")


    var smallTruck= new truck(4, "land")
    var bigTruck= new truck(8, "land")
    var smallBoat = new boat(0,"water")
    println(smallTruck)
    println(bigTruck)


  }


}
