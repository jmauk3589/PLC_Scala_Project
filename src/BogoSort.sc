
object BogoSort{

  val items = List(1,2,3,4,5,6,7,8,9,0)

  scala.util.Random.shuffle(items)


  def bogoSort(list:List[Int], backup:List[Int]): List[Int] =(list,backup) match {
    case (Nil,Nil) =>List()
    case (List(x:Int),Nil) => List(x)
    //case (List(x:Int), List(y:Int))=>

  }







}
