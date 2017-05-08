
object BogoSort{

  val items = List(1,5,2,9,7,6,3,0,8,4) // List to sort

  bogoSort(items,List()) // sorted with the infamous bogoSort

  /**
    * Takes in a List of numbers and an empty list to put sorted values.
    * If the items in the list are sorted, returns the list. If not,
    * then shuffles the list and restarts the process.
    * @param list of Ints to be sorted
    * @param backup of sorted list
    * @return sorted list
    */
  def bogoSort(list:List[Int], backup:List[Int]): List[Int] =(list,backup) match {
    case (Nil,Nil) =>List()  // when nothing is passed
    case (List(x:Int),Nil) => List(x) // when only one Int is present
    case (Nil, x::xs) => x::xs // when finished sorting original list
    case (x::xs,Nil) => bogoSort(xs,List(x)) // if original list has values but hasnt started sorted list yet
    case (List(x:Int), List(y:Int))=> // when each list has 1 value
      if( x < y )
        List(x,y) // if sorted, returns list together
      else
        bogoSort(scala.util.Random.shuffle(List(x,y)), List()) // if not sorted restarts with shuffled list
    case (List(x:Int),y::ys)=> // when one value left in original list but sorted has many
      if( x < y )
        bogoSort(List(),x::y::ys) // adds original list value to sorted list
      else
        bogoSort(scala.util.Random.shuffle(x::y::ys) , List()) // restarts after shuffling
    case (x::xs,y::ys)=> // when both lists have many items
      if( x < y)
        bogoSort(xs,x::y::ys) // moves original list head to other list and continues process
      else
        bogoSort(scala.util.Random.shuffle((x::xs):::(y::ys)),List()) // restarts after shuffling
  }

}
