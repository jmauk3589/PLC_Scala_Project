//Programmed by Noah Doershuk
object QuickSort{
  /**
    * Sorts a list of integers using the QuickSort algorithm
    * @param L, a list of integers
    * @return a new list of integers, sorted.
    */
  def sort(L: List[Int]) :List[Int] = {
    //if the list is one or zero elements, return it
    if(L.length < 2) L
    else {
      //Otherwise, we'll sort it
      //Picking the center element as the pivot point
      val pivot = L((L.length/2))
      //now we split into two lists, elements larger than
      //the pivot and those smaller. We'll sort these lists
      //recursively and recombine.
      val Lless = L.filter(_<pivot)
      val Lgreater = L.filter(_>pivot)
      //sort the lower numbers and the higher numbers, stick them
      //back together in order with the pivot in between.
      sort(Lless) ++ L.filter(_==pivot) ++ sort(Lgreater)
    }
  }
  //quick and dirty test
  val list = List(22, 42, 21, 2, 47, 19, 7, 9, 16, 46)

  sort(list)
}