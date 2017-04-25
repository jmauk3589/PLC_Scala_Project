object MergeSort {

  /**
    * Initiates mergesort process. Takes in a list, finds the length, cuts it in half.
    * Then sends each half to be sorted and merged. When both are finished the two
    * halves are joined together to make the whole list again... but sorted
    * @param list of integers
    * @return sorted list
    */
  def mergeSort(list: List[Int]):List[Int] = {
    if(list.length/2==0) list
    else{
      val(left,right) = list.splitAt(list.length/2)
      merge(mergeSort(left),mergeSort(right))
    }
  }


  /**
    * Takes the split halves and combines them by looking at each value in the list.
    * @param left half of original list
    * @param right half of original list
    * @return two halves sorted
    */
  def merge(left: List[Int], right: List[Int]):List[Int] = (left,right) match{
    case (x::xs,y::ys) =>
      if(x>y) y::merge(x::xs,ys)
      else x::merge(xs,y::ys)
    case (Nil,y) => right
    case (x,Nil) => left
  }

  val list = List(1,5,456,2,90,33,10,324,7,12,11,13)

  mergeSort(list)








}