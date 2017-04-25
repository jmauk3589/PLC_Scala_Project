//Programmed by Noah Doershuk
object InsertionSort{
  /**
    * This is a helper function for insertion sort
    * it takes an integer and a list and inserts the integer
    * such that it is larger than all preceding elements and smaller
    * than all subsequent elements
    * @param i an integer to be inserted
    * @param L a list of integers that we wish to insert into
    * @return a new list with i in the proper location
    */
  def insert(i: Int, L: List[Int]): List[Int] = {
    //match on the nature of the list
    L match{
      case Nil => List(i) //empty list, we just return a list with i in it
      case h :: xs => //case where we have a list with head h and some xs
        if(h >= i) i :: L //if the head of the list is greater than the element we want to insert, we can put it as the new head
        else h :: insert(i, xs) //if the head is smaller than the inserted element we need to recurse to find the right spot
        //meaning we need to recurse on the list without h
    }
  }

  /**
    * Performs insertion sort on an input list of integers
    * relies on a helper function insert
    * @param L list of integers to be sorted
    * @return sorted version of L
    */
  def sort(L: List[Int]): List[Int] = {
    L match{
      case Nil => L //the empty list is sorted by definition
      case h :: xs => insert(h, sort(xs)) //recursively sort
    }
  }

  //test it real quick
  val list = List(22, 42, 21, 2, 47, 19, 7, 9, 16, 46)
  sort(list)
}