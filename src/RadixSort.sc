
object RadixSort {



  val list = List(1,33,2000,3432,99,666,7,25,1023,23232,23,1,2563)
  maxSigFigs(list,0)
  radix(list, 1,maxSigFigs(list,0)+1)

  /**
    * Returns the maximum significant figures for the given list of integers.
    * As it matches the patterns it is increasing the count value. This value is returned
    * and then each number is divided by 10 to the power of the counter. Each value corresponds
    * to another significant figure. If the value is less, then it is disregarded.
    * If the resulting value is more, then it is repeated with a higher exponent.
    * @param list of numbers to find sig fig
    * @param count or exponent for 10 to the (value)
    * @return Int of sig figs for list
    */
  def maxSigFigs(list: List[Int], count: Int): Int = (list,count) match{
    case (Nil, numb) => 0 // if the list is empty returns 0 since there are NO significant figures
    case (List(x: Int) , numb) =>
      if(Math.floor(x / Math.pow(10,numb))>0) maxSigFigs(List(x),numb+1)
      else numb
    case (x :: y, numb)=>
      if(Math.floor(x / Math.pow(10,numb))>0)
        maxSigFigs( x::y,numb+1) // reruns the same list but increases exponent
      else maxSigFigs(y,numb) // discards first member of list and continues with same exponent
  }

  /**
    * Repeats the process of sorting by significant figures by how many signifcant figures there are for the list.
    * Which is brought in by an external function as maxSF. Typically for radix sorts it is always maxSF+1 to account
    * for the largest of values
    * @param list of numbers to be sorted
    * @param count to know what significant figure it is currently on
    * @param maxSF the max number of signicant figures present
    * @return
    */
  def radix(list: List[Int], count: Int, maxSF:Int): List[Int] = (list,count,maxSF) match{
    case (List(x: Int), counter, limit) => List(x)
    case (x::xs, counter, limit)=>
      if(counter<limit) {
        radix(splitByValue(x::xs,List(),List(),List(),List(),List(),List(),List(),List(),List(),List(),counter),count+1,maxSF)
      }
      else x::xs
    case (Nil,counter,limit) => List()
    //case (x::y, counter, limit)=>

  }

  /**
    * Takes the input list and sorts the numbers into various lists according to significant figures. When there's no
    * more numbers in the list - all of the other lists are combined in order of lists corresponding from 0 to 9 in
    * terms of significant figure.
    * @param A List of numbers to be dumped into corresponding significant figure spot
    * @param B list of '0's
    * @param C list of '1's
    * @param D list of '2's
    * @param E list of '3's
    * @param F list of '4's
    * @param G list of '5's
    * @param H list of '6's
    * @param I list of '7's
    * @param J list of '8's
    * @param K list of '9's
    * @param pred used for exponent to check significant figure placement
    * @return list of all numbers sorted for that significant figure
    */
  def splitByValue(A: List[Int],B: List[Int], C: List[Int],D: List[Int],E: List[Int],F: List[Int],G: List[Int],H: List[Int],I: List[Int],J: List[Int],K: List[Int], pred: Int):List[Int] = (A,B,C,D,E,F,G,H,I,J,K,pred) match {
    case (a::as, b,c,d,e,f,g,h,i,j,k, value) => // given a list with multiple members
    {
      /*
       This gets the significant digit by getting the remainder and dividing by one less than the current exponent
       so an example of this would be...

       if the number was 678 and we wanted to sort by the tens place.

       678 % 100 = 78

       78/10 = 7.8

       Since we are just looking at this as an Int, the 0.8 is dropped and 7 is returned.

       */
      val buck = ((a%Math.pow(10,value))/ Math.pow(10,value-1)).toInt

      // This appends the value from the original list to the correct spot.
      // This is where this starts looking a little like bucketSort.
      // It sorts the values by significant digits, so if the digit is 0 it goes into B list.
      // If the digit is 9 it goes into the K list
      if(buck==0) {
        splitByValue(as,(a::b.reverse).reverse,c,d,e,f,g,h,i,j,k,value)
      }else
      if(buck==1) {
        splitByValue(as,b,(a::c.reverse).reverse,d,e,f,g,h,i,j,k,value)
      }else
      if(buck==2) {
        splitByValue(as,b,c,(a::d.reverse).reverse,e,f,g,h,i,j,k,value)
      }else
      if(buck==3) {
        splitByValue(as,b,c,d,(a::e.reverse).reverse,f,g,h,i,j,k,value)
      }else
      if(buck==4) {
        splitByValue(as,b,c,d,e,(a::f.reverse).reverse,g,h,i,j,k,value)
      }else
      if(buck==5) {
        splitByValue(as,b,c,d,e,f,(a::g.reverse).reverse,h,i,j,k,value)
      }else
      if(buck==6) {
        splitByValue(as,b,c,d,e,f,g,(a::h.reverse).reverse,i,j,k,value)
      }else
      if(buck==7) {
        splitByValue(as,b,c,d,e,f,g,h,(a::i.reverse).reverse,j,k,value)
      }else
      if(buck==8) {
        splitByValue(as,b,c,d,e,f,g,h,i,(a::j.reverse).reverse,k,value)
      }else
      if(buck==9) {
        splitByValue(as,b,c,d,e,f,g,h,i,j,(a::k.reverse).reverse,value)
      }else{
        List() // essentially there for when there is no list.
      }

    }
    case (Nil,b,c,d,e,f,g,h,i,j,k,value)=> // given an empty list - it will return the joint of all the other lists
      //This basically "Sorts them by significant figures"
      b:::c:::d:::e:::f:::g:::h:::i:::j:::k
  }

}