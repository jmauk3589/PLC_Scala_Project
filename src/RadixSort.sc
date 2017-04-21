
object RadixSort {


  def maxSigFigs(list: List[Int], count: Int): Int = (list,count) match{
    case (Nil, numb) => 0
    case (List(x: Int) , numb) =>
      if(Math.floor(x / Math.pow(10,numb))>0) maxSigFigs(List(x),numb+1)
      else numb
    case (x :: y, numb)=>
      if(Math.floor(x / Math.pow(10,numb))>0)

        maxSigFigs( x::y,numb+1)
      else maxSigFigs(y,numb)
  }

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

  def splitByValue(A: List[Int],B: List[Int], C: List[Int],D: List[Int],E: List[Int],F: List[Int],G: List[Int],H: List[Int],I: List[Int],J: List[Int],K: List[Int], pred: Int):List[Int] = (A,B,C,D,E,F,G,H,I,J,K,pred) match {
    case (a::as, b,c,d,e,f,g,h,i,j,k, value) =>
    {
      val buck = ((a%Math.pow(10,value))/ Math.pow(10,value-1)).toInt
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
        List()
      }

    }
    case (Nil,b,c,d,e,f,g,h,i,j,k,value)=>
      b:::c:::d:::e:::f:::g:::h:::i:::j:::k
  }

  val list = List(1,33,2000,3432,99,666,7,25,1023,23232,23,1,2563)
  maxSigFigs(list,0)
  radix(list, 1,maxSigFigs(list,0)+1)

}

/*case(leftHead :: leftTail, rightHead :: rightTail) =>
if (leftHead < rightHead) leftHead::merge(leftTail, right)
else rightHead :: merge(left, rightTail)*/