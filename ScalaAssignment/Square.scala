package square

class Square(xNumber:Int,yNumber:Int, values:List[Char] = List('-','S'), solved:Boolean=false){

  val x = xNumber;
  val y = yNumber;
  val possibleValues = values;
  val isSolved = solved;


  def setValue(solution:Char):Square = {
    return new Square(x,y,List(solution),true);
  }
  def removeValue(wrongSolution:Int):Square = {
    val newList = this.possibleValues.filter(_ != wrongSolution);
    if(newList.length > 1)
      return new Square(x,y,newList,false);
    else
      return new Square(x,y,newList,true);
  }
  def getCorrectValue():Char = {
    if(this.isSolved)
    {
      return this.possibleValues(0);
    }
    else
    {
      return 0
    }
  }
}
