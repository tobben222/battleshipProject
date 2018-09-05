object BattleShipSolatare extends App {
  class Square(xNumber:Int,yNumber:Int,
               values:List[Char] = List('?','-','S','A','V','<','>','+','*'),
               solved:Boolean=false){
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

  var allSquares = List[Square]();

  for(xValue <- List(1,2,3,4,5,6,7)) //TODO replace with variable form file
  {
    for(yValue <- List(1,2,3,4,5,6,7))
    {
      val s = new Square(xValue,yValue)
      allSquares = allSquares :+ s;
    }
  }

  def getAllFromX(x:Int):List[Square] = {
    return allSquares.filter((s:Square) => s.x==x)
  }
  def getAllFromY(y:Int):List[Square] = {
    return allSquares.filter((s:Square) => s.y==y)
  }
  def getSquare(x:Int,y:Int):Square = {
    return allSquares.filter(_.x==x).filter(_.y==y)(0);
  }
  def setValue(x:Int,y:Int,solution:Char):Boolean = {
    val s = getSquare(x,y);
    if(s.getCorrectValue()==solution){
      return false;
    }
    else{
      allSquares = allSquares.filter(_ !=s);
      val s2 = s.setValue(solution);
      allSquares = allSquares :+ s2;
      return true;
    }
  }

  def removeValue(x:Int,y:Int,wrongSolution:Char) = {
    val s = getSquare(x,y);
    allSquares = allSquares.filter(_ !=s);
    val s2 = s.removeValue(wrongSolution);
    allSquares = allSquares :+ s2;
  }

  def printIt() = {
    implicit def intToSqrt = ((x:Double) => x.toInt)
    val size = scala.math.sqrt(allSquares.length)
    for(x<-1 to size){
      val values = for(y<-1 to size) yield getSquare(x,y).possibleValues
      println(values.map(i=> if(i.length == 1) i(0) else "_").mkString(" "));
    }
  }

  def isValid(x:Int,y:Int,solution:Char):Boolean = {
    for(s<- getAllFromX(x):::getAllFromY(y))
    {
      var oneSquare = s.asInstanceOf[Square];
      if(x != oneSquare.x || y!=oneSquare.y){
        if(oneSquare.isSolved && oneSquare.possibleValues(0)==solution){
          return false;
        }
      }
    }
    return true;
  }

  def reomveIfNotValied(x:Int,y:Int,solution:Char): Unit ={
    if(!isValid(x,y,solution)){
      removeValue(x,y,solution);
    }
  }


}
