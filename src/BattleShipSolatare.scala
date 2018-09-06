object BattleShipSolatare extends App {
  val fileName = getClass.getResourceAsStream("/puzzle.txt");
  import scala.io.Source;
  val lines = Source.fromInputStream(fileName).getLines()

  val nrOfPuzzlesString = lines.next();
  val nrOfPuzzleParts = nrOfPuzzlesString split " ";
  var allPuzzles:List[Puzzle] = List();

  for( i <- 1 to nrOfPuzzleParts(1).toInt)
  {
    val sizexsize = lines.next() split " "(0);
    val size = sizexsize(1) split "x"(0);
    val restInt = 4+ size(0).toInt;
    var restOfPuzzle:List[String] = List();

    for(a <-  1 to restInt)
      restOfPuzzle = restOfPuzzle :+ lines.next();
    val puzzle = new Puzzle(size(0).toInt,restOfPuzzle);
    allPuzzles = allPuzzles :+ puzzle;
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
