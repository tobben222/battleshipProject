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

  val possible = List('S','-');
  for( puzzle <-allPuzzles)
  {

    var allSquares = List[Square]();
    for(xValue <- 0 to puzzle.size -1)
    {
      for(yValue <- 0 to puzzle.size -1)
      {
        val s = new Square(xValue,yValue)
        allSquares = allSquares :+ s;

        if(puzzle.hints(xValue)(yValue) != '?')
        {
          if(puzzle.hints(xValue)(yValue) == '-') {
            allSquares = allSquares.filter(_ != s);
            val s2 = s.removeValue('S');
            allSquares = allSquares :+ s2;
          }else
          {
            allSquares = allSquares.filter(_ != s);
            val s2 = s.removeValue('-');
            allSquares = allSquares :+ s2;
          }
        }

      }
    }

    //before enything
    //println("Puzzle");
    printIt();
    println("");

    // apply rules

    // bruteforce
    bruteForce(puzzle);
    println("After Brute Force");
    printIt();

    println("");
    println("");







    def bruteForce(p:Puzzle): Unit =
    {
      for(x<- 0 to p.size -1){
        for(y<- 0 to p.size -1){
          for(s<- List('S','-')){
            reomveIfNotValied(x,y,s);
          }
        }
      }
    }


    def reomveIfNotValied(x:Int,y:Int,solution:Char): Unit ={
      if(!isValid(x,y,solution)){
        removeValue(x,y,solution);
      }

    }

    def removeValue(x:Int,y:Int,wrongSolution:Char) = {
      val s = getSquare(x,y);
      if(!s.isSolved)
      {
        allSquares = allSquares.filter(_ !=s);
        val s2 = s.removeValue(wrongSolution);
        allSquares = allSquares :+ s2;
      }

    }

    def isValid(x:Int,y:Int,solution:Char):Boolean = {
      val box = getSquare(x,y);

      //val corners = checkCorners(box);
      //val room = roomForMore(box);

      //if(!room && solution == 'S')return false;

      //if(corners && solution == 'S')return false;


      val mbWater = MustBeWater(box);
      val mbBoat = MustBeBoat(box);



      if(!mbWater && solution == 'S')return false;
      if(!mbBoat && solution == '-')return false;
      if(sInCorner(box) && solution == 'S')return false;
      if (!placeShip()) return false;


      return true;
      }

    // Returns number of legal ships
    def getShips(): Int = {
      var puzzleShips = 0;
      for (l <- allPuzzles) {
        puzzleShips = l.ships.foldLeft(1)(_+_._2);
      }
      return puzzleShips;
    }

    def placeShip():Boolean = {
      var counter = 0;
      for (l <- allSquares) {
        if (l.getCorrectValue().equals('S')) {
          if (counter < getShips()) counter = counter +1;
          return true;
        }
      }
      return false;
    }


    def MustBeWater(box:Square):Boolean =
    {
      var yCounter  = 0;
      var xCounter = 0;

      for(size <- 0 to puzzle.size -1)
      {
        if(getSquare(size,box.y).isSolved && getSquare(size,box.y).possibleValues(0) == 'S') yCounter = yCounter +1;
        if(getSquare(box.x,size).isSolved && getSquare(box.x,size).possibleValues(0) == 'S') xCounter = xCounter +1;
      }
      if(yCounter == puzzle.vertical(box.y))return false;
      if(xCounter == puzzle.horizontal(box.x))return false;

      return true
    }
    def MustBeBoat(box:Square):Boolean =
    {

      var yBoatCounter  = 0;
      var ySeeCounter = 0;

      var xBoatCounter = 0;
      var xSeeCounter = 0;

      for(size <- 0 to puzzle.size -1)
      {
        if(getSquare(size,box.y).isSolved && getSquare(size,box.y).possibleValues(0) == 'S') yBoatCounter = yBoatCounter +1;
        if(getSquare(box.x,size).isSolved && getSquare(box.x,size).possibleValues(0) == 'S') xBoatCounter = xBoatCounter +1;
        if(getSquare(size,box.y).isSolved && getSquare(size,box.y).possibleValues(0) == '-') ySeeCounter = ySeeCounter +1;
        if(getSquare(box.x,size).isSolved && getSquare(box.x,size).possibleValues(0) == '-') xSeeCounter = xSeeCounter +1;
      }
      val emptyX = puzzle.size - (xSeeCounter + xBoatCounter);
      val emptyY = puzzle.size - (ySeeCounter + yBoatCounter);
      if(puzzle.hints(box.x) == xBoatCounter + emptyX)return true;

      if(puzzle.hints(box.y) == yBoatCounter + emptyY) return true;


      return false;
    }

    def sInCorner(box:Square):Boolean = //cheks if there are ships in its corners
    {
      if(box.x > 0 && box.x < puzzle.size -1)
      {
        if(box.y > 0 && box.y < puzzle.size -1)
        {
          val leftTop     = getSquare(box.x -1,box.y -1)
          val letBottom   = getSquare(box.x -1,box.y +1)
          val rightTop    = getSquare(box.x +1,box.y -1)
          val rightBottom = getSquare(box.x +1,box.y +1)
          if((leftTop.isSolved && leftTop.possibleValues(0) == 'S') ||
          (letBottom.isSolved && letBottom.possibleValues(0) == 'S') ||
          (rightTop.isSolved && rightTop.possibleValues(0) == 'S') ||
          (rightBottom.isSolved && rightBottom.possibleValues(0) == 'S'))
          {
            //println("found one")
            return true;
          }

        }
      }//done cheking middle
      if(box.x == 0 && box.y != 0 && box.y != puzzle.size -1)
      {
        val rightTop    = getSquare(box.x +1,box.y -1)
        val rightBottom = getSquare(box.x +1,box.y +1)
        if((rightTop.isSolved && rightTop.possibleValues(0) == 'S') ||
          (rightBottom.isSolved && rightBottom.possibleValues(0) == 'S'))
        {
          return true
        }
      }//cheking x on left

      if(box.x == puzzle.size -1 && box.y != 0 && box.y != puzzle.size -1)
      {
        val leftTop     = getSquare(box.x -1,box.y -1)
        val leftBottom   = getSquare(box.x -1,box.y +1)
        if((leftTop.isSolved && leftTop.possibleValues(0) == 'S') ||
          (leftBottom.isSolved && leftBottom.possibleValues(0) == 'S'))
        {
          return true
        }
      }//cheking x on right


      return false;
    }


    def printIt() = {
      implicit def intToSqrt = ((x: Double) => x.toInt)

      val size = puzzle.size;
      for (x <- 0 to size - 1) {
        val values = for (y <- 0 to size - 1) yield getSquare(x, y).possibleValues
        println(values.map(i => if (i.length == 1) i(0) else "_").mkString(" "));
      }

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


    def getAllFromX(x:Int):List[Square] = {
      return allSquares.filter((s:Square) => s.x==x)
    }
    def getAllFromY(y:Int):List[Square] = {
      return allSquares.filter((s:Square) => s.y==y)
    }
    def getSquare(x:Int,y:Int):Square = {
      return allSquares.filter(_.x==x).filter(_.y==y)(0);
    }
  }
}








