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

    //before everything
    println("Puzzle")
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
      var solved = false
      while(!solved)
      {
        for(x<- 0 to p.size -1){
          for(y<- 0 to p.size -1){
            for(s<- List('S','-')){
              reomveIfNotValied(x,y,s);
              printIt();
              println("")
            }
          }
        }
        solved = true;
        for(s <- allSquares)
        {
          if(!s.isSolved)
          {
            solved = false
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
      
      if(!MustBeWater(box) && solution == 'S')return false;
      //if(!MustBeBoat(box)  && solution == '-')return false;
      if(sInCorner(box)    && solution == 'S')return false;
      if(!romeForShip(box) && solution == 'S') return false;

      return true;
      }
    // Checks that amount of legal ships is greater than placed ships
    def romeForShip(box:Square):Boolean = {
      if(CanPlaceMoreShips() || nextToShip(box))return true

      return false;
    }

    def CanPlaceMoreShips():Boolean = //true if there are fever ships than given
    {
      val shipParts = allSquares.filter(x => x.isSolved && x.possibleValues(0) == 'S')

      val shipPartsNumber = trimList(shipParts);

      if(shipPartsNumber == puzzle.sumShips)
      {
        return false
      }

      return true;
    }


    def trimList(ships:List[Square]):Int = //trims list removing one square if it is next to another one(but not the other one)
    {
      var newList = ships;
      var nr = 0;

      while(newList.size > 0)
      {

        val nextTo = getSNextTo(newList(0), List());


        if(nextTo.size == 0)
        {
          newList = newList.filter(_ != newList(0));

        }else
        {
          newList = newList diff nextTo
        }


        nr = nr+1
      }
      return nr;
    }

    def getSNextTo(box:Square, boxes:List[Square]): List[Square] =
    {
      var S:List[Square] = List[Square]();

      if(box.y < puzzle.size - 1) //checking right
      {
        if(getSquare(box.x , box.y +1).isSolved && getSquare(box.x ,box.y +1).possibleValues(0) == 'S')S = S :+ getSquare(box.x ,box.y +1);
      }

      if(box.y > 0) // checking left
      {
        if(getSquare(box.x ,box.y -1).isSolved && getSquare(box.x -1,box.y).possibleValues(0) == 'S') S = S :+ getSquare(box.x -1,box.y);
      }

      if(box.x < puzzle.size - 1) //checking bottom
      {
        if(getSquare(box.x +1,box.y).isSolved && getSquare(box.x +1,box.y ).possibleValues(0) == 'S') S = S :+ getSquare(box.x +1,box.y);
      }

      if(box.x > 0) //checking top
      {
        if(getSquare(box.x -1,box.y).isSolved && getSquare(box.x -1,box.y).possibleValues(0) == 'S') S = S :+ getSquare(box.x -1 ,box.y);
      }

      S = S :+ box;

      //delete everyting that overlaps between boxes and S
      val distinctAll = S ::: boxes;
      val distinct = distinctAll.distinct;
      val notCheckedwhithbox = distinct diff boxes
      val notChecked = notCheckedwhithbox diff List(box)

      if(boxes.size == notCheckedwhithbox.size)
      {
        for(square <- S)
        {
          S = S ::: getSNextTo(square, distinct)
        }
      }


      return S.distinct;
    }
    def nextToShip(box:Square):Boolean =
    {
      if(box.x < puzzle.size - 1) //checking right
      {
        if(getSquare(box.x +1,box.y).isSolved && getSquare(box.x +1,box.y).possibleValues(0) == 'S')return true;
      }
      if(box.x > 0) // checking left
      {
        if(getSquare(box.x -1,box.y).isSolved && getSquare(box.x -1,box.y).possibleValues(0) == 'S')return true;
      }
      if(box.y < puzzle.size - 1) //checking bottom
      {
        if(getSquare(box.x,box.y +1).isSolved && getSquare(box.x ,box.y +1).possibleValues(0) == 'S')return true;
      }
      if(box.y > 0) //checking top
      {
        if(getSquare(box.x ,box.y -1).isSolved && getSquare(box.x ,box.y -1).possibleValues(0) == 'S')return true;
      }

      return false
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

      val xLine = getAllFromX(box.x)
      val yLine = getAllFromY(box.y)



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
      if (box.x > 0 && box.x < puzzle.size - 1 && box.y > 0 && box.y < puzzle.size - 1) // is in the middle
      {
        val leftTop = getSquare(box.x - 1, box.y - 1)
        val leftBottom = getSquare(box.x + 1, box.y - 1)
        val rightTop = getSquare(box.x - 1, box.y + 1)
        val rightBottom = getSquare(box.x + 1, box.y + 1)

        if ((leftTop.isSolved && leftTop.possibleValues(0) == 'S') ||
          (leftBottom.isSolved && leftBottom.possibleValues(0) == 'S') ||
          (rightTop.isSolved && rightTop.possibleValues(0) == 'S') ||
          (rightBottom.isSolved && rightBottom.possibleValues(0) == 'S')) return true
      }
      if (box.x == 0 && box.y > 0 && box.y < puzzle.size - 1) //is at the top middle
      {
        val leftBottom = getSquare(box.x + 1, box.y - 1)
        val rightBottom = getSquare(box.x + 1, box.y + 1)

        if ((leftBottom.isSolved && leftBottom.possibleValues(0) == 'S') ||
          (rightBottom.isSolved && rightBottom.possibleValues(0) == 'S')) {
          return true;
        }
      }
      if (box.x == puzzle.size - 1 && box.y > 0 && box.y < puzzle.size - 1) //is at the bootom middle
      {
        val leftTop = getSquare(box.x - 1, box.y - 1)
        val rightTop = getSquare(box.x - 1, box.y + 1)

        if ((leftTop.isSolved && leftTop.possibleValues(0) == 'S') ||
          (rightTop.isSolved && rightTop.possibleValues(0) == 'S')) {
          return true;
        }
      }
      if (box.x > 0 && box.x < puzzle.size - 1 && box.y == 0) // is on the middle left
      {
        val rightTop = getSquare(box.x - 1, box.y + 1)
        val rightBottom = getSquare(box.x + 1, box.y + 1)

        if ((rightTop.isSolved && rightTop.possibleValues(0) == 'S') ||
          (rightBottom.isSolved && rightBottom.possibleValues(0) == 'S')) {
          return true;
        }
      }
      if (box.x > 0 && box.x < puzzle.size - 1 && box.y == puzzle.size - 1) //is on the middle right
      {
        val leftTop = getSquare(box.x - 1, box.y - 1)
        val leftBottom = getSquare(box.x + 1, box.y - 1)

        if ((leftTop.isSolved && leftTop.possibleValues(0) == 'S') ||
          (leftBottom.isSolved && leftBottom.possibleValues(0) == 'S')) {
          return true;
        }
      }
      if (box.x == 0 && box.y == 0) //top left corner
      {
        val rightBottom = getSquare(box.x + 1, box.y + 1)

        if (rightBottom.isSolved && rightBottom.possibleValues(0) == 'S') {
          return true;
        }
      }
      if (box.x == puzzle.size - 1 && box.y == 0) //bottom left corner
      {
        val rightTop = getSquare(box.x - 1, box.y + 1)

        if (rightTop.isSolved && rightTop.possibleValues(0) == 'S') {
          return true;
        }
      }
      if (box.x == 0 && box.y == puzzle.size - 1) //top right corner
      {
        val leftBottom = getSquare(box.x + 1, box.y - 1)

        if (leftBottom.isSolved && leftBottom.possibleValues(0) == 'S') {
          return true;
        }
      }
      if (box.x == puzzle.size - 1 && box.y == puzzle.size - 1) //bottom right corner
      {
        val leftTop = getSquare(box.x - 1, box.y - 1)

        if (leftTop.isSolved && leftTop.possibleValues(0) == 'S') {
          return true;
        }
      }
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








