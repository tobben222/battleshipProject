import java.io._

object PuzzleSolver extends App {

  //classes places here because when testing whit banboo cant find class in other file
  class Puzzle(s:Int, information:List[String]) {
    val size = s;
    val ships = MapOfShips(information(0));
    val horizontal = fromStringToIntList(information(2));
    val vertical = fromStringToIntList(information(1));
    val hints = fromStingsToCharList(information.slice(4, s+4));
    val sumShips = NumberOfShips(ships);
    val sumShipParts = NumberOfShipParts(vertical);

    private def MapOfShips(text:String):Map[Int,Int] =
    {
      val whioutSpace = text split " ";
      val whitoutWord = whioutSpace.slice(1,whioutSpace.length);
      var map:Map[Int,Int] = Map();
      for(i <- 0 to whitoutWord.length- 1)
      {
        val shipSet = whitoutWord(i);
        val seperated = shipSet split "x";
        map += (seperated(1).toInt -> seperated(0).toInt);
      }

      return map;
    }
    private def NumberOfShips(map:Map[Int,Int]): Int =
    {
      return map.foldLeft(0.0)(_+_._2).toInt;
    }

    private def NumberOfShipParts(l:List[Int]):Int =
    {
      return l.foldLeft(0)(_ + _);
    }

    private def fromStringToIntList(text:String):List[Int] =
    {
      val whioutSpace = text split " ";
      val whitoutWord = whioutSpace.slice(1,whioutSpace.length);
      var intList:List[Int] = List();
      for(i <- 0 to size- 1)
      {
        intList = intList :+ whitoutWord(i).toInt;
      }

      return intList;
    }

    private def fromStingsToCharList(hints:List[String]):List[List[Char]] =
    {
      var hints2D:List[List[Char]] = List()

      for(i <- 0 to size -1)
      {
        val splitString = hints(i) split " ";
        var row:List[Char] = List();
        //print(splitString(1));
        for(j <- 0 to size -1)
        {
          val c =  splitString(j);
          row = row :+ c.charAt(0);

        }
        hints2D = hints2D :+ row;
      }
      return hints2D;
    }


  }
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

  val unsolved_puzzle_path = "puzzle_unsolved.txt"
  val solved_puzzle_path = "puzzle_solved.txt"

  val sol = solveBattleShip(unsolved_puzzle_path);
  writeSolutionTofile(sol,solved_puzzle_path);

  def solveBattleShip(path:String): String =
  {
    val fileName = getClass.getResourceAsStream(path);
    import scala.io.Source;
    val lines = Source.fromInputStream(fileName).getLines()


    val nrOfPuzzlesString = lines.next();
    val nrOfPuzzleParts = nrOfPuzzlesString split " ";
    var allPuzzles:List[Puzzle] = List();
    var sol = "puzzles " + nrOfPuzzleParts(1).toInt + "\n"

    //creates number of puzzle items equal to puzzles given
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


    //forlop runs equal times to nuber of puzzles given
    for( puzzle <-allPuzzles) {

      var allSquares = List[Square]();
      for (xValue <- 0 to puzzle.size - 1) {
        for (yValue <- 0 to puzzle.size - 1) {
          val s = new Square(xValue, yValue)
          allSquares = allSquares :+ s;

          if (puzzle.hints(xValue)(yValue) != '?') {
            if (puzzle.hints(xValue)(yValue) == '-') {
              allSquares = allSquares.filter(_ != s);
              val s2 = s.removeValue('S');
              allSquares = allSquares :+ s2;
            } else {
              allSquares = allSquares.filter(_ != s);
              val s2 = s.removeValue('-');
              allSquares = allSquares :+ s2;
            }
          }
        }
      }


      bruteForce(puzzle);

      //write solution of singe puzzle to file
      sol += "size " + puzzle.size + "x" + puzzle.size + "\n"
      for(x <- 0 to puzzle.size-1)
      {
        for(y <- 0 to puzzle.size-1)
        {
          sol += "" + getSquare(x,y).possibleValues(0) + " "
        }
        sol += "\n"
      }

      //bruteforce runs until the puzzle is solved
      def bruteForce(p:Puzzle): Unit = {
        var solved = false
        while(!solved)
        {
          for(x<- 0 to p.size -1){
            for(y<- 0 to p.size -1){
              for(s<- List('S','-')){
                if(!getSquare(x,y).isSolved)
                {
                  reomveIfNotValied(x,y,s);
                }
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
      // removeIfNotVlaied checks is a solution, if it is not the posibility is removed from the given box
      def reomveIfNotValied(x:Int,y:Int,solution:Char): Unit = {
        if(!isValid(x,y,solution)){

          removeValue(x,y,solution);
        }
      }
      //removeValue removes a posible value form a square
      def removeValue(x:Int,y:Int,wrongSolution:Char) = {
        val s = getSquare(x,y);
        if(!s.isSolved)
        {
          allSquares = allSquares.filter(_ !=s);
          val s2 = s.removeValue(wrongSolution);
          allSquares = allSquares :+ s2;
        }

      }
      //isValid runs all the checks to se if a given solution is valid
      def isValid(x:Int,y:Int,solution:Char):Boolean = {
        val box = getSquare(x,y);

        if(MustBeWater(box) && solution == 'S')return false;
        if(MustBeBoat(box)  && solution == '-')return false;
        if(nextToShipPartHint(box) && solution == '-') return false;
        if(WrongsideOfPartHint(box) && solution == 'S') return false;
        if(sInCorner(box)    && solution == 'S')return false;
        if(!romeForShip(box) && solution == 'S')return false;
        if(!canBeSingle(box) && solution == 'S')return false;
        if(CantBeShip(box) && solution == 'S')return false;

        return true;
      }
      //canBeSingle checks is there can be places new 1x1 ships and and if the given ship can be
      def canBeSingle(box:Square):Boolean = {
        val single = allSquares.filter(p => (nextToNoShipsAndNoEmpty(p) && p.isSolved && p.possibleValues(0) == 'S'));
        val fullOfSingels = (single.size == puzzle.ships(1));
        if(fullOfSingels) //There are already full of 1x1 ships
        {
          if(nextToNoShipsAndNoEmpty(box)) //The Box is isolated
          {
            return false
          }
        }

        return true
      }

      //gets all ship parts next to the given square
      def getPosibleShip(box:Square, boxes:List[Square]): List[Square] = {
        var S:List[Square] = List[Square]();

        if(box.y < puzzle.size - 1) //checking right
        {
          if(!getSquare(box.x , box.y +1).isSolved || (getSquare(box.x ,box.y +1).isSolved && getSquare(box.x ,box.y +1).possibleValues(0) == 'S'))S = S :+ getSquare(box.x ,box.y +1);
        }
        if(box.y > 0) // checking left
        {
          if(!getSquare(box.x ,box.y -1).isSolved  || (getSquare(box.x ,box.y -1).isSolved && getSquare(box.x ,box.y -1).possibleValues(0) == 'S')) S = S :+ getSquare(box.x ,box.y-1);
        }
        if(box.x < puzzle.size - 1) //checking bottom
        {
          if(!getSquare(box.x +1,box.y).isSolved   || (getSquare(box.x +1, box.y).isSolved && getSquare(box.x +1,box.y).possibleValues(0) == 'S')) S = S :+ getSquare(box.x +1,box.y);
        }
        if(box.x > 0) //checking top
        {
          if(!getSquare(box.x -1,box.y).isSolved || (getSquare(box.x -1,box.y).isSolved && getSquare(box.x -1,box.y).possibleValues(0) == 'S')) S = S :+ getSquare(box.x -1 ,box.y);
        }
        val thusFar:List[Square] = List(box) ::: boxes ::: S;
        val next = S diff (boxes ::: List(box))
        var answer = thusFar;
        if(next.size > 0)
        {
          for(p <- S)
          {
            answer = answer ::: getSNextTo(p,thusFar.distinct)
          }
        }
        return answer.distinct;

      }


      def CantBeShip(box:Square):Boolean =
      {
        for((k,p) <- puzzle.ships)
        {
          if(k >= 3)
          {
            if(FulleOfTypeOfShip(k))//full of sertan size
            {
              if(!box.isSolved)
              {
                if(getSNextTo(box,List()).size == k)
                {
                  return true
                }
              }
            }
          }
        }
        return false
      }

      def FulleOfTypeOfShip(x:Int): Boolean =
      {
        val ships = GetFinishedShips();
        var number = 0;
        for(ship <- ships)
        {
          if( x == ship.size)
          {
            number = number +1
          }
        }
        if(puzzle.ships(x) == number)return true;

        return false;
      }
      def GetFinishedShips():List[List[Square]]=
      {
        var finishedSquares  = allSquares.filter(p => (p.isSolved && p.possibleValues(0) == 'S'));
        var ships:List[List[Square]] =List()
        while(finishedSquares.size > 0)
        {
          val ship = getSNextTo(finishedSquares(0), List())

          if(ship.size > 1)
          {
            if(ship(0).x == ship(1).x) //in the samme vertical line
            {
              val shipSorted = ship.sortWith( _.y < _.y)
              val okLeft = (shipSorted(0).y == 0 || (getSquare(shipSorted(0).x,shipSorted(0).y -1).isSolved && getSquare(ship(0).x,shipSorted(0).y -1).possibleValues(0) == '-'))
              val okRight = (shipSorted(shipSorted.size -1).y == puzzle.size -1 ||(getSquare(shipSorted(0).x, ship(shipSorted.size -1).y +1).isSolved && getSquare(shipSorted(0).x, shipSorted(shipSorted.size -1).y +1).possibleValues(0) == '-'))
              if(okLeft && okRight)
              {
                ships = ships :+ shipSorted
              }

            }else // in same horisontal line
            {
              val shipSorted = ship.sortWith( _.x < _.x)
              val okOnTop = (shipSorted(0).x == 0 || (getSquare(shipSorted(0).x -1,shipSorted(0).y).isSolved && getSquare(shipSorted(0).x -1,shipSorted(0).y).possibleValues(0) == '-'))
              val okOnBot = (shipSorted(shipSorted.size -1).x == puzzle.size -1 || (getSquare(shipSorted(shipSorted.size -1).x +1, shipSorted(0).y).isSolved && getSquare(shipSorted(shipSorted.size -1).x +1, shipSorted(0).y).possibleValues(0) == '-'));
              if(okOnTop && okOnBot)
              {
                ships = ships :+ shipSorted
              }
            }
          }
          finishedSquares = finishedSquares diff ship
        }
        return ships;
      }

      //the square given is next to no ships and no unsolved squares
      def nextToNoShipsAndNoEmpty(box:Square):Boolean ={
        if(box.x < puzzle.size - 1) //checking right
        {
          if(getSquare(box.x +1,box.y).isSolved && getSquare(box.x +1,box.y).possibleValues(0) == 'S')return false;
          if(!getSquare(box.x +1,box.y).isSolved)return false
        }
        if(box.x > 0) // checking left
        {
          if(getSquare(box.x -1,box.y).isSolved && getSquare(box.x -1,box.y).possibleValues(0) == 'S')return false;
          if(!getSquare(box.x -1,box.y).isSolved)return false
        }
        if(box.y < puzzle.size - 1) //checking bottom
        {
          if(getSquare(box.x,box.y +1).isSolved && getSquare(box.x ,box.y +1).possibleValues(0) == 'S')return false;
          if(!getSquare(box.x,box.y +1).isSolved)return false
        }
        if(box.y > 0) //checking top
        {
          if(getSquare(box.x ,box.y -1).isSolved && getSquare(box.x ,box.y -1).possibleValues(0) == 'S')return false;
          if(!getSquare(box.x ,box.y -1).isSolved)return false
        }
        return true
      }
      //The square is nest to at least one solved square that is a ship
      def nextToShip(box:Square):Boolean = {
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

        return false;
      }
      //The square is next to a hint indicating that the should be a ship in the given square
      def nextToShipPartHint(box:Square): Boolean = {
        if(box.x > 1)
        {
          if(puzzle.hints(box.x -1)(box.y) == 'A')return true;
        }
        if(box.x < puzzle.size -1)
        {
          if(puzzle.hints(box.x +1)(box.y) == 'V')return true;
        }
        if(box.y > 1)
        {
          if(puzzle.hints(box.x )(box.y -1) == '<')return true;
        }
        if(box.y < puzzle.size -1)
        {
          if(puzzle.hints(box.x )(box.y +1) == '>')return true;
        }
        return false;
      }
      //the square is on the opposite side of a hint meaning it must be vater
      def WrongsideOfPartHint(box:Square):Boolean = {

        if(box.x > 1)
        {
          if(puzzle.hints(box.x -1)(box.y) == 'V')return true;
          if(puzzle.hints(box.x -1)(box.y) == '*')return true;
        }
        if(box.x < puzzle.size -1)
        {
          if(puzzle.hints(box.x +1)(box.y) == 'A')return true;
          if(puzzle.hints(box.x +1)(box.y) == '*')return true;
        }
        if(box.y > 1)
        {
          if(puzzle.hints(box.x )(box.y -1) == '>')return true;
          if(puzzle.hints(box.x )(box.y -1) == '*')return true;
        }
        if(box.y < puzzle.size -1)
        {
          if(puzzle.hints(box.x )(box.y +1) == '<')return true;
          if(puzzle.hints(box.x )(box.y +1) == '*')return true;
        }
        return false;
      }
      //checks if is is possible to place new ships or the given square is next to another ship
      def romeForShip(box:Square):Boolean = {
        if(CanPlaceMoreShips() || !nextToNoShipsAndNoEmpty(box))return true
        //|| nextToShip(box)

        return false;
      }
      ///checks if is is possible to place new ships
      def CanPlaceMoreShips():Boolean ={
        val shipParts = allSquares.filter(x => x.isSolved && x.possibleValues(0) == 'S')

        val shipPartsNumber = trimList(shipParts);

        if(shipPartsNumber == puzzle.sumShips)
        {
          return false
        }

        return true;
      }
      //  removes contected ship-parts from a list and counts whole ships
      def trimList(ships:List[Square]):Int = {
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
      //gets all ship parts next to the given square
      def getSNextTo(box:Square, boxes:List[Square]): List[Square] = {
        var S:List[Square] = List[Square]();

        if(box.y < puzzle.size - 1) //checking right
        {
          if(getSquare(box.x , box.y +1).isSolved && getSquare(box.x ,box.y +1).possibleValues(0) == 'S')S = S :+ getSquare(box.x ,box.y +1);
        }
        if(box.y > 0) // checking left
        {
          if(getSquare(box.x ,box.y -1).isSolved && getSquare(box.x ,box.y-1).possibleValues(0) == 'S') S = S :+ getSquare(box.x ,box.y-1);
        }
        if(box.x < puzzle.size - 1) //checking bottom
        {
          if(getSquare(box.x +1,box.y).isSolved && getSquare(box.x +1,box.y ).possibleValues(0) == 'S') S = S :+ getSquare(box.x +1,box.y);
        }
        if(box.x > 0) //checking top
        {
          if(getSquare(box.x -1,box.y).isSolved && getSquare(box.x -1,box.y).possibleValues(0) == 'S') S = S :+ getSquare(box.x -1 ,box.y);
        }
        val thusFar:List[Square] = List(box) ::: boxes ::: S;
        val next = S diff (boxes ::: List(box))
        var answer = thusFar;
        if(next.size > 0)
        {
          for(p <- S)
          {
            answer = answer ::: getSNextTo(p,thusFar.distinct)
          }
        }
        return answer.distinct;

      }
      //checks if the row and colum is full of boats and must therefor be water
      def MustBeWater(box:Square):Boolean = {
        var yCounter  = 0;
        var xCounter = 0;

        for(size <- 0 to puzzle.size -1)
        {
          if(getSquare(size,box.y).isSolved && getSquare(size,box.y).possibleValues(0) == 'S') yCounter = yCounter +1;
          if(getSquare(box.x,size).isSolved && getSquare(box.x,size).possibleValues(0) == 'S') xCounter = xCounter +1;
        }
        if(yCounter == puzzle.vertical(box.y))return true;
        if(xCounter == puzzle.horizontal(box.x))return true;

        return false
      }
      //checks if the remaining squares must be boats to fill row
      def MustBeBoat(box:Square):Boolean = {
        val xLine = getAllFromX(box.x)
        val yLine = getAllFromY(box.y)

        var xS = 0;
        var xVater = 0

        var yS = 0;
        var yVater = 0

        for(i <- 0 to puzzle.size -1)
        {
          if(xLine(i).isSolved)
          {
            if(xLine(i).possibleValues(0) == 'S')xS = xS +1;
            else{xVater = xVater +1}
          }
          if(yLine(i).isSolved)
          {
            if(yLine(i).possibleValues(0) == 'S')yS = yS +1;
            else{yVater = yVater +1}
          }
        }

        val xEmpty = puzzle.size - (xS +xVater)
        val yEmpty = puzzle.size - (yS +yVater)

        if(xEmpty == (puzzle.horizontal(box.x)-xS))return true
        if(yEmpty == (puzzle.vertical(box.y)-yS))return true
        return false;
      }
      //checks if there is a shippart in the corner and must therefore be water
      def sInCorner(box:Square):Boolean = {
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
      //prints the gamebord
      def printIt() = {
        implicit def intToSqrt = ((x: Double) => x.toInt)

        val size = puzzle.size;
        for (x <- 0 to size - 1) {
          val values = for (y <- 0 to size - 1) yield getSquare(x, y).possibleValues
          println(values.map(i => if (i.length == 1) i(0) else "_").mkString(" "));
        }

      }
      //gets all boxes with given X
      def getAllFromX(x:Int):List[Square] = {
        return allSquares.filter((s:Square) => s.x==x)
      }
      //gets all boxes with given Y
      def getAllFromY(y:Int):List[Square] = {
        return allSquares.filter((s:Square) => s.y==y)
      }
      //gets spesified square by x and y value
      def getSquare(x:Int,y:Int):Square   = {
        return allSquares.filter(_.x==x).filter(_.y==y)(0);
      }
    }

    return sol;
  }

  //writes solution to file
  def writeSolutionTofile(obj:String, path:String):Unit =
  {
    val out = new BufferedWriter(new FileWriter(new File(path)))
    out.write(obj)
    out.close()
  }

}








