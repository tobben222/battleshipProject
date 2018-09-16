class Puzzle(s:Int, information:List[String]) {
  val size = s;
  val ships = MapOfShips(information(0));
  val horizontal = fromStringToIntList(information(1));
  val vertical = fromStringToIntList(information(2));
  val hints = fromStingsToCharList(information.slice(4, s+4));
  val sumShips = NumberOfShips(ships);

  private def MapOfShips(text:String):Map[Int,Int] =
  {
    val whioutSpace = text split " ";
    val whitoutWord = whioutSpace.slice(1,whioutSpace.length);
    var map:Map[Int,Int] = Map();
    for(i <- 0 to whitoutWord.length- 1)
    {
      val shipSet = whitoutWord(i);
      val seperated = shipSet split "x";
      map += (seperated(0).toInt -> seperated(1).toInt);
    }

    return map;
  }
  private def NumberOfShips(map:Map[Int,Int]): Int =
  {
    return map.foldLeft(0.0)(_+_._2).toInt;
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

