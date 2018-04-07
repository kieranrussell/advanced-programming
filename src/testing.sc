import scala.collection.{Map, SortedMap}
import scala.io.Source

var weatherData: Map[String, List[(Int, Int)]] = SortedMap()

var data = Source.fromFile("C:/Users/Kieran/Documents/Uni/advance-programming/src/data/weatherdata14March.txt").getLines().map {
  dataSet => dataSet.split(",")
}.map {
  array => Map("name" -> array.head, "values" -> array.tail)
}.map {
  cityMap => cityMap.get("values").toArray.map {
    numberPairs => numberPairs.toString.split(":")
  }.map {
    stringsNumbers => stringsNumbers.map {
      string => string.toInt
    }
  }
}

data.length