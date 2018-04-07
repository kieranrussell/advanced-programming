import scala.io.Source

/**
  * Created by Kieran on 04/04/2018.
  */
object ApplicationTestFileRead extends App {
  var data: Map[String, List[(Int, Int)]] = Map()
  data = Source.fromFile("C:/Users/Kieran/Documents/Uni/advance-programming/src/data/weatherdata14March.txt").getLines().map {
    dataSet => dataSet.split(",")
  }.map {
    array => array.head -> array.tail.map {
      pairs => pairs.split(":")
    }.map {
      stringNumbers => (stringNumbers(0).toInt, stringNumbers(1).toInt)
    }.toList
  }.toMap

  println(data)
}

//
//var data: Map[String, List[(Int, Int)]] = Map()
//
//data = Source.fromFile("C:/Users/Kieran/Documents/Uni/advance-programming/src/data/weatherdata14March.txt").getLines().map {
//  dataSet => dataSet.split(",")
//}.map {
//  array => Map("name" -> array.head, "values" -> array.tail.map {
//    string => string.split(":").map {
//      stringNumber => stringNumber.toInt
//    }.toList
//  }.toList)
//}.map {
//  keyValue => keyValue.get("name").toString -> keyValue.get("values")
//}.toMap