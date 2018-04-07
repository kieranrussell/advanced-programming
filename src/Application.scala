import scala.collection.{Map, SortedMap}
import scala.io.Source

/**
  * Created by Kieran on 04/04/2018.
  */
object Application extends App {
  var weatherData: Map[String, List[(Int, Int)]] = getWeatherData

  startApplication()

  def getWeatherData: Map[String, List[(Int, Int)]] = {
    var weatherData: Map[String, List[(Int, Int)]] = SortedMap()
    try{
      weatherData = Source.fromFile("C:/Users/Kieran/Documents/Uni/advance-programming/src/data/weatherdata14March.txt").getLines().map {
        dataSet => dataSet.split(",")
      }.map {
        array => array.head -> array.tail.map {
          pairs => pairs.split(":")
        }.map {
          stringNumbers => (stringNumbers(0).toInt, stringNumbers(1).toInt)
        }.toList
      }.toMap
    } catch{
      case e: Exception => println("Failed to read and transform data.", e.getStackTrace)
    }

    weatherData
  }

  def startApplication(): Unit = {
    var option = ""
    do {
      printMenu()
      option = readOption
    } while (executeSelectedOption(option))
  }

  def executeSelectedOption(option: String): Boolean = getMenuOptions.get(option.toInt) match {
    case Some(f) => f(weatherData)
    case None => printInvalidEntry
  }

  def printInvalidEntry(): Boolean = {
    println("Invalid entry\n")
    true
  }

  def printMenu() {
    println(
      """|Please select one of the following:
        |1.	Get the most recent temperature values recorded for each city i.e. last year’s.
        |2.	Get the difference between minimum and maximum temperatures for each year for each city. A new map structure of type Map[String, List[Int]] could be used to store the differences.
        |3.	Get the average (mean) difference between minimum and maximum temperatures for each city
        |4.	Get the greatest difference in minimum and maximum temperatures for each year for each city
        |5.	Allow the user to input a set of potential city destinations and construct a summary which maps the city name to last year’s recorded temperature values - the summary should also display the overall average maximum and minimum temperatures for the chosen itinerary.""".stripMargin)
  }

  def readOption: String = {
    val line = readLine("\nPlease enter option:")
    println("\n")
    line
  }

  def getMenuOptions: Map[Int, (Map[String, List[(Int, Int)]]) => Boolean] = {
    Map(
      1 -> handle1,
      2 -> handle2,
      3 -> handle3,
      4 -> handle4,
      5 -> handle5
    )
  }

  def getLatestTemperature(data: Map[String, List[(Int, Int)]]) = {
    data.map {
      case (name, values) => Map(name -> values.last)
    }
  }

  def printKeyValue(latestTemps: Iterable[Map[String, (Int, Int)]]) = {
    val latestTempsMap = latestTemps.map {
      pair => pair.keys.head + ": " + pair.get(pair.keys.head).toList.mkString(",")
    }.toList.mkString("\n")
    println(latestTempsMap)
    println()
  }

  def handle1(data: Map[String, List[(Int, Int)]]): Boolean = {
    printKeyValue(getLatestTemperature(data))
    true
  }
  def handle2(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("Executing method handler 2")
    true
  }
  def handle3(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("Executing method handler 3")
    true
  }
  def handle4(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("Executing method handler 4")
    true
  }
  def handle5(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("Executing method handler 5")
    true
  }
  def handle6(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("Executing method handler 6")
    true
  }
}