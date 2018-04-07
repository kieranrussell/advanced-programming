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
        |3.	Get the average (mean) difference between minimum and maximum temperatures for each city.
        |4.	Get the greatest difference in minimum and maximum temperatures for each year for each city.
        |5.	Allow the user to input a set of potential city destinations and construct a summary which maps the city name to last year’s recorded temperature values - the summary should also display the overall average maximum and minimum temperatures for the chosen itinerary.
        |6.	Exit.""".stripMargin)
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
      5 -> handle5,
      6 -> handle6
    )
  }

  def getLatestTemperature(data: Map[String, List[(Int, Int)]]) = {
    data.map {
      case (name, values) => Map(name -> values.last)
    }
  }

  def getTemperateDifference(data: Map[String, List[(Int, Int)]]): Map[String, List[Int]] = {
    data.map {
      case (name, values) => name -> values.map{
        case (min, max) => max - min
      }
    }.toMap
  }

  def getMeanAverageTemperatures(stringToInts: Map[String, List[Int]]): Map[String, List[Int]] = {
    stringToInts.map {
      case (name, values) => name -> List(values.sum / values.length)
    }.toMap
  }

  def getMinMaxTemperatures(stringToInts: Map[String, List[Int]]): Map[String, List[Int]] = {
    stringToInts.map {
      case (name, values) => name -> List(values.min, values.max)
    }.toMap
  }

  def printKeyValueTuple(latestTemps: Iterable[Map[String, (Int, Int)]]) = {
    val latestTempsMap = latestTemps.map {
      pair => pair.keys.head + ": " + pair.get(pair.keys.head).toList.mkString(",")
    }.toList.mkString("\n")
    println(latestTempsMap)
    println()
  }

  def printKeyValue(stringToInts: Map[String, List[Int]]) = {
    val stringKeyValueMap = stringToInts.map {
      case (key, value) =>
        key + ": " + value.toList.mkString(", ")
    }.toList.mkString("\n")
    println(stringKeyValueMap)
    println()
  }

  def handle1(data: Map[String, List[(Int, Int)]]): Boolean = {
    printKeyValueTuple(getLatestTemperature(data))
    true
  }

  def handle2(data: Map[String, List[(Int, Int)]]): Boolean = {
    printKeyValue(getTemperateDifference(data))
    true
  }

  def handle3(data: Map[String, List[(Int, Int)]]): Boolean = {
    printKeyValue(getMeanAverageTemperatures(getTemperateDifference(data)))
    true
  }

  def handle4(data: Map[String, List[(Int, Int)]]): Boolean = {
    printKeyValue(getMinMaxTemperatures(getTemperateDifference(data)))
    true
  }

  def getSummary(city: String, latestTemperature: Iterable[Map[String, (Int, Int)]], minMaxTemperatures: Map[String, List[Int]]) = {
    val latestTempsTuple = latestTemperature.filter(_.contains(city)).flatMap(_.get(city)).toList.head
    val tatestTempsAsList = List(latestTempsTuple._1,latestTempsTuple._2)
    val minMaxTemps = minMaxTemperatures.get(city).toList.head

    Map(city -> Map("latest" -> tatestTempsAsList, "exceptions" -> minMaxTemps))
  }

  def generateSummaryText(summary: Map[String, Map[String, List[Int]]]) = {
    val summaryText = summary.map{
      case (city, summary) =>
        city + ":\n Latest: " + summary.get("latest").mkString(",") + "\n Min/Max: " + summary.get("exceptions").mkString(",")
    }.mkString("\n")

    println(summaryText)
    println()
  }

  def handle5(data: Map[String, List[(Int, Int)]]): Boolean = {
    val city = "Sevilla"
    val summary = getSummary(city, getLatestTemperature(data), getMinMaxTemperatures(getTemperateDifference(data)))

    generateSummaryText(summary)
    true
  }

  def handle6(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("Exiting Application...")
    println("Goodbye!")
    System.exit(1)
    true
  }
}