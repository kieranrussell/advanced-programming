import scala.collection.{Map, SortedMap}
import scala.io.Source

/**
  * Created by Kieran on 04/04/2018.
  */
object Application extends App {
  //Partial function application of getWeatherData to pass in current file location
  var getWeatherDataFromDataDir = getWeatherData("C:/Users/Kieran/Documents/Uni/advance-programming/src/data/weatherdata14March.txt")

  //Setup Map variable to store weather data and call partial function to read and parse file
  var weatherData: Map[String, List[(Int, Int)]] = getWeatherDataFromDataDir

  //Call start application function which powers the menu and option handling
  startApplication()

  //Function wich takes a file path as a parameter. Reads the file and parses it to A Map with City name as the key and A list of tuples as the value
  def getWeatherData(path: String): Map[String, List[(Int, Int)]] = {
    //Varible to store weather Data
    var weatherData: Map[String, List[(Int, Int)]] = SortedMap()

    //Exception handling incase no file found or unable to parse file
    try {
      //Read lines from file and parse into required data structure
      weatherData = Source.fromFile(path).getLines().map {
        dataSet => dataSet.split(",")
      }.map {
        array => array.head -> array.tail.map {
          pairs => pairs.split(":")
        }.map {
          stringNumbers => (stringNumbers(0).toInt, stringNumbers(1).toInt)
        }.toList
      }.toMap
    } catch {
      case e: Exception =>
        //If file not found or invalid then ask user to input path to file and recurse to parse
        println("Failed to read and transform data.", e.getStackTrace)
        //Function composition to read path from user and send that to getWeatherData function
        return getWeatherData(readOption)
    }

    //Return weather data
    weatherData
  }

  //Function to run application menu etc.
  def startApplication(): Unit = {
    var option = ""
    do {
      //Print menu options on screen
      printMenu()
      //Read user input
      option = readOption

      //Request user input until Quit option is selected
    } while (executeSelectedOption(option))
  }

  //Function which takes a string option, parses it to an int and pattern matches to call the corresponding handler function
  def executeSelectedOption(option: String): Boolean = getMenuOptions.get(option.toInt) match {
    case Some(f) => f(weatherData)
    case None => printInvalidEntry
  }

  //Function to inform user of invalid entry: returns true to keep the application alive
  def printInvalidEntry(): Boolean = {
    println("Invalid entry\n")
    true
  }

  //Function to print the available menu option to the user
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

  //Function which reads the user option from the command line as a string
  def readOption: String = {
    val line = readLine("\n:::>")
    println("\n")
    line
  }

  //Function which returns a map of option to handler
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

  //Given a parameter of data this function returns a city's name and the last temperature recorded
  def getLatestTemperature(data: Map[String, List[(Int, Int)]]) = {
    data.map {
      case (name, values) => Map(name -> values.last)
    }
  }

  //Given a parameter of data this function will return a map of city's name to temperature differences. Calculated by subtracting the min from the max per year
  def getTemperateDifference(data: Map[String, List[(Int, Int)]]): Map[String, List[Int]] = {
    data.map {
      case (name, values) => name -> values.map {
        case (min, max) => max - min
      }
    }.toMap
  }

  //Given a parameter of city's name to temperature differences,
  //this function returns a map of city's name to average of differences
  def getMeanAverageTemperatures(stringToInts: Map[String, List[Int]]): Map[String, List[Double]] = {
    stringToInts.map {
      case (name, values) => name -> List[Double](values.sum.toDouble / values.length)
    }.toMap
  }

  //Given a parameter of city's name to temperature differences,
  //this function returns a map of city's name to min/max temperature differences
  def getMinMaxTemperatures(stringToInts: Map[String, List[Int]]): Map[String, List[Int]] = {
    stringToInts.map {
      case (name, values) => name -> List(values.min, values.max)
    }.toMap
  }

  //Given a parameters o city to min/max temperature this function returns a map of city's name to the max temperature value only
  def getGreatestOnly(cityToMinMaxDifferences: Map[String, List[Int]]): Map[String, List[Int]] = {
    cityToMinMaxDifferences.map {
      case (name, values) => name -> List(values.max)
    }.toMap
  }

  //Given a city name, latest temperature and max/min temperatures are returned in a map to city name
  def getSummary(city: String, latestTemperature: Iterable[Map[String, (Int, Int)]], minMaxTemperatures: Map[String, List[String]]) = {
    val latestTempsTuple = latestTemperature.filter(_.contains(city)).flatMap(_.get(city)).toList.head
    val latestTempsAsList = List(latestTempsTuple._1, latestTempsTuple._2)
    val minMaxTemps = minMaxTemperatures.get(city).toList.head

    Map(city -> Map("latest" -> latestTempsAsList, "exceptions" -> minMaxTemps))
  }

  //Given the weathr data set this function will return a map of City name to Average Min/Max temperature
  def getAverageMinMax(weatherData: Map[String, List[(Int, Int)]]): Map[String, List[String]] = {
    weatherData.map {
      case (cityName, values) => cityName -> List(
        values.foldLeft(0) { (acc, tup) => acc + tup._1 } / values.length.toDouble,
        values.foldLeft(0) { (acc, tup) => acc + tup._2 } / values.length.toDouble
      ).map {
        double => roundToTwoPlaces(double)
      }
    }.toMap
  }

  //Helper function for rounding Decimals to 2 decimal places. As String
  def roundToTwoPlaces(value: Double): String = {
    "%.2f".format(value)
  }

  //Partial application of getSummary function. Passing in requited data for latest and min/max temperatures.
  //getSummaryForCity accepts a single parameter of city name
  def getSummaryForCity(city: String): Map[String, Map[String, List[Any]]] = {
    getSummary(city, getLatestTemperature(weatherData), getAverageMinMax(weatherData))
  }

  //Turns a List of Ints which are a Map option to a string value of comma seperated values
  def arrayToString(list: Option[List[Any]]): String = {
    list.toList.flatten.mkString(", ")
  }

  //Function which constructs the message to print for each summary given a map of city name to summary details
  def generateSummaryText(summary: Map[String, Map[String, List[Any]]]) = {
    val summaryText = summary.map {
      case (city, summary) =>
        city + ":\n Latest: " + arrayToString(summary.get("latest")) + "\n Min/Max: " + arrayToString(summary.get("exceptions"))
    }.mkString("\n")

    println(summaryText)
    println()
  }

  //Function which takes a list og cities and calls getSummaryForCity and then calls the generateSummaryText function to print he summary on screen
  def printSummaryForEachCity(cities: List[String], data: Map[String, List[(Int, Int)]]) = {
    cities.map {
      city => getSummaryForCity(city)
    }.foreach(summary => generateSummaryText(summary))
  }

  //Request user input from user in order to get list of cities.
  //Reads them in as a string, parses to an array, and returns only the ones which exist in the loaded data set
  def getListOfCitiesFromUser(data: Map[String, List[(Int, Int)]]) = {
    var cities: List[String] = List()

    try {
      cities = readLine("\nPlease enter a list of cites as comma separated values: ").split(",").map(_.trim).toList
    } catch {
      case e: Exception => println("Malformatted Input. Please try again.", e.getStackTrace)
    }

    cities.filter(city => data.contains(city))
  }

  //Helper function to print a Tuple as a value in a map
  def printKeyValueTuple(latestTemps: Iterable[Map[String, (Int, Int)]]) = {
    val latestTempsMap = latestTemps.map {
      pair => pair.keys.head + ": " + pair.get(pair.keys.head).toList.mkString(",")
    }.toList.mkString("\n")
    println(latestTempsMap)
    println()
  }

  //Helper function to print the key and the value given a map of string to list of ints
  def printIntKeyValue(stringToInts: Map[String, List[Int]]) = {
    val stringKeyValueMap = stringToInts.map {
      case (key, value) =>
        key + ": " + value.toList.mkString(", ")
    }.toList.mkString("\n")
    println(stringKeyValueMap)
    println()
  }

  //Helper function to print the key and the value given a map of string to list of doubles
  def printDoubleKeyValue(stringToInts: Map[String, List[Double]]) = {
    val stringKeyValueMap = stringToInts.map {
      case (key, value) =>
        key + ": " + value.toList.map("%.2f".format(_)).mkString(", ")
    }.toList.mkString("\n")
    println(stringKeyValueMap)
    println()
  }

  def handle1(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("City Name | Latest Temperature (Low, High)")
    printKeyValueTuple(getLatestTemperature(data))
    true
  }

  def handle2(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("City Name | Difference per year")
    printIntKeyValue(getTemperateDifference(data))
    true
  }

  def handle3(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("City Name | Average Difference")
    printDoubleKeyValue(getMeanAverageTemperatures(getTemperateDifference(data)))
    true
  }

  def handle4(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("City Name | Greatest Difference")
    printIntKeyValue(getGreatestOnly(getMinMaxTemperatures(getTemperateDifference(data))))
    true
  }

  //Handler for option 5 - summary - gets list of cities and then prints or asks user to re-enter their list if invalid (Recursive)
  def handle5(data: Map[String, List[(Int, Int)]]): Boolean = {
    val cities = getListOfCitiesFromUser(data)

    if (cities.nonEmpty) {
      println("Number of valid cities found: " + cities.length)
      printSummaryForEachCity(cities, data)
    } else {
      println("No valid cities entered. Please try again.")
      handle5(data)
    }

    true
  }

  def handle6(data: Map[String, List[(Int, Int)]]): Boolean = {
    println("Exiting Application...")
    println("Goodbye!")
    false
  }
}