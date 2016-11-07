import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import spray.routing.{Route, SimpleRoutingApp}

import scala.collection.mutable.ListBuffer
import com.typesafe.config.ConfigFactory

object Boot extends App with SimpleRoutingApp {

  implicit val system = ActorSystem()

  val hotelList= new listHotel() //List of Hotel detail from hoteldb.csv
  var client:Map[String,keyTime] = Map() //List of client ip and Map of api-key and current request time
  var blockClient:Map[String,keyTime] = Map() //List of client ip that was suspended and Map of api-key and current request time
  var apiKeyList:Map[String,Int] = Map() // List of Api key and rate per second
    apiKeyList += "10s"->10 // key for 1 request per 10 seconds
    apiKeyList += "5s"->5 // key for 1 request per 5 seconds
    apiKeyList += "1s"->1 // key for 1 request per 1 seconds

  startServer(interface = "localhost", port = 8080) {
    //route to get all hotel in hoteldb file
    get{
      path("listall") {
        ratelimit{
            var data: String = ""
            for (t <- hotelList.hlist) {
              data += t.showDetail
            }
            complete {
              data+"\n"
            }
        }
      }
    }~
    // route to search hotel by city name
      (path("search") & get) {
        ratelimit{
          parameters("name", "op" ?) {
            (name, op) =>
              var ops = ""
              if(op.isEmpty){ //check op that is empty or not
                ops = "None"
              }else{
                ops = op.get.toLowerCase
              }
              complete{
                searchHotel(name,ops)
              }
          }
        }
      }
  }

  // RateLimit function
  def ratelimit(route:Route): Route = {
    //get api key
    headerValueByName("api-key") { key =>
      // check api key
      validate(apiKeyList.contains(key), "Invalid API key\n") {
      // get client ip
       clientIP { ip =>
          // get request time
          var time: Long = java.lang.System.currentTimeMillis()
          // check client ip with blockClient list and check type of api key is match or not
          if (checkBlock(ip.toString(),apiKeyList(key))) {
            // check client is suspended or not
            if (TimeUnit.MILLISECONDS.toSeconds(time - blockClient(ip.toString()).kt(apiKeyList(key))) < 300) {
              // report that client is still suspended and remaining time
              complete {
                "Your request is exceeding the limite. Your api key will be suspended for " + timeleft(ip.toString, time,apiKeyList(key)) + " minites."
              }
            } else {
              // refuse client from client block list and client list
              blockClient(ip.toString()).kt -= apiKeyList(key)
              client(ip.toString()).kt -= apiKeyList(key)
              route
            }
          } else {
            // check client ip with client list and check type of api-key is match or not
            if (checkClient(ip.toString(),apiKeyList(key))) {
              // the different time between previous request and this request
              val difference: Long = TimeUnit.MILLISECONDS.toSeconds(time - client(ip.toString()).kt(apiKeyList(key)))
              // check different time over rate limited or not
              if (difference < apiKeyList(key)) {
                // add cilent api key and request time in to blockClient list of there api key
                blockClient(ip.toString()).kt += apiKeyList(key)->time
                // report that client is suspended and remaining time
                complete {
                  "Your request is exceeding the limite. Your api key will be suspended for " + timeleft(ip.toString, time,apiKeyList(key)) + " minites."
                }
              } else {
                // keep the new request time and route to function
                client(ip.toString()).kt += apiKeyList(key)->time
                route
              }
            } else {
               // check client already exist or not
              if(client.contains(ip.toString())) {
                client(ip.toString()).kt += apiKeyList(key)->time
              }else{
                client += ip.toString() -> new keyTime()
                blockClient += ip.toString() -> new keyTime()
              }
              route
            }
          }
        }
      }
    }
  }

  //checkBlock() check client was blocked or not
  def checkBlock(ip:String,key:Int): Boolean ={
    if(blockClient.contains(ip)){
      if(blockClient(ip).kt.contains(key)){
        true
      }else{
        false
      }
  }else{
      false
    }
  }

    // checkClient() check client was existed or not
  def checkClient(ip:String,key:Int): Boolean ={
    if(client.contains(ip)){
      if(client(ip).kt.contains(key)){
        true
      }else{
        false
      }
    }else{
      false
    }
  }

  // searchHotel function get city name and option parameter
  def searchHotel(name:String,op:String): String = {
    // get the search data from matchOp function
    var data:String = matchOp(name,op)
    // check data is emptr or not, if not report that Not found hotels in this city
    if (data.equals("")) data = "Not found hotels in this city\n"
    // return data
    data
  }
  // timeleft function report the suspended remaining time
  def timeleft(ip:String,time:Long,key:Int): String ={
    var re: String = ""
    // calculate the different time and change to Minute.second formant
    var difference:Long = TimeUnit.MILLISECONDS.toSeconds(time - blockClient(ip).kt(key))
    var m:Int = ((300-difference)/60).toInt
    var s:Int = (300-difference).toInt%60
    re = m+"."+s+"s"
    return re
  }

  // matchOp function match option input and return hotel data
  def matchOp(name: String,op:String): String = op match {
      // option is nothing
    case "None" => {
      var data =""
      for (t <- hotelList.searchList(name)) {
      data += t.showDetail
    }
      data
    }
      // option is asc
    case "asc" => {
      var data =""
      for (t <- hotelList.sortASC(name)) {
        data += t.showDetail
      }
      data
    }
    // option is desc
    case "desc" => {
      var data =""
      for (t <- hotelList.sortDESC(name)) {
        data += t.showDetail
      }
      data
    }
    // option is other
    case _ => {
      var data =""
      for (t <- hotelList.searchList(name)) {
        data += t.showDetail
      }
      data
    }
  }

}

// listhotel class contain hotel and fucntion to read csv file
class listHotel(){
  var hlist = ListBuffer[hotel]()
  //searchList search hotel from city name
  val searchList = (cn:String)=>hlist.filter(_.city.equalsIgnoreCase(cn))
  //sortASC search hotel from city name and sort with ASC option
  val sortASC = (cn:String) =>searchList(cn).sortWith(_.prince < _.prince)
  //sortDESC search hotel from city name and sort with DESC option
  val sortDESC = (cn:String) =>searchList(cn).sortWith(_.prince > _.prince)

  // read csv file and store data in hlist
  using(io.Source.fromFile("src/main/scala/hoteldb.csv")) { source =>
    for (line <- source.getLines) {
      var a:Array[String]  = line.split(",")
      if(!a(3).equals("PRICE"))
        hlist += new hotel(a(0),a(1).toInt,a(2),a(3).toInt)
    }
  }
  //using function open and close file
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

}

// hotel class contain hotel informantion
class hotel(val c: String, val h: Int, val r: String, val p: Int){
  var city: String = c
  var hotelId: Int = h
  var room: String = r
  var prince: Int = p

  // showDetail report all hotel information in string
  val showDetail = city+" "+hotelId+" "+room+" "+prince+"\n"
}

// KeyTime class contain Map[Key-type,request time]
class keyTime(){
  var kt: Map[Int,Long] = Map()
}