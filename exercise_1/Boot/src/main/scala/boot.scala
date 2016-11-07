import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

object boot{

  def main(args: Array[String]) {

    test(50,0) // test(numberOfTeam: number, Length of Track: meter)

  }

  val cars = scala.collection.mutable.ArrayBuffer.empty[car] ;
  val ranges = scala.collection.mutable.ArrayBuffer.empty[Double]
  val preRan = scala.collection.mutable.ArrayBuffer.empty[Double]
  var numCar: Int = 0
  var distant: Double = 0
  var done: Int = 0;
  var timecount: Int = 0

  var carDis = (i:Double,j:Double) => (i+j) // calculate distance per time
  var diff = (i:Double,j:Double) => Math.abs(i-j)

  // checkRange() use to check range between each car that less than 10 meter or not, then return Boolean .
  def checkRange(x: Int): Boolean = {
    var check = false
    for(i<-0 to numCar-1){
      //if distance between car less than 10 and there are not same car and car is not finish
      if(diff(preRan(x),preRan(i))<=10 && x != i && cars(i).done){
        check = true
        Breaks
      }else{
        check
      }
    }
    check
  }

  // checkAllDone() use to check that every team pass the finished line.
  def checkAllDone(): Boolean = {
    var check = false
    for(i<-0 to numCar-1){
      if(cars(i).done){
        check = true
        Breaks
      }
    }
    check
  }

  // test fucntion, input number of team and length track
  def test(x: Int,y: Int){

    numCar = x
    distant = y

    //create car equal to number of team
    for(i<-1 to numCar){
      cars+=new car(i);
    }

    //add the car in their track
    for(i<-0 to numCar-1){
      ranges+=(-200)*i
      preRan +=(-200)*i
    }

    println("Rank, Team, Time, Speed")

    // Start competition, Done when all car are finished
    while(checkAllDone()){
      for(i<-0 to numCar-1) {
        // check car pass the gold or not
        if (preRan(i) < distant) {
          //check car is the lastest or not, and it already use nitro or not
          if (preRan.min == preRan(i) && cars(i).nitro) {
            cars(i).useNitro()
            ranges(i) += carDis(cars(i).speed, cars(i).nextSpeed)
          } else if (checkRange(i)==true) { // check there has other car in 10 meter or not
            cars(i).drowSpeed()
            ranges(i) += carDis(cars(i).speed, cars(i).nextSpeed)
          } else {
            cars(i).calNextSpeed()
            ranges(i) += carDis(cars(i).speed, cars(i).nextSpeed)
          }
          // set new speed
          cars(i).setSpeed()
        }else{ // if car pass the gold
          if(cars(i).done){
            done+=1  //count the rank
            cars(i).done = false // set it finish
            cars(i).rank = done // set rank
            cars(i).timeDone = timecount // set time finish
            cars(i).setSpeed() // set the lastest speed
            println(cars(i).rank+",     "+i+",   "+cars(i).timeDone+"s,  "+cars(i).speed+" m/s")
          }
        }
      }
      for(i <- 0 to numCar-1) { // ser the previous speed
        preRan(i) = ranges(i)
      }
      timecount+=2
    }
  }

  //car class
  class car(i : Int){
    val topspeed: Double = (((150+(10*i))*10)/36); // set top speed and convert Km/hr to m/s
    val a: Double = 2*i; // set Acceleration
    val hf: Double = 0.8; // set handle factor
    var speed: Double = 0;
    var nitro: Boolean = true;
    var nextSpeed: Double = 0
    var done: Boolean = true
    var rank: Int = 0
    var timeDone: Int = 0

    // calculate speed fucntion
    def calNextSpeed(){
      var tempSpeed: Double = speed+(a*2);
      // check new speed less than top speed or not
      if(tempSpeed<=topspeed){
        nextSpeed=tempSpeed;
      }else {
        // set new speed equal to top speed
        nextSpeed=topspeed;
      }
    }

    // calculate speed when using nitro
    def useNitro(){
      // if speed = 0
      if(speed == 0){
        //set new speed = 2*Acceleration
        var tempSpeed: Double = 2*a;
      }
      var tempSpeed: Double = speed*2;
      //check new speed less than top speed or not
      if(tempSpeed<=topspeed){
        nextSpeed=tempSpeed;
      }else {
        nextSpeed=topspeed;
      }
      nitro = false
    }

    //set speed = new speed
    def setSpeed(){
      speed = nextSpeed;
    }

    // calculate speed when get handle factor
    def drowSpeed(){
      // check speed more than 1 or not
      if(speed>1) {
        nextSpeed = speed * hf
      }else{ // if not set speed = 1
        nextSpeed = 1
      }
    }

  }
}

