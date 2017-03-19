/**
  * Created by Xiangguo.Kong on 2017/3/16.
  */


import java.net.Socket
import java.io.{BufferedReader,InputStreamReader,PrintStream}
import scala.collection.mutable.{ArrayBuffer}
//import java.lang.Thread

object YuTu {

  val road_map = new ArrayBuffer[Tuple4[Float,Float,Float, Float]]
  val current_road = new Array[Tuple4[Float,Float,Float, Float]](2)
  val angle = new Array[Float](2)
  var timer_cout:Int = 0
  var task_not_complete = true

  def main(args : Array[String]): Unit={
    val host = "localhost"
    val port = 8088
    val sock = new Socket(host,port)
    val is = new BufferedReader(new InputStreamReader(sock.getInputStream()))
    val os = new PrintStream(sock.getOutputStream())

    actors.Actor.actor{
      while(true){
        if(is.ready()){

          //recived road map from TianGong
          val output = is.readLine()
          println(output)
          if (output.startsWith("Task")){
            parseRoadMap(output)
            set_current_map(road_map)
            caluate_angle(current_road)

          }

          if (output.startsWith("Complete")) {
            task_not_complete = false
            println(output)
            println("Good Bye! TianGong.")
            sys.exit()
          }

        }
      }
    }

    var i = 0
    val id = new(util.Random).nextInt(1000)
    //report id of Yutu to TianGongMonitor.
    os.println("YuTu"+id)
    while(task_not_complete) {
      Thread.sleep(1000)
      timer_cout = timer_cout + 1 //timer for Yutu
      i = i + 1

      val position = report_postion(current_road)
      os.println(position)
    }

    //    while(true){
    //      val input = readLine
    //      os.println(input)
    //    }
  }

  //get current road map
  def set_current_map(task_map:ArrayBuffer[Tuple4[Float,Float,Float, Float]]): Unit = {
    current_road(0) = task_map.head
    current_road(1) = task_map(1)
//    println("=="*30)
//    println(task_map(1)._1)
//    println(task_map(1)._2)

  }

  //calulate angle of position.
  def caluate_angle(current_road:Array[Tuple4[Float,Float,Float, Float]]): Unit = {

      val x:Float = current_road(1)._1 -  current_road.head._1
      val y:Float = current_road(1)._2 - current_road.head._2
      val xy2 = math.pow(x,2) + math.pow(y,2)
      val b = math.sqrt(xy2).toFloat
      angle(0) = y/b //sin
      angle(1) = x/b //cos

  }

  //calculate positon of running Yutu.
  def report_postion(current_road:Array[Tuple4[Float,Float,Float, Float]]):String = {
    //current coordinate
    val x: Float = timer_cout * 1 * angle(1) //t * v * cos
    val y: Float = timer_cout * 1 * angle(0) //t * v * sin

    //whether or not reach to next coordinate
    //current road length
    val dl2 = math.pow(x, 2) + math.pow(y, 2)
    val dl = math.sqrt(dl2)
    //total road length
    val px = current_road.last._1 - current_road.head._1
    val py = current_road.last._2 - current_road.head._2
    val pl2 = math.pow(px, 2) + math.pow(py, 2)
    val pl = math.sqrt(pl2)

    println("@@"*30)
    println(dl)
    println(pl)

    var result = x.toString + ',' + y.toString + ',' + 1 + ',' + angle(0).toString + ';' + current_road.last._1.toString + ',' + current_road.last._2.toString + ',' + current_road.last._3.toString + ',' + angle(0).toString
    if (dl > pl) {
      if (road_map.length == 2) {
        result = "End Task!"

        result
      } else {
        //set next road map
        road_map.trimStart(1)
        val v = 1.toFloat
        road_map(0) = Tuple4(x, y, v, angle(0))
        set_current_map(road_map)
        caluate_angle(current_road)
        timer_cout = 0

        result
      }
    } else {

      result
    }

  }

  //for parse road map
  def parseRoadMap(maps:String):Unit = {
      val tmp:Array[String] = maps.split("=>")
//      println("-="*50)
      val tmp0 = tmp(1).split(';')

//      for (x <- tmp0) {println(x)}
      for (i <- tmp0){
          val tmp1 = i.split(',')
          val tmp2 = for (x <- tmp1)  yield x.toFloat
          road_map += Tuple4(tmp2(0), tmp2(1), tmp2(2),0)
      }

  }
}
