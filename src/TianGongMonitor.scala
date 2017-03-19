/**
  * Created by Xiangguo.Kong on 2017/3/16.
  */

import java.net.{ServerSocket, Socket}
import java.io.{BufferedInputStream, BufferedReader, File, InputStream, InputStreamReader, PrintStream, PrintWriter}

import collection.mutable.{ArrayBuffer, HashMap, SynchronizedBuffer, SynchronizedMap}
import scala.collection.mutable
import scala.io.Source

object TianGongMonitor {

  case class User(sock:Socket,is:BufferedReader,ps:PrintStream,name:String)
  var message_boxs = new HashMap[String,String] with SynchronizedMap[String,String]
  val road_map = new ArrayBuffer[String] with SynchronizedBuffer[String]

  def main(args : Array[String]) : Unit = {

    val users = new ArrayBuffer[User] with SynchronizedBuffer[User]

    //load road map.
    load_road_map()

    val ss = new ServerSocket(8088)
    actors.Actor.actor{
      println("TianGongMonitor is running ...")
      println("Waiting for server YuTu.")
      while(true){
        val sock = ss.accept()
        val is = new BufferedReader(new InputStreamReader(sock.getInputStream()))
        val os = new PrintStream(sock.getOutputStream())
        actors.Actor.actor{

          val yutu_name = is.readLine()
          users += User(sock,is,os,yutu_name)
          //join message into message box for per client.
          message_boxs(yutu_name) = ""

          if (road_map.nonEmpty){
            //assign jobs for per client of YuTu.
            val r_map = road_map.last
            road_map.trimEnd(1)
            os.println("Task:Hello "+ yutu_name + "! This your road map, please run it,God bless you. => " + r_map)
            println(r_map)
          } else {
            os.println("Complete Task of all! please exit: "+ yutu_name + " This no road map for you, please go out. ")
          }

        }
      }
    }

    while(true){
      for(user <- users){
        if(user.is.ready()){
          val input = user.is.readLine()
          val recieve = user.name + "=>" + input

          //update status for client.
          message_boxs(user.name) = recieve

          //checked whether or not complete task for per YuTu
          if (message_boxs.contains(user.name)){
            if ( input.startsWith("End")){
              user.ps.println("Complete Task! please exit: " + user.name)
              println(user.name + " have completed task and will exit.")
              message_boxs -= user.name
            } else {
              user.ps.println(" Hello Yu!" + user.name)
            }
          }

        }

        // print monitor message.
        Thread.sleep(500)
        if (message_boxs.nonEmpty){
          println("=="*80)
          for ((k,v) <- message_boxs) {
            println("--"*80)
            val ms = parse_yutu_message(v)

            if (ms.nonEmpty && ms.length==2) {
              val  f_p = forecast_postion(ms) // forecast coordinate after two seconds.
              println("Name: " + k + " current coordinate:["+ ms(0)(0) +','+ ms(0)(1) + "] destination coordinate:[" + ms(1)(0) +','+ ms(1)(2) +
                        "] speed:[" + ms(0)(2) + "] angle:[" + ms(0)(3) + "] forecast coordinate:[" + f_p(0) + ',' + f_p(1) + "]"
                        )
            }

          }
        }

      }


    }



  }

  //parse message from client from YuTu
  def parse_yutu_message(message:String):Array[List[String]] = {
    var yms = new ArrayBuffer[List[String]]
    val ms = message.split("=>")
    val tms = ms.last.split(';')

    for (x <- tms){
      val xs = x.split(',')
      yms += xs.toList
    }

    yms.toArray
  }

  //load  road map for Yutu.
  def load_road_map():Unit = {
    Source.fromFile("road_map.txt").getLines.foreach{
      x => road_map += x.toString
    }

//    result.foreach(x => println(x))
  }

  //forecast current coordinate for next two seconds.
  def forecast_postion(position:Array[List[String]]):List[String] = {
    //road
    val tmp0 = position.head
    val tmp1 = for (x <- tmp0) yield x.toFloat

    var dl = tmp1(2) * 2 // v*t
    var y1 = dl * tmp1(3) // l * sin()
    var tx2 = math.pow(dl,2) - math.pow(y1,2)
    var x1 = math.sqrt(tx2)
    //coordinate after two seconds.
    var x = tmp1.head + x1
    var y = tmp1(1) + y1

    val forecast_p = List(x,y,tmp1(2),tmp1(3))
    val fp = for (x <- forecast_p) yield x.toString

    fp
  }

}
