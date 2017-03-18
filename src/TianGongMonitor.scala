/**
  * Created by Xiangguo.Kong on 2017/3/16.
  */

import java.net.{ServerSocket,Socket}
import java.io.{InputStream, BufferedInputStream, PrintStream,InputStreamReader,BufferedReader }
import collection.mutable.{ArrayBuffer, SynchronizedBuffer, HashMap, SynchronizedMap}

object TianGongMonitor {

  case class User(sock:Socket,is:BufferedReader,ps:PrintStream,name:String)
  val message_boxs = new HashMap[String,String] with SynchronizedMap[String,String]

  def main(args : Array[String]) : Unit = {

    val users = new ArrayBuffer[User] with SynchronizedBuffer[User]
    val road_map = new ArrayBuffer[String] with SynchronizedBuffer[String]
    road_map += ("0,0,1;10,0,1;50,0,1","0,0,1;0,2000,1;0,5000,1", "0,0,1;1000,1000,1;5000,5000,1", "0,0,1;20,0,1;30,10,1", "0,0,1;10,0,1;20,10,1")
//    road_map += ("0,0,1;10,0,1;50,0,1","0,0,1;0,2000,1;0,5000,1", "0,0,1;1000,1000,1;5000,5000,1", "0,0,1;20,0,1;30,10,1")
//    val message_boxs = new HashMap[String,String]

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

          //assign jobs for per client of YuTu.
          val r_map = road_map.last
          road_map.trimEnd(1)
          os.println("Task:Hello "+ yutu_name + "! This your map, please run it,God bless you. => " + r_map)
          println(r_map)

        }
      }
    }


    var i = 0
    while(true){
      for(user <- users){
        if(user.is.ready()){
          val input = user.is.readLine()
          val recieve = user.name + "=>" + input

          //update status for client.
          message_boxs(user.name) = recieve
//          println(message_boxs)
//          println(message_boxs(user.name))

          i = i + 1
          if (input.startsWith("End")){
            user.ps.println("Complete Task! please exit: " + user.name)
            println(user.name + " have complete task and will exit.")
          } else {
            user.ps.println(" Hello Yu!" + user.name + " " + i)
          }
        }

        Thread.sleep(500)
        println("=="*80)
        for ((k,v) <- message_boxs) {
          println("--"*80)
//          println(k)
          println(v)

        }

      }

      // print monitor message.
//      while (true) {
//        Thread.sleep(2000)
//        println("=="*80)
//        for ((k,v) <- message_boxs) {
//          println("--"*80)
//          println(k)
//          println(v)
//          println(message_boxs)
//        }
//      }

    }



  }

}
