package org.plummtw.shadowhunter.snippet

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import code.lib._
import Helpers._

class HelloWorld {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  // replace the contents of the element with id "time" with the date
  def howdy = "#time *" #> date.map(_.toString)

  /*
   lazy val date: Date = DependencyFactory.time.vend // create the date via factory

   def howdy = "#time *" #> date.toString
   */
   /*
   def game_info = <span>
                現在時間：{date.map(_.toString)} <br/>
                Lift 版本： {LiftRules.liftVersion }   <br/><br/>
                OS 資訊：{System.getProperty("os.arch")} 
                             {System.getProperty("os.name")}
                             {System.getProperty("os.version")} <br/> 
                Java 版本： {System.getProperty("java.runtime.version")} <br/>
                檔案編碼：  {System.getProperty("file.encoding")} <br/>
                Web Server：{S.servletSession.map(u => u.getServletContext().getServerInfo()) openOr ""}
              </span>
              */
}

/*
package org.plummtw.jinrou.snippet

import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._

class HelloController {
  def game_info = <span>
                現在時間：{new _root_.java.util.Date} <br/>
                Lift 版本： {LiftRules.liftVersion }   <br/><br/>
                OS 資訊：{System.getProperty("os.arch")} 
                             {System.getProperty("os.name")}
                             {System.getProperty("os.version")} <br/> 
                Java 版本： {System.getProperty("java.runtime.version")} <br/>
                檔案編碼：  {System.getProperty("file.encoding")} <br/>
                Web Server：{S.servletSession.map(u => u.getServletContext().getServerInfo()) openOr ""}
              </span>

}

*/