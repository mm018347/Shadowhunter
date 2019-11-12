package org.plummtw.shadowhunter.snippet

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._

import scala.xml.NodeSeq

import collection.mutable.{LinkedList, HashMap, SynchronizedMap}

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.util._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.heavy.CardHelper

class OldLogSnippet {
  /*
  def next (xhtml : NodeSeq) : NodeSeq = {
    val page_no : Int =
      try { S.param("page_no").getOrElse("0").toInt }
      catch { case e: Exception => 0 }

    <a href={"oldlog_list.html?page_no=" + (page_no+1).toString}>下一頁</a>
  }
  */
  
  def list  = {
    val page_no : Int = 
      try { S.param("page_no").getOrElse("0").toInt }
      catch { case e: Exception => 0 }
      
    val filter =
      S.param("filter").getOrElse("")

    def victorys(room : Room) = 
      RoomVictoryEnum.victory_name(room.victory.is) + " " + 
      (if (room.victory_all.is != "")
        room.victory_all.is.split(",").map(RoleEnum.get_role(_).role_name).mkString(",")
       else "")
      
    val room_list = 
      if (filter == "noabandon")
        Room.findAll(By(Room.status, RoomStatusEnum.ENDED.toString),
                     NotBy(Room.victory, RoomVictoryEnum.ABANDONED.toString),
                     OrderBy(Room.id,Descending),
                     StartAt(page_no * 20), MaxRows(20))
      else
        Room.findAll(By(Room.status, RoomStatusEnum.ENDED.toString),
                     OrderBy(Room.id,Descending),
                     StartAt(page_no * 20), MaxRows(20))
   
    val filter_str = 
      if (filter != "")
        "&filter=" + filter
      else ""
    
    val last_page = if (page_no == 0) <span></span>
                    else <a href={"oldlog_list.html?page_no=" + (page_no-1).toString + filter_str}>上一頁</a>
    val next_page = if (room_list.length != 20) <span></span>
                    else <a href={"oldlog_list.html?page_no=" + (page_no+1).toString + filter_str}>下一頁</a>

    val room_table = <div class="row gtr-25 gtr-uniform">
      { for (room <- room_list) yield 
        <div class="col-1 col-2-small">
          <code class="number">{room.id.is.toString}</code>
        </div>
        <div class="col-5 col-10-small">
          <a href={"oldlog_view.html?room_no="+room.id.is.toString} class="title">{room.room_name.is}</a>
        </div>
        <div class="col-4 col-9-small">
          <span class="room_comment">～ {room.room_comment.is.toString} ～</span>
        </div>
        <div class="col-2 col-3-small"><span class="room_comment side">
          <code class="upper">最多 {room.max_user.is.toString} 人</code>
        </span></div>
        <div class="col-6"><code class="victory">勝利</code>{victorys(room)}</div>
        <div class="col-6 room_comment"><code class="time">{room.talk_time.is.toString}</code></div>
        <div class="col-12 room_option">{room.option_text}</div>
      }
      </div>
    
    "#last_page"  #>  last_page &
    "#next_page"  #>  next_page &
    "#room_table" #>  room_table
  }
  
  def view = {
    val room_no = 
      try { S.param("room_no").getOrElse("0").toLong }
      catch { case e:Exception => 0}
    var room_box = Room.find(By(Room.id, room_no)) 
    if (room_box.isEmpty) {
      for (room <- CurrentRoom.get) {
        // Reload Room
        room_box = Room.find(By(Room.id, room.id.is))
      }
    }
    
    if (room_box.isEmpty) {
      S.error(<b>找不到房間</b>)
      S.redirectTo("main.html")
    }
    
    val room = room_box.get
    //Room_R.set(room)
    val room_id = room.id.is
    
    if (room.status.is != RoomStatusEnum.ENDED.toString()) {
      S.error(<b>遊戲尚未結束</b>)
      S.redirectTo("main.html")
    }
    
    val roomround_box  = RoomRound.find(By(RoomRound.room_id, room_id), OrderBy(RoomRound.round_no, Descending))
    if (roomround_box.isEmpty) {
      S.error(<b>找不到遊戲回合</b>)
      S.redirectTo("main.html")
    }
    
    val roomround = roomround_box.get
    //println("RoomRound ID : " + roomround.id.is)
    //RoomRound_R.set(roomround)
    
    val roomphase_box  = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is), OrderBy(RoomPhase.phase_no, Descending))
    if (roomphase_box.isEmpty) {
      S.error(<b>找不到遊戲階段</b>)
      S.redirectTo("main.html")
    }
    
    val roomphase = roomphase_box.get
    //RoomPhase_R.set(roomphase)
    
    val userentrys    = UserEntry.findAllByRoom(room)
    //UserEntrys_R.set(userentrys)
    val userentrys_in = userentrys.filter(! _.revoked.is)
    //UserEntrys_RR.set(userentrys_in)
    
    val time_table = 
      if (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
        <span>遊戲大廳</span>
      else if (roomphase.phase_type.is == RoomPhaseEnum.ENDED.toString) {
        if (room.victory_all.is == "")
          <span>遊戲結束，勝利者：{RoomVictoryEnum.victory_name(room.victory.is)}</span>
        else {
          val victorys = room.victory_all.is.split(",").map(RoleEnum.get_role(_).role_name).mkString(",")
          <span>遊戲結束，勝利者：{RoomVictoryEnum.victory_name(room.victory.is)} {victorys}</span>
        }
      } else
        <span>第{roomround.round_no.is}回合　等待 {userentrys.filter(_.id.is == roomphase.player.is)(0).handle_name.is} {RoomPhaseEnum.get_cname(roomphase.phase_type.is)}</span>
      
    val user_table = UserEntryHelper.user_table(room, roomphase, GlobalUserEntry.NoUserEntry, userentrys_in, true)
    val location_table = LocationHelper.location_table(room, userentrys_in)
    val talk_table = MessageHelper.messages_all(room.id.is, room, userentrys, true)
    val card_table = CardHelper.card_table(room, CardPool.findAll(By(CardPool.room_id, room.id.is), OrderBy(CardPool.card_no, Ascending)))
    
    //<meta http-equiv="refresh" content={auto_reload_str} />
    "#room_no"        #> room_no &
    "#room_name"      #> room.room_name.is &
    "#room_comment"   #> room.room_comment.is &
    "name=room_no [value]" #> room_no &
    "#time-table *"      #> time_table &
    "#user-table *"      #> user_table &
    "#card-table *"      #> card_table &
    "#location-table  *" #> location_table &
    "#talk-table *"      #> talk_table
  }
  
  //def reveal_mode =
  //  (Room_R.get.has_flag(RoomFlagEnum.TEST_MODE) || (Room_R.get.status.is == RoomStatusEnum.ENDED.toString))  
}
