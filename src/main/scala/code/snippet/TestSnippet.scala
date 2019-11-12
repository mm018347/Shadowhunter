package org.plummtw.shadowhunter.snippet

import _root_.net.liftweb._
import net.liftweb.mapper._
import view.MapperPaginatorSnippet
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._

import scala.xml._
import scala.xml.transform._ 

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.util._

class TestSnippet { 
  def all_enum = RoomFlagEnum.FLAGNAME_MAP.keys
  
  def fix_room = {
    val rooms = Room.findAll()
    rooms.foreach { room =>
      val room_flags = room.room_flags.is.split(",")
      val new_room_flags = room_flags.map { flag =>
        if (flag == "VG")
          "VG1"
        else if (flag.length == 2)
          flag + "_"
        else
          flag
      }.mkString(",")
      
      room.room_flags(new_room_flags).save
    }
    
    S.redirectTo("main.html")
  }
  
  def create_test1 = {
    /*
    //var java_role_array : java.util.LinkedList[RoleEnum.Value] = new java.util.LinkedList()
    var option_list : java.util.LinkedList[RoomFlagEnum.Value] = new java.util.LinkedList()
    //option_list = RoomFlagEnum.SYSTEM_LIST ::: RoomFlagEnum.ROLE_CLUMP_LIST ::: RoomFlagEnum.ROLE_CLOSE_LIST ::: RoomFlagEnum.ROLE_SETTING_LIST ::: RoomFlagEnum.GREENCARD_LIST ::: RoomFlagEnum.BLACKCARD_LIST ::: RoomFlagEnum.WHITECARD_LIST
    option_list.addAll(RoomFlagEnum.SYSTEM_LIST)
    option_list.addAll(RoomFlagEnum.ROLE_CLUMP_LIST)
    option_list.addAll(RoomFlagEnum.ROLE_CLOSE_LIST)
    option_list.addAll(RoomFlagEnum.ROLE_SETTING_LIST)
    option_list.addAll(RoomFlagEnum.GREENCARD_LIST)
    option_list.addAll(RoomFlagEnum.BLACKCARD_LIST)
    option_list.addAll(RoomFlagEnum.WHITECARD_LIST)
    var close_option_list = RoomFlagEnum.TESTMODE_CLOSE_OPTION_LIST
    val close_options = close_option_list.size()
    if (close_options != 0) {
      for ( i <- 1 to close_options) {
        if (option_list.contains(close_option_list(i))) {
          option_list.remove(RoomFlagEnum.close_option_list(i))
        }
      }
    }
    */
    val room = Room.create.room_name("測試用").room_comment("8人測試用房間")
                   .max_user(8)
                   .move_time(999).action_time(999).reaction_time(999)
                   .room_flags(all_enum.map(_.toString).mkString("",",",""))
                   .status(RoomStatusEnum.WAITING.toString).victory("")
      
    room.validate match {
      case Nil => ;
      case xs  => S.error(xs)
                  S.redirectTo("main.html")
    }
      
    room.save

    val game_hall = RoomRound.create.round_no(0)
    game_hall.room_id(room.id.is)
    game_hall.save
        
    val room_phase = RoomPhase.create.roomround_id(game_hall.id.is).phase_no(0).phase_type(RoomPhaseEnum.GAMEHALL.toString)
    room_phase.deadline(PlummUtil.dateAddMinute(new java.util.Date(), 30))
        
    room_phase.save
        
    val talk = Talk.create.roomround_id(game_hall.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("建立房間 " + (new java.util.Date).toString)
    talk.save
    
    // 加入7人
    for (i <- 'a' to 'h') {
      val voted  = if (i=='a') "" else UserEntryRoomFlagEnum.VOTED.toString
      val i_str  = i.toString
      val player = UserEntry.create
                    .room_id(room.id.is)
                    .uname(i_str).handle_name(i_str).sex("M").user_icon_id(1)
                    .password(PlummUtil.generateSHA1(i_str).substring(0,20))
                    .last_words("").role("")
                    .room_flags(voted).subrole("").haterole("")
                    .ip_address(S.request.map{x=>PlummUtil.getIpAddress(x)}.openOr(""))
                      
      player.save
    }
        
    S.redirectTo("main.html")
  }
}
