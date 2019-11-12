package org.plummtw.shadowhunter.util

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
import scala.xml.Unparsed

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.util._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.comet._
import org.plummtw.shadowhunter.snippet._
import org.plummtw.shadowhunter.heavy.GameProcessor

object MessageHelper {
  // 一般言論
  def simple_talk_tag(talk:Talk, userentrys: List[UserEntry]) : NodeSeq= {
    val mtype : MTypeEnum.Value = 
      try { MTypeEnum.withName(talk.mtype.is) }
      catch { case e: Exception                    => MTypeEnum.TALK }
    //val font_size =
    //  try {talk.font_type.is.toInt}
    //  catch { case e:Exception                   => 0}
    //val grey_out_str = if (grey_out)  "color:#FFFFFF;background-color:#777777;" else ""
    //val style_str = if (font_size >= 20) "font-size:" + talk.font_type.is +"pt;font-weight:bold;"
   //                 else "font-size:" + talk.font_type.is +"pt;"
   // val user_entry_list  = user_entrys.filter(_.id.is == talk.actioner_id.is)
    var userentry = UserEntry.get(talk.actioner_id.is, userentrys) //UserEntrys_R.get
    val usericon  = userentry.get_user_icon

    mtype match {
      case MTypeEnum.TALK_DAY                      =>
        Seq(<tr class="user-talk"><td class="user-name">
          <font color={usericon.color.is}>◆</font>{userentry.handle_name.is} </td>
          <td class={"say " + talk.cssclass.is}> {Unparsed(talk.message.is)} </td></tr>)
       case MTypeEnum.TALK_ADMIN                   =>
        Seq(<tr class="admin-talk"><td class="user-name">
          <font color={usericon.color.is}>◆</font><span class="parrot">管理員</span> </td>
          <td class={"say " + talk.cssclass.is}> {Unparsed(talk.message.is)} </td></tr>)
       case MTypeEnum.TALK_ADMIN_PRIVATE           =>
        val useractionee = UserEntry.get(talk.actionee_id.is, userentrys)
        
        Seq(<tr class="admin-talk"><td class="user-name">
          <font color={usericon.color.is}>◆</font><span class="parrot">管理員</span>的悄悄話({useractionee.handle_name.is}) </td>
          <td class={"say " + talk.cssclass.is}> {Unparsed(talk.message.is)} </td></tr>)
        
      case _                                       => NodeSeq.Empty 
    }
  }
  
  def simple_message_tag(message: String) : NodeSeq= {
    //Seq(<tr><td width="1000" colspan="3" align="left" style="background-color:#efefef;color:black;font-weight:bold;border-top: silver 1px dashed;">　　　　　{message} </td></tr>)  
    Seq(<tr><td class="system-user" colspan="2">　　　　　{message}</td></tr>)
  }
  
  def simple_message_tag(message: String, reveal_mode: Boolean, cssclass : String) : NodeSeq= {
  //  val style_str = "background-color:" + background_color + ";color:" + color + ";" //font-weight:bold;border-top: silver 1px dashed;"  
    val css_str = if (cssclass == "") "system-user" else  cssclass 
  
    if (reveal_mode)
      //Seq(<tr><td width="1000" colspan="3" align="left" style={style_str}>　　　　　　　　　　　　{message} </td></tr>)
      Seq(<tr><td class={css_str} colspan="2">　　　　　{message}</td></tr>)
    else
      NodeSeq.Empty  
  }
  
  def talk_tag(talk: Talk, room: Room, userentrys: List[UserEntry], reveal_mode: Boolean): NodeSeq = {
    val currentuserentry_id = CurrentUserEntry.get match {
      case Full(x)                                 => x.id.is
      case x                                       => 0
    }

    val mtype : MTypeEnum.Value = 
      try { MTypeEnum.withName(talk.mtype.is) }
      catch { case e: Exception                    => MTypeEnum.TALK }
    
    val useractioner = UserEntry.get(talk.actioner_id.is, userentrys)
    val useractionee = UserEntry.get(talk.actionee_id.is, userentrys)
    
    /*
    val mtype : MTypeEnum.Value = MTypeEnum.valueOf(talk.mtype.is) getOrElse(null)
    val user_entry_list  = user_entrys.filter(_.id.is == talk.actioner_id.is)
    val user_target_list = user_entrys.filter(_.id.is == talk.actionee_id.is)

    var user_entry  : UserEntry = null
    var user_target : UserEntry = null

    //println("user_entry_list length : " +user_entry_list.length)
    var generated_message : String = ""

    //println("user_target_list length : " +user_target_list.length)
    if ((mtype == MTypeEnum.MESSAGE_COME) || (mtype == MTypeEnum.MESSAGE_LEAVE) || (mtype == MTypeEnum.MESSAGE_KICKED)) {
      var handle_name : String = ""
      if ((talk.message.is == null) || (talk.message.is == "")) {
        user_entry  = get_user_entry(user_entry_list, talk.actioner_id.is)
        handle_name = user_entry.handle_name.is
      } else
        handle_name = talk.message.is

      generated_message = handle_name + (mtype match {
        case MTypeEnum.MESSAGE_COME                => " 加入遊戲"
        case MTypeEnum.MESSAGE_LEAVE               => " 離開遊戲"
        case MTypeEnum.MESSAGE_KICKED              => " 被踢出遊戲"
        case xs                                    => ""
      })
    } else if (mtype == MTypeEnum.VOTE_KICK) {
      if ((talk.message.is == null) || (talk.message.is == "")){
        user_entry  = get_user_entry(user_entry_list, talk.actioner_id.is)
        user_target = get_user_entry(user_target_list, talk.actionee_id.is)
        val user_entry_handle_name = (if (user_entry != null) user_entry.handle_name.is else "")
        val user_target_handle_name = (if (user_target != null) user_target.handle_name.is else "")
        generated_message = user_entry_handle_name + " 對 " + user_target_handle_name + " 投票踢出"
      } else
        generated_message = talk.message.is
    } else {
      user_entry  = get_user_entry(user_entry_list, talk.actioner_id.is)
      user_target = get_user_entry(user_target_list, talk.actionee_id.is)
    }
    */

    mtype match {
      case MTypeEnum.TALK_ADMIN                    => simple_talk_tag(talk, userentrys)
      case MTypeEnum.TALK_ADMIN_PRIVATE            => //simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
        if ((reveal_mode) || (currentuserentry_id == useractionee.id.is))
          simple_talk_tag(talk, userentrys)
        else
          Seq()
      case MTypeEnum.TALK_DAY                      => simple_talk_tag(talk, userentrys)
      
      case MTypeEnum.MESSAGE_GENERAL               => simple_message_tag(talk.message.is)
      case MTypeEnum.MESSAGE_COME                  => simple_message_tag(useractioner.handle_name.is + " 加入遊戲")
      case MTypeEnum.MESSAGE_LEAVE                 => simple_message_tag(useractioner.handle_name.is +" 離開遊戲")
      case MTypeEnum.MESSAGE_KICKED                => simple_message_tag(useractioner.handle_name.is +" 被踢出遊戲")
      case MTypeEnum.MESSAGE_REVOTE0               => simple_message_tag("＜投票重新開始 請儘速重新投票＞")
      case MTypeEnum.MESSAGE_LAST2MIN              => simple_message_tag("最後 2 分還不投票將會暴斃")
      case MTypeEnum.MESSAGE_DEATHSUDDEN           => simple_message_tag(useractioner.handle_name.is + "  突然暴斃死亡")
      case MTypeEnum.MESSAGE_REVOTE                => simple_message_tag("＜投票結果有問題 請重新投票＞")
      case MTypeEnum.MESSAGE_TIMEOUT               => simple_message_tag(useractioner.handle_name.is +" 超過時限，放棄回合")
      case MTypeEnum.MESSAGE_DEATH                 => simple_message_tag(useractioner.handle_name.is + "  死亡")
        
      case MTypeEnum.RESULT_MOVE                   => simple_message_tag(talk.message.is, true, "move-do")
      case MTypeEnum.RESULT_ATTACK                 => simple_message_tag(talk.message.is, true, "attack-do")
      case MTypeEnum.RESULT_BLACKCARD              => simple_message_tag(talk.message.is, true, "black-do")
      case MTypeEnum.RESULT_WHITECARD              => simple_message_tag(talk.message.is, true, "white-do")
      case MTypeEnum.RESULT_GREENCARD              => simple_message_tag(talk.message.is, true, "green-do")
      case MTypeEnum.RESULT_SHADOW                 => simple_message_tag(talk.message.is, true, "shadow-do")
      case MTypeEnum.RESULT_HUNTER                 => simple_message_tag(talk.message.is, true, "hunter-do")
      case MTypeEnum.RESULT_NEUTRAL                => simple_message_tag(talk.message.is, true, "neutral-do")
      case MTypeEnum.RESULT_DRAGON                 => simple_message_tag(talk.message.is, true, "dragon-do")
      case MTypeEnum.RESULT_LOCROB                 => simple_message_tag(talk.message.is, true, "loc-do")
      case MTypeEnum.RESULT_GREENREVEAL            => 
        val message = 
          if ((!reveal_mode) && (currentuserentry_id == useractioner.id.is) &&
              (useractioner.get_role == RoleDetective))
                "您發現 " + useractionee.handle_name.is + " 的生命是 " + useractionee.get_real_role.role_life.toString
          else if ((reveal_mode) || (currentuserentry_id == useractioner.id.is) ||
              (currentuserentry_id == useractionee.id.is)) talk.message.is
          else "但是什麼也沒發生"
        simple_message_tag(message, true, "green-do")
      case MTypeEnum.ACTION_WHITECARD_BALANCE      => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用天秤", true, "white-do") 
      
        
      case MTypeEnum.OBJECTION_MALE                =>
          Seq(<tr><td class="objection-male" colspan="2">　　　　　{useractioner.handle_name.is} 要求廢止</td></tr>)
          //Seq(<tr><td width="1000" colspan="3" align="left" style="background-color:#336699;color:white;font-weight:bold;border-top: silver 1px dashed;">　　　　　{user_entry.handle_name.is} 要求廢止</td></tr>)
      case MTypeEnum.OBJECTION_FEMALE              =>
          Seq(<tr><td class="objection-male" colspan="2">　　　　　{useractioner.handle_name.is} 要求廢止</td></tr>)
          //Seq(<tr><td width="1000" colspan="3" align="left" style="background-color:#FF0099;color:white;font-weight:bold;border-top: silver 1px dashed;">　　　　　{user_entry.handle_name.is} 要求廢止</td></tr>)

      case MTypeEnum.ACTION_KICK                   => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 投票踢出", true, "kick-do")
      case MTypeEnum.ACTION_FLIP                   => simple_message_tag(useractioner.handle_name.is + " 翻開角色卡", true, "flip-do")
      case MTypeEnum.ACTION_MOVE                   => simple_message_tag(useractioner.handle_name.is + " 進行移動，自選：" + LocationEnum.get_cname(talk.message_flags.is), true, "move-do")
      case MTypeEnum.ACTION_ATTACK                 => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 進行攻擊！！", true, "attack-do")
      case MTypeEnum.ACTION_NOATTACK               => simple_message_tag(useractioner.handle_name.is + " 放棄攻擊", true, "attack-do")
      case MTypeEnum.ACTION_MULTIATTACK            => simple_message_tag(useractioner.handle_name.is + " 進行範圍攻擊！！", true, "attack-do")
      //case MTypeEnum.VOTE_HANG                   => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 投票處死",heaven_mode || ((user != null) && (!user.live.is)),"#AAAA33","snow")

      case MTypeEnum.ACTION_DRAWBLACKCARD          => simple_message_tag(useractioner.handle_name.is + " 抽取黑卡 " + CardEnum.get_card(talk.message_flags.is).card_name, true, "loc-do")
      case MTypeEnum.ACTION_DRAWWHITECARD          => simple_message_tag(useractioner.handle_name.is + " 抽取白卡 " + CardEnum.get_card(talk.message_flags.is).card_name, true, "loc-do")
      case MTypeEnum.ACTION_STARS_DAMAGE           => simple_message_tag(useractioner.handle_name.is + " 的損傷增加 2 點(菲爾特)", true, "shadow-do")
      case MTypeEnum.ACTION_STARS_LOWER            => simple_message_tag(useractioner.handle_name.is + " 的損傷減少 1 點(菲爾特) ", true, "shadow-do")
      case MTypeEnum.ACTION_DRAWGREENCARD          => 
        val card_name =
          if ((reveal_mode) || (currentuserentry_id == useractioner.id.is))
            CardEnum.get_card(talk.message_flags.is).card_name
          else ""
        simple_message_tag(useractioner.handle_name.is + " 抽取綠卡 " + card_name, true, "loc-do")  
      case MTypeEnum.ACTION_LOCDAMAGE              => 
        val damage_str =
          if (talk.message_flags.is == UserEntryFlagEnum.BARRIER.toString)
            useractioner.handle_name.is + " 將 " + useractionee.handle_name.is + " 的損傷增加 0 點(怪異樹林&防護罩)"
          else if (talk.message_flags.is == CardEnum.W_FORTUNE_BROOCH.toString) 
            useractioner.handle_name.is + " 將 " + useractionee.handle_name.is + " 的損傷增加 0 點(怪異樹林&財富胸針)"
          else if (talk.message_flags.is == RoleEnum.UNSEEN.toString) 
            useractioner.handle_name.is + " 將 " + useractionee.handle_name.is + " 的損傷增加 1 點(怪異樹林&隱形人)"
          else if (talk.message_flags.is == RoleEnum.LION.toString) 
            useractioner.handle_name.is + " 將 " + useractionee.handle_name.is + " 的損傷增加 1 點(怪異樹林&特羅修)"
          else
            useractioner.handle_name.is + " 將 " + useractionee.handle_name.is + " 的損傷增加 2 點(怪異樹林)"
            
        simple_message_tag(damage_str, true, "loc-do")
      case MTypeEnum.ACTION_LOCHEAL                => simple_message_tag(useractioner.handle_name.is + " 將 " + useractionee.handle_name.is + " 的損傷減少 1 點(怪異樹林)", true, "loc-do")
      case MTypeEnum.ACTION_LOCROB                 => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 進行搶奪(供品祭壇)", true, "loc-do")
      case MTypeEnum.ACTION_NOLOC                  => simple_message_tag(useractioner.handle_name.is + " 放棄行動", true, "loc-do")  
      case MTypeEnum.ACTION_NOCARD                 => simple_message_tag(useractioner.handle_name.is + " 放棄卡片", true, "loc-do")  

      case MTypeEnum.ACTION_GREENCARD              => 
        val card_name =
          if ((reveal_mode) || (currentuserentry_id == useractioner.id.is) ||
              (currentuserentry_id == useractionee.id.is)) CardEnum.get_card(talk.message_flags.is).card_name
          else ""
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用綠卡 " + card_name, true, "green-do")  
      case MTypeEnum.ACTION_WHITECARD              => 
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用白卡 " + CardEnum.get_card(talk.message_flags.is).card_name, true, "white-do")  
      case MTypeEnum.ACTION_BLACKCARD              => 
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用黑卡 " + CardEnum.get_card(talk.message_flags.is).card_name, true, "black-do")  
        
      case MTypeEnum.ACTION_ALLIE_MOTHERLOVE       => simple_message_tag(useractioner.handle_name.is + " 施展母愛，" + useractioner.handle_name.is + " 的損傷設定為 0", true, "neutral-do")
      case MTypeEnum.ACTION_ANGEL_REINCARNATE      => simple_message_tag(useractioner.handle_name.is + " 選擇 " + useractionee.handle_name.is + " 為重生對象", 
                                                                     (reveal_mode) || (currentuserentry_id == useractioner.id.is), "neutral-do")  
      case MTypeEnum.ACTION_ADECOY_TAUNT           => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展嘲諷，添加嘲諷狀態", true, "neutral-do")
      case MTypeEnum.ACTION_TEL_WATERMIRROR        => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展水鏡", true, "neutral-do")
      case MTypeEnum.ACTION_BOMB_BOMB              => simple_message_tag(useractioner.handle_name.is + " 投擲炸彈，地點：" + LocationEnum.get_cname(talk.message_flags.is), true, "neutral-do")
      case MTypeEnum.ACTION_CHARLES_BLOODFEAST     => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展血祭", true, "neutral-do")
      case MTypeEnum.ACTION_CASSANDRA_FATECHANGE   => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展命運轉變，給予 " + CardEnum.get_card(talk.message_flags.is).card_name, true, "neutral-do")  
      case MTypeEnum.ACTION_CASSANDRA_GIVE         => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is  + " 施展交易，給予 " + CardEnum.get_card(talk.message_flags.is).card_name, true, "neutral-do")  
      case MTypeEnum.ACTION_DAVID_GRAVEDIG         => simple_message_tag(useractioner.handle_name.is + " 施展盜墓，獲得 " + CardEnum.get_card(talk.message_flags.is).card_name, true, "neutral-do")  
      case MTypeEnum.ACTION_WESTLOBE_PRY           =>
        val roleside_name =
          if ((reveal_mode) || (currentuserentry_id == useractioner.id.is) ||
              (currentuserentry_id == useractionee.id.is)) RoleSideEnum.get_roleside_cname(talk.message_flags.is)
          else "？？"
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 的靈魂窺探是否為 " + roleside_name +
                                                                     (if (useractionee.get_real_role.role_side.toString == talk.message_flags.is) " (成功)" else " (失敗)"),
                                                                     true, "neutral-do")
      case MTypeEnum.ACTION_DETECTIVE_REASONA      => simple_message_tag(useractioner.handle_name.is + " 推理 " + useractionee.handle_name.is + " 為 " + RoleSideEnum.get_roleside_cname(talk.message_flags.is) +
                                                                     (if (useractionee.get_real_role.role_side.toString == talk.message_flags.is) " (成功)" else " (失敗)"),
                                                                     (reveal_mode) || (currentuserentry_id == useractioner.id.is), "neutral-do")
      case MTypeEnum.ACTION_DETECTIVE_REASONR      => simple_message_tag(useractioner.handle_name.is + " 推理 " + useractionee.handle_name.is + " 為 " + RoleEnum.get_role(talk.message_flags.is).role_name +
                                                                     (if (useractionee.get_real_role.role_enum.toString == talk.message_flags.is) " (成功)" else " (失敗)"),
                                                                     (reveal_mode) || (currentuserentry_id == useractioner.id.is), "neutral-do")

      case MTypeEnum.ACTION_ULTRASOUL_RAY          => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展光線", true, "shadow-do")
      case MTypeEnum.ACTION_ULTRASOUL_URAY         => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展究極光線", true, "shadow-do")
      case MTypeEnum.ACTION_UNKNOWN_DECEIVE        => simple_message_tag(useractioner.handle_name.is + " 偽裝為 " + RoleEnum.get_role(talk.message_flags.is).role_name, 
                                                                      (reveal_mode) || (currentuserentry_id == useractioner.id.is) , "shadow-do")
        
      case MTypeEnum.ACTION_WEREWOLF_COUNTER       => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 反擊！！" +
                                                                    (if (talk.message_flags.is == MTypeEnum.ACTION_WEREWOLF_AMBUSH.toString) "(伏擊)" else "")
                                                                    , true, "attack-do")
      case MTypeEnum.ACTION_WEREWOLF_AMBUSH        => simple_message_tag(useractioner.handle_name.is + " 準備伏擊",  ((reveal_mode) || (currentuserentry_id == useractioner.id.is)), "shadow-do")
      case MTypeEnum.ACTION_WIGHT_MANIPULATE       => simple_message_tag(useractioner.handle_name.is + " 施展操弄 (" + talk.message_flags.is + ")", true, "shadow-do")
        
      case MTypeEnum.ACTION_ELLEN_CURSECHAIN       => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展禁咒之鍊，添加禁咒狀態", true, "hunter-do")
      case MTypeEnum.ACTION_EVAN_BRACEUP           => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展振作，可再次使用一次性能力", true, "hunter-do")
      case MTypeEnum.ACTION_FATHEROCONNEL_PRAY     => simple_message_tag(useractioner.handle_name.is + " 施展祈禱，觸發 " + CardEnum.get_card(talk.message_flags.is).card_name, true, "hunter-do")
      case MTypeEnum.ACTION_LUBE_RESPONSE          => simple_message_tag(useractioner.handle_name.is + " 施展自然反撲，觸發 " + CardEnum.get_card(talk.message_flags.is).card_name, true, "neutral-do")
      case MTypeEnum.ACTION_FRANKLIN_LIGHTNING     => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展閃電， " + useractionee.handle_name.is + " 的損傷增加 (1D6=" + talk.message_flags.is + (if ((useractionee.get_role.role_side == RoleSideEnum.SHADOW) && (useractionee.revealed.is)) "+1" else "") + ") 點" + (if ((useractionee.get_role.role_side == RoleSideEnum.SHADOW) && (useractionee.revealed.is)) "(暗影)" else "")
                                                                            , true, "hunter-do")
      case MTypeEnum.ACTION_FUKA_DYNAMITEHEAL      => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展爆破治療", true, "hunter-do")
      case MTypeEnum.ACTION_FENG_KIKOU             => 
        val kikou_damage_str =
          if (talk.message_flags.is == "") ""
          else " (" + talk.message_flags.is + ")"
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展氣功， " + useractionee.handle_name.is + " 的損傷增加 " + kikou_damage_str + " 點", true, "hunter-do")
      case MTypeEnum.ACTION_GEORGE_DEMOLISH        => 
        val card_name =
          if (room.has_flag(RoomFlagEnum.GEORGE_REUSE2))
            "，且 " + useractioner.handle_name.is + " 的損傷減少 " + talk.message_flags.is + " 點"
          else ""
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展粉碎， " + useractionee.handle_name.is + " 的損傷增加 (1D4=" + talk.message_flags.is + ") 點" + card_name, true, "hunter-do")
      case MTypeEnum.ACTION_GREGOR_BARRIER         => simple_message_tag(useractioner.handle_name.is + " 施展防護罩", true, "hunter-do")
      case MTypeEnum.ACTION_ARSIS                  => simple_message_tag("全部翻開角色卡的獵人角色的損傷減少 1 點(阿爾西斯)", true, "hunter-do")
      case MTypeEnum.ACTION_GODFAT_EXCHANGE        => 
        val useractionee_list2 = userentrys.filter(_.id.is.toString == talk.message_flags.is) //UserEntrys_R.get
        val useractionee2 =
          if (useractionee_list2.length > 0) useractionee_list2(0)
          else GlobalUserEntry.NoUserEntry 
          
        simple_message_tag(useractioner.handle_name.is +  " 將 " + useractionee.handle_name.is + 
                           " 與 " +  useractionee2.handle_name.is + " 的順位交換", true, "hunter-do")  
      case MTypeEnum.ACTION_PUZZLE_SPIKE   => 
        val useractionee_list2 = userentrys.filter(_.id.is.toString == talk.message_flags.is) //UserEntrys_R.get
        val useractionee2 =
          if (useractionee_list2.length > 0) useractionee_list2(0)
          else GlobalUserEntry.NoUserEntry 
          
        simple_message_tag(useractioner.handle_name.is +  " 對 " + useractionee.handle_name.is + 
                           " 與 " +  useractionee2.handle_name.is + " 施展瞬殺， " + useractionee.handle_name.is + 
                           " 與 " +  useractionee2.handle_name.is + " 的損傷增加 3 點", true, "neutral-do")  
      case MTypeEnum.ACTION_FIGHTER_STRIKE   => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展嗜天斬， " + useractionee.handle_name.is + " 的損傷增加 (1D4=" + talk.message_flags.is + ") 點", true, "shadow-do")
      case MTypeEnum.ACTION_FIGHTER_STRIKE2  => simple_message_tag(useractionee.handle_name.is + " 的損傷增加 " + talk.message_flags.is + " 點，且持有的全部裝備被丟棄(嗜天斬)", true, "shadow-do")
      case MTypeEnum.ACTION_SHAHEARTACK      => simple_message_tag(useractionee.handle_name.is + " 的損傷增加 (1D6=" + talk.message_flags.is + ") 點(黑暗瓦解)", true, "neutral-do")
      case MTypeEnum.ACTION_HUNSOULACK       => simple_message_tag(useractionee.handle_name.is + " 的損傷增加 99 點(光明驟滅)", true, "neutral-do")
      case MTypeEnum.ACTION_HUNSOULLOWER     => simple_message_tag(useractioner.handle_name.is + " 的損傷減少 4 點(療癒之魂)", true, "neutral-do")
      case MTypeEnum.ACTION_JUDGMENTACK      => simple_message_tag(useractioner.handle_name.is + " 以外所有玩家的損傷設定為 7 (審判)", true, "neutral-do")
      case MTypeEnum.ACTION_LION             => simple_message_tag(useractionee.handle_name.is + " 的損傷減少 1 點(特羅修)", true, "hunter-do")
      case MTypeEnum.ACTION_EMMA_W           => simple_message_tag("全部翻開角色卡的獵人角色的損傷減少 1 點(艾瑪)", true, "hunter-do")
      case MTypeEnum.ACTION_EMMA_B           => simple_message_tag("全部翻開角色卡的暗影角色的損傷增加 1 點(艾瑪)", true, "hunter-do")
      case MTypeEnum.ACTION_MICAH_CONFUSED   => simple_message_tag(useractioner.handle_name.is + " 施展遺忘(夏彌加)，分別抽取黑卡、白卡各一張並放入棄牌堆", true, "neutral-do")
      case MTypeEnum.ACTION_MICAH_CONFUSED2  => simple_message_tag(useractioner.handle_name.is + " 跳過回合(遺忘)", true, "neutral-do")
      case MTypeEnum.ACTION_AICHA_GRASP      => simple_message_tag(useractioner.handle_name.is + " 施展光之掌握( " + talk.message_flags.is + " )", true, "hunter-do")
      case MTypeEnum.ACTION_CLACKEN_CAPTURE  => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展捕捉", true, "shadow-do")
      case MTypeEnum.ACTION_SETH_CONTROL     => simple_message_tag(useractioner.handle_name.is + " 施展掌管(塞特)，將自己添加遲緩狀態", true, "neutral-do")
      case MTypeEnum.ACTION_SETH_CONTROL2    => simple_message_tag(useractioner.handle_name.is + " 跳過回合(掌管)", true, "neutral-do")
      case MTypeEnum.ACTION_LEON_CHARGES     => simple_message_tag(useractioner.handle_name.is + " 施展碎片", ((reveal_mode) || (currentuserentry_id == useractioner.id.is)), "neutral-do")
      case MTypeEnum.ACTION_LEON_CHARGES2    => simple_message_tag(talk.message.is, ((reveal_mode) || (currentuserentry_id == useractioner.id.is)), "neutral-do")
      case MTypeEnum.ACTION_LEON_USE         => simple_message_tag(useractioner.handle_name.is + " 變化為 " + RoleEnum.get_role(talk.message_flags.is).role_name + " (閃爍)", ((reveal_mode) || (currentuserentry_id == useractioner.id.is)), "neutral-do")
      case MTypeEnum.ACTION_AMETSUKI_CURSE   => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 施展訃影禁咒(佐藤雨月)", true, "hunter-do")
      case MTypeEnum.ACTION_GINGER_RESENTFUL => simple_message_tag(useractioner.handle_name.is + " 施展憤槌(金格)，將自己添加憤槌狀態", true, "hunter-do")
      
      
      
      
      
      //case MTypeEnum.VOTE_NO_ACTION        => simple_message_tag(user_entry.handle_name.is + " 放棄行動",heaven_mode,"#AAAA33","snow")
      
      case xs => NodeSeq.Empty
    }
  }
  
  // Message Table
  def messages_normal(room: Room, roomround : RoomRound, userentrys: List[UserEntry], reveal:Boolean) : NodeSeq = {
    //val roomround = RoomRound_R.get
    var talks =  Talk.findAll(By(Talk.roomround_id, roomround.id.is), OrderBy(Talk.id, Descending))
    
    val lastround_id = roomround.last_round.is
    if (lastround_id != 0)
      talks = talks ++ Talk.findAll(By(Talk.roomround_id, lastround_id), OrderBy(Talk.id, Descending))
    
    if (roomround.round_no.is == 0) {
      val revotes = talks.filter(_.mtype.is == MTypeEnum.MESSAGE_REVOTE0.toString)

      // 新增 投票重新開始 50 次時廢村
      if ((revotes.length >= 50) || (talks.length >= 1000)) {
        //val room = Room_R.get
        /*
        room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
        room.save
            
        RoomActor ! SessionVarSet(room = room)
        RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
        */
       GameProcessor.abandon(room)
      }
    }
    
    // border="0" cellpadding="0" cellspacing="0"
    Seq(<table class="talk"> <tbody id="talk-tbody">{
        for (talk <- talks) yield talk_tag(talk, room, userentrys, reveal)
      } </tbody></table>)
  }
  
  //val amountRange = Expense.findAll(   BySql("amount between ? and ?", lowVal, highVal))
  
  def messages_all(room_id : Long, room: Room, userentrys: List[UserEntry], reveal:Boolean) : NodeSeq = {
    val talks        =  Talk.findAllByPreparedStatement({ superconn =>
      val statement = superconn.connection.prepareStatement(
      "select * from Talk join RoomRound on Talk.roomround_id = RoomRound.id where RoomRound.room_id = ? order by Talk.id desc")
      statement.setString(1, room_id.toString)
      statement
    })
    
    // border="0" cellpadding="0" cellspacing="0"
    Seq(<table class="talk"> <tbody id="talk-tbody">{
        for (talk <- talks) yield talk_tag(talk, room, userentrys, reveal)
      } </tbody></table>)
  }
}


/*
<tr>
<td class="system-user" colspan="2"><img src="img/icon/hum.gif"> clojure 來到了幻想鄉</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/msg.gif"> ＜投票結果重置，請重新投票＞</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/spy.gif"> 映 被踢出村莊了</td>
</tr>
<tr class="system-message">
<td class="kick-do" colspan="2">映 對 映 投票踢出</td>
</tr>
<tr class="system-message">
<td class="kick-do" colspan="2">映 對 右代宮  戰人 投票踢出</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/msg.gif"> ＜投票結果重置，請重新投票＞</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/spy.gif"> 天晴 被踢出村莊了</td>
</tr>
<tr class="system-message">
<td class="kick-do" colspan="2">天晴 對 天晴 投票踢出</td>
</tr>
<tr class="user-talk">
<td class="user-name"><font style="color:#00DD77">◆</font>天晴</td>
<td class="say normal">「鑽石開始了 我先自刪 掰」</td>
</tr> */