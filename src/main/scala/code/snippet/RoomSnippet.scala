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

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.util._

class RoomListSnippet {
  def render(in : NodeSeq) = {
    val room_list  = Room.findAll(By(Room.status, RoomStatusEnum.WAITING.toString), OrderBy(Room.id,Descending)) :::
                     Room.findAll(By(Room.status, RoomStatusEnum.PLAYING.toString), OrderBy(Room.id,Descending))
    //<img class="option" src="images/waiting.gif" alt="等待中" title="等待中"/>
    //<img class="option" src="images/playing.gif" alt="進行中" title="進行中"/>

    room_list.flatMap { room => Seq(
      <div class="row gtr-25 gtr-uniform">
        <div class="col-2 col-12-small">
          {
            if (room.status.is == RoomStatusEnum.WAITING.toString) {
              <code class="water">招募中</code>
            } else {
              <code class="red">進行中</code>
            }
          }
          <code class="">{room.id}</code>
        </div>
        <div class="col-6 col-12-small">
          <a href={"login.html?room_no=" + room.id}>{room.room_name}</a>
          <span class="room_comment">～ {room.room_comment} ～</span>
        </div>
        <div class="col-4 col-12-medium"><span class="room_comment">
          <code class="">移動 {room.move_time} 秒</code><code class="">行動 {room.action_time} 秒</code>
          <code class="">反應 {room.reaction_time} 秒</code><code class="">最多 {room.max_user} 人</code>
        </span></div>
        <div class="col-12 room_option">{room.option_text}</div>
      </div>
      )
    }
  }
}

// 創新村莊的 Lock，以免村莊數超過村莊上限
object RoomCreateLock {}

class RoomCreateSnippet extends StatefulSnippet with Logger{
  private var room_name        = ""
  private var room_comment     = ""
  private var max_user         = 10
  private var move_time        = 75
  private var action_time      = 75
  private var reaction_time    = 75
  
  def dispatch = {
    case _ => render
  }

  def render =
  {
    var option_list : List[RoomFlagEnum.Value] = List()

    def process() {
      debug("In Process")
      
      val room_flags : String= option_list.distinct.map(_.toString).mkString("",",","")
      
      val room = Room.create.room_name(room_name.replace('　',' ').trim()).room_comment(room_comment.replace('　',' ').trim()).max_user(max_user)
                     .move_time(move_time).action_time(action_time).reaction_time(reaction_time)
                     .room_flags(room_flags).status(RoomStatusEnum.WAITING.toString).victory("")
                     
      room.validate match {
        case Nil => ;
        case xs  => S.error(xs); return redirectTo("main.html")
      }
      
      // 加入大廳
      val game_hall = RoomRound.create.round_no(0)
      
      val room_params = AdminManage.findAll(Like(AdminManage.param_name, "room%"))

      val current_time =  new java.util.GregorianCalendar
      val current_hour =  current_time.get(java.util.Calendar.HOUR_OF_DAY)

      val room_start =
          try { room_params.filter(_.param_name.is == "room_start")(0).param_value.is.toInt }
          catch { case e: Exception => AdminHelper.DEFAULT_ROOM_START}
      val room_end =
          try { room_params.filter(_.param_name.is == "room_end")(0).param_value.is.toInt }
          catch { case e: Exception => AdminHelper.DEFAULT_ROOM_END}

      if ((current_hour >= room_end) || (current_hour< room_start)) {
        S.error((room_end.toString) + ":00 - " + (room_start.toString) +":00 請不要建立房間")
        return redirectTo("main.html")
      }
      
      RoomCreateLock.synchronized {
      
        val room_count  = Room.count(By(Room.status, RoomStatusEnum.WAITING.toString)) +
                          Room.count(By(Room.status, RoomStatusEnum.PLAYING.toString))
        val room_count_limit =
          try { room_params.filter(_.param_name.is == "room_count")(0).param_value.is.toInt }
          catch { case e: Exception => AdminHelper.DEFAULT_ROOM_COUNT}

        if (room_count_limit == 0) {
          S.error("管理員禁止建村（請詢問管理員）"); return redirectTo("main.html")
        }
		
		if (room_count >= room_count_limit) {
          S.error("超過房數上限"); return redirectTo("main.html")
        }
		
		if (room_count_limit == 99) {
          S.error("管理員禁止建村（伺服器即將關閉或剛開啟）"); return redirectTo("main.html")
        }
		
		if (room_count_limit == 98) {
          S.error("管理員禁止建村（伺服器正在更新）"); return redirectTo("main.html")
        }
                          
        room.save

        game_hall.room_id(room.id.is)
        game_hall.save
        
        val room_phase = RoomPhase.create.roomround_id(game_hall.id.is).phase_no(0).phase_type(RoomPhaseEnum.GAMEHALL.toString)
        room_phase.deadline(PlummUtil.dateAddMinute(new java.util.Date(), 30))
        
        room_phase.save
        
        val talk = Talk.create.roomround_id(game_hall.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                       .message("房間建立 " + (new java.util.Date).toString)
        talk.save
      }

      /*
      try {
        val plurk_client = new PlurkClient(plurk_apiKey)     // 建立 SPlurk 物件
        plurk_client.Users.login (plurk_username, plurk_password)  // 登入噗浪

        // 發噗
        plurk_client.Timeline.plurkAdd (
          qualifier = Qualifier.Says,  // 設定噗文前的修飾詞（說、喜歡、正在……等）
          content   = "第" + room.id.is.toString + "號房已建立",  // 噗文的內容
          language  = Some(Language.tr_ch)  // 修飾詞的語言（tr_ch 為中文）
        )
      } catch { case e: Exception =>  S.notice("Plurk 發佈失敗") }
      */

      
      S.notice(room.id.toString() + "號房已建立") 
    }
    
    "name=room_name"                #> SHtml.text(room_name, x => room_name = x) & 
    "name=room_comment"             #> SHtml.text(room_comment, x => room_comment = x) & 
    "name=max_user"                 #> SHtml.select(Seq(("4","4"),("5","5"),("6","6"),("7","7"),("8","8"),("9","9"),("10","10")),
                                Full(max_user.toString),  x => asInt(x).foreach(y => (max_user = y))) &
    "name=move_time"                #> SHtml.text(move_time.toString,     s => asInt(s).foreach(x => move_time = x)) &
    "name=action_time"              #> SHtml.text(action_time.toString,   s => asInt(s).foreach(x => action_time = x)) &
    "name=reaction_time"            #> SHtml.text(reaction_time.toString, s => asInt(s).foreach(x => reaction_time = x)) &
    "name=test_mode"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.TEST_MODE), "id" -> "test_mode") &
    "name=wish_align"               #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.WISH_ALIGN), "id" -> "wish_align") &
    "name=wish_role"                #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.WISH_ROLE), "id" -> "wish_role") &
    "name=hate_role"                #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.HATE_ROLE), "id" -> "hate_role") &
    //"name=death_look"             #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.DEATH_LOOK), "id" -> "death_look") &
    "name=expansion_role"           #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.EXPANSION_ROLE), "id" -> "expansion_role") &
    "name=custom_role"              #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.CUSTOM_ROLE), "id" -> "custom_role") &
    "name=custom_cat_role"          #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.CUSTOM_CAT_ROLE), "id" -> "custom_cat_role") &
    "name=no_hunsoul_shaheart"      #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_HUNSOUL_SHAHEART), "id" -> "no_hunsoul_shaheart") &
    "name=no_ultrasoul"             #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_ULTRASOUL), "id" -> "no_ultrasoul") &
    "name=no_unknown"               #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_UNKNOWN), "id" -> "no_unknown") &
    "name=no_unseen"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_UNSEEN), "id" -> "no_unseen") &
    "name=no_undead"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_UNDEAD), "id" -> "no_undead") &
    "name=no_valkyrie"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_VALKYRIE), "id" -> "no_valkyrie") &
    "name=no_vampire"               #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_VAMPIRE), "id" -> "no_vampire") &
    "name=no_vengeful_ghost"        #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_VENGEFUL_GHOST), "id" -> "no_vengeful_ghost") &
    "name=no_viper"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_VIPER), "id" -> "no_viper") &
    "name=no_werewolf"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_WEREWOLF), "id" -> "no_werewolf") &
    "name=no_wight"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_WIGHT), "id" -> "no_wight") &
    "name=no_witch"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_WITCH), "id" -> "no_witch") &
    "name=no_wicked"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_WICKED), "id" -> "no_wicked") &
    "name=no_bane"                  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_BANE), "id" -> "no_bane") &
    "name=no_stars"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_STARS), "id" -> "no_stars") &
    "name=no_fighter"               #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_FIGHTER), "id" -> "no_fighter") &
    "name=no_borogove"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_BOROGOVE), "id" -> "no_borogove") &
    "name=no_clacken"               #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_CLACKEN), "id" -> "no_clacken") &
    "name=no_animalbones"           #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_ANIMALBONES), "id" -> "no_animalbones") &
    "name=no_concubine"             #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_CONCUBINE), "id" -> "no_concubine") &
    "name=no_mossen"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_MOSSEN), "id" -> "no_mossen") &
    "name=no_magician"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_MAGICIAN), "id" -> "no_magician") &
    "name=no_fallomen"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_FALLOMEN), "id" -> "no_fallomen") &
    "name=no_ellen"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_ELLEN), "id" -> "no_ellen") &
    "name=no_emi"                   #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_EMI), "id" -> "no_emi") &
    "name=no_emma"                  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_EMMA), "id" -> "no_emma") &
    "name=no_evan"                  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_EVAN), "id" -> "no_evan") &
    "name=no_father_oconnel"        #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_FATHER_OCONNEL), "id" -> "no_father_oconnel") &
    "name=no_feng"                  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_FENG), "id" -> "no_feng") &
    "name=no_franklin"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_FRANKLIN), "id" -> "no_franklin") &
    "name=no_fuka"                  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_FUKA), "id" -> "no_fuka") &
    "name=no_george"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_GEORGE), "id" -> "no_george") &
    "name=no_gregor"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_GREGOR), "id" -> "no_gregor") &
    "name=no_ginger"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_GINGER), "id" -> "no_ginger") &
    "name=no_godfat"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_GODFAT), "id" -> "no_godfat") &
    "name=no_mars"                  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_MARS), "id" -> "no_mars") &
    "name=no_lion"                  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_LION), "id" -> "no_lion") &
    "name=no_arsis"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_ARSIS), "id" -> "no_arsis") &
    "name=no_shinai"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_SHINAI), "id" -> "no_shinai") &
    "name=no_aicha"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_AICHA), "id" -> "no_aicha") &
    "name=no_yakali"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_YAKALI), "id" -> "no_yakali") &
    "name=no_aki"                   #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_AKI), "id" -> "no_aki") &
    "name=no_ametsuki"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_AMETSUKI), "id" -> "no_ametsuki") &
    "name=no_lilia"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_LILIA), "id" -> "no_lilia") &
    "name=no_cloudbow"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_CLOUDBOW), "id" -> "no_cloudbow") &
    "name=no_agnes"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_AGNES), "id" -> "no_agnes") &
    "name=no_allie"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_ALLIE), "id" -> "no_allie") &
    "name=no_angel"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_ANGEL), "id" -> "no_angel") &
    "name=no_adecoy"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_ADECOY), "id" -> "no_adecoy") &
    "name=no_bellandona"            #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_BELLANDONA), "id" -> "no_bellandona") &
    "name=no_bob"                   #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_BOB), "id" -> "no_bob") &
    "name=no_bomb"                  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_BOMB), "id" -> "no_bomb") &
    "name=no_bryan"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_BRYAN), "id" -> "no_bryan") &
    "name=no_catherine"             #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_CATHERINE), "id" -> "no_catherine") &
    "name=no_charles"               #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_CHARLES), "id" -> "no_charles") &
    "name=no_cassandra"             #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_CASSANDRA), "id" -> "no_cassandra") &
    "name=no_cheshire"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_CHESHIRE), "id" -> "no_cheshire") &
    "name=no_david"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_DAVID), "id" -> "no_david") &
    "name=no_daniel"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_DANIEL), "id" -> "no_daniel") &
    "name=no_despair"               #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_DESPAIR), "id" -> "no_despair") &
    "name=no_detective"             #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_DETECTIVE), "id" -> "no_detective") &
    "name=no_judgment"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_JUDGMENT), "id" -> "no_judgment") &
    "name=no_shaheart"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_SHAHEART), "id" -> "no_shaheart") &
    "name=no_hunsoul"               #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_HUNSOUL), "id" -> "no_hunsoul") &
    "name=no_adriatic"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_ADRIATIC), "id" -> "no_adriatic") &
    "name=no_micah"                 #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_MICAH), "id" -> "no_micah") &
    "name=no_seth"                  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_SETH), "id" -> "no_seth") &
    "name=no_dragon"                #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_DRAGON), "id" -> "no_dragon") &
    "name=no_leon"                  #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_LEON), "id" -> "no_leon") &
    "name=no_puzzle"                #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_PUZZLE), "id" -> "no_puzzle") &
    "name=no_westlobe"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_WESTLOBE), "id" -> "no_westlobe") &
    "name=no_lube"                  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_LUBE), "id" -> "no_lube") &
    "name=no_tel"                   #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_TEL), "id" -> "no_tel") &
    "name=neutral_back_1"           #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NEUTRAL_BACK_1), "id" -> "neutral_back_1") &
    "name=neutral_back_2"           #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NEUTRAL_BACK_2), "id" -> "neutral_back_2") &
    "name=all_neutral"              #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ALL_NEUTRAL), "id" -> "all_neutral") &
    "name=init_location"            #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.INIT_LOCATION), "id" -> "init_location") &
    "name=init_green"               #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.INIT_GREEN), "id" -> "init_green") &
    "name=random_position"          #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.RANDOM_POSITION), "id" -> "random_position") &
    "name=four_neutral"             #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.FOUR_NEUTRAL), "id" -> "four_neutral") &
    "name=ultrasoul_ray"            #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ULTRASOUL_RAY), "id" -> "ultrasoul_ray") &
    "name=unseen_resist"            #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.UNSEEN_RESIST), "id" -> "unseen_resist") &
    "name=vampire_weaken"           #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.VAMPIRE_WEAKEN), "id" -> "vampire_weaken") &
    "name=valkyrie_enhance"         #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.VALKYRIE_ENHANCE), "id" -> "valkyrie_enhance") &
    "name=vghost_expand"            #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.VGHOST_EXPAND), "id" -> "vghost_expand") &
    "name=emi_enhance"              #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.EMI_ENHANCE), "id" -> "emi_enhance") &
    "name=emi_send"                 #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.EMI_SEND), "id" -> "emi_send") &
    "name=ellen_heal"               #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ELLEN_HEAL), "id" -> "ellen_heal") &
    "name=evan_heal"                #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.EVAN_HEAL), "id" -> "evan_heal") &
    "name=franklin_reuse"           #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.FRANKLIN_REUSE), "id" -> "franklin_reuse") &
    "name=george_reuse"             #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.GEORGE_REUSE), "id" -> "george_reuse") &
    "name=george_reuse2"            #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.GEORGE_REUSE2), "id" -> "george_reuse2") &
    "name=godfat_reuse"             #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.GODFAT_REUSE), "id" -> "godfat_reuse") &
    "name=emma_reuse"               #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.EMMA_REUSE), "id" -> "emma_reuse") &
    "name=ginger_reuse"             #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.GINGER_REUSE), "id" -> "ginger_reuse") &
    "name=angel_choose"             #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ANGEL_CHOOSE), "id" -> "angel_choose") &
    "name=adecoy_intimate"          #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ADECOY_INTIMATE), "id" -> "adecoy_intimate") &
    "name=bellandona_choose"        #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.BELLANDONA_CHOOSE), "id" -> "bellandona_choose") &
    "name=blackcard_dagger"         #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_DAGGER), "id" -> "blackcard_dagger") &
    "name=blackcard_lamirror"       #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_LAMIRROR), "id" -> "blackcard_lamirror") &
    "name=blackcard_mask"           #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_MASK), "id" -> "blackcard_mask") &
    "name=blackcard_decline"        #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_DECLINE), "id" -> "blackcard_decline") &
    "name=blackcard_firehorse"      #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_FIREHORSE), "id" -> "blackcard_firehorse") &
    "name=blackcard_splintered"     #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_SPLINTERED), "id" -> "blackcard_splintered") &
    "name=blackcard_evilsword"      #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_EVILSWORD), "id" -> "blackcard_evilsword") &
    "name=blackcard_gemwand"        #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_GEMWAND), "id" -> "blackcard_gemwand") &
    "name=blackcard_supplybomb"     #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_SUPPLYBOMB), "id" -> "blackcard_supplybomb") &
    "name=blackcard_explode"        #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_EXPLODE), "id" -> "blackcard_explode") &
    "name=whitecard_tea"            #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.WHITECARD_TEA), "id" -> "whitecard_tea") &
    "name=whitecard_balance"        #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.WHITECARD_BALANCE), "id" -> "whitecard_balance") &
    "name=whitecard_firework"       #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.WHITECARD_FIREWORK), "id" -> "whitecard_firework") &
    "name=whitecard_flyhigh"        #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WHITECARD_FLYHIGH), "id" -> "whitecard_flyhigh") &
    "name=whitecard_enchantment"    #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.WHITECARD_ENCHANTMENT), "id" -> "whitecard_enchantment") &
    "name=whitecard_magicspirit"    #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.WHITECARD_MAGICSPIRIT), "id" -> "whitecard_magicspirit") &
    "name=whitecard_earthquake"     #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WHITECARD_EARTHQUAKE), "id" -> "whitecard_earthquake") &
    "name=whiteCard_volcanic"       #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WHITECARD_VOLCANIC), "id" -> "whiteCard_volcanic") &
    "name=whiteCard_tsunami"        #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WHITECARD_TSUNAMI), "id" -> "whiteCard_tsunami") &
    "name=greencard_hunterheal2"    #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.GREENCARD_HUNTERHEAL2), "id" -> "greencard_hunterheal2") &
    "name=greencard_lifeunder11_2"  #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.GREENCARD_LIFEUNDER11_2), "id" -> "greencard_lifeunder11_2") &
    //"type=submit"                 #> SHtml.onSubmitUnit(() => debug("TEST")) 
     "type=submit"                  #> SHtml.onSubmitUnit(S.callOnce(process))
  }
}
