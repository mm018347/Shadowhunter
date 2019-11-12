package org.plummtw.shadowhunter.heavy

import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import common._
import S._
import SHtml._
import Helpers._


import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.snippet._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.card._
import org.plummtw.shadowhunter.util.{PlummUtil, LocationHelper}

object CardHelper extends Logger {
  def card_table(room : Room, cardpools : List[CardPool]) = {
    if (cardpools.length == 0)
      <span></span>
    else {
      val black_remains = cardpools.filter(x => (x.card_no.is >= room.blackcard_index.is)
                                           && (x.discarded.is == false) && (x.card_type.is == CardTypeEnum.BLACK.toString))
      val white_remains = cardpools.filter(x => (x.card_no.is >= room.whitecard_index.is)
                                           && (x.discarded.is == false) && (x.card_type.is == CardTypeEnum.WHITE.toString))
      val green_remains = cardpools.filter(x => (x.card_no.is >= room.greencard_index.is)
                                           && (x.discarded.is == false) && (x.card_type.is == CardTypeEnum.GREEN.toString))
      
      val black_discards = cardpools.filter(x => (x.discarded.is == true) && (x.card_type.is == CardTypeEnum.BLACK.toString))
      val white_discards = cardpools.filter(x => (x.discarded.is == true) && (x.card_type.is == CardTypeEnum.WHITE.toString))
      val green_discards = cardpools.filter(x => (x.discarded.is == true) && (x.card_type.is == CardTypeEnum.GREEN.toString))
      
      <div class="row">
        <div class="col-6 col-12-small">
          牌庫：黑：{black_remains.length.toString} 白：{white_remains.length.toString} 綠：{green_remains.length.toString}
        </div>
        <div class="col-6 col-12-small">
          棄牌：黑：{black_discards.length.toString} 白：{white_discards.length.toString} 綠：{green_discards.length.toString}    
        </div>
      </div>
    }
  }
  
  def process_green_internal(actioner : UserEntry, actionee : UserEntry, card : Card, userentrys: List[UserEntry]) : Talk = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    //val actioner_role = actioner.get_role
    var actionee_role = actionee.get_hermit_role
    
    if (card.card_enum == CardEnum.G_REVEAL_PREV) {
      if (actioner.get_role == RoleDetective) {
        actionee_role = actionee.get_real_role
      } else if ((actionee_role == RoleNoEffect) || (actionee_role == RoleNone)) {
        actionee_role = actionee.get_role
      }
        
     val talk1 = Talk.create.mtype(MTypeEnum.RESULT_GREENREVEAL.toString)
                      .message("您發現 " + actionee.handle_name.is + " 是 " + actionee_role.role_name)
                      .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
     talk1                 
    } else {
      var arsis_skill = false
      val message =
        card.card_enum match {
          case CardEnum.G_SHADOW_LOSE1        => 
            if (actionee_role.role_side == RoleSideEnum.SHADOW) {
              if (actionee.inflict_g_card_damage(1, actioner))
                actionee.handle_name.is + " 的損傷增加 1 點"
              else ""
            } else ""
          case CardEnum.G_SHADOW_LOSE2        => 
            if (actionee_role.role_side == RoleSideEnum.SHADOW) {
              if (actionee.inflict_g_card_damage(2, actioner))
                actionee.handle_name.is + " 的損傷增加 2 點"
              else ""
            } else ""
          case CardEnum.G_HUNTER_LOSE1         => 
            if (actionee_role.role_side == RoleSideEnum.HUNTER) {
              if (actionee.inflict_g_card_damage(1, actioner))
                actionee.handle_name.is + " 的損傷增加 1 點"
              else ""
            } else ""
          case CardEnum.G_SHADOW_HUNTER_EQUIP  => 
            if ((actionee_role.role_side == RoleSideEnum.SHADOW) ||
                (actionee_role.role_side == RoleSideEnum.HUNTER)) {
              if (actionee.item_preferred.is == CardEnum.PREFER_LIFE.toString) {
                if (actionee.inflict_g_card_damage(1, actioner))
                  actionee.handle_name.is + " 的損傷增加 1 點"
                else ""
              } else if ((actionee.item_preferred.is != "") &&
                         (actionee.has_item(CardEnum.get_card(actionee.item_preferred.is).card_enum))) {
                val robbed_item = CardEnum.get_card(actionee.item_preferred.is)
                GameProcessor.rob_specific(room, roomround, actioner, actionee, robbed_item)
                actioner.beads(actioner.beads.is + actionee.beads.is).save
                actionee.beads(0)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else if (actionee.items.length > 0) {
                val robbed_item = GameProcessor.rob_single(room, roomround, actioner, actionee)
                actioner.beads(actioner.beads.is + actionee.beads.is).save
                actionee.beads(0)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else {
                if (actionee.inflict_g_card_damage(1, actioner))
                  actionee.handle_name.is + " 的損傷增加 1 點"
                else ""
              }
            } else ""
          case CardEnum.G_SHADOW_NEUTRAL_EQUIP => 
            if ((actionee_role.role_side == RoleSideEnum.SHADOW) ||
                (actionee_role.role_side == RoleSideEnum.NEUTRAL)) {
              if (actionee.item_preferred.is == CardEnum.PREFER_LIFE.toString) {
                if (actionee.inflict_g_card_damage(1, actioner))
                  actionee.handle_name.is + " 的損傷增加 1 點"
                else ""
              } else if ((actionee.item_preferred.is != "") &&
                         (actionee.has_item(CardEnum.get_card(actionee.item_preferred.is).card_enum))) {
                val robbed_item = CardEnum.get_card(actionee.item_preferred.is)
                GameProcessor.rob_specific(room, roomround, actioner, actionee, robbed_item)
                actioner.beads(actioner.beads.is + actionee.beads.is).save
                actionee.beads(0)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else if (actionee.items.length > 0) {
                val robbed_item = GameProcessor.rob_single(room, roomround, actioner, actionee)
                actioner.beads(actioner.beads.is + actionee.beads.is).save
                actionee.beads(0)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else {
                if (actionee.inflict_g_card_damage(1, actioner))
                  actionee.handle_name.is + " 的損傷增加 1 點"
                else ""
              }
            } else ""
          case CardEnum.G_HUNTER_NEUTRAL_EQUIP => 
            if ((actionee_role.role_side == RoleSideEnum.HUNTER) ||
                (actionee_role.role_side == RoleSideEnum.NEUTRAL)) {
              if (actionee.item_preferred.is == CardEnum.PREFER_LIFE.toString) {
                if (actionee.inflict_g_card_damage(1, actioner))
                  actionee.handle_name.is + " 的損傷增加 1 點"
                else ""
              } else if ((actionee.item_preferred.is != "") &&
                         (actionee.has_item(CardEnum.get_card(actionee.item_preferred.is).card_enum))) {
                val robbed_item = CardEnum.get_card(actionee.item_preferred.is)
                GameProcessor.rob_specific(room, roomround, actioner, actionee, robbed_item)
                actioner.beads(actioner.beads.is + actionee.beads.is).save
                actionee.beads(0)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else if (actionee.items.length > 0) {
                val robbed_item = GameProcessor.rob_single(room, roomround, actioner, actionee)
                actioner.beads(actioner.beads.is + actionee.beads.is).save
                actionee.beads(0)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else {
                if (actionee.inflict_g_card_damage(1, actioner))
                  actionee.handle_name.is + " 的損傷增加 1 點"
                else ""
              }
            } else ""
          case CardEnum.G_SHADOW_HEAL1         => 
            if ((actionee_role.role_side == RoleSideEnum.SHADOW) ||
                ((actionee_role.role_side == RoleSideEnum.NONE) && (actionee.damaged.is != 0))) {
              if (actionee.damaged.is == 0) {
                if (actionee.inflict_g_card_damage(1, actioner))
                  actionee.handle_name.is + " 的損傷增加 1 點"
                else ""
              } else {
                actionee.lower_damage(1, userentrys)
                actionee.handle_name.is + " 的損傷減少 1 點"
              } 
            } else ""
          case CardEnum.G_HUNTER_HEAL2         => 
            if ((actionee_role.role_side == RoleSideEnum.HUNTER) ||
                ((actionee_role.role_side == RoleSideEnum.NONE) && (actionee.damaged.is != 0))) {
              if (actionee.damaged.is == 0) {
                if (actionee.inflict_g_card_damage(2, actioner))
                  actionee.handle_name.is + " 的損傷增加 2 點"
                else ""
              } else {
                if ((actionee_role == RoleArsis) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
                  arsis_skill = true
                }
                actionee.lower_damage(2, userentrys)
                actionee.handle_name.is + " 的損傷減少 2 點"

              } 
            } else ""
          case CardEnum.G_HUNTER_HEAL1         => 
            if ((actionee_role.role_side == RoleSideEnum.HUNTER) ||
                ((actionee_role.role_side == RoleSideEnum.NONE) && (actionee.damaged.is != 0))) {
              if (actionee.damaged.is == 0) {
                if (actionee.inflict_g_card_damage(1, actioner))
                  actionee.handle_name.is + " 的損傷增加 1 點"
                else ""
              } else {
                if ((actionee_role == RoleArsis) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
                  arsis_skill = true
                }
                actionee.lower_damage(1, userentrys)
                actionee.handle_name.is + " 的損傷減少 1 點"

              } 
            } else ""  
          case CardEnum.G_NEUTRAL_HEAL1        => 
            if ((actionee_role.role_side == RoleSideEnum.NEUTRAL) ||
                ((actionee_role.role_side == RoleSideEnum.NONE) && (actionee.damaged.is != 0))) {
              if (actionee.damaged.is == 0) {
                if (actionee.inflict_g_card_damage(1, actioner))
                  actionee.handle_name.is + " 的損傷減少 1 點"
                else ""
              } else {
                actionee.lower_damage(1, userentrys)
                actionee.handle_name.is + " 的損傷減少 1 點"
              } 
            } else ""
          case CardEnum.G_LIFE_UNDER11_2         => 
            if ((actionee_role.role_life <= 11) ||
                ((actionee_role.role_side == RoleSideEnum.NONE) && (actionee.damaged.is != 0))) {
              if (actionee.damaged.is == 0) {
                if (actionee.inflict_g_card_damage(2, actioner))
                  actionee.handle_name.is + " 的損傷增加 2 點"
                else ""
              } else {
                actionee.lower_damage(2, userentrys)
                actionee.handle_name.is + " 的損傷減少 2 點"
              } 
            } else ""  
          case CardEnum.G_LIFE_OVER12          => 
            if ((actionee_role.role_life >= 12) && (actionee_role != RoleNoEffect))  {
              if (actionee.inflict_g_card_damage(2, actioner))
                actionee.handle_name.is + " 的損傷增加 2 點"
              else ""
            } else ""
          case CardEnum.G_LIFE_UNDER11         => 
            if ((actionee_role.role_life <= 11) && (actionee_role != RoleNoEffect))  {
              if (actionee.inflict_g_card_damage(1, actioner))
                actionee.handle_name.is + " 的損傷增加 1 點"
              else ""
            } else ""
        }
      if (arsis_skill == true) {
        val talka = Talk.create.mtype(MTypeEnum.ACTION_ARSIS.toString)
                      .actioner_id(actioner.id.is)
        talka.save
	    talka.send(actionee.room_id.is)
      }
      val talk1_message = if (message == "") "但是什麼也沒發生" else message
      val talk1 = Talk.create.mtype(MTypeEnum.RESULT_GREENCARD.toString)
                      .message(talk1_message)
                      .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
      talk1
    }
  }
  
  def process_green(action: Action, room: Room, roomround: RoomRound,
                     roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    val actionee_id = action.actionee_id.is
    val actionee_list : List[UserEntry] = userentrys.filter(_.id.is == actionee_id)
    
    if (actionee_list.length == 0) {
      warn("process_green : actionee_list.length == 0")
      warn("room : " + room.id.is)
      warn("roomround : " + roomround.id.is)
      warn("roomphase : " + roomphase.id.is)
      warn("actioner : " + actioner.id.is)
      warn("actionee : " + actionee_id)
    }
    
    val actionee = actionee_list(0)
    val actionee_role = actionee.get_skill_role
        
    val card = CardEnum.get_card(roomphase.phase_flags.is.toString)
    
    val talk1 = process_green_internal(actioner, actionee, card, userentrys)    
    talk1.roomround_id(roomround.id.is)
    talk1.save
    talk1.send(actioner.room_id.is)
    
    //GameProcessor.check_item_victory(room, roomround, actioner)

    GameProcessor.check_death(actionee, actioner, action, userentrys)
    // 檢查遊戲是否結束
    if (!GameProcessor.check_victory(room, roomround, userentrys)) {

      val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                               .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is)
                               .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
      new_phase.save
      RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
      RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TIME_TABLE)))
      RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))  
    }
  }
  
  def process_drawwhite(action: Action, room: Room, roomround: RoomRound,
                          roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    if ((actioner.get_skill_role == RoleEmma) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
      val live_hunters = userentrys.filter (x => (x.get_role.role_side == RoleSideEnum.HUNTER) &&
                                                 (x.live.is) && (x.revealed.is))
      if (live_hunters.length > 0) {
        live_hunters.foreach { live_hunter =>
          val talk_t = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_HUNTER.toString).actioner_id(live_hunter.id.is)
                                 .message(live_hunter.handle_name.is + " 的損傷減少 1 點(艾瑪)")
          talk_t.save
          talk_t.send(live_hunter.room_id.is)
          live_hunter.lower_damage(1, userentrys)
          live_hunter.save
          
          if ((live_hunter.get_role == RoleArsis) && (live_hunter.revealed.is) && (live_hunter.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (live_hunter.hasnt_item(CardEnum.B_MASK))) {
            val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARSIS.toString).actioner_id(live_hunter.id.is)
            talk_a.save
            talk_a.send(live_hunter.room_id.is)
          }
        }
      }
    }
    
    // 菲爾特_白卡回血
    val live_starss = userentrys.filter (x => (x.get_skill_role == RoleStars) &&
                                                 (x.live.is) && (x.revealed.is) && (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (x.hasnt_item(CardEnum.B_MASK)))
    if((live_starss.length > 0) && (actioner.get_skill_role != RoleStars) && (actioner.revealed.is) && (actioner.get_role.role_side == RoleSideEnum.SHADOW)){
      live_starss.foreach { live_stars =>
        live_stars.lower_damage(1, userentrys)
        live_stars.save
        val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_STARS_LOWER.toString).actioner_id(live_stars.id.is)
        talk_a.save
        talk_a.send(live_stars.room_id.is)
      }
    }
    
    val card = CardEnum.get_card(action.action_flags.is)
    
    val phase_type =
      if (card.isInstanceOf[Equipment]) {
        actioner.add_item(card.card_enum)
        actioner.save
        GameProcessor.check_item_victory(room, roomround, actioner)
        RoomPhaseEnum.ATTACK
      } else if (card.isInstanceOf[UserEntryTargetable]) {
        if (actioner.get_skill_role == RoleAki){
          actioner.action_point(1)
          actioner.card_flags(card.card_enum)
          actioner.save
        }
        RoomPhaseEnum.CARD
      } else {
        if (actioner.get_skill_role == RoleAki){
          actioner.action_point(1)
          actioner.card_flags(card.card_enum)
          actioner.save
        }
        card.card_enum match {
          case CardEnum.W_HOLY_WATER_OF_HEALING =>
            actioner.lower_damage(2, userentrys)
            actioner.save
            val talk_t = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString).actioner_id(actioner.id.is)
                               .message(actioner.handle_name.is + " 的損傷減少 2 點")
            talk_t.save
            talk_t.send(actioner.room_id.is)
          case CardEnum.W_ADVENT                =>
            val role = actioner.get_role
            actioner.add_user_flag(UserEntryFlagEnum.ADVENT)
            if (actioner.has_user_flag(UserEntryFlagEnum.ADVENT) && (role.role_side == RoleSideEnum.HUNTER) && (actioner.revealed))
              actioner.damaged(0)
            actioner.save
          case CardEnum.W_CHOCOLATE             =>
            val role = actioner.get_role
            val life_thresh = role.role_side match {
              case RoleSideEnum.NEUTRAL => 8
              case RoleSideEnum.SHADOW => 11
              case RoleSideEnum.HUNTER => 11
            }
            if (role == RoleCheshire) {
                val talk_t = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString).actioner_id(actioner.id.is)
                               .message("貓咪不可以吃巧克力！")
                talk_t.save
                talk_t.send(actioner.room_id.is)
                val saved_damaged = actioner.damaged.is
                actioner.inflict_b_card_damage(99, actioner)
                GameProcessor.check_death(actioner, actioner, action, userentrys)
                //actioner.damaged(saved_damaged)
                actioner.save 
            }
            if (role.role_life <= life_thresh) {
              actioner.add_user_flag(UserEntryFlagEnum.CHOCOLATE)
              if (actioner.has_user_flag(UserEntryFlagEnum.CHOCOLATE) && (actioner.revealed))
                actioner.damaged(0) 
              actioner.save 
            }
          case CardEnum.W_CONCEALED_KNOWLEDGE   =>
            roomphase.additional(roomphase.additional.is + 1).save
          // <!--[守護天使]-->
          case CardEnum.W_GUARDIAN_ANGEL        =>
            actioner.add_user_flag(UserEntryFlagEnum.GUARDIAN)
            actioner.save
          // <!--[閃電制裁]-->
          case CardEnum.W_FLARE_OF_JUDGEMENT    =>
            val userentrys_r = userentrys.filter(x => (!x.revoked.is) && (x.live.is) && (x.id.is != actioner.id.is) )
            val death_number = userentrys.filter(x => !x.live.is).length
              
            userentrys_r.foreach { userentry1 =>
              if (userentry1.hasnt_item(CardEnum.B_GEMWAND)) {
                if (userentry1.inflict_card_damage(2, actioner)) {
                  val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                              .message(userentry1.handle_name.is + " 的損傷增加 2 點")
                  talk2.save
                  talk2.send(userentry1.room_id.is)
                  GameProcessor.check_death(userentry1, actioner, action, userentrys)
                }
              } else {
                val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                              .message(userentry1.handle_name.is + " 的損傷增加 0 點(血寶石魔杖)")
                talk2.save
                talk2.send(userentry1.room_id.is)
              }
            }
            
            val death_number2 = userentrys.filter(x => !x.live.is).length
            if ((actioner.get_role == RolePuzzle) && (death_number2 - death_number >= 2)) {
              userentrys.filter(_.get_role == RolePuzzle).foreach { userentry1 =>
                userentry1.add_user_flag(UserEntryFlagEnum.VICTORY)
              }
            }
          // <!--[大地震]-->
          case CardEnum.W_EARTHQUAKE    =>
            val userentrys_r = userentrys.filter(x => (!x.revoked.is) && (x.live.is))
            val death_number = userentrys.filter(x => !x.live.is).length
            val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                              .message("目標區域：隱士木屋、教堂、怪異樹林")
            talk1.save
            talk1.send(actioner.room_id.is)
              
            userentrys_r.foreach { userentry1 =>
              if ( (userentry1.live.is)
                  && ( (userentry1.location.is == LocationEnum.HERMIT_CABIN.toString)
                  || (userentry1.location.is == LocationEnum.CHURCH.toString)
                  || (userentry1.location.is == LocationEnum.WIERD_WOODS.toString) ) 
              ) {
                if (userentry1.hasnt_item(CardEnum.B_GEMWAND)) {
                  if (userentry1.inflict_card_damage(2, actioner)) {
                    val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                                .message(userentry1.handle_name.is + " 的損傷增加 2 點")
                    talk2.save
                    talk2.send(userentry1.room_id.is)
                    if (GameProcessor.check_death(userentry1, actioner, action, userentrys)) {
                      // <!--[露比勝利判定]-->
                      if ( (actioner.get_role == RoleLube)
                        && (actioner.has_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED))
                        && (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString)
                        && (userentry1.get_role.role_side == RoleSideEnum.SHADOW) ) {
                        actioner.add_user_flag(UserEntryFlagEnum.VICTORY)
                      }
                    }
                  }
                } else {
                  val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                                .message(userentry1.handle_name.is + " 的損傷增加 0 點(血寶石魔杖)")
                  talk2.save
                  talk2.send(userentry1.room_id.is)
                }
              }
            }
            
            val death_number2 = userentrys.filter(x => !x.live.is).length
            if ((actioner.get_role == RolePuzzle) && (death_number2 - death_number >= 2)) {
              userentrys.filter(_.get_role == RolePuzzle).foreach { userentry1 =>
                userentry1.add_user_flag(UserEntryFlagEnum.VICTORY)
              }
            }
          // <!--[火山爆發]-->
          case CardEnum.W_VOLCANIC    =>
            val userentrys_r = userentrys.filter(x => (!x.revoked.is) && (x.live.is))
            val death_number = userentrys.filter(x => !x.live.is).length
            val random1d6 = GameProcessor.random.nextInt(6) + 1
            val random1d4 = GameProcessor.random.nextInt(4) + 1
            
            val new_location = LocationEnum.from_dice(random1d6 + random1d4)
            val location_str = 
              if (new_location == LocationEnum.OPTION)
                "失敗"
              else
                "目標區域與鄰近區域除外：" + LocationEnum.get_cname(new_location)
            
            val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                              .message("1D6=" + random1d6 + ", 1D4=" + random1d4 + " " + location_str)
            talk1.save
            talk1.send(actioner.room_id.is)
              
            userentrys_r.foreach { userentry1 =>
              if ( (userentry1.location.is != new_location.toString)
                && (userentry1.location.is != LocationHelper.neighbor(room, new_location.toString))
              ) {
                if (userentry1.hasnt_item(CardEnum.B_GEMWAND)) {
                  if (userentry1.inflict_card_damage(3, actioner)) {
                    val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                                .message(userentry1.handle_name.is + " 的損傷增加 3 點")
                    talk2.save
                    talk2.send(userentry1.room_id.is)
                    if (GameProcessor.check_death(userentry1, actioner, action, userentrys)) {
                      // <!--[露比勝利判定]-->
                      if ( (actioner.get_role == RoleLube)
                        && (actioner.has_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED))
                        && (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString)
                        && (userentry1.get_role.role_side == RoleSideEnum.SHADOW) ) {
                        actioner.add_user_flag(UserEntryFlagEnum.VICTORY)
                      }
                    }
                  }
                } else {
                  val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                                .message(userentry1.handle_name.is + " 的損傷增加 0 點(血寶石魔杖)")
                  talk2.save
                  talk2.send(userentry1.room_id.is)
                }
              }
            }
            
            val death_number2 = userentrys.filter(x => !x.live.is).length
            if ((actioner.get_role == RolePuzzle) && (death_number2 - death_number >= 2)) {
              userentrys.filter(_.get_role == RolePuzzle).foreach { userentry1 =>
                userentry1.add_user_flag(UserEntryFlagEnum.VICTORY)
              }
            }
          // <!--[大海嘯]-->
          case CardEnum.W_TSUNAMI    =>
            val userentrys_r = userentrys.filter(x => (!x.revoked.is) && (x.live.is))
            val death_number = userentrys.filter(x => !x.live.is).length
            val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                              .message("目標區域：隱士木屋、時空之門、教堂")
            talk1.save
            talk1.send(actioner.room_id.is)
              
            userentrys_r.foreach { userentry1 =>
              if ( (userentry1.live.is)
                  && ( (userentry1.location.is == LocationEnum.HERMIT_CABIN.toString)
                  || (userentry1.location.is == LocationEnum.CHURCH.toString)
                  || (userentry1.location.is == LocationEnum.UNDERWORLD_GATE.toString) ) 
              ) {
                if (userentry1.hasnt_item(CardEnum.B_GEMWAND)) {
                  if (userentry1.inflict_card_damage(2, actioner)) {
                    val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                                .message(userentry1.handle_name.is + " 的損傷增加 2 點")
                    talk2.save
                    talk2.send(userentry1.room_id.is)
                    if (GameProcessor.check_death(userentry1, actioner, action, userentrys)) {
                      // <!--[露比勝利判定]-->
                      if ( (actioner.get_role == RoleLube)
                        && (actioner.has_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED))
                        && (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString)
                        && (userentry1.get_role.role_side == RoleSideEnum.SHADOW) ) {
                        actioner.add_user_flag(UserEntryFlagEnum.VICTORY)
                      }
                    }
                  }
                } else {
                  val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                                .message(userentry1.handle_name.is + " 的損傷增加 0 點(血寶石魔杖)")
                  talk2.save
                  talk2.send(userentry1.room_id.is)
                }
              }
            }
            
            val death_number2 = userentrys.filter(x => !x.live.is).length
            if ((actioner.get_role == RolePuzzle) && (death_number2 - death_number >= 2)) {
              userentrys.filter(_.get_role == RolePuzzle).foreach { userentry1 =>
                userentry1.add_user_flag(UserEntryFlagEnum.VICTORY)
              }
            }
          case CardEnum.W_DISENCHANTED_MIRROR   =>
            val role = actioner.get_role
            if ((!actioner.revealed) && (role.role_side == RoleSideEnum.SHADOW) &&
                (role.role_life >= 12) && (role != RoleUnknown)) {
              GameProcessor.flip(actioner, action, userentrys)
            }
          case CardEnum.W_TEA   =>
            val items_num = actioner.items.length
            if (items_num == 0){
              actioner.lower_damage(3, userentrys)
              actioner.save
              
              val talk_t = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString).actioner_id(actioner.id.is)
                               .message(actioner.handle_name.is + " 的損傷減少 3 點")
              talk_t.save
              talk_t.send(actioner.room_id.is)
              
              if ((actioner.get_skill_role == RoleArsis) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
                val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARSIS.toString).actioner_id(actioner.id.is)
                talk_a.save
                talk_a.send(actioner.room_id.is)
              }

            } else {
              val talk_t = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString).actioner_id(actioner.id.is)
                               .message(actioner.handle_name.is + " 的損傷增加 " + items_num + " 點")
              talk_t.save
              talk_t.send(actioner.room_id.is)
              if (actioner.inflict_card_damage(items_num, actioner))
                GameProcessor.check_death(actioner, actioner, action, userentrys)
              if (!actioner.live.is) {
                GameProcessor.next_player(room, roomround, roomphase, userentrys)
              }
            }
          case CardEnum.W_GODDESS   =>
            val l_users = userentrys.filter(x => (x.location.is == LocationHelper.neighbor(room, actioner.location.is)) ||
                               (x.location.is == actioner.location.is))
            l_users.foreach { l_user =>
              l_user.lower_damage(3, userentrys)
              l_user.save
              val talk_t = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString).actioner_id(l_user.id.is)
                               .message(l_user.handle_name.is + " 的損傷減少 3 點")
              talk_t.save
              talk_t.send(l_user.room_id.is)
              if ((l_user.get_skill_role == RoleArsis) && (l_user.revealed.is) && (l_user.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (l_user.hasnt_item(CardEnum.B_MASK))) {
                val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARSIS.toString).actioner_id(l_user.id.is)
                talk_a.save
                talk_a.send(l_user.room_id.is)
              }
            }
          case CardEnum.W_ENCHANTMENT  =>
            actioner.add_user_flag(UserEntryFlagEnum.ENCHANTMENT)
            actioner.save
          case CardEnum.W_FIREWORK  =>
            val l_users = userentrys.filter(x => (x.live.is))
            val talk_t = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString).actioner_id(actioner.id.is)
                               .message("稅金爆炸了，全部玩家暫時無法攻擊")
            talk_t.save
            talk_t.send(actioner.room_id.is)
            l_users.foreach { l_user =>
              l_user.add_user_flag(UserEntryFlagEnum.FIREWORK)
              l_user.save
            }
          case CardEnum.W_MAGICSPIRIT  =>
            actioner.add_user_flag(UserEntryFlagEnum.MAGICSPIRIT)
            actioner.save
          case CardEnum.W_FLYHIGH  =>
            val l_users = userentrys.filter(x => (x.live.is) && (!x.revoked.is))
            var all_damages = 0
            l_users.foreach { l_user =>
              all_damages += l_user.damaged.is
            }
            var average_damages : Int = (all_damages / l_users.length)
            val talk_t = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString).actioner_id(actioner.id.is)
                               .message("全部玩家的損傷值設定為 (" + all_damages + " / " + l_users.length + ") = " + average_damages)
            talk_t.save
            talk_t.send(actioner.room_id.is)
            l_users.foreach { l_user =>
              l_user.damaged(average_damages).save
              GameProcessor.check_death(l_user, l_user, action, userentrys)
            }
            if (!actioner.live.is) {
              GameProcessor.next_player(room, roomround, roomphase, userentrys)
            }
        }
        RoomPhaseEnum.ATTACK
      }
    
    val phase_type2 = 
      if (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) {
        if (phase_type == RoomPhaseEnum.CARD)
          RoomPhaseEnum.CARD_SKILL
        else if (phase_type == RoomPhaseEnum.ATTACK)
          RoomPhaseEnum.MOVEMENT
        else phase_type
      } else if ((roomphase.phase_type.is != RoomPhaseEnum.LOCATION.toString) && (actioner.get_skill_role == RoleAki)) {
        if (phase_type == RoomPhaseEnum.CARD)
          RoomPhaseEnum.CARD_AKI
        else RoomPhaseEnum.ENDED
      } else phase_type
    
    if (!GameProcessor.check_victory(room, roomround, userentrys)) {
      if ((!actioner.live.is) || (phase_type2 == RoomPhaseEnum.ENDED))
        GameProcessor.next_player(room, roomround, roomphase, userentrys)
      else {
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                    .phase_type(phase_type2.toString).player(roomphase.player.is).phase_flags(action.action_flags.is)
                                    .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
      
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys=userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      }
    }  
  }
                       
  def process_white(action: Action, room: Room, roomround: RoomRound,
                    roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    val actionee_id = action.actionee_id.is
    val actionee : UserEntry = userentrys.filter(_.id.is == actionee_id)(0)
    
    val card = CardEnum.get_card(action.action_flags.is)
    card.card_enum match {
      case CardEnum.W_BLESSING =>
        val heal1d6 = GameProcessor.random.nextInt(6) + 1

        actionee.lower_damage(heal1d6, userentrys)
        actionee.save

        
        val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                        .message(actionee.handle_name.is + " 的損傷減少 (1D6 = " + heal1d6 + ") 點")
                        .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
        talk1.save
        talk1.send(actioner.room_id.is)
        
        if ((actionee.get_skill_role == RoleArsis) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARSIS.toString).actioner_id(actioner.id.is)
          talk_a.save
          talk_a.send(actioner.room_id.is)
          
        }

      case CardEnum.W_FIRST_AID =>

        actionee.damaged(7)
        actionee.save
    }
    
    val phase_type = 
      if (roomphase.phase_type.is == RoomPhaseEnum.CARD_SKILL.toString) 
        RoomPhaseEnum.MOVEMENT
      else if (roomphase.phase_type.is == RoomPhaseEnum.CARD_AKI.toString) 
        RoomPhaseEnum.ENDED
      else
        RoomPhaseEnum.ATTACK

    if (!GameProcessor.check_victory(room, roomround, userentrys)) {
      if ((!actioner.live.is) || (phase_type == RoomPhaseEnum.ENDED))
        GameProcessor.next_player(room, roomround, roomphase, userentrys)
      else {
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                 .phase_type(phase_type.toString).player(roomphase.player.is).phase_flags(action.action_flags.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys=userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      }
    }
  }
  
  def process_drawblack(action: Action, room: Room, roomround: RoomRound,
                          roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    if ((actioner.get_skill_role == RoleEmma) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
      val live_shadows = userentrys.filter (x => (x.get_role.role_side == RoleSideEnum.SHADOW) &&
                                                 (x.live.is) && (x.revealed.is))
      live_shadows.foreach { live_shadow =>
        live_shadow.inflict_damage(1, actioner)
        GameProcessor.check_death(live_shadow, actioner, action, userentrys)
      }
    }
    
    val live_starss = userentrys.filter (x => (x.get_skill_role == RoleStars) &&
                                                 (x.live.is) && (x.revealed.is) && (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (x.hasnt_item(CardEnum.B_MASK)))
    if((live_starss.length > 0) && (actioner.get_skill_role != RoleStars)){
      val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_STARS_DAMAGE.toString).actioner_id(actioner.id.is)
      talk_a.save
      talk_a.send(actioner.room_id.is)
      actioner.inflict_damage(2, actioner)
      if ((actioner.get_skill_role == RoleLion) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
        val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actioner.id.is)
        talk.save
        talk.send(actioner.room_id.is)
      }
      GameProcessor.check_death(actioner, actioner, action, userentrys)
    }
    
/*    //柴郡貓認主
    if (actioner.get_role != RoleCheshire) {
      val cheshires = userentrys.filter(_.get_role == RoleCheshire)
      cheshires.foreach { cheshire =>
        if (cheshire.target_user.is == 0)
          cheshire.target_user(actioner.id.is).save
      }
    }*/
    
    val card = CardEnum.get_card(action.action_flags.is)
    
    val phase_type =
      if (card.isInstanceOf[Equipment]) {
        actioner.add_item(card.card_enum)
        actioner.save
        GameProcessor.check_item_victory(room, roomround, actioner)
        RoomPhaseEnum.ATTACK
      } else if (card.isInstanceOf[UserEntryTargetable]) {
        if (actioner.get_skill_role == RoleAki) {
          actioner.action_point(0)
          actioner.card_flags(card.card_enum)
          actioner.save
        }
        RoomPhaseEnum.CARD
      } else {
        if (actioner.get_skill_role == RoleAki) {
          actioner.action_point(0)
          actioner.card_flags(card.card_enum)
          actioner.save
        }
        card.card_enum match {
          case CardEnum.B_DYNAMITE =>
            val userentrys_r = userentrys.filter(x => (!x.revoked.is) && (x.live.is))
            val death_number = userentrys.filter(x => !x.live.is).length
            val random1d6 = GameProcessor.random.nextInt(6) + 1
            val random1d4 = GameProcessor.random.nextInt(4) + 1
            var damage_num = 3
            if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
              damage_num = damage_num + 2
              val talk_mr = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                            .message("黑卡的「增加損傷」效果的數值額外增加 2 點(魔魂師)")
              talk_mr.save
              talk_mr.send(actioner.room_id.is)
            }
            
            val new_location = LocationEnum.from_dice(random1d6 + random1d4)
            val location_str = 
              if (new_location == LocationEnum.OPTION)
                "失敗"
              else
                "目標地點：" + LocationEnum.get_cname(new_location)
            
            val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message("1D6=" + random1d6 + ", 1D4=" + random1d4 + " " + location_str)
            talk1.save
            talk1.send(actioner.room_id.is)
              
            userentrys_r.foreach { userentry1 =>
              if (userentry1.hasnt_item(CardEnum.W_TALISMAN) && (userentry1.location.is == new_location.toString))
                if (userentry1.inflict_card_damage(damage_num, actioner)) {
                  val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(userentry1.handle_name.is + " 的損傷增加 " + damage_num + " 點")
                  talk2.save
                  talk2.send(userentry1.room_id.is)
                  if((userentry1.get_skill_role == RoleLion) && (userentry1.revealed.is) && (userentry1.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (userentry1.hasnt_item(CardEnum.B_MASK))){
                    val talk_l = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(userentry1.id.is).actionee_id(userentry1.id.is)
                    talk_l.save
                    talk_l.send(userentry1.room_id.is)
                  }
                  GameProcessor.check_death(userentry1, actioner, action, userentrys)
                }
            }
            
            val death_number2 = userentrys.filter(x => !x.live.is).length
            if ((actioner.get_role == RolePuzzle) && (death_number2 - death_number >= 2)) {
              userentrys.filter(_.get_role == RolePuzzle).foreach { userentry1 =>
                userentry1.add_user_flag(UserEntryFlagEnum.VICTORY)
              }
            }
          case CardEnum.B_DIABOLIC_RITUAL                =>
            val role = actioner.get_role
            actioner.add_user_flag(UserEntryFlagEnum.DIABOLIC)
            if (actioner.has_user_flag(UserEntryFlagEnum.DIABOLIC) && (role.role_side == RoleSideEnum.SHADOW) && (actioner.revealed))
              actioner.damaged(0)
            actioner.save 
          case CardEnum.B_BANANA_PEEL             =>
            if (actioner.items.length > 0) {
              val userentrys_r = userentrys.filter(x => (!x.revoked.is) && (x.live.is))
              val index = userentrys_r.indexOf(actioner)
              val prev_index = (index + userentrys_r.length - 1) % (userentrys_r.length)
              val prev_userentry = userentrys_r(prev_index)

              val robbed_item = GameProcessor.rob_single(room, roomround, prev_userentry, actioner)
              actioner.beads(actioner.beads.is + prev_userentry.beads.is).save
              prev_userentry.beads(0).save
              val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                      .message(actioner.handle_name.is + " 對 " + prev_userentry.handle_name.is + " 給予 " + CardEnum.get_card(robbed_item.card_enum).card_name)
              talk1.save
              talk1.send(actioner.room_id.is)
              
              //GameProcessor.check_item_victory(room, roomround, prev_userentry) 
            } else {
              var damage_num = 1
              if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
                damage_num = 0
                val talk_me = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                              .message("黑卡的「增加損傷」效果的數值變更為 0 點(魔魂師)")
                talk_me.save
                talk_me.send(actioner.room_id.is)
              }
              val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(actioner.handle_name.is + " 的損傷增加 " + damage_num + " 點")
              talk1.save
              talk1.send(actioner.room_id.is)
              if((actioner.get_skill_role == RoleLion) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))){
                val talk_l = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                talk_l.save
                talk_l.send(actioner.room_id.is)
              }
              if (actioner.inflict_card_damage(damage_num, actioner))
                GameProcessor.check_death(actioner, actioner, action, userentrys)
            }
            actioner.save
          case CardEnum.B_PUPIL   =>
            val role = actioner.get_role
            if ((!actioner.revealed) && (role.role_side == RoleSideEnum.HUNTER) &&
                (role.role_life >= 11)) {
              GameProcessor.flip(actioner, action, userentrys)
            }
          case CardEnum.B_DECLINE   =>
            val live_userentrys = userentrys.filter(x => (!x.revoked.is) && (x.live.is))
            var max_d = 0
            var min_d = 13
            live_userentrys.foreach { live_userentry =>
              if (live_userentry.damaged.is > max_d){
                max_d = live_userentry.damaged.is
              }
              if (live_userentry.damaged.is < min_d){
                min_d = live_userentry.damaged.is
              }
            }
            if (max_d > 13) {
              max_d = 13
            }
            val min_userentrys = userentrys.filter(x => (!x.revoked.is) && (x.live.is) && (x.damaged.is == min_d) && (x.hasnt_item(CardEnum.W_TALISMAN)))
            min_userentrys.foreach { min_userentry =>
              val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(min_userentry.handle_name.is + " 的損傷設定為 " + max_d)
              talk1.save
              talk1.send(min_userentry.room_id.is)
              
              min_userentry.damaged(max_d)
              GameProcessor.check_death(min_userentry, min_userentry, action, userentrys)
              min_userentry.save
            }
          // [爆炸]
          case CardEnum.B_EXPLODE   =>
            val supplybomb_userentrys = userentrys.filter(x => (!x.revoked.is) && (x.live.is) && (x.has_item(CardEnum.B_SUPPLYBOMB)))
            if (supplybomb_userentrys.length > 0) {
              if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
                val talk_mr = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                              .message("黑卡的「增加損傷」效果的數值額外增加 2 點(魔魂師)")
                talk_mr.save
                talk_mr.send(actioner.room_id.is)
              }
              supplybomb_userentrys.foreach { supplybomb_userentry =>
                supplybomb_userentry.remove_item(CardEnum.B_SUPPLYBOMB)
    
                // 從 CardPool 修改 owner 資訊
                val card = CardPool.find(By(CardPool.room_id, actioner.room_id.is),
                                         By(CardPool.card, CardEnum.B_SUPPLYBOMB.toString)).get
                card.owner_id(0).discarded(true).save
                
                val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                             .message(supplybomb_userentry.handle_name.is + " 身上的 供品炸彈 爆炸了")
                talk.save
                talk.send(supplybomb_userentry.room_id.is)
                
                val location_users = userentrys.filter(x => ((x.location.is == LocationHelper.neighbor(room, supplybomb_userentry.location.is)) || (x.location.is == supplybomb_userentry.location.is)) && (x.hasnt_item(CardEnum.W_TALISMAN)))
                location_users.foreach { location_user =>
                  var damage_num = 4
                  if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
                    damage_num = damage_num + 2
                  }
                  if ((location_user.get_skill_role == RoleMagician) && (location_user.revealed.is) && (location_user.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (location_user.hasnt_item(CardEnum.B_MASK))) {
                    damage_num = 0
                    val talk_mr = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                                  .message("黑卡的「增加損傷」效果的數值變更為 0 點(魔魂師)")
                    talk_mr.save
                    talk_mr.send(location_user.room_id.is)
                  }
                  val talk_f = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                             .message("轟隆！ " + location_user.handle_name.is + " 的損傷增加 " + damage_num + " 點(供品炸彈)")
                  talk_f.save
                  talk_f.send(location_user.room_id.is)
                  if ((location_user.get_skill_role == RoleLion) && (location_user.revealed) && (location_user.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (location_user.hasnt_item(CardEnum.B_MASK))) {
                    val talk_l = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(location_user.id.is)
                    talk_l.save
                    talk_l.send(location_user.room_id.is)
                  }
                  if (location_user.inflict_card_damage(damage_num, actioner)) {
                    GameProcessor.check_death(location_user, actioner, action, userentrys)
                    location_user.save
                  }
                }
              }
            } else {
              var damage_num = 2
              if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
                damage_num = 0
                val talk_mr = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                              .message("黑卡的「增加損傷」效果的數值變更為 0 點(魔魂師)")
                talk_mr.save
                talk_mr.send(actioner.room_id.is)
              }
              if (actioner.hasnt_item(CardEnum.W_TALISMAN)) {
                val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                             .message(actioner.handle_name.is + " 的損傷增加 " + damage_num + " 點")
                talk.save
                talk.send(actioner.room_id.is)
                if (actioner.inflict_card_damage(damage_num, actioner)) {
                      GameProcessor.check_death(actioner, actioner, action, userentrys)
                      actioner.save
                }
              } else {
                val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                             .message(actioner.handle_name.is + " 的損傷增加 " + damage_num + " 點(護身符)")
                talk.save
                talk.send(actioner.room_id.is)
              }
            }
            
        }
        RoomPhaseEnum.ATTACK
      }
      
    val phase_type2 = 
      if (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) {
        if (phase_type == RoomPhaseEnum.CARD)
          RoomPhaseEnum.CARD_SKILL
        else if (phase_type == RoomPhaseEnum.ATTACK)
          RoomPhaseEnum.MOVEMENT
        else phase_type
      } else if ((roomphase.phase_type.is != RoomPhaseEnum.LOCATION.toString) && (actioner.get_skill_role == RoleAki)) {
        if (phase_type == RoomPhaseEnum.CARD)
          RoomPhaseEnum.CARD_AKI
        else RoomPhaseEnum.ENDED
      } else phase_type
    
    if (!GameProcessor.check_victory(room, roomround, userentrys)) {
      if ((!actioner.live.is) || (phase_type2 == RoomPhaseEnum.ENDED))
        GameProcessor.next_player(room, roomround, roomphase, userentrys)
      else {
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                    .phase_type(phase_type2.toString).player(roomphase.player.is).phase_flags(action.action_flags.is)
                                    .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
      
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys=userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      }
    }
  }  
  
  def process_black(action: Action, room: Room, roomround: RoomRound,
                    roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    val actionee_id = action.actionee_id.is
    val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
    
    val card = CardEnum.get_card(action.action_flags.is)
    card.card_enum match {
      case CardEnum.B_VAMPIRE_BAT            =>
        var damage_num = 2
        if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
          damage_num = damage_num + 2
          val talk_mr = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                        .message("黑卡的「增加損傷」效果的數值額外增加 2 點(魔魂師)")
          talk_mr.save
          talk_mr.send(actioner.room_id.is)
        }
        if ((actionee.get_skill_role == RoleMagician) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          damage_num = 0
          val talk_me = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                        .message("黑卡的「增加損傷」效果的數值變更為 0 點(魔魂師)")
          talk_me.save
          talk_me.send(actionee.room_id.is)
        }
        if (actionee.hasnt_item(CardEnum.W_TALISMAN)) {
          
          if (actionee.inflict_card_damage(damage_num, actioner))
            GameProcessor.check_death(actionee, actioner, action, userentrys)
          actioner.lower_damage(1, userentrys)
          actioner.save
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(actioner.handle_name.is + " 的損傷減少 1 點")
          talk1.save
          talk1.send(actioner.room_id.is)
          val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(actionee.handle_name.is + " 的損傷增加 " + damage_num + " 點")
          talk2.save
          talk2.send(actioner.room_id.is)
          if((actionee.get_skill_role == RoleLion) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))){
            val talk_l = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
            talk_l.save
            talk_l.send(actioner.room_id.is)
          }
          if ((actioner.get_skill_role == RoleArsis) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
            val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARSIS.toString).actioner_id(actioner.id.is)
            talk_a.save
            talk_a.send(actioner.room_id.is)
          }
        } else {
          actioner.lower_damage(1, userentrys)
          actioner.save
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(actioner.handle_name.is + " 的損傷減少 1 點")
          talk1.save
          talk1.send(actioner.room_id.is)
          if ((actioner.get_skill_role == RoleArsis) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
            val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARSIS.toString).actioner_id(actioner.id.is)
            talk_a.save
            talk_a.send(actioner.room_id.is)
          }
          val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(actionee.handle_name.is + " 的損傷增加 0 點(護身符)")
          talk2.save
          talk2.send(actioner.room_id.is)
        }
      case CardEnum.B_BLOODTHIRSTY_SPIDER    =>
        val death_number = userentrys.filter(x => !x.live.is).length
        var damage_num = 2
        if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
          damage_num = damage_num + 2
          val talk_mr = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                        .message("黑卡的「增加損傷」效果的數值額外增加 2 點(魔魂師)")
          talk_mr.save
          talk_mr.send(actioner.room_id.is)
        }
        if ((actionee.get_skill_role == RoleMagician) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          damage_num = 0
          val talk_me = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                        .message("黑卡的「增加損傷」效果的數值變更為 0 點(魔魂師)")
          talk_me.save
          talk_me.send(actionee.room_id.is)
        }
        if (actionee.hasnt_item(CardEnum.W_TALISMAN)) {
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                                .message(actionee.handle_name.is + " 的損傷增加 " + damage_num + " 點")
          talk1.save
          talk1.send(actionee.room_id.is)
          if((actionee.get_skill_role == RoleLion) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))){
            val talk_l = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
            talk_l.save
            talk_l.send(actioner.room_id.is)
          }
          if (actionee.inflict_card_damage(damage_num, actioner))
            GameProcessor.check_death(actionee, actioner, action, userentrys)
        } else {
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                                .message(actionee.handle_name.is + " 的損傷增加 0 點(護身符)")
          talk1.save
          talk1.send(actionee.room_id.is)
        }
        damage_num = 2
        if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
          damage_num = 0
          val talk_me2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                        .message("黑卡的「增加損傷」效果的數值變更為 0 點(魔魂師)")
          talk_me2.save
          talk_me2.send(actioner.room_id.is)
        }
        if (actioner.hasnt_item(CardEnum.W_TALISMAN)) {
          val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                                .message(actioner.handle_name.is + " 的損傷增加 " + damage_num + " 點")
          talk2.save
          talk2.send(actioner.room_id.is)
          if (actioner.inflict_card_damage(damage_num, actioner))
            GameProcessor.check_death(actioner, actioner, action, userentrys)
        } else {
          val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                                .message(actioner.handle_name.is + " 的損傷增加 0 點(護身符)")
          talk2.save
          talk2.send(actioner.room_id.is)
        }
          
        val death_number2 = userentrys.filter(x => !x.live.is).length 
        if ((actioner.get_role == RolePuzzle) && (death_number2 - death_number >= 2)) {
          userentrys.filter(_.get_role == RolePuzzle).foreach { userentry1 =>
            userentry1.add_user_flag(UserEntryFlagEnum.VICTORY)
          }
        }  
      case CardEnum.B_MOODY_GOBLIN          =>
        val message = 
          if (actionee.items.length > 0) {
            val robbed_item = GameProcessor.rob_single(room, roomround, actioner, actionee)
            actioner.beads(actioner.beads.is + actionee.beads.is).save
            actionee.beads(0)
            actionee.save
            "搶奪裝備： " + CardEnum.get_card(robbed_item.card_enum).card_name
          } else {
            actioner.beads(actioner.beads.is + actionee.beads.is).save
            actionee.beads(0).save
            "但是什麼也沒有搶到"
          }
                                        
        val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                    .message(message)
        talk1.save
        talk1.send(actioner.room_id.is)
      case CardEnum.B_SPIRITUAL_DOLL          =>
        var damage_num = 3
        val random1d6 = GameProcessor.random.nextInt(6) + 1
        val target = if (random1d6 < 5) actionee else actioner
        val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message("擲骰結果：1D6=" + random1d6)
        talk.save
        talk.send(actioner.room_id.is)
        if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
          damage_num = damage_num + 2
          val talk_mr = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                        .message("黑卡的「增加損傷」效果的數值額外增加 2 點(魔魂師)")
          talk_mr.save
          talk_mr.send(actioner.room_id.is)
        }
        if ((target.get_skill_role == RoleMagician) && (target.revealed.is) && (target.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (target.hasnt_item(CardEnum.B_MASK))) {
          damage_num = 0
          val talk_me = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                        .message("黑卡的「增加損傷」效果的數值變更為 0 點(魔魂師)")
          talk_me.save
          talk_me.send(target.room_id.is)
        }
        if (target.hasnt_item(CardEnum.W_TALISMAN)) {
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                            .message(target.handle_name.is + " 的損傷增加 " + damage_num + " 點")
          talk1.save
          talk1.send(actioner.room_id.is)
          if((target.get_skill_role == RoleLion) && (target.revealed.is) && (target.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (target.hasnt_item(CardEnum.B_MASK))){
            val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(target.id.is)
            talk1.save
            talk1.send(actioner.room_id.is)
          }
          if (target.inflict_card_damage(damage_num, actioner))
          GameProcessor.check_death(target, actioner, action, userentrys)
        } else {
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                            .message(target.handle_name.is + " 的損傷增加 0 點(護身符)")
          talk1.save
          talk1.send(actioner.room_id.is)
        }
        
        
        
      case CardEnum.B_LAMIRROR          =>
        if (!actionee.revealed.is) {
          GameProcessor.flip(actionee, action, userentrys)
          if (actionee.get_skill_role == RoleAnimalBones) {
            actionee.damaged(0)
            actionee.save
            val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                              .message(actionee.handle_name.is + " 的損傷設定為 0 (獸骸)")
            talk1.save
            talk1.send(actioner.room_id.is)
          }
        }
      case CardEnum.B_GIVEBLOOD          =>
        val random1d4 = GameProcessor.random.nextInt(4) + 1
        var damage_num = random1d4
        val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message("擲骰結果：1D4=" + random1d4)
        talk.save
        talk.send(actioner.room_id.is)
        
        if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
          damage_num = 0
          val talk_mr = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                        .message("黑卡的「增加損傷」效果的數值變更為 0 點(魔魂師)")
          talk_mr.save
          talk_mr.send(actioner.room_id.is)
        }
        
        if (actioner.hasnt_item(CardEnum.W_TALISMAN)) {
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(actioner.handle_name.is + " 的損傷增加 " + damage_num + " 點")
          talk1.save
          talk1.send(actioner.room_id.is)
          if ((actioner.get_skill_role == RoleLion) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
                val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                talk.save
                talk.send(actioner.room_id.is)
          }
          if (actioner.inflict_card_damage(damage_num, actioner)) {
            GameProcessor.check_death(actioner, actioner, action, userentrys)
          }
        } else {
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(actioner.handle_name.is + " 的損傷增加 0 點(護身符)")
          talk1.save
          talk1.send(actioner.room_id.is)
        }
        damage_num = random1d4
        actionee.lower_damage(damage_num, userentrys)
        actionee.save
        val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(actionee.handle_name.is + " 的損傷減少 " + damage_num + " 點")
        talk1.save
        talk1.send(actionee.room_id.is)
        if ((actionee.get_skill_role == RoleArsis) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARSIS.toString).actioner_id(actionee.id.is)
          talk_a.save
          talk_a.send(actionee.room_id.is)
        }
      case CardEnum.B_SPLINTERED          =>
        val userentry_equips = actionee.items
        if (userentry_equips.length > 0) {
          actionee.items.foreach { userentry_item =>
            val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                     By(CardPool.card, userentry_item.card_enum.toString)).get
            card.owner_id(0).discarded(true).save
          }
          actionee.item_flags("")
          actionee.save  
        }
        //end
    }
    val phase_type = 
      if (roomphase.phase_type.is == RoomPhaseEnum.CARD_SKILL.toString) 
        RoomPhaseEnum.MOVEMENT
      else if (roomphase.phase_type.is == RoomPhaseEnum.CARD_AKI.toString) 
        RoomPhaseEnum.ENDED
      else
        RoomPhaseEnum.ATTACK

    if (!GameProcessor.check_victory(room, roomround, userentrys)) {
      if ((!actioner.live.is) || (phase_type == RoomPhaseEnum.ENDED))
        GameProcessor.next_player(room, roomround, roomphase, userentrys)
      else {
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                    .phase_type(phase_type.toString).player(roomphase.player.is).phase_flags(action.action_flags.is)
                                    .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys=userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      }
    }
  }
}
