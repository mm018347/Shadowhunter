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

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer 

import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.snippet._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.util.{PlummUtil, LocationHelper}

object ActionHelper extends Logger {
  def action_list(roomphase: RoomPhase, currentuserentry : UserEntry) : List[ActionData] = {
    //roomphase = RoomPhase_R.get
    if (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
      List(ActionKick, ActionStartGame) // ActionTestAlert
    else if (roomphase.phase_type.is == RoomPhaseEnum.ENDED.toString)
      List()
    else {
      // currentuserentry = CurrentUserEntry_R.get
      val role = currentuserentry.get_skill_role
      if (roomphase.player.is != currentuserentry.id.is)
        role.free_skill :: List(ActionFlip, ActionWishing, ActionItemPreferred)
      else if (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString)
        role.free_skill :: role.movement_skill ::: List(ActionFlip, ActionMove, ActionCassandraGive, ActionWhiteCardBalance, ActionWishing, ActionItemPreferred)
      else if (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString) {
        if (roomphase.phase_flags != "")
          if ((role == RoleAki) && (currentuserentry.revealed.is) && (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (currentuserentry.hasnt_item(CardEnum.B_MASK))) {
            List(ActionCardChoose2)
          } else {
            List(ActionCardChoose)
          }
        else role.free_skill :: List(ActionFlip, ActionDrawBlackCard, ActionDrawWhiteCard, ActionDrawGreenCard,
                                     ActionLocDamage, ActionLocHeal, ActionLocRob, ActionNoLoc, 
                                     ActionWishing, ActionItemPreferred)
      } else if ((roomphase.phase_type.is == RoomPhaseEnum.CARD.toString) || 
               (roomphase.phase_type.is == RoomPhaseEnum.CARD_SKILL.toString) || 
               (roomphase.phase_type.is == RoomPhaseEnum.CARD_AKI.toString)) {
        val card_type = CardEnum.get_card(roomphase.phase_flags.is).cardtype_enum
        val card_skill = card_type match {
          case CardTypeEnum.BLACK => List(ActionBlackCard, ActionNoCard)
          case CardTypeEnum.WHITE => List(ActionWhiteCard, ActionNoCard)
          case CardTypeEnum.GREEN => List(ActionGreenCard, ActionNoCard)
        }
        role.free_skill :: card_skill
      } else if (roomphase.phase_type.is == RoomPhaseEnum.POST_ATTACK.toString) {
        role.post_skill :: List(ActionNextRound)
      } else
        role.attack_skill :: List(ActionFlip, ActionAttack, ActionNoAttack, ActionWishing, ActionItemPreferred)
    }  
  }
  
  def enabled_action_list(room:Room, roomround:RoomRound, roomphase:RoomPhase, 
                          currentuserentry:UserEntry, userentrys_rr:List[UserEntry]) : List[ActionData] = {
    action_list(roomphase, currentuserentry).filter {x => 
      if (!x.enabled(room, roomround, roomphase, currentuserentry, userentrys_rr)) false
      else {
        if (x.isInstanceOf[UserEntryTargetable])
          (x.asInstanceOf[UserEntryTargetable].targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr).length != 0)
        else if  (x.isInstanceOf[LocationTargetable])
          (x.asInstanceOf[LocationTargetable].targetable_locations(room, roomround, roomphase, currentuserentry, userentrys_rr).length != 0)
        else if  (x.isInstanceOf[CardTargetable])
          (x.asInstanceOf[CardTargetable].targetable_cards(room, roomround, roomphase, currentuserentry, userentrys_rr).length != 0)
        else
          true
      }
    }
  }
  
  def process_action(action: Action, room: Room, roomround: RoomRound,
                   roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    val actioner_id = action.actioner_id.is
    val action_enum = MTypeEnum.get_action(action.mtype.is)
    val role = actioner.get_role
    val skill_role = actioner.get_skill_role

    action_enum match {
      case MTypeEnum.ACTION_KICK =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actionee.inflict_damage(1, actioner)
        actionee.save

        // 如果被踢到 3 次
        val room = Room.find(By(Room.id, actionee.room_id.is)).get

        if (actionee.damaged.is >= 3) {
          UserEntrySnippet.revoke(room, actionee)
          RoomActor.sendUserEntryMessage(actionee.id.is, ForceOut(actionee.id.is))
        }
        val userentrys_reload = UserEntry.findAllByRoom(room)
        RoomActor.sendRoomMessage(actionee.room_id.is, SessionVarSet(room = room, userentrys = userentrys_reload))
        RoomActor.sendRoomMessage(actionee.room_id.is, RoomForceUpdate(actionee.room_id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.ACTION_BAR)))

      case MTypeEnum.ACTION_STARTGAME =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        actioner.add_room_flag(UserEntryRoomFlagEnum.VOTED)
        actioner.save

        // 如果全員都開始遊戲
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        val userentrys_r = UserEntry.rr(userentrys)
        val userentrys_notready = userentrys_r.filter(x => (x.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED)))

        if (((room.has_flag(RoomFlagEnum.ALL_NEUTRAL) && (userentrys_r.length >= 2)) || (userentrys_r.length >= 4)) && (userentrys_notready.length == 0)) {
          val room = Room.find(By(Room.id, actioner.room_id.is)).get
          room.status(RoomStatusEnum.PLAYING.toString)
          room.save
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room))
          
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is,
                                                                         List(ForceUpdateEnum.GO_OUT_LINK)))

          GameProcessor.process_start_game(room)
          // New Round
          //val room_reload       = Room.find(By(Room.id, actioner.room_id.is)).get
          val roomround_reload  = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                                 OrderBy(RoomRound.round_no, Descending)).get
          val roomphase_reload  = RoomPhase.find(By(RoomPhase.roomround_id, roomround_reload.id.is),
                                                 OrderBy(RoomPhase.phase_no, Descending)).get
          val userentrys_reload = UserEntry.findAllByRoom(room)

          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomround = roomround_reload,
                                                                        roomphase = roomphase_reload,
                                                                        userentrys = userentrys_reload))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is,
                                                                          List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.LOCATION_TABLE,
                                                                               ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.ACTION_BAR)))
        } else {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
      case MTypeEnum.ACTION_FLIP =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        // 降臨
        if (actioner.has_user_flag(UserEntryFlagEnum.ADVENT) && (role.role_side == RoleSideEnum.HUNTER))
          actioner.damaged(0)
        // 魔鬼儀式
        if (actioner.has_user_flag(UserEntryFlagEnum.DIABOLIC) && (role.role_side == RoleSideEnum.SHADOW))
          actioner.damaged(0)
        // 巧克力
        if (actioner.has_user_flag(UserEntryFlagEnum.CHOCOLATE)) {
          val life_thresh = role.role_side match {
            case RoleSideEnum.NEUTRAL => 8
            case RoleSideEnum.SHADOW => 11
            case RoleSideEnum.HUNTER => 11
          }
          if (role.role_life <= life_thresh) 
            actioner.damaged(0)
        }
        //柴郡貓認主(新設定的位置)
        if (actioner.get_role != RoleCheshire) {
          val cheshires = userentrys.filter(_.get_role == RoleCheshire)
          cheshires.foreach { cheshire =>
            if (cheshire.target_user.is == 0)
              cheshire.target_user(actioner.id.is).save
          }
        }
        // 移動階段
        if (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) {
          if ((skill_role == RoleCatherine) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
            actioner.lower_damage(1, userentrys)
          }
          if ((skill_role == RoleGinger) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
            val shadow_rl = userentrys.filter(x => (x.live.is) && (!x.revoked.is) && (x.get_role.role_side == RoleSideEnum.SHADOW)) 
            val hunter_rl = userentrys.filter(x => (x.live.is) && (!x.revoked.is) && (x.get_role.role_side == RoleSideEnum.HUNTER)) 
            if (shadow_rl.length > hunter_rl.length) {
              if (room.has_flag(RoomFlagEnum.GINGER_REUSE)) {
                actioner.lower_damage(4, userentrys)
              } else {
                actioner.lower_damage(2, userentrys)
              }
            }
          }
          // 小彩_魂狩之地
          if ((skill_role == RoleCloudBow) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) && (roomphase.player.is == actioner.id.is)) {
            actioner.add_user_flag(UserEntryFlagEnum.SOULHUNT)
            val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                                 .mtype(MTypeEnum.RESULT_HUNTER.toString).message(actioner.handle_name.is + " 添加「魂狩」狀態(小彩)")
            talk.save
            talk.send(actioner.room_id.is)
          }
          // 審判_淨化
          if ((skill_role == RoleJudgment) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) && (roomphase.player.is == actioner.id.is)) {
            val userall_rl = userentrys.filter(x => (x.live.is) && (!x.revoked.is) && (x.id.is != actioner_id))
            userall_rl.foreach { userentry =>
              userentry.damaged(7)
              userentry.save
            }
            val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                                 .mtype(MTypeEnum.ACTION_JUDGMENTACK.toString)
            talk.save
            talk.send(actioner.room_id.is) 
          }
          // 墮天魔_暴雷狂襲
          if ((skill_role == RoleFallOmen) && (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) && (roomphase.player.is == actioner.id.is)) {
            val talk1 = Talk.create.roomround_id(action.roomround_id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                              .message("目標區域： " + LocationEnum.get_cname(actioner.location.is) + " 和 " + LocationEnum.get_cname(LocationHelper.neighbor(room, actioner.location.is)) + " 除外全部區域")
            talk1.save
            talk1.send(actioner.room_id.is)
            val userall_rl = userentrys.filter(x => (x.live.is) && (!x.revoked.is) && (x.location.is != LocationHelper.neighbor(room, actioner.location.is)) && (x.location.is != actioner.location.is))
            if (userall_rl.length > 0) {
              userall_rl.foreach { userentry =>
                val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is).actionee_id(userentry.id.is)
                                   .mtype(MTypeEnum.RESULT_SHADOW.toString).message(userentry.handle_name.is + " 受到 5 點損傷(墮天魔)")
                talk.save
                talk.send(userentry.room_id.is)
                userentry.inflict_damage(5, actioner)
                userentry.save
                GameProcessor.check_death(userentry, actioner, action, userentrys)
              }
            }
          }
          
          // 多提
          if ((role == RoleHunsoul) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) && (roomphase.player.is == actioner.id.is)) {
            actioner.lower_damage(4, userentrys)
            val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                                 .mtype(MTypeEnum.ACTION_HUNSOULLOWER.toString)
            talk.save
            talk.send(actioner.room_id.is)
          }
        }
        // 伊凡
        if ((role.role_side == RoleSideEnum.SHADOW) && 
           (userentrys.filter(x => (x.get_role.role_side == RoleSideEnum.SHADOW) && (x.revealed.is) && (!x.revoked.is)).length == 0)) {
          val live_evans = userentrys.filter(x => (x.live.is) && (!x.revoked.is) && (x.get_role == RoleEvan))
          if (live_evans.length != 0) {
            val live_evan = live_evans(0)
            live_evan.add_user_flag(UserEntryFlagEnum.LOVER)
            live_evan.target_user(actioner.id.is)
            actioner.add_user_flag(UserEntryFlagEnum.LOVER)
            
            if (room.has_flag(RoomFlagEnum.EVAN_HEAL)) {
              if (actioner.live.is) 
                actioner.damaged(0)
              live_evan.damaged(0)
            }
          
            if (!live_evan.revealed.is)
              GameProcessor.flip(live_evan, action, userentrys)
            else
              live_evan.save
          }
        }
        
        actioner.revealed(true)
        actioner.save
        //全員翻開
        val live_unrevealed = userentrys.filter(x => (x.get_role != RoleDetective) &&(x.live.is) && (!x.revealed.is) && (!x.revoked.is))
        if (live_unrevealed.length == 0) {
          val live_detectives = userentrys.filter(x =>(x.get_role == RoleDetective) && (x.live.is) && (!x.revoked.is))
          live_detectives.foreach { live_detective =>
            val saved_damaged = live_detective.damaged.is
            live_detective.damaged(99)
            GameProcessor.check_death(live_detective, live_detective, action, userentrys)
            live_detective.damaged(saved_damaged).save
          }
        }
        val live_unrevealed2 = userentrys.filter(x => (x.live.is) && (!x.revealed.is) && (!x.revoked.is))
        if (live_unrevealed2.length == 0) {
          val live_judgments = userentrys.filter(x =>(x.get_role == RoleJudgment) && (x.live.is) && (!x.revoked.is))
          live_judgments.foreach { live_judgment =>
            live_judgment.add_user_flag(UserEntryFlagEnum.VICTORY)
            live_judgment.save
          }
        }

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!actioner.live.is)
            GameProcessor.next_player(room, roomround, roomphase, userentrys) 
          else {
            roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE)))
            RoomActor.sendUserEntryMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }  
        }
      case MTypeEnum.ACTION_MOVE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get

        var random1d6 = GameProcessor.random.nextInt(6) + 1
        var random1d4 = GameProcessor.random.nextInt(4) + 1
        var new_location = LocationEnum.from_dice(random1d6 + random1d4)
        
        while (new_location.toString == actioner.location.is.toString) {
          random1d6 = GameProcessor.random.nextInt(6) + 1
          random1d4 = GameProcessor.random.nextInt(4) + 1
          new_location = LocationEnum.from_dice(random1d6 + random1d4)
        }
        
        val (new_location_str : String, display_location_str : String) =
          //訃影
          if (actioner.has_user_flag(UserEntryFlagEnum.SEAL) && (actioner.location.is != ""))
            (LocationHelper.seal(room, actioner.location.is.toString), LocationEnum.SEAL.toString)
          //黏稠
          else if (actioner.has_user_flag(UserEntryFlagEnum.STICKY) && (actioner.location.is != ""))
            if (new_location.toString == LocationEnum.OPTION.toString) {
                (actioner.location.is.toString, LocationEnum.STICKY.toString)
            } else {
              (new_location.toString, new_location.toString)
            }
          //艾米
          else if ((actioner.get_skill_role == RoleEmi) && (actioner.revealed) && (room.has_flag(RoomFlagEnum.EMI_SEND)) &&
              (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)))
            (action.action_flags.is.toString, LocationEnum.TELEPORT.toString)
          else if ((actioner.get_skill_role == RoleEmi) && (actioner.revealed) &&
              ((LocationHelper.left(room, actioner.location.is.toString) == action.action_flags.is.toString) ||
               (LocationHelper.right(room, actioner.location.is.toString) == action.action_flags.is.toString)) &&
              (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)))
            (action.action_flags.is.toString, LocationEnum.TELEPORT.toString)
          //一般移動
          else  {
            if ((actioner.get_skill_role == RoleEmi) && (actioner.revealed) &&
                (room.has_flag(RoomFlagEnum.EMI_ENHANCE)))
              actioner.add_role_flag(UserEntryRoleFlagEnum.ENHANCED)
          
            if (new_location.toString == LocationEnum.OPTION.toString)
              (action.action_flags.is.toString, LocationEnum.OPTION.toString)
            else if (actioner.has_item(CardEnum.W_MYSTIC_COMPASS) && (math.abs(random1d6 - random1d4) == 1))
              (action.action_flags.is.toString, LocationEnum.COMPASS.toString)
            else if (new_location.toString == actioner.location.is.toString)
              (action.action_flags.is.toString, LocationEnum.REPEAT.toString)
            else
              (new_location.toString, new_location.toString)
          }
          
        //結界
        val enchantments = userentrys.filter(x => ( ((x.location.is == new_location_str) || (x.location.is == LocationHelper.neighbor(room, new_location_str))) &&
           (x.has_user_flag(UserEntryFlagEnum.ENCHANTMENT)) ))
           
        val peer_users = userentrys.filter(x => ( ((x.location.is == actioner.location.is) || (x.location.is == LocationHelper.neighbor(room, actioner.location.is))) && (x.live.is) && (x.get_skill_role != RoleClacken)))
        
        if (enchantments.length == 0) {
          actioner.location(new_location_str)
          
          if ((actioner.get_skill_role == RoleClacken) && (actioner.revealed) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
            peer_users.foreach { peer_user =>
              peer_user.location(new_location_str)
              peer_user.add_user_flag(UserEntryFlagEnum.STICKY)
              peer_user.save
            }
          }
        }
        
        val vengeful_ghosts = userentrys.filter(x =>
          ((x.location.is == actioner.location.is) || 
           ((room.has_flag(RoomFlagEnum.VGHOST_EXPAND)) && (x.location.is == LocationHelper.neighbor(room, actioner.location.is))))
          &&
          (x.live.is) && (x.revealed.is) && (x.get_skill_role == RoleVengefulGhost) &&
          (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (x.hasnt_item(CardEnum.B_MASK)))
        
        var update_enum = List(ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TIME_TABLE)
         
        val vengeful_ghost_str =
          if (((!actioner.revealed) || (actioner.get_role.role_side != RoleSideEnum.SHADOW)) && (vengeful_ghosts.length != 0)) {
            if (actioner.inflict_damage(2, vengeful_ghosts(0))) {
              GameProcessor.check_death(actioner, vengeful_ghosts(0), action, userentrys)
              if((actioner.get_skill_role == RoleLion) && (actioner.revealed) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))){
                update_enum = List(ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)
                "，且損傷增加 1 點(復仇鬼&特羅修)"
              } else {
                update_enum = List(ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)
                "，且損傷增加 2 點(復仇鬼)"
              }
            } else ""
          } else ""
        
        val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_MOVE.toString)
                       .message("1D6=" + random1d6 + ",1D4=" + random1d4 + " 移動結果：" + LocationEnum.get_cname(display_location_str) + vengeful_ghost_str)
        talk.save
        talk.send(actioner.room_id.is)
        
        if (enchantments.length > 0) {
          val talk_e = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_MOVE.toString)
                       .message(actioner.handle_name.is + " 的移動目的地有結界阻擋，停留於原地")
          talk_e.save
          talk_e.send(actioner.room_id.is)
        }
        
        if (enchantments.length == 0) {
          if ((actioner.get_skill_role == RoleClacken) && (actioner.revealed) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
            peer_users.foreach { peer_user =>
              val vengeful_ghost_peer_str =
                if (((!peer_user.revealed) || (peer_user.get_role.role_side != RoleSideEnum.SHADOW)) && (vengeful_ghosts.length != 0)) {
                  if (peer_user.inflict_damage(2, vengeful_ghosts(0))) {
                    GameProcessor.check_death(peer_user, vengeful_ghosts(0), action, userentrys)
                    peer_user.save
                    if((peer_user.get_skill_role == RoleLion) && (peer_user.revealed) && (peer_user.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (peer_user.hasnt_item(CardEnum.B_MASK))){
                      update_enum = List(ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)
                      "，且損傷增加 1 點(復仇鬼&特羅修)"
                    } else {
                      update_enum = List(ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)
                      "，且損傷增加 2 點(復仇鬼)"
                    }
                  } else ""
                } else ""
              val talk_p = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_MOVE.toString)
                       .message(peer_user.handle_name.is + " 被 " + actioner.handle_name.is + " 吸附一同移動(克拉肯)" + vengeful_ghost_peer_str)
              talk_p.save
              talk_p.send(actioner.room_id.is)
            }
          }
        }
        
        //火馬
        if (actioner.has_item(CardEnum.B_FIREHORSE)) {
          val location_users = userentrys.filter(x => ((x.location.is == LocationHelper.neighbor(room, actioner.location.is)) || (x.location.is == actioner.location.is)) && (x.id.is != actioner.id.is) && (x.hasnt_item(CardEnum.W_TALISMAN)))
          location_users.foreach { location_user =>
            val talk_f = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_MOVE.toString)
                       .message("駕駕！ " + location_user.handle_name.is + " 的損傷增加 1 點(煉獄馬)")
            talk_f.save
            talk_f.send(actioner.room_id.is)
            if ((location_user.get_skill_role == RoleLion) && (location_user.revealed) && (location_user.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (location_user.hasnt_item(CardEnum.B_MASK))) {
              val talk_l = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(location_user.id.is)
              talk_l.save
              talk_l.send(actioner.room_id.is)
            }
            if (location_user.inflict_damage(1, actioner)) {
              GameProcessor.check_death(location_user, actioner, action, userentrys)
              location_user.save
            }
          }
        }
        
        //供品炸彈
        if (actioner.has_item(CardEnum.B_SUPPLYBOMB)) {
          if (actioner.location.is == LocationEnum.ERSTWHILE_ALTER.toString) {
            actioner.remove_item(CardEnum.B_SUPPLYBOMB)
            
            // 從 CardPool 修改 owner 資訊
            val card = CardPool.find(By(CardPool.room_id, actioner.room_id.is),
                                     By(CardPool.card, CardEnum.B_SUPPLYBOMB.toString)).get
            card.owner_id(0).discarded(true).save
            if ((actioner.get_skill_role == RoleMagician) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
              val talk_mr = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                            .message("黑卡的「增加損傷」效果的數值額外增加 2 點(魔魂師)")
              talk_mr.save
              talk_mr.send(actioner.room_id.is)
            }
            val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_MOVE.toString)
                         .message(actioner.handle_name.is + " 身上的 供品炸彈 爆炸了。")
            talk.save
            talk.send(actioner.room_id.is)
            val location_users = userentrys.filter(x => ((x.location.is == LocationHelper.neighbor(room, actioner.location.is)) || (x.location.is == actioner.location.is)) && (x.hasnt_item(CardEnum.W_TALISMAN)))
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
              val talk_f = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_MOVE.toString)
                         .message("轟隆！ " + location_user.handle_name.is + " 的損傷增加 " + damage_num + " 點(供品炸彈)")
              talk_f.save
              talk_f.send(actioner.room_id.is)
              if ((location_user.get_skill_role == RoleLion) && (location_user.revealed) && (location_user.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (location_user.hasnt_item(CardEnum.B_MASK))) {
                val talk_l = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(location_user.id.is)
                talk_l.save
                talk_l.send(actioner.room_id.is)
              }
              if (location_user.inflict_damage(damage_num, actioner)) {
                GameProcessor.check_death(location_user, actioner, action, userentrys)
                location_user.save
              }
            }
          }
        }
        
        //艾瑪
        if ((room.has_flag(RoomFlagEnum.EMMA_REUSE)) && (actioner.get_skill_role == RoleEmma) && (actioner.revealed) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
          if (actioner.location.is == LocationEnum.CHURCH.toString) {
            actioner.lower_damage(2, userentrys)
            val talk_h = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_HUNTER.toString).actioner_id(actioner.id.is)
                        .message(actioner.handle_name.is + " 的損傷減少 2 點(艾瑪)")
            talk_h.save
            talk_h.send(actioner.room_id.is)
          } else if (actioner.location.is == LocationEnum.GRAVEYARD.toString) {
            val talk_h = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_HUNTER.toString).actioner_id(actioner.id.is)
                        .message(actioner.handle_name.is + " 添加精靈共鳴狀態(艾瑪)")
            talk_h.save
            talk_h.send(actioner.room_id.is)
            actioner.add_user_flag(UserEntryFlagEnum.FAITH)
          }
        }
        
        actioner.save

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!actioner.live.is)
            GameProcessor.next_player(room, roomround, roomphase, userentrys)
          else {
            val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                     .phase_type(RoomPhaseEnum.LOCATION.toString).player(roomphase.player.is)
                                     .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
            new_phase.save
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, update_enum))
            RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }
        }
      case MTypeEnum.ACTION_ATTACK =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //var roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get

        val death_number = userentrys.filter(x => !x.live.is).length
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val (attack_power, attack_str) = GameProcessor.attack(actioner, actionee, action, userentrys)
        
        val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                              .message(attack_str)
        talk.save
        talk.send(actioner.room_id.is)
        
        val live_evans = userentrys.filter(x => (x.live.is) && (x.revealed.is) && (!x.revoked.is) && (x.get_skill_role == RoleEvan) && (x.has_user_flag(UserEntryFlagEnum.LOVER)))
        if ((actionee.get_skill_role == RoleADecoy) && (actionee.revealed.is)) {
          if (actioner.inflict_a_damage(attack_power, actionee, true))
            GameProcessor.check_death(actioner, actionee, action, userentrys)
          if (!actioner.live.is)
            actionee.add_user_flag(UserEntryFlagEnum.VICTORY).save
        } else if ((actioner.revealed.is) && (actioner.get_skill_role == RoleBorogove) && (attack_power != 0) &&
                   (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) && ((actioner.hasnt_user_flag(UserEntryFlagEnum.LOVER)) && (live_evans.length == 0))) {
          if ((actionee.get_role.role_side == RoleSideEnum.SHADOW) && (actionee.revealed.is) && ((actionee.hasnt_user_flag(UserEntryFlagEnum.LOVER)) && (live_evans.length == 0))) {
            //波若哥夫
            val lower_power = attack_power / 2
            actionee.lower_damage(lower_power, userentrys)
            actionee.save
          } else {
            actionee.inflict_a_damage(attack_power, actioner, true)
            GameProcessor.check_death(actionee, actioner, action, userentrys)
          }
        } else if (actionee.inflict_a_damage(attack_power, actioner, true))
          GameProcessor.check_death(actionee, actioner, action, userentrys)
        
        val is_ambush =         
          if ((!actionee.revealed.is) && (actionee.get_skill_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK)) &&
              (actionee.has_role_flag(UserEntryRoleFlagEnum.AMBUSH))){
            GameProcessor.flip(actionee, action, userentrys)
            true  
          } else false
        
        val is_adecoy =
          ((room.has_flag(RoomFlagEnum.ADECOY_INTIMATE)) &&
           (actioner.get_skill_role == RoleADecoy) && (actioner.revealed.is) &&
           (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) && (attack_power == 0))
        
        val is_actionee_live = (actionee.live.is)
          
        // 狼人反擊
        if (is_adecoy || ((actionee.revealed.is) && (actionee.get_skill_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK)))) {
          val action1 = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                              .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
          action1.save
          
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                              .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                              .message_flags(if(is_ambush)MTypeEnum.ACTION_WEREWOLF_AMBUSH.toString else "")
          talk1.save
          talk1.send(actioner.room_id.is)
          
          val (attack_power_1, attack_str_1) = GameProcessor.attack(actionee, actioner, action, userentrys, is_ambush)
          
          val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                                .message(attack_str_1)
          talk2.save
          talk2.send(actioner.room_id.is)

          if ((actioner.get_skill_role == RoleADecoy) && (actioner.revealed.is)) {
            if (actionee.inflict_a_damage(attack_power_1, actioner, true))
              GameProcessor.check_death(actionee, actioner, action, userentrys)
            if ((!actionee.live.is) && (is_actionee_live))
              actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save
          } else if (actioner.inflict_a_damage(attack_power_1, actionee, true))
            GameProcessor.check_death(actioner, actionee, action, userentrys)
        }
        var isaki = false
        //亞希
        if ((actioner.revealed.is) && (actioner.get_skill_role == RoleAki) && (attack_power != 0) && (action.action_flags.is != "") && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))){
          isaki = true
          val talk3 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_HUNTER.toString)
                              .actioner_id(actioner.id.is).message(actioner.handle_name.is + " 再次發動" + CardEnum.get_card(action.action_flags.is).card_name + "(亞希)")
          talk3.save
          talk3.send(actioner.room_id.is)
          if (actioner.action_point.is != 0) {
            CardHelper.process_drawwhite(action, room, roomround, roomphase, actioner, userentrys)
          } else {
            CardHelper.process_drawblack(action, room, roomround, roomphase, actioner, userentrys)
          }
          actioner.card_flags("")
          actioner.save
        }
        val death_number2 = userentrys.filter(x => !x.live.is).length
        if ((actioner.get_role == RolePuzzle) && (death_number2 - death_number >= 2)) {
          userentrys.filter(_.get_role == RolePuzzle).foreach { userentry1 =>
            userentry1.add_user_flag(UserEntryFlagEnum.VICTORY).save
          }
        }

        // 檢查遊戲是否結束
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!isaki) {
            post_attack_or_next_player(room, roomround, roomphase, userentrys)
          }
        }
      
      case MTypeEnum.ACTION_FENG_KIKOU =>

        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val kikou_damage =
          if (actionee.items.length <= 1)
            2
          else 
            1
        
        if (actionee.inflict_damage(kikou_damage, actioner))
          GameProcessor.check_death(actionee, actioner, action, userentrys)
      
        if ((actionee.get_skill_role == RoleLion) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
          talk.save
          talk.send(actioner.room_id.is)
        }
        // 檢查遊戲是否結束
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          post_attack_or_next_player(room, roomround, roomphase, userentrys)
        }  
        //範圍攻擊
      case MTypeEnum.ACTION_MULTIATTACK =>
        val actionee_s = ActionMultiAttack.targetable_users(room, roomround, roomphase, actioner, userentrys)
        val death_number = userentrys.filter(x => !x.live.is).length
        var isaki = false
        var isakiskill = false
        val ginger_power = (actionee_s.length - 1)

        actionee_s.foreach { actionee => 
          var (attack_power, attack_str) = GameProcessor.attack(actioner, actionee, action, userentrys, false, ginger_power)
          
          if (attack_power > 0) {
            isakiskill = true
          }
        
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                               .message("對 " + actionee.handle_name.is +  "：" +attack_str)
          talk.save
          talk.send(actioner.room_id.is)
          
          val live_evans = userentrys.filter(x => (x.live.is) && (x.revealed.is) && (!x.revoked.is) && (x.get_role == RoleEvan) && (x.has_user_flag(UserEntryFlagEnum.LOVER)))
          if ((actionee.get_skill_role == RoleADecoy) && (actionee.revealed.is)) {
            if (actioner.inflict_a_damage(attack_power, actionee, true))
              GameProcessor.check_death(actioner, actionee, action, userentrys)
            if (!actioner.live.is)
              actionee.add_user_flag(UserEntryFlagEnum.VICTORY).save
          } else if ((actioner.revealed.is) && (actioner.get_skill_role == RoleBorogove) && (attack_power != 0) &&
                   (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) && ((actioner.hasnt_user_flag(UserEntryFlagEnum.LOVER)) && (live_evans.length == 0))) {
            if ((actionee.get_role.role_side == RoleSideEnum.SHADOW) && (actionee.revealed.is) && ((actionee.hasnt_user_flag(UserEntryFlagEnum.LOVER)) && (live_evans.length == 0))) {
              //波若哥夫
              val lower_power = attack_power / 2
              actionee.lower_damage(lower_power, userentrys)
              actionee.save
            } else {
              actionee.inflict_a_damage(attack_power, actioner, true)
              GameProcessor.check_death(actionee, actioner, action, userentrys)
            }
          } else if (actionee.inflict_a_damage(attack_power, actioner, true))
            GameProcessor.check_death(actionee, actioner, action, userentrys)
        
          val is_ambush =         
          if ((!actionee.revealed.is) && (actionee.get_skill_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK)) &&
              (actionee.has_role_flag(UserEntryRoleFlagEnum.AMBUSH))){
            GameProcessor.flip(actionee, action, userentrys)
            true  
          } else false

          val is_adecoy =
            ((room.has_flag(RoomFlagEnum.ADECOY_INTIMATE)) &&
             (actioner.get_skill_role == RoleADecoy) && (actioner.revealed.is) &&
             (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) && (attack_power == 0))

          val is_actionee_live = (actionee.live.is)
          
          // 狼人反擊
          if (is_adecoy || ((actionee.revealed.is) && (actionee.get_skill_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK)))) {
            val action1 = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                               .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
            action1.save
          
            val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                                  .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                                  .message_flags(if(is_ambush)MTypeEnum.ACTION_WEREWOLF_AMBUSH.toString else "")
            talk1.save
            talk1.send(actioner.room_id.is)
          
            val (attack_power_1, attack_str_1) = GameProcessor.attack(actionee, actioner, action, userentrys, is_ambush)
          
            val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                                  .message(attack_str_1)
            talk2.save
            talk2.send(actioner.room_id.is)

            if ((actioner.get_skill_role == RoleADecoy) && (actioner.revealed.is)) {
              if (actionee.inflict_a_damage(attack_power_1, actioner, true))
                GameProcessor.check_death(actionee, actioner, action, userentrys)
              if ((!actionee.live.is) && is_actionee_live)
                actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save
            } else if (actioner.inflict_a_damage(attack_power_1, actionee, true))
              GameProcessor.check_death(actioner, actionee, action, userentrys)
          }
        }
        
        //亞希
        if ((isakiskill) && (actioner.revealed.is) && (actioner.get_skill_role == RoleAki) && (action.action_flags.is != "") && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))){
          isaki = true
          val talk3 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_HUNTER.toString)
                            .actioner_id(actioner.id.is).message(actioner.handle_name.is + " 再次觸發" + CardEnum.get_card(action.action_flags.is).card_name + "(亞希)")
          talk3.save
          talk3.send(actioner.room_id.is)
          if (actioner.action_point.is != 0) {
            CardHelper.process_drawwhite(action, room, roomround, roomphase, actioner, userentrys)
          } else {
            CardHelper.process_drawblack(action, room, roomround, roomphase, actioner, userentrys)
          }
          actioner.card_flags("")
          actioner.save
        }
        
        val death_number2 = userentrys.filter(x => !x.live.is).length
        if ((actioner.get_role == RolePuzzle) && (death_number2 - death_number >= 2)) {
          userentrys.filter(_.get_role == RolePuzzle).foreach { userentry1 =>
            userentry1.add_user_flag(UserEntryFlagEnum.VICTORY).save
          }
        }  
        
        // 檢查遊戲是否結束
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!isaki) {
            post_attack_or_next_player(room, roomround, roomphase, userentrys)
          }
        }
      case MTypeEnum.ACTION_NOATTACK =>
        // 進行下一玩家
        post_attack_or_next_player(room, roomround, roomphase, userentrys)
        
      case MTypeEnum.ACTION_DRAWBLACKCARD =>
        CardHelper.process_drawblack(action, room, roomround, roomphase, actioner, userentrys)
        
      case MTypeEnum.ACTION_DRAWWHITECARD => 
        CardHelper.process_drawwhite(action, room, roomround, roomphase, actioner, userentrys)
        
      case MTypeEnum.ACTION_DRAWGREENCARD => 
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                .phase_type(RoomPhaseEnum.CARD.toString).player(roomphase.player.is).phase_flags(action.action_flags.is)
                                .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.TIME_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        
      case MTypeEnum.ACTION_LOCDAMAGE =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        // 財富胸針
        if (actionee.hasnt_item(CardEnum.W_FORTUNE_BROOCH)) {
          val locdamage_damage = 
            // 隱形人
            if ((actionee.get_skill_role == RoleUnseen) && (actionee.revealed.is) &&
               (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK)))
              1
            else 
              2
          // 女巫
          if (actionee.inflict_damage(locdamage_damage, actioner)) {
            if ((actioner.get_skill_role == RoleWitch) && (actioner.revealed.is) &&
                (!actioner.has_user_flag(UserEntryFlagEnum.SEALED)) && (!actioner.has_item(CardEnum.B_MASK))) 
              actionee.add_user_flag(UserEntryFlagEnum.FROG)
          }
          if ((actionee.get_skill_role == RoleLion) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
            val talk_l = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
            talk_l.save
            talk_l.send(actioner.room_id.is)
          }
          GameProcessor.check_death(actionee, actioner, action, userentrys)
        }
        
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!actioner.live.is)
            GameProcessor.next_player(room, roomround, roomphase, userentrys)
          else {
            val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                     .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is)
                                     .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
            new_phase.save
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
            RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }
        }  

      case MTypeEnum.ACTION_LOCHEAL =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actionee.lower_damage(1, userentrys)
        actionee.save
        
        if ((actionee.get_skill_role == RoleArsis) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARSIS.toString).actioner_id(actioner.id.is)
          talk_a.save
          talk_a.send(actioner.room_id.is)
        }
        
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                 .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        
      case MTypeEnum.ACTION_LOCROB =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        if ((action.action_flags.is != "") &&
            (actionee.has_item(CardEnum.get_card(action.action_flags.is).card_enum))) {
          val robbed_item = CardEnum.get_card(action.action_flags.is)
          GameProcessor.rob_specific(room, roomround, actioner, actionee, robbed_item)  
          var talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_LOCROB.toString)
                            .message("搶奪道具： " + CardEnum.get_card(action.action_flags.is).card_name)
          talk.save
          talk.send(actioner.room_id.is)
        } else if (actionee.items.length > 0){
          val robbed_item = GameProcessor.rob_single(room, roomround, actioner, actionee)
          var talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_LOCROB.toString)
                            .message("搶奪道具： " + CardEnum.get_card(robbed_item.card_enum).card_name)
          talk.save
          talk.send(actioner.room_id.is)
        }
        actioner.beads(actioner.beads.is + actionee.beads.is).save
        actionee.beads(0)
        actionee.save
        
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                   .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is)
                                   .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
          new_phase.save
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }          

      case MTypeEnum.ACTION_NOLOC =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is).additional(roomphase.additional.is)
                                 .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.TIME_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))  

      case MTypeEnum.ACTION_NOCARD =>
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
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.TIME_TABLE)))
            RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }
        }   

      case MTypeEnum.ACTION_WHITECARD =>
        CardHelper.process_white(action, room, roomround,
                                 roomphase, actioner, userentrys)
 
      case MTypeEnum.ACTION_BLACKCARD =>
        CardHelper.process_black(action, room, roomround,
                                 roomphase, actioner, userentrys)        
        
      case MTypeEnum.ACTION_GREENCARD =>
        CardHelper.process_green(action, room, roomround,
                                 roomphase, actioner, userentrys)
      // 母愛
      case MTypeEnum.ACTION_ALLIE_MOTHERLOVE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        actioner.damaged(0)
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      case MTypeEnum.ACTION_WEREWOLF_AMBUSH =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        actioner.add_role_flag(UserEntryRoleFlagEnum.AMBUSH)
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))  
      
      case MTypeEnum.ACTION_ULTRASOUL_RAY =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        if (actionee.inflict_damage(1, actioner))
          GameProcessor.check_death(actionee, actioner, action, userentrys)
      
        if ((actionee.get_skill_role == RoleLion) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
          talk.save
          talk.send(actioner.room_id.is)
        }

        // 檢查死亡和勝利條件
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }  
        
      case MTypeEnum.ACTION_ULTRASOUL_URAY =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        if (actionee.inflict_damage(3, actioner))
          GameProcessor.check_death(actionee, actioner, action, userentrys)
      
        if ((actionee.get_skill_role == RoleLion) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
          talk.save
          talk.send(actioner.room_id.is)
        }

        // 檢查死亡和勝利條件
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
        
      case MTypeEnum.ACTION_ELLEN_CURSECHAIN =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actionee.add_user_flag(UserEntryFlagEnum.SEALED)
        actionee.save
        
        if (room.has_flag(RoomFlagEnum.ELLEN_HEAL) && (!actionee.revealed.is))
          actioner.lower_damage(4, userentrys)
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        
      case MTypeEnum.ACTION_FRANKLIN_LIGHTNING =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        var damage = try {
          action.action_flags.is.toInt}
          catch {case e : Exception => 0}
        if ((actionee.get_role.role_side == RoleSideEnum.SHADOW) && (actionee.revealed.is)){
            damage += 1
        }
        if (actionee.inflict_damage(damage, actioner))
          GameProcessor.check_death(actionee, actioner, action, userentrys)
      
        if ((actionee.get_skill_role == RoleLion) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
          talk.save
          talk.send(actioner.room_id.is)
        }

        // 檢查死亡和勝利條件
        if (room.has_flag(RoomFlagEnum.FRANKLIN_REUSE) &&(damage <= 2))
          actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        else   
          actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }

      case MTypeEnum.ACTION_FUKA_DYNAMITEHEAL =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val actionee_role = actionee.get_role

        val damaged =
          if ((actionee.revealed.is) && (actionee_role.role_side == RoleSideEnum.HUNTER))
            6
          else if ((actionee.revealed.is) && (actionee_role.role_side == RoleSideEnum.SHADOW))
            8
          else
            7
            
        actionee.damaged(damaged)
        actionee.save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        
      case MTypeEnum.ACTION_GEORGE_DEMOLISH =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val damage = try {
          action.action_flags.is.toInt}
          catch {case e : Exception => 0}

        if (actionee.inflict_damage(damage, actioner)) {
          GameProcessor.check_death(actionee, actioner, action, userentrys)
          if (room.has_flag(RoomFlagEnum.GEORGE_REUSE2)) {
            actioner.lower_damage(damage, userentrys)
          }
          actioner.save
        }
        
        if ((actionee.get_skill_role == RoleLion) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
          talk.save
          talk.send(actioner.room_id.is)
        }

        // 檢查死亡和勝利條件
        if (room.has_flag(RoomFlagEnum.GEORGE_REUSE) && (damage <= 1))
          actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        else
          actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
      //嗜天斬
      case MTypeEnum.ACTION_FIGHTER_STRIKE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val damage = try {
          action.action_flags.is.toInt}
          catch {case e : Exception => 0}
        val actionee_equips = actionee.items

        if (actionee.inflict_damage(damage + actionee_equips.length, actioner)){
          if (actionee_equips.length > 0) {
            val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_FIGHTER_STRIKE2.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                  .message_flags(actionee_equips.length.toString)
            talk.save
            talk.send(actioner.room_id.is)
            actionee.items.foreach { actionee_item =>
            val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                    By(CardPool.card, actionee_item.card_enum.toString)).get
              card.owner_id(0).discarded(true).save
            }
            actionee.item_flags("")
          }
          if ((actionee.get_skill_role == RoleLion) && (actionee.revealed.is) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
            val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
            talk.save
            talk.send(actioner.room_id.is)
          }
          GameProcessor.check_death(actionee, actioner, action, userentrys)
        }
        // 檢查死亡和勝利條件
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
      //遺忘
      case MTypeEnum.ACTION_MICAH_CONFUSED =>
        //黑卡
        val card_b = GameProcessor.draw_card(room, CardTypeEnum.BLACK)
        card_b.discarded(true)
        card_b.save
    
        val action_b = Action.create.roomround_id(roomround.id.is).actioner_id(actioner.id.is)
                                 .mtype(MTypeEnum.ACTION_DRAWBLACKCARD.toString)
                                 .action_flags(card_b.card.is)
        action_b.save
        val talk_b = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_DRAWBLACKCARD.toString).actioner_id(actioner.id.is).message_flags(card_b.card.is)
        talk_b.save
        talk_b.send(actioner.room_id.is)
        //白卡
        val card_w = GameProcessor.draw_card(room, CardTypeEnum.WHITE)
        card_w.discarded(true)
        card_w.save
    
        val action_c = Action.create.roomround_id(roomround.id.is).actioner_id(actioner.id.is)
                                 .mtype(MTypeEnum.ACTION_DRAWWHITECARD.toString)
                                 .action_flags(card_w.card.is)
        action_c.save
        val talk_w = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_DRAWWHITECARD.toString).actioner_id(actioner.id.is).message_flags(card_w.card.is)
        talk_w.save
        talk_w.send(actioner.room_id.is)
        //跳過回合

        val talk_n = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MICAH_CONFUSED2.toString).actioner_id(actioner.id.is)
        talk_n.save
        talk_n.send(actioner.room_id.is)
        GameProcessor.next_player(room, roomround, roomphase, userentrys)

      case MTypeEnum.ACTION_GREGOR_BARRIER =>
        actioner.add_user_flag(UserEntryFlagEnum.BARRIER)
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        
        GameProcessor.next_player(room, roomround, roomphase, userentrys)
        
      case MTypeEnum.ACTION_WIGHT_MANIPULATE =>
        val additional = try {
          action.action_flags.is.toInt
        } catch { case e: Exception => 0 }
        
        roomphase.additional(roomphase.additional.is + additional).save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        
        GameProcessor.next_player(room, roomround, roomphase, userentrys)  
      //捕捉
      case MTypeEnum.ACTION_CLACKEN_CAPTURE =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
        actionee.location(actioner.location.is)
        actionee.save
        
        val vengeful_ghosts = userentrys.filter(x =>
          ((x.location.is == actioner.location.is) || 
           ((room.has_flag(RoomFlagEnum.VGHOST_EXPAND)) && (x.location.is == LocationHelper.neighbor(room, actioner.location.is))))
          &&
          (x.live.is) && (x.revealed.is) && (x.get_skill_role == RoleVengefulGhost) &&
          (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (x.hasnt_item(CardEnum.B_MASK)))
        var update_enum = List(ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TIME_TABLE)
        val vengeful_ghost_peer_str =
           if (((!actionee.revealed) || (actionee.get_role.role_side != RoleSideEnum.SHADOW)) && (vengeful_ghosts.length != 0)) {
              if (actionee.inflict_damage(2, vengeful_ghosts(0))) {
                GameProcessor.check_death(actionee, vengeful_ghosts(0), action, userentrys)
                actionee.save
                if((actionee.get_skill_role == RoleLion) && (actionee.revealed) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))){
                  update_enum = List(ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)
                  " 的損傷增加 1 點(復仇鬼&特羅修)"
                } else {
                  update_enum = List(ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)
                  " 的損傷增加 2 點(復仇鬼)"
                }
              } else ""
            } else ""
        if (vengeful_ghost_peer_str != "") {
          val talk_c = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                .message(actionee.handle_name.is + vengeful_ghost_peer_str)
          talk_c.save
          talk_c.send(actioner.room_id.is)
        }
        //actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        actioner.save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      case MTypeEnum.ACTION_AICHA_GRASP =>
        val additional = try {
          action.action_flags.is.toInt
        } catch { case e: Exception => 0 }
        
        roomphase.additional(roomphase.additional.is + additional).save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        
        GameProcessor.next_player(room, roomround, roomphase, userentrys) 
      
      case MTypeEnum.ACTION_NEXTROUND =>
        GameProcessor.next_player(room, roomround, roomphase, userentrys)  
        //血祭
      case MTypeEnum.ACTION_CHARLES_BLOODFEAST =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val (attack_power, attack_str) = GameProcessor.attack(actioner, actionee, action, userentrys)
        
        val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                              .message(attack_str)
        talk.save
        talk.send(actioner.room_id.is)

        if ((actionee.get_skill_role == RoleADecoy) && (actionee.revealed.is)) {
          if (actioner.inflict_a_damage(attack_power, actionee, true))
            GameProcessor.check_death(actioner, actionee, action, userentrys)
          if (!actioner.live.is)
            actionee.add_user_flag(UserEntryFlagEnum.VICTORY).save
        } else if (actionee.inflict_a_damage(attack_power, actioner, true))
          GameProcessor.check_death(actionee, actioner, action, userentrys)
      
        if (actioner.inflict_damage(2, actioner))
          GameProcessor.check_death(actioner, actioner, action, userentrys)

        val is_ambush =         
          if ((!actionee.revealed.is) && (actionee.get_skill_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK)) &&
              (actionee.has_role_flag(UserEntryRoleFlagEnum.AMBUSH))){
            GameProcessor.flip(actionee, action, userentrys)
            true  
          } else false
        
        // 狼人反擊
        if ((actionee.revealed.is) && (actionee.get_skill_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
          val action1 = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                              .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
          action1.save
          
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                              .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                              .message_flags(if(is_ambush)MTypeEnum.ACTION_WEREWOLF_AMBUSH.toString else "")
          talk1.save
          talk1.send(actioner.room_id.is)
          
          val (attack_power_1, attack_str_1) = GameProcessor.attack(actionee, actioner, action, userentrys, is_ambush)
          
          val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                                .message(attack_str_1)
          talk2.save
          talk2.send(actioner.room_id.is)

          if (actioner.inflict_a_damage(attack_power_1, actionee, true))
            GameProcessor.check_death(actioner, actionee, action, userentrys)
        }
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        // 檢查遊戲是否結束
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          post_attack_or_next_player(room, roomround, roomphase, userentrys)
        }
        
      case MTypeEnum.ACTION_UNKNOWN_DECEIVE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        //val role = RoleEnum.get_skill_role(action.action_flags.is.toString)
        
        actioner.role_flags(action.action_flags.is)
        actioner.save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        //RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        //RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))  
      case MTypeEnum.ACTION_DAVID_GRAVEDIG =>
        
        val card = CardPool.find(By(CardPool.room_id, room.id.is),
                                 By(CardPool.card, action.action_flags.is)).get
        card.owner_id(actioner.id.is).discarded(false).save
        
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.add_item(CardEnum.get_card(action.action_flags.is).card_enum)
        actioner.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        if (GameProcessor.check_item_victory(room, roomround, actioner)) 
          GameProcessor.check_victory(room, roomround, userentrys)
        else {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
      case MTypeEnum.ACTION_FATHEROCONNEL_PRAY =>
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.save
        CardHelper.process_drawwhite(action, room, roomround, roomphase, actioner, userentrys)
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      case MTypeEnum.ACTION_LUBE_RESPONSE =>
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        actioner.save
        CardHelper.process_drawwhite(action, room, roomround, roomphase, actioner, userentrys)
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!actioner.live.is)
            GameProcessor.next_player(room, roomround, roomphase, userentrys) 
          else {
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE)))
            RoomActor.sendUserEntryMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }  
        }
        
      case MTypeEnum.ACTION_CASSANDRA_FATECHANGE =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.target_user(actionee_id)
        
        val item = CardEnum.get_card(action.action_flags.is).card_enum
        
        actionee.add_item(item)
        actioner.remove_item(item)
        
        val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                 By(CardPool.card, item.toString)).get
        card.owner_id(actionee_id).discarded(false).save
        
        actioner.save
        actionee.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        if (GameProcessor.check_item_victory(room, roomround, actionee))
          GameProcessor.check_victory(room, roomround, userentrys)
        else {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
          RoomActor.sendUserEntryMessage(actionee.room_id.is, RoomForceUpdate(actionee.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
        
      case MTypeEnum.ACTION_CASSANDRA_GIVE =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
        actionee.target_user(actioner_id)
        
        val item = CardEnum.get_card(action.action_flags.is).card_enum
        
        actionee.add_item(item)
        actioner.remove_item(item)
        
        val card = CardPool.find(By(CardPool.room_id, actioner.room_id.is),
                                 By(CardPool.card, item.toString)).get
        card.owner_id(actioner_id).discarded(false).save
        
        actioner.save
        actionee.save
        
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        if (GameProcessor.check_item_victory(room, roomround, actionee))
          GameProcessor.check_victory(room, roomround, userentrys)
        else {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
          RoomActor.sendUserEntryMessage(actionee.room_id.is, RoomForceUpdate(actionee.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
      
      case MTypeEnum.ACTION_ITEMPREFERRED =>
        actioner.item_preferred(action.action_flags.is).save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        
      case MTypeEnum.ACTION_WISHING =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        //RESULT_NEUTRAL
        if(actioner.beads.is >= 7){
          for (i <- 1 to 7) {
            val live_users = userentrys.filter (x => (x.live.is) && (!x.revoked.is))
            val random_no = GameProcessor.random.nextInt(live_users.length)
            live_users(random_no).beads(live_users(random_no).beads.is + 1)
            live_users(random_no).save
          }
        }
        actioner.beads((actioner.beads.is - 7))
        val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_DRAGON.toString)
                            .message(actioner.handle_name.is + " 使用 7 顆龍珠施展許願")
        talk.save
        talk.send(actioner.room_id.is)
        
        val dragon_users = userentrys.filter(x => (x.live.is) && (x.get_skill_role == RoleDragon) && (!x.revealed.is))
        if (dragon_users.length > 0) {
          dragon_users.foreach { dragon_user =>
            GameProcessor.flip(dragon_user, action, userentrys)
          }
        }
        val dragon_users_r = userentrys.filter(x => (x.live.is) && (x.get_skill_role == RoleDragon))
        action.action_flags.is match {
          case "0" =>
            val tal_say = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.TALK_DAY.toString).actioner_id(actioner.id.is).cssclass("large")
                            .message("「讓每個人都復活！」")
            tal_say.save
            tal_say.send(actioner.room_id.is)
            val death_users = userentrys.filter(x => !x.live.is)
            death_users.foreach { death_user =>
              death_user.live(true)
              death_user.damaged(0)
              death_user.save
              var talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_DRAGON.toString)
                            .message(death_user.handle_name.is + " 奇蹟的復活了！")
              talk2.save
              talk2.send(death_user.room_id.is)
            }
          case "1" =>
            val tal_say = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.TALK_DAY.toString).actioner_id(actioner.id.is).cssclass("large")
                            .message("「讓" + actionee.handle_name.is + "復活！」")
            tal_say.save
            tal_say.send(actioner.room_id.is)
            if (!actionee.live.is) {
              actionee.live(true)
              actionee.damaged(0)
              actionee.save
              var talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_DRAGON.toString)
                            .message(actionee.handle_name.is + " 奇蹟的復活了！")
              talk2.save
              talk2.send(actioner.room_id.is)
            } else {
              var talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_DRAGON.toString)
                            .message(actionee.handle_name.is + " 根本沒有死，什麼都沒有發生。")
              talk2.save
              talk2.send(actioner.room_id.is)
              actioner.beads(7)
            }
          case "2" =>
            val tal_say = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.TALK_DAY.toString).actioner_id(actioner.id.is).cssclass("large")
                            .message("「讓我能夠復活！」")
            tal_say.save
            tal_say.send(actioner.room_id.is)
            actioner.add_user_flag(UserEntryFlagEnum.REVIVE)
            var talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_DRAGON.toString)
                            .message(actioner.handle_name.is + " 添加復活狀態(許願)")
            talk2.save
            talk2.send(actioner.room_id.is)
          case "3" =>
            val tal_say = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.TALK_DAY.toString).actioner_id(actioner.id.is).cssclass("large")
                            .message("「勝利在我手中！」")
            tal_say.save
            tal_say.send(actioner.room_id.is)
            actioner.add_user_flag(UserEntryFlagEnum.VICTORY)
            var talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_DRAGON.toString)
                            .message(actioner.handle_name.is + " 勝利！")
            talk2.save
            talk2.send(actioner.room_id.is)
          case "4" =>
            val tal_say = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.TALK_DAY.toString).actioner_id(actioner.id.is).cssclass("large")
                            .message("「世界在我手中！」")
            tal_say.save
            tal_say.send(actioner.room_id.is)
          case "5" =>
            val tal_say = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.TALK_DAY.toString).actioner_id(actioner.id.is).cssclass("large")
                            .message("「我要找到工作！」")
            tal_say.save
            tal_say.send(actioner.room_id.is)
            val all_role_list = RoleEnum.ALL_ROLE_LIST
            var java_role_array: java.util.List[RoleEnum.Value] = ListBuffer(all_role_list: _*)
            java.util.Collections.shuffle(java_role_array)
            actioner.role(java_role_array.get(0).toString)
            actioner.action_point(0)
            actioner.remove_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
            GameProcessor.check_death(actioner, actioner, action, userentrys)
            var talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_DRAGON.toString)
                            .message(actioner.handle_name.is + " 的角色改變了！")
            talk2.save
            talk2.send(actioner.room_id.is)
          case "6" =>
            val tal_say = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.TALK_DAY.toString).actioner_id(actioner.id.is).cssclass("large")
                            .message("「姆咪姆咪心動動！」")
            tal_say.save
            tal_say.send(actioner.room_id.is)
            if (dragon_users_r.length > 0) {
              val tal_say2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.TALK_DAY.toString).actioner_id(dragon_users_r(0).id.is).cssclass("normal")
                              .message("「媽的智障。」")
              tal_say2.save
              tal_say2.send(actioner.room_id.is)
            }
            val saved_damaged = actioner.damaged.is
            actioner.inflict_damage(99, actionee)
            GameProcessor.check_death(actioner, actioner, action, userentrys)
            //actioner.damaged(saved_damaged).save
          case "7" =>
            val tal_say = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.TALK_DAY.toString).actioner_id(actioner.id.is).cssclass("large")
                            .message("「我要女孩子的內褲！」")
            tal_say.save
            tal_say.send(actioner.room_id.is)
            val card_pool = CardPool.findAll(By(CardPool.room_id, room.id.is),
                              By(CardPool.card_type, "W"))
            val card = CardPool.create.room_id(room.id.is).card_no(card_pool.length).card_type("W")
                   .card("W23").discarded(false).owner_id(actioner.id.is)
            card.save
            var talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                            .message(actioner.handle_name.is + " 抽取白卡 內褲")
            talk2.save
            talk2.send(actioner.room_id.is)
            actioner.add_item(CardEnum.W_PANTIES)
            GameProcessor.check_item_victory(room, roomround, actioner)
          case "99" =>
            val tal_say = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.TALK_DAY.toString).actioner_id(actioner.id.is).cssclass("large")
                            .message("「我什麼都不要。」")
            tal_say.save
            tal_say.send(actioner.room_id.is)
            var talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_DRAGON.toString)
                            .message("寶珠消失了。")
            talk2.save
            talk2.send(actioner.room_id.is)
        }
        actioner.save
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if ((!actioner.live.is) && (roomphase.player.is == actioner.id.is))
            GameProcessor.next_player(room, roomround, roomphase, userentrys) 
          else {
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE)))
            RoomActor.sendUserEntryMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }  
        }
        
      case MTypeEnum.ACTION_BOMB_BOMB =>
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED).save
        val death_number = userentrys.filter(x => !x.live.is).length
        userentrys.foreach { userentry1 =>
          if (userentry1.location.is == action.action_flags.is.toString)
            if (userentry1.inflict_damage(2, actioner)) {
              val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(userentry1.handle_name.is + " 的損傷增加 2 點")
              talk.save
              talk.send(actioner.room_id.is)
              GameProcessor.check_death(userentry1, actioner, action, userentrys)
            }
        }
        val death_number2 = userentrys.filter(x => !x.live.is).length
        if (death_number2 - death_number >= 2)
          actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save
        
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!actioner.live.is)
            GameProcessor.next_player(room, roomround, roomphase, userentrys) 
          else {
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE)))
            RoomActor.sendUserEntryMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }  
        }

      case xs =>
        //warn("Unprocessed Action : " + action.toString)
    }
    
    
    //受限於BUG，得開個新行動比對
    action_enum match {
      case MTypeEnum.ACTION_ANGEL_REINCARNATE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        //val actionee : UserEntry = userentrys.filter(_.id.is == actionee_id)(0)

        // 檢查死亡和勝利條件
        actioner.target_user(actionee_id).save

        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        
      case MTypeEnum.ACTION_EVAN_BRACEUP =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val damage = try {
          action.action_flags.is.toInt}
          catch {case e : Exception => 0}

        actionee.remove_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save

        // 檢查死亡和勝利條件
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          //RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }  
        
      case MTypeEnum.ACTION_ADECOY_TAUNT =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED).save
        actionee.add_user_flag(UserEntryFlagEnum.TAUNT).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }  
        
      case MTypeEnum.ACTION_AMETSUKI_CURSE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        actionee.add_user_flag(UserEntryFlagEnum.SEAL).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }  
      
      case MTypeEnum.ACTION_GODFAT_EXCHANGE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val actionee2 : UserEntry = userentrys.filter(_.id.is.toString == action.action_flags.is)(0)

        val saved_user_no = actionee.user_no.is
        actionee.user_no(actionee2.user_no.is).save
        actionee2.user_no(saved_user_no).save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = UserEntry.findAllByRoom(room)))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        } 
        
      // 水鏡
      case MTypeEnum.ACTION_TEL_WATERMIRROR =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val actionee_role = actionee.get_role

        //actionee.add_user_flag(UserEntryFlagEnum.REASONAED).save
        actioner.subrole(actionee.role.is.substring(0,2))
        actioner.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        
        GameProcessor.check_death(actioner, actioner, action, userentrys)
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
        if (!actioner.live.is) {
          GameProcessor.next_player(room, roomround, roomphase, userentrys)
        }
        
      //靈魂窺探
      case MTypeEnum.ACTION_WESTLOBE_PRY =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        //actionee.add_user_flag(UserEntryFlagEnum.REASONAED).save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        
        
        if (actionee.get_real_role.role_side.toString == action.action_flags.is) {
          val er_damage = actioner.damaged.is
          val ee_damage = actionee.damaged.is
          actioner.damaged(ee_damage)
          actionee.damaged(er_damage)
        }
        actioner.save
        actionee.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        
        GameProcessor.check_death(actionee, actioner, action, userentrys)
        GameProcessor.check_death(actioner, actioner, action, userentrys)
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
        
        if (!actioner.live.is) {
          GameProcessor.next_player(room, roomround, roomphase, userentrys)
        }
      
      case MTypeEnum.ACTION_DETECTIVE_REASONA =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actionee.add_user_flag(UserEntryFlagEnum.REASONAED).save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        
        
        if (actionee.get_real_role.role_side.toString == action.action_flags.is) {
          actioner.action_point(actioner.action_point.is + 2)
          if (actioner.action_point.is >= userentrys.filter(!_.revoked.is).length)
            actioner.add_user_flag(UserEntryFlagEnum.VICTORY)
                    .add_user_flag(UserEntryFlagEnum.VICTORY2)
        }
        
        actioner.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          //RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        } 

      case MTypeEnum.ACTION_DETECTIVE_REASONR =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        
        if (actionee.get_real_role.role_enum.toString == action.action_flags.is) {
          actioner.add_user_flag(UserEntryFlagEnum.VICTORY)
                  .add_user_flag(UserEntryFlagEnum.VICTORY2) 
        }
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        actioner.save

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          //RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
        
      case MTypeEnum.ACTION_WHITECARD_BALANCE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actioner.remove_item(CardEnum.W_BALANCE)
    
        // 從 CardPool 修改 owner 資訊
        val card = CardPool.find(By(CardPool.room_id, actioner.room_id.is),
                                 By(CardPool.card, CardEnum.W_BALANCE.toString)).get
        card.owner_id(0).discarded(true).save
        
        val total_damage = actioner.damaged.is + actionee.damaged.is
        actioner.damaged(total_damage / 2).save
        actionee.damaged(total_damage / 2).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        GameProcessor.check_death(actionee, actioner, action, userentrys)
        GameProcessor.check_death(actioner, actioner, action, userentrys)
        GameProcessor.check_victory(room, roomround, userentrys)
        
        if (!actioner.live.is) {
          GameProcessor.next_player(room, roomround, roomphase, userentrys)
        }
      //課金
      case MTypeEnum.ACTION_LEON_CHARGES =>
        val star = GameProcessor.random.nextInt(10) + 1
        val star_text = if(star > 8){
          "★★★"
        } else if (star > 5) {
          "★★"
        } else {
          "★"
        }
        val charges_role_list = if(star > 8){
          RoleEnum.V3_ROLE_LIST
        } else if (star > 5) {
          RoleEnum.V2_ROLE_LIST
        } else {
          RoleEnum.V1_ROLE_LIST
        }
        var java_role_array: java.util.List[RoleEnum.Value] = ListBuffer(charges_role_list: _*)
        java.util.Collections.shuffle(java_role_array)
        actioner.add_getrole(java_role_array.get(0))
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        actioner.save
        val get_role_name = RoleEnum.get_role(java_role_array.get(0).toString).role_name
        var talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LEON_CHARGES2.toString)
                            .message(actioner.handle_name.is + " 獲得 " + star_text + " " + get_role_name + " (碎片)").actioner_id(actioner.id.is)
        talk.save
        talk.send(actioner.room_id.is)
        if (star > 8) {
          var talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                            .message("恭喜 " + actioner.handle_name.is + " 於碎片抽獎中獲得 " + star_text + " " + get_role_name + " ！真是太幸運了！")
          talk2.save
          talk2.send(actioner.room_id.is)
        }
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      //轉職
      case MTypeEnum.ACTION_LEON_USE =>
        actioner.role(action.action_flags.is + actioner.role.is)
        actioner.remove_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        actioner.save
        if( action.action_flags.is == "DR" ){
          for (i <- 1 to 7) {
            val live_users = userentrys.filter (x => (x.live.is) && (!x.revoked.is))
            val random_no = GameProcessor.random.nextInt(live_users.length)
            live_users(random_no).beads(live_users(random_no).beads.is + 1)
            live_users(random_no).save
          }
        }
        GameProcessor.check_death(actioner, actioner, action, userentrys)
        if( !GameProcessor.check_victory(room, roomround, userentrys)) {
          roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
        if(!actioner.live.is) {
          GameProcessor.next_player(room, roomround, roomphase, userentrys)
        }
      //掌管
      case MTypeEnum.ACTION_SETH_CONTROL =>
        var item_1 = false
        var item_2 = false
        //指定道具已經被持有的話，清理道具
        userentrys.foreach { userentry1 =>
          if (userentry1.has_item(CardEnum.W_LANCE_OF_LONGINUS)) {
            userentry1.remove_item(CardEnum.W_LANCE_OF_LONGINUS)
            userentry1.save
            item_1 = true
            val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(userentry1.handle_name.is + " 的朗基努斯槍移動到 " + actioner.handle_name.is + " 身上")
            talk.save
            talk.send(actioner.room_id.is)
          }
          if (userentry1.has_item(CardEnum.B_EVILSWORD)) {
            userentry1.remove_item(CardEnum.B_EVILSWORD)
            userentry1.save
            item_2 = true
            val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(userentry1.handle_name.is + " 的邪骸劍移動到 " + actioner.handle_name.is + " 身上")
            talk.save
            talk.send(actioner.room_id.is)
          }
        }
        //訊息
        if (item_1 == false) {
            val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(actioner.handle_name.is + " 從牌堆取得朗基努斯槍")
            talk.save
            talk.send(actioner.room_id.is)
        }
        if (item_2 == false) {
            val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(actioner.handle_name.is + " 從牌堆取得邪骸劍")
            talk.save
            talk.send(actioner.room_id.is)
        }
        actioner.add_item(CardEnum.W_LANCE_OF_LONGINUS)
        actioner.add_item(CardEnum.B_EVILSWORD)
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        val live_users = userentrys.filter (x => (x.live.is) && (!x.revoked.is))
        if (live_users.length >= 5) {
          actioner.action_point(2)
        } else if (live_users.length >= 3) {
          actioner.action_point(3)
        } else {
          actioner.action_point(4)
        }
        actioner.add_user_flag(UserEntryFlagEnum.SLOW)
        actioner.save
        // 從 CardPool 修改 owner 資訊
        val card = CardPool.find(By(CardPool.room_id, actioner.room_id.is),
                                 By(CardPool.card, CardEnum.W_LANCE_OF_LONGINUS.toString)).get
        card.owner_id(actioner.id.is).discarded(false).save
        val card2 = CardPool.find(By(CardPool.room_id, actioner.room_id.is),
                                 By(CardPool.card, CardEnum.B_EVILSWORD.toString)).get
        card2.owner_id(actioner.id.is).discarded(false).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        //跳過回合
        val talk_n = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SETH_CONTROL2.toString).actioner_id(actioner.id.is)
        talk_n.save
        talk_n.send(actioner.room_id.is)
        GameProcessor.next_player(room, roomround, roomphase, userentrys)
    
      case MTypeEnum.ACTION_PUZZLE_SPIKE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val actionee2 : UserEntry = userentrys.filter(_.id.is.toString == action.action_flags.is)(0)

        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        val death_number = userentrys.filter(x => !x.live.is).length
        if (actionee.inflict_damage(3, actioner)) {
          GameProcessor.check_death(actionee, actioner, action, userentrys)
        }
        if (actionee2.inflict_damage(3, actioner)) {
          GameProcessor.check_death(actionee2, actioner, action, userentrys)
        }
        val death_number2 = userentrys.filter(x => !x.live.is).length
        if (death_number2 - death_number >= 2)
          actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!actioner.live.is)
            GameProcessor.next_player(room, roomround, roomphase, userentrys) 
          else {
            roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
            RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }  
        }
        
      case MTypeEnum.ACTION_GINGER_RESENTFUL =>
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.add_user_flag(UserEntryFlagEnum.RESENTFUL).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!actioner.live.is)
            GameProcessor.next_player(room, roomround, roomphase, userentrys) 
          else {
            roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
            RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }  
        }
        
      case xs =>
        //warn("Unprocessed Action : " + action.toString)
    }
  }
  
  def post_attack_or_next_player(room : Room, roomround : RoomRound, roomphase : RoomPhase, userentrys : List[UserEntry]) = {
    val actioner = UserEntry.get(roomphase.player.is, userentrys)
    val role = actioner.get_skill_role
    
    if (!actioner.live.is)
      GameProcessor.next_player(room, roomround, roomphase, userentrys)
    else if ((actioner.revealed) && (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
        (role.post_skill != ActionNoAction)) {
      val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                 .phase_type(RoomPhaseEnum.POST_ATTACK.toString).player(roomphase.player.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
      new_phase.save
      RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
      RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TIME_TABLE)))
      RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
    } else
      GameProcessor.next_player(room, roomround, roomphase, userentrys)
  }
}
