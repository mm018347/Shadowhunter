package org.plummtw.shadowhunter.model

import net.liftweb._
import net.liftweb.mapper._
import net.liftweb.util.FieldError
import net.liftweb.http.{SHtml, SessionVar, RequestVar}
import net.liftweb.common.{Empty, Box, Full}

import scala.xml.NodeSeq
import scala.util.matching.Regex

import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.util.PlummUtil
import org.plummtw.shadowhunter.heavy.GameProcessor


object CurrentUserEntry   extends SessionVar[Box[UserEntry]](Empty)
object CurrentUserEntry_E extends SessionVar[UserEntry](GlobalUserEntry.NoUserEntry)
object CurrentUserEntry_R extends SessionVar[UserEntry](GlobalUserEntry.NoUserEntry)

object UserEntrys_E        extends SessionVar[List[UserEntry]](List())
object UserEntrys_ER       extends SessionVar[List[UserEntry]](List())
object UserEntrys_R        extends SessionVar[List[UserEntry]](List())
object UserEntrys_RR       extends SessionVar[List[UserEntry]](List())


class UserEntry extends LongKeyedMapper[UserEntry] with CreatedUpdated with IdPK {
  def getSingleton = UserEntry // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  //object id            extends MappedLongIndex(this)
  object room_id       extends MappedLongForeignKey(this, Room)
  object user_id       extends MappedLongForeignKey(this, User)
  
  object user_icon_id  extends MappedLongForeignKey(this, UserIcon) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Long): List[FieldError] = 
      if (in == 0)  List(FieldError(this, <b>尚未選擇圖像</b>)) 
      else Nil
  }
  
  object user_no       extends MappedInt(this)
  // Login 用 id
  object uname         extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() <  4)           List(FieldError(this, <b>帳號過短＜４</b>))
             else if (in.length() > 20)  List(FieldError(this, <b>帳號過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>帳號包含控制碼</b>)) else Nil).flatten
  }

  // 顯示用暱名
  object handle_name   extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() <  1)           List(FieldError(this, <b>暱稱過短＜１</b>))
             else if (in.length() > 20)  List(FieldError(this, <b>暱稱過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>暱稱包含控制碼</b>)) else Nil).flatten
  }
  
  // trip
  object trip         extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() > 20)  List(FieldError(this, <b>ｔｒｉｐ過長＞２０</b>))
           else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>ｔｒｉｐ包含控制碼</b>)) else Nil).flatten
  }
  
  object password      extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() < 4)          List(FieldError(this, <b>密碼過短＜４</b>))
             else if (in.length() > 20)  List(FieldError(this, <b>密碼過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>密碼包含控制碼</b>)) else Nil).flatten
  }
    
  object sex           extends MappedString(this, 1)  {
    override def validations = validPriority _ :: super.validations
    override def defaultValue = "M"

    val sex_map: Map[String, String] = Map("M"->"男", "F"->"女")
    def sex_radios =
      SHtml.radio(sex_map.keys.toList, Full(UserEntry.this.sex.toString), UserEntry.this.sex(_))

    def generateHtml = sex_radios.flatMap(PlummUtil.htmlize(_, sex_map))

    def validPriority(in: String): List[FieldError] =
      if (!sex_map.contains(in)) List(FieldError(this, <b>性別錯誤</b>))
      else Nil
  }
  
  object role          extends MappedString(this,10)
  object subrole       extends MappedString(this,10) {
    override def defaultValue = ""
  }
  object haterole      extends MappedString(this,10)
  
  object damaged extends MappedInt(this) {
    override def defaultValue = 0
  }
  
  object location extends MappedString(this, 2)
  
  object action_point  extends MappedInt(this) {
    override def defaultValue = 0
  }
  
  object beads  extends MappedInt(this) {
    override def defaultValue = 0
  }

  object cash  extends MappedInt(this) {
    override def defaultValue = 0
  }
  
  object live          extends MappedBoolean(this) {
    override def defaultValue = true
  }
  
  object won          extends MappedBoolean(this) {
    override def defaultValue = false
  }
  
  object revealed      extends MappedBoolean(this) {
    override def defaultValue = false
  }
  
  object revoked      extends MappedBoolean(this) {
    override def defaultValue = false
  }
  
  object last_words    extends MappedString(this, 600) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length() > 600)  List(FieldError(this, <b>遺言過長＞６００</b>))
      else Nil
  }
  
  object ip_address0     extends MappedString(this, 20) with LifecycleCallbacks {
    override def beforeCreate = {
      this(ip_address.is)
    }
  }
  object ip_address     extends MappedString(this, 20)
  object ip_address_md5 extends MappedString(this, 34) with LifecycleCallbacks {
    override def beforeCreate = {
      this(PlummUtil.generateMD5(ip_address.is))
    }
  }

  object last_round_no extends MappedInt(this)
  object reaction      extends MappedBoolean(this)
  object last_talk      extends MappedString(this, 600)

  object target_user  extends MappedLongForeignKey(this, UserEntry) {
    override def defaultValue = 0
  }
  
  object room_flags   extends MappedString(this, 20)
  object role_flags    extends MappedString(this, 20)
  object user_flags    extends MappedString(this, 80)
  object card_flags    extends MappedString(this, 20)
  object getrole_flags extends MappedString(this, 40)
  object item_flags    extends MappedString(this, 80)
  object item_preferred extends MappedString(this, 10)
  // 普通損傷處理
  def inflict_damage(in : Int, actioner : UserEntry, is_attack : Boolean = false) : Boolean = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val er_skill_role = actioner.get_skill_role
    var damaged_num = in
    
    if ((has_user_flag(UserEntryFlagEnum.BARRIER)) && (damaged_num < 99)) {
      false
    } else if ((has_user_flag(UserEntryFlagEnum.MAGICSPIRIT)) && (damaged_num > 0) && (damaged_num < 3)) {
      remove_user_flag(UserEntryFlagEnum.MAGICSPIRIT).save
      false
    } else {
      // 
      if ((!is_attack) && (actioner != this) && (er_skill_role == RoleBob) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) &&
         (actioner.has_item(CardEnum.W_SILVER_ROSARY)) && (damaged_num >= 2) ) {
        if (items.length > 0) {
          val robbed_item = GameProcessor.rob_single(room, roomround, actioner, this)
          actioner.beads(actioner.beads.is + beads.is)
          beads(0)
          actioner.save
          GameProcessor.check_item_victory(room, roomround, actioner)
        }
      }
      //特羅修
      if((get_skill_role == RoleLion) && (revealed.is) && (hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (hasnt_item(CardEnum.B_MASK)) && (damaged_num > 0)){
        damaged_num = math.max(0, damaged_num - 1)
      }
      // 
      if (damaged_num > 0) {
        damaged(damaged.is + damaged_num)
        true
      } else {
        false
      }
    } 
  }
  // 攻擊損傷處理
  def inflict_a_damage(in : Int, actioner : UserEntry, is_attack : Boolean = false) : Boolean = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val er_skill_role = actioner.get_skill_role
    var damaged_num = in
    
    if (has_user_flag(UserEntryFlagEnum.BARRIER)) {
      false
    } else if ((has_user_flag(UserEntryFlagEnum.MAGICSPIRIT)) && (damaged_num > 0) && (damaged_num < 3)) {
      remove_user_flag(UserEntryFlagEnum.MAGICSPIRIT).save
      true
    } else {
      if ((!is_attack) && (actioner != this) && (er_skill_role == RoleBob) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) &&
         (actioner.has_item(CardEnum.W_SILVER_ROSARY)) && (damaged_num >= 2) ) {
        if (items.length > 0) {
          val robbed_item = GameProcessor.rob_single(room, roomround, actioner, this)
          actioner.beads(actioner.beads.is + beads.is)
          beads(0)
          actioner.save
          GameProcessor.check_item_victory(room, roomround, actioner)
        }
      }
      damaged(damaged.is + damaged_num)
      
      true
    }  
  }
  // 綠卡損傷處理
  def inflict_g_damage(in : Int, actioner : UserEntry, is_attack : Boolean = false) : Boolean = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val er_skill_role = actioner.get_skill_role
    var damaged_num = in
    
    if (has_user_flag(UserEntryFlagEnum.BARRIER)) {
      true
    } else if ((has_user_flag(UserEntryFlagEnum.MAGICSPIRIT)) && (damaged_num > 0) && (damaged_num < 3)) {
      true
    } else {
      val actioner_role = actioner.get_skill_role
      if ((!is_attack) && (actioner != this) && (er_skill_role == RoleBob) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) &&
         (actioner.has_item(CardEnum.W_SILVER_ROSARY)) && (damaged_num >= 2) ) {
        if (items.length > 0) {
          val robbed_item = GameProcessor.rob_single(room, roomround, actioner, this)
          actioner.beads(actioner.beads.is + beads.is)
          beads(0)
          actioner.save
          GameProcessor.check_item_victory(room, roomround, actioner)
        }
      }
      damaged(damaged.is + damaged_num)
      
      true
    }  
  }
  // 卡片損傷前置處理
  def inflict_card_damage(in : Int, actioner : UserEntry) = {
    val result = inflict_damage(in, actioner)

    if (result && (in > 0) && (actioner.get_skill_role == RoleWitch) && (actioner.revealed.is) &&
        (!actioner.has_user_flag(UserEntryFlagEnum.SEALED) && (this != actioner)) &&
        (!actioner.has_item(CardEnum.B_MASK) && (this != actioner)))
      add_user_flag(UserEntryFlagEnum.FROG)
        
    result
  }
  // 黑卡損傷前置處理
  def inflict_b_card_damage(in : Int, actioner : UserEntry) = {
    val result = inflict_damage(in, actioner)

    if (result && (in > 0) && (actioner.get_skill_role == RoleWitch) && (actioner.revealed.is) &&
        (!actioner.has_user_flag(UserEntryFlagEnum.SEALED) && (this != actioner)) &&
        (!actioner.has_item(CardEnum.B_MASK) && (this != actioner)))
      add_user_flag(UserEntryFlagEnum.FROG)
        
    result
  }
  // 綠卡損傷前置處理
  def inflict_g_card_damage(in : Int, actioner : UserEntry) = {
    val result = inflict_g_damage(in, actioner)
    
    if (has_user_flag(UserEntryFlagEnum.BARRIER)) {
    } else if ((has_user_flag(UserEntryFlagEnum.MAGICSPIRIT)) && (in > 0) && (in < 3)) {
      remove_user_flag(UserEntryFlagEnum.MAGICSPIRIT).save
    } else if (result && (actioner.get_skill_role == RoleWitch) && (actioner.revealed.is) &&
        (!actioner.has_user_flag(UserEntryFlagEnum.SEALED) && (this != actioner)) &&
        (!actioner.has_item(CardEnum.B_MASK) && (this != actioner))) {
      add_user_flag(UserEntryFlagEnum.FROG)
    }
      
    result
  }
  // 減少損傷處理
  def lower_damage(in : Int, userentrys : List[UserEntry]) = {
    damaged(math.max(0, damaged.is - in)).remove_user_flag(UserEntryFlagEnum.POISON)
    
    if((get_skill_role == RoleArsis) && (revealed.is) && (hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (hasnt_item(CardEnum.B_MASK))){
      val hunters = userentrys.filter(x => (x.live.is) && (x.get_role.role_side == RoleSideEnum.HUNTER) && (x.revealed.is))
        hunters.foreach { hunter =>
          hunter.damaged(math.max(0, hunter.damaged.is - 1))
          hunter.save
      }
    }
  }
  def get_user_icon : UserIcon = {
    UserIconCache.getOr(user_icon_id.is) { () => 
      UserIcon.find(By(UserIcon.id, user_icon_id.is)) match {
        case Full(x) => x
        case x       => UserIcon.find(By(UserIcon.id, 1)).get
    }}
  }
  
  def get_subrole = {
    val subrole_str =
      if (subrole.is.length > 2) subrole.is.substring(0,2)
      else subrole.is
    
    RoleEnum.get_role(subrole_str)
  }
  // 技能角色
  def get_skill_role = {
    val role_str =
      if (role.is.length > 2) role.is.substring(0,2)
      else role.is
    
    if ((role_str == RoleEnum.TEL.toString) &&
        (revealed.is) &&
        (subrole.is != ""))
      RoleEnum.get_role(subrole.is)
    else
      RoleEnum.get_role(role_str)
  }
  // 綠卡顯示角色
  def get_hermit_role = {
    val role_str =
      if (role.is.length > 2) role.is.substring(0,2)
      else role.is
    
    if ((role_str == RoleEnum.UNKNOWN.toString) &&
        (!revealed.is) &&
        (role_flags.is != ""))
      RoleEnum.get_role(role_flags.is)
    else
      RoleEnum.get_role(role_str)
  }
  // 初始角色
  def get_real_role = {
    val role_str =
      if (role.is.length > 2)
        role.is.substring(role.is.length - 2, role.is.length)
      else role.is
    
    RoleEnum.get_role(role_str)
  }
  // 當前角色
  def get_role = {
    val role_str =
      if (role.is.length > 2) role.is.substring(0,2)
      else role.is
    
    RoleEnum.get_role(role_str)
  }
  
  def get_role_field = {
    var result : NodeSeq = 
      if (role.is.length > 2) Seq(RoleEnum.get_role(role.is.substring(0,2)).cfield)
    else Seq(RoleEnum.get_role(role.is).cfield)
      
    for (i <- 1 until (role.is.length / 2))
      result = result ++ RoleEnum.get_role(role.is.substring(i*2,i*2+2)).simple_cfield
      
    result
  }
  
  def get_subrole_field = {
    var result : NodeSeq = 
      if (subrole.is.length > 2) Seq(RoleEnum.get_role(subrole.is.substring(0,2)).cfield)
    else Seq(RoleEnum.get_role(subrole.is).cfield)
      
    for (i <- 1 until (subrole.is.length / 2))
      result = result ++ RoleEnum.get_role(subrole.is.substring(i*2,i*2+2)).simple_cfield
      
    result
  }
  
  def get_secret_field = {
    var result : NodeSeq = 
      if (role.is.length > 2) Seq(RoleEnum.get_role(role.is.substring(0,2)).cfield)
    else Seq(RoleEnum.get_role(role.is).cfield)
      
    for (i <- 1 until (role.is.length / 2))
      result = result ++ RoleEnum.get_role(role.is.substring(i*2,i*2+2)).simple_cfield
      
    result
  }
  
  def has_room_flag(flag : UserEntryRoomFlagEnum.Value) : Boolean = 
    return (room_flags.is.indexOf(flag.toString) != -1)
  def hasnt_room_flag(flag : UserEntryRoomFlagEnum.Value) : Boolean = 
    !has_room_flag(flag)
  def add_room_flag(flag : UserEntryRoomFlagEnum.Value) : UserEntry = 
    room_flags(room_flags.is + flag.toString)
  def remove_room_flag(flag : UserEntryRoomFlagEnum.Value) : UserEntry = 
    room_flags(room_flags.is.replace(flag.toString, ""))
  
  def has_role_flag(flag : UserEntryRoleFlagEnum.Value) : Boolean = 
    return (role_flags.is.indexOf(flag.toString) != -1)
  def hasnt_role_flag(flag : UserEntryRoleFlagEnum.Value) : Boolean = 
    !has_role_flag(flag)
  def add_role_flag(flag : UserEntryRoleFlagEnum.Value) : UserEntry = 
    role_flags(role_flags.is + flag.toString)
  def remove_role_flag(flag : UserEntryRoleFlagEnum.Value) : UserEntry = 
    role_flags(role_flags.is.replace(flag.toString, ""))
  
  def has_user_flag(flag : UserEntryFlagEnum.Value) : Boolean = 
    return (user_flags.is.indexOf(flag.toString) != -1)
  def hasnt_user_flag(flag : UserEntryFlagEnum.Value) : Boolean = 
    !has_user_flag(flag)
  def add_user_flag(flag : UserEntryFlagEnum.Value) : UserEntry = 
    user_flags(user_flags.is + flag.toString)
  def remove_user_flag(flag : UserEntryFlagEnum.Value) : UserEntry = 
    user_flags(user_flags.is.replace(flag.toString, ""))
  
  def has_item(item : CardEnum.Value) : Boolean = 
    return (item_flags.is.indexOf(item.toString) != -1)
  def hasnt_item(item : CardEnum.Value) : Boolean = 
    !has_item(item)
  def add_item(item : CardEnum.Value) : UserEntry = 
    item_flags(item_flags.is + item.toString)
  def remove_item(item : CardEnum.Value) : UserEntry = 
    item_flags(item_flags.is.replace(item.toString, ""))
  
  def items =
    item_flags.is.grouped(3).toList.map(x => CardEnum.get_card(x))
    
  def has_card(card : CardEnum.Value) : Boolean = 
    return (card_flags.is.indexOf(card.toString) != -1)
  def hasnt_card(card : CardEnum.Value) : Boolean = 
    !has_card(card)
  def add_card(card : CardEnum.Value) : UserEntry = 
    card_flags(card_flags.is + card.toString)
  def remove_card(card : CardEnum.Value) : UserEntry = 
    card_flags(card_flags.is.replace(card.toString, ""))
  
  def cards =
    card_flags.is.grouped(3).toList.map(x => CardEnum.get_card(x))
    
  def has_getrole(getrole : RoleEnum.Value) : Boolean = 
    return (getrole_flags.is.indexOf(getrole.toString) != -1)
  def hasnt_getrole(getrole : RoleEnum.Value) : Boolean = 
    !has_getrole(getrole)
  def add_getrole(getrole : RoleEnum.Value) : UserEntry = 
    getrole_flags(getrole_flags.is + getrole.toString)
  def remove_getrole(getrole : RoleEnum.Value) : UserEntry = 
    getrole_flags(getrole_flags.is.replace(getrole.toString, ""))
  
  def getroles =
    getrole_flags.is.grouped(2).toList.map(x => RoleEnum.get_role(x))
  //def action_list 
}

object UserEntry extends UserEntry with LongKeyedMetaMapper[UserEntry] {
  override def fieldOrder = List(id, room_id, user_id, user_icon_id, user_no, uname, handle_name, trip, password, sex,
                               role, subrole, haterole, damaged, location, action_point, beads, cash, live, last_words, revealed, revoked,
                               won, ip_address0, ip_address, ip_address_md5,
                               last_round_no, reaction, last_talk, target_user, room_flags, 
                               role_flags, user_flags, card_flags, getrole_flags, item_flags, item_preferred)

  def get (get_id : Long, userentrys : List[UserEntry]) : UserEntry= {
    if (get_id <= 0) {
      get_id match {
        case -1 => GlobalUserEntry.AdminUserEntry
        case  _ => GlobalUserEntry.NoUserEntry
      }
    } else
      userentrys.find(_.id.is == get_id).getOrElse(GlobalUserEntry.NoUserEntry)
  }
  
  def rrnc(currentuserentry : UserEntry, userentrys : List[UserEntry]) =
    userentrys.filter(x =>  (!x.revoked.is) && (x != currentuserentry))
  
  def rr(userentrys : List[UserEntry]) =
    userentrys.filter(!_.revoked.is)
  
  def rlnc(currentuserentry : UserEntry, userentrys : List[UserEntry]) =
    userentrys.filter(x =>  (x.live.is) && (x != currentuserentry))
  
  def rl(userentrys : List[UserEntry]) =
    userentrys.filter(x =>  (x.live.is))
  
  def rrrlnc(currentuserentry : UserEntry, userentrys : List[UserEntry]) =
    userentrys.filter(x =>  (!x.revoked.is) && (x.live.is) && (x != currentuserentry))
  
  def findAllByRoom (room : Room) : List[UserEntry] = {
    if ((room.status.is == RoomStatusEnum.WAITING.toString)) // ||
        // (room.hasnt_flag(RoomFlagEnum.RANDOM_POSITION)))
      UserEntry.findAll(By(UserEntry.room_id, room.id.is))
    else
      UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                        OrderBy(UserEntry.user_no, Ascending))
  }
}

object GlobalUserEntry{
  val NoUserEntry    = UserEntry.create.handle_name("無").user_icon_id(1)
  val AdminUserEntry = UserEntry.create.handle_name("管理員").user_icon_id(1)
  
  val GLOBAL_USERENTRY_LIST = List(NoUserEntry, AdminUserEntry)
}
/*
 * CREATE TABLE `userentry` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `user_id` bigint(20) DEFAULT NULL,
  `room_id` bigint(20) DEFAULT NULL,
  `name` varchar(20) DEFAULT NULL,
  `uname` varchar(20) DEFAULT NULL,
  `handle_name` varchar(20) DEFAULT NULL,
  `trip` varchar(80) DEFAULT NULL,
  `password` varchar(20) DEFAULT NULL,
  `role` varchar(3) DEFAULT NULL,
  `cash` int(11) DEFAULT NULL,
  `live` varchar(1) DEFAULT NULL,
  `room_flags` varchar(80) DEFAULT NULL,
  `item_flags` varchar(80) DEFAULT NULL,
  `card_flags` varchar(20) DEFAULT NULL,
  `get_flags` varchar(40) DEFAULT NULL,
  `user_flags` varchar(600) DEFAULT NULL,
  `created_ip` varchar(20) DEFAULT NULL,
  `created` datetime DEFAULT NULL,
  `updated` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `userentry_room_id` (`room_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
 */