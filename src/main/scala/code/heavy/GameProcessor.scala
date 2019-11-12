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

//import scala.collection.JavaConversions
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer 

import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.card._
import org.plummtw.shadowhunter.util.{PlummUtil, LocationHelper}

object GameProcessor extends Logger{
  val random = scala.util.Random
  
  def shuffle_cardpool(room : Room, card_type : CardTypeEnum.Value, card_pool : List[CardPool]) {
    //val room = Room_R.get
    //val card_pool = CardPool.findAll(By(CardPool.room_id, room.id.is),
    //                              By(CardPool.card_type, card_type.toString))
    
    if (card_pool.length == 0) {
      val card_list = card_type match {
        case CardTypeEnum.BLACK => 
          var result = CardEnum.BLACK_LIST
          if (room.has_flag(RoomFlagEnum.BLACKCARD_DAGGER))
            result = CardEnum.B_DAGGER :: result
          if (room.has_flag(RoomFlagEnum.BLACKCARD_LAMIRROR))
            result = CardEnum.B_LAMIRROR :: result
          if (room.has_flag(RoomFlagEnum.BLACKCARD_MASK))
            result = CardEnum.B_MASK :: result
          if (room.has_flag(RoomFlagEnum.BLACKCARD_DECLINE))
            result = CardEnum.B_DECLINE :: result
          if (room.has_flag(RoomFlagEnum.BLACKCARD_FIREHORSE))
            result = CardEnum.B_FIREHORSE :: result
          if (room.has_flag(RoomFlagEnum.BLACKCARD_SPLINTERED))
            result = CardEnum.B_SPLINTERED :: result
          if (room.has_flag(RoomFlagEnum.BLACKCARD_EVILSWORD))
            result = CardEnum.B_EVILSWORD :: result
          if (room.has_flag(RoomFlagEnum.BLACKCARD_GEMWAND))
            result = CardEnum.B_GEMWAND :: result
          if (room.has_flag(RoomFlagEnum.BLACKCARD_SUPPLYBOMB))
            result = CardEnum.B_SUPPLYBOMB :: result
          if (room.has_flag(RoomFlagEnum.BLACKCARD_EXPLODE))
            result = CardEnum.B_EXPLODE :: result
          result
        case CardTypeEnum.WHITE => 
          var result = CardEnum.WHITE_LIST
          if (room.has_flag(RoomFlagEnum.WHITECARD_TEA))
            result = CardEnum.W_TEA :: result
          if (room.has_flag(RoomFlagEnum.WHITECARD_BALANCE))
            result = CardEnum.W_BALANCE :: result
          if (room.has_flag(RoomFlagEnum.WHITECARD_FIREWORK))
            result = CardEnum.W_FIREWORK :: result
          if (room.has_flag(RoomFlagEnum.WHITECARD_FLYHIGH))
            result = CardEnum.W_FLYHIGH :: result
          if (room.has_flag(RoomFlagEnum.WHITECARD_ENCHANTMENT))
            result = CardEnum.W_ENCHANTMENT :: result
          if (room.has_flag(RoomFlagEnum.WHITECARD_MAGICSPIRIT))
            result = CardEnum.W_MAGICSPIRIT :: result
          if (room.has_flag(RoomFlagEnum.WHITECARD_EARTHQUAKE))
            result = CardEnum.W_EARTHQUAKE :: result
          if (room.has_flag(RoomFlagEnum.WHITECARD_VOLCANIC))
            result = CardEnum.W_VOLCANIC :: result
          if (room.has_flag(RoomFlagEnum.WHITECARD_TSUNAMI))
            result = CardEnum.W_TSUNAMI :: result
          result
        case CardTypeEnum.GREEN => 
          var result = CardEnum.GREEN_LIST
          if (room.has_flag(RoomFlagEnum.GREENCARD_HUNTERHEAL2))
            result = CardEnum.G_HUNTER_HEAL2 :: result
          if (room.has_flag(RoomFlagEnum.GREENCARD_LIFEUNDER11_2))
            result = CardEnum.G_LIFE_UNDER11_2 :: result
          result
      }
      
      val java_card_list: java.util.List[CardEnum.Value] = ListBuffer(card_list: _*)
      
      java.util.Collections.shuffle(java_card_list)
      
      val card_list2 = java_card_list.toList
      var card_no = 0
      card_list2 foreach { card_data =>
        val card = CardPool.create.room_id(room.id.is).card_no(card_no).card_type(card_type.toString)
                           .card(card_data.toString).discarded(false)
        card.save
        card_no = card_no + 1
      }
    } else {
      val java_card_no_list: java.util.List[Int] = new java.util.ArrayList()
      for (i <- 0 until card_pool.length) 
        java_card_no_list.add(i)  
      
      java.util.Collections.shuffle(java_card_no_list)
      
      var card_index = 0
      card_pool foreach { card =>
        if (card.owner_id.is == 0)
          card.discarded(false)
        card.card_no(java_card_no_list.get(card_index))
        card.save
        card_index = card_index + 1
      }
    }
  }
  
  def draw_card(room : Room, card_type : CardTypeEnum.Value) : CardPool = {
    //val room = Room_R.get
    var card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                     OrderBy(CardPool.card_no, Ascending))
    var card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    
    var card_index = card_type match {
      case CardTypeEnum.BLACK => room.blackcard_index.is
      case CardTypeEnum.WHITE => room.whitecard_index.is
      case CardTypeEnum.GREEN => room.greencard_index.is
    }
    
    var result : CardPool = null
    do {
      if (card_index >= card_pool.length) {
        shuffle_cardpool(room, card_type, card_pool)
        card_index = 0
      
        card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                     OrderBy(CardPool.card_no, Ascending))
        card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
        //card_pool = CardPool.findAll(By(CardPool.room_id, room.id.is),
        //                            By(CardPool.card_type, card_type.toString))
      }
    
      result = card_pool(card_index)
      card_index = card_index + 1
    } while ((result.discarded.is) || (result.owner_id.is != 0))
    
    if (!CardEnum.get_card(result.card.is).isInstanceOf[Equipment])
      result.discarded(true).save
    

    card_type match {
      case CardTypeEnum.BLACK => room.blackcard_index(card_index)
      case CardTypeEnum.WHITE => room.whitecard_index(card_index)
      case CardTypeEnum.GREEN => room.greencard_index(card_index)
    }
    
    room.save
    
    RoomActor ! SessionVarSet(room = room, card_list = card_list)
    RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.CARD_TABLE))
    
    result
  }
  
  def draw_specific(room : Room, card_str : String) : CardPool = {
    
    val card_id = try {
      card_str.toLong 
    } catch { case e : Exception => 0L}
    
    //val room = Room_R.get
    val card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                        OrderBy(CardPool.card_no, Ascending))
    //val card_pool = card_list.filter(x => x.card_type.is == card_type.toString)

    val result = card_list.filter(_.id.is == card_id)(0)
    
    if (!CardEnum.get_card(result.card.is).isInstanceOf[Equipment])
      result.discarded(true).save
    

    RoomActor ! SessionVarSet(room = room, card_list = card_list)
    RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.CARD_TABLE))
    
    result
  }
  
  def peek_card2(room : Room, card_type : CardTypeEnum.Value) : (CardPool, CardPool) = {
    //val room = Room_R.get
    var card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                     OrderBy(CardPool.card_no, Ascending))
    var card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    
    var card_index = card_type match {
      case CardTypeEnum.BLACK => room.blackcard_index.is
      case CardTypeEnum.WHITE => room.whitecard_index.is
      case CardTypeEnum.GREEN => room.greencard_index.is
    }
    
    var result : CardPool = null
    if (card_index >= card_pool.length) {
      shuffle_cardpool(room, card_type, card_pool)
      card_index = 0
      
      card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                   OrderBy(CardPool.card_no, Ascending))
      card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    }
    
    var card_pool2 = card_pool.filter(x => (x.card_no.is >= card_index) &&
                                           (!x.discarded.is) && (x.owner_id.is == 0))
    
    if (card_pool2.length >= 2)
      ((card_pool2(0), card_pool2(1)))
    else if (card_pool2.length == 1)
      ((card_pool2(0), card_pool2(0)))
    else {
      shuffle_cardpool(room, card_type, card_pool)
      card_index = 0
      
      card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                   OrderBy(CardPool.card_no, Ascending))
      card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
      card_pool2 = card_pool.filter(x => (x.card_no.is >= card_index) &&
                                           (!x.discarded.is) && (x.owner_id.is == 0))
      card_type match {
        case CardTypeEnum.BLACK => room.blackcard_index(card_index)
        case CardTypeEnum.WHITE => room.whitecard_index(card_index)
        case CardTypeEnum.GREEN => room.greencard_index(card_index)
      }
    
      room.save
    
      RoomActor ! SessionVarSet(room = room, card_list = card_list)
      RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.CARD_TABLE))

      ((card_pool2(0), card_pool2(1)))
    }
  }
  
  def peek_card3(room : Room, card_type : CardTypeEnum.Value) : (CardPool, CardPool, CardPool) = {
    //val room = Room_R.get
    var card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                     OrderBy(CardPool.card_no, Ascending))
    var card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    
    var card_index = card_type match {
      case CardTypeEnum.BLACK => room.blackcard_index.is
      case CardTypeEnum.WHITE => room.whitecard_index.is
      case CardTypeEnum.GREEN => room.greencard_index.is
    }
    
    var result : CardPool = null
    if (card_index >= card_pool.length) {
      shuffle_cardpool(room, card_type, card_pool)
      card_index = 0
      
      card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                   OrderBy(CardPool.card_no, Ascending))
      card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    }
    
    var card_pool3 = card_pool.filter(x => (x.card_no.is >= card_index) &&
                                           (!x.discarded.is) && (x.owner_id.is == 0))
    
    if (card_pool3.length >= 3)
      ((card_pool3(0), card_pool3(1), card_pool3(2)))
    else if (card_pool3.length == 2)
      ((card_pool3(0), card_pool3(0), card_pool3(1)))
    else if (card_pool3.length == 1)
      ((card_pool3(0), card_pool3(0), card_pool3(0)))
    else{
      shuffle_cardpool(room, card_type, card_pool)
      card_index = 0
      
      card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                   OrderBy(CardPool.card_no, Ascending))
      card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
      card_pool3 = card_pool.filter(x => (x.card_no.is >= card_index) &&
                                           (!x.discarded.is) && (x.owner_id.is == 0))
      card_type match {
        case CardTypeEnum.BLACK => room.blackcard_index(card_index)
        case CardTypeEnum.WHITE => room.whitecard_index(card_index)
        case CardTypeEnum.GREEN => room.greencard_index(card_index)
      }
    
      room.save
    
      RoomActor ! SessionVarSet(room = room, card_list = card_list)
      RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.CARD_TABLE))

      ((card_pool3(0), card_pool3(1), card_pool3(2)))
    }
  }
  
  // 分配職業
  def dispatch_role(room : Room, userentrys : List[UserEntry]) {
    val random = new scala.util.Random
    var shadow_hunter_number = 
        if ((userentrys.length>=10) && (room.has_flag(RoomFlagEnum.NEUTRAL_BACK_2)))
          5
        else if ((userentrys.length>=8) && (room.has_flag(RoomFlagEnum.NEUTRAL_BACK_1)))
          4
        else if (userentrys.length>=8)
          3
        else 
          2
    var neutral_number = userentrys.length
    if (room.hasnt_flag(RoomFlagEnum.ALL_NEUTRAL)) {
      neutral_number = userentrys.length - 2 * shadow_hunter_number
    } else {
      shadow_hunter_number = 0
    }
    
    // 先產生職業清單
    // 標準職業
    var all_role_list = RoleEnum.STANDARD_ROLE_LIST
    if (room.has_flag(RoomFlagEnum.NO_UNKNOWN))
        all_role_list = all_role_list filterNot(_ == RoleEnum.UNKNOWN)
    if (room.has_flag(RoomFlagEnum.NO_VAMPIRE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.VAMPIRE)
    if (room.has_flag(RoomFlagEnum.NO_WEREWOLF))
        all_role_list = all_role_list filterNot(_ == RoleEnum.WEREWOLF)
    if (room.has_flag(RoomFlagEnum.NO_EMI))
        all_role_list = all_role_list filterNot(_ == RoleEnum.EMI)
    if (room.has_flag(RoomFlagEnum.NO_FRANKLIN))
        all_role_list = all_role_list filterNot(_ == RoleEnum.FRANKLIN)
    if (room.has_flag(RoomFlagEnum.NO_GEORGE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.GEORGE)
    if (room.has_flag(RoomFlagEnum.NO_ALLIE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.ALLIE)
    if (room.has_flag(RoomFlagEnum.NO_BOB))
        all_role_list = all_role_list filterNot(_ == RoleEnum.BOB)
    if ((room.has_flag(RoomFlagEnum.NO_CHARLES)) || (userentrys.length <= 3))
        all_role_list = all_role_list filterNot(_ == RoleEnum.CHARLES)
    if ((room.has_flag(RoomFlagEnum.NO_DANIEL)) || (room.has_flag(RoomFlagEnum.ALL_NEUTRAL)))
        all_role_list = all_role_list filterNot(_ == RoleEnum.DANIEL)
    // 擴充職業 ULTRASOUL, VALKYRIE, WIGHT, ELLEN, FUKA, GREGOR, AGNES, BRYAN, CATHERINE, DAVID
    if (room.has_flag(RoomFlagEnum.EXPANSION_ROLE)) {
      all_role_list = all_role_list ++ RoleEnum.EXPANSION_ROLE_LIST
      if (room.has_flag(RoomFlagEnum.NO_ULTRASOUL))
        all_role_list = all_role_list filterNot(_ == RoleEnum.ULTRASOUL)
      if (room.has_flag(RoomFlagEnum.NO_VALKYRIE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.VALKYRIE)
      if (room.has_flag(RoomFlagEnum.NO_WIGHT))
        all_role_list = all_role_list filterNot(_ == RoleEnum.WIGHT)
      if (room.has_flag(RoomFlagEnum.NO_ELLEN))
        all_role_list = all_role_list filterNot(_ == RoleEnum.ELLEN)
      if (room.has_flag(RoomFlagEnum.NO_FUKA))
        all_role_list = all_role_list filterNot(_ == RoleEnum.FUKA)
      if (room.has_flag(RoomFlagEnum.NO_GREGOR))
        all_role_list = all_role_list filterNot(_ == RoleEnum.GREGOR)
      if (room.has_flag(RoomFlagEnum.NO_AGNES))
        all_role_list = all_role_list filterNot(_ == RoleEnum.AGNES)
      if (room.has_flag(RoomFlagEnum.NO_BRYAN))
        all_role_list = all_role_list filterNot(_ == RoleEnum.BRYAN)
      if ((room.has_flag(RoomFlagEnum.NO_CATHERINE)) || (userentrys.length < 4))
        all_role_list = all_role_list filterNot(_ == RoleEnum.CATHERINE)
      if (room.has_flag(RoomFlagEnum.NO_DAVID))
        all_role_list = all_role_list filterNot(_ == RoleEnum.DAVID)
    }
    // 自製職業 UNSEEN, UNDEAD, VENGEFUL_GHOST, VIPER, WITCH, WICKED, 
    // EVAN, EMMA, FATHER_OCONNEL, FENG, GINGER, GODFAT,
    // ANGEL, BELLANDONA, CASSANDRA, DESPAIR, 
    // ADECOY, BOMB, CHESHIRE, DETECTIVE
    if (room.has_flag(RoomFlagEnum.CUSTOM_ROLE)) {
      all_role_list = all_role_list ++ RoleEnum.CUSTOM_ROLE_LIST
      if (room.has_flag(RoomFlagEnum.NO_UNSEEN))
        all_role_list = all_role_list filterNot(_ == RoleEnum.UNSEEN)
      if (room.has_flag(RoomFlagEnum.NO_UNDEAD))
        all_role_list = all_role_list filterNot(_ == RoleEnum.UNDEAD)
      if (room.has_flag(RoomFlagEnum.NO_VENGEFUL_GHOST))
        all_role_list = all_role_list filterNot(_ == RoleEnum.VENGEFUL_GHOST)
      if (room.has_flag(RoomFlagEnum.NO_VIPER))
        all_role_list = all_role_list filterNot(_ == RoleEnum.VIPER)
      if (room.has_flag(RoomFlagEnum.NO_WITCH))
        all_role_list = all_role_list filterNot(_ == RoleEnum.WITCH)
      if (room.has_flag(RoomFlagEnum.NO_WICKED))
        all_role_list = all_role_list filterNot(_ == RoleEnum.WICKED)
      if (room.has_flag(RoomFlagEnum.NO_EVAN))
        all_role_list = all_role_list filterNot(_ == RoleEnum.EVAN)
      if (room.has_flag(RoomFlagEnum.NO_EMMA))
        all_role_list = all_role_list filterNot(_ == RoleEnum.EMMA)
      if (room.has_flag(RoomFlagEnum.NO_FATHER_OCONNEL))
        all_role_list = all_role_list filterNot(_ == RoleEnum.FATHER_OCONNEL)
      if (room.has_flag(RoomFlagEnum.NO_FENG))
        all_role_list = all_role_list filterNot(_ == RoleEnum.FENG)
      if (room.has_flag(RoomFlagEnum.NO_GINGER))
        all_role_list = all_role_list filterNot(_ == RoleEnum.GINGER)
      if (room.has_flag(RoomFlagEnum.NO_GODFAT))
        all_role_list = all_role_list filterNot(_ == RoleEnum.GODFAT)
      if ((room.has_flag(RoomFlagEnum.NO_ANGEL)) || (userentrys.length < 3))
        all_role_list = all_role_list filterNot(_ == RoleEnum.ANGEL)
      if (room.has_flag(RoomFlagEnum.NO_BELLANDONA))
        all_role_list = all_role_list filterNot(_ == RoleEnum.BELLANDONA)
      if (room.has_flag(RoomFlagEnum.NO_CASSANDRA))
        all_role_list = all_role_list filterNot(_ == RoleEnum.CASSANDRA)
      if (room.has_flag(RoomFlagEnum.NO_DESPAIR))
        all_role_list = all_role_list filterNot(_ == RoleEnum.DESPAIR)
      if (room.has_flag(RoomFlagEnum.NO_ADECOY))
        all_role_list = all_role_list filterNot(_ == RoleEnum.ADECOY)
      if (room.has_flag(RoomFlagEnum.NO_BOMB))
        all_role_list = all_role_list filterNot(_ == RoleEnum.BOMB)
      if (room.has_flag(RoomFlagEnum.NO_CHESHIRE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.CHESHIRE)
      if (room.has_flag(RoomFlagEnum.NO_DETECTIVE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.DETECTIVE)
    }
    // 參考職業 BANE, STARS, FIGHTER, BOROGOVE, CLACKEN, ANIMALBONES,
    // MARS, LION, ARSIS, SHINAI, AICHA, AKI, AMETSUKI,
    // JUDGMENT, SHAHEART, HUNSOUL, MICAH, SETH, DRAGON, LEON, PUZZLE, ADRIATIC, WESTLOBE
    if (room.has_flag(RoomFlagEnum.CUSTOM_CAT_ROLE)) {
      all_role_list = all_role_list ++ RoleEnum.CUSTOM_CAT_ROLE_LIST
      if (room.has_flag(RoomFlagEnum.NO_BANE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.BANE)
      if (room.has_flag(RoomFlagEnum.NO_STARS))
        all_role_list = all_role_list filterNot(_ == RoleEnum.STARS)
      if (room.has_flag(RoomFlagEnum.NO_FIGHTER))
        all_role_list = all_role_list filterNot(_ == RoleEnum.FIGHTER)
      if (room.has_flag(RoomFlagEnum.NO_BOROGOVE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.BOROGOVE)
      if (room.has_flag(RoomFlagEnum.NO_CLACKEN))
        all_role_list = all_role_list filterNot(_ == RoleEnum.CLACKEN)
      if (room.has_flag(RoomFlagEnum.NO_ANIMALBONES))
        all_role_list = all_role_list filterNot(_ == RoleEnum.ANIMALBONES)
      if (room.has_flag(RoomFlagEnum.NO_CONCUBINE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.CONCUBINE)
      if (room.has_flag(RoomFlagEnum.NO_MOSSEN))
        all_role_list = all_role_list filterNot(_ == RoleEnum.MOSSEN)
      if (room.has_flag(RoomFlagEnum.NO_MAGICIAN))
        all_role_list = all_role_list filterNot(_ == RoleEnum.MAGICIAN)
      if (room.has_flag(RoomFlagEnum.NO_FALLOMEN))
        all_role_list = all_role_list filterNot(_ == RoleEnum.FALLOMEN)
      if (room.has_flag(RoomFlagEnum.NO_MARS))
        all_role_list = all_role_list filterNot(_ == RoleEnum.MARS)
      if (room.has_flag(RoomFlagEnum.NO_LION))
        all_role_list = all_role_list filterNot(_ == RoleEnum.LION)
      if (room.has_flag(RoomFlagEnum.NO_ARSIS))
        all_role_list = all_role_list filterNot(_ == RoleEnum.ARSIS)
      if (room.has_flag(RoomFlagEnum.NO_SHINAI))
        all_role_list = all_role_list filterNot(_ == RoleEnum.SHINAI)
      if (room.has_flag(RoomFlagEnum.NO_AICHA))
        all_role_list = all_role_list filterNot(_ == RoleEnum.AICHA)
      if (room.has_flag(RoomFlagEnum.NO_YAKALI))
        all_role_list = all_role_list filterNot(_ == RoleEnum.YAKALI)
      if (room.has_flag(RoomFlagEnum.NO_AKI))
        all_role_list = all_role_list filterNot(_ == RoleEnum.AKI)
      if (room.has_flag(RoomFlagEnum.NO_AMETSUKI))
        all_role_list = all_role_list filterNot(_ == RoleEnum.AMETSUKI)
      if (room.has_flag(RoomFlagEnum.NO_LILIA))
        all_role_list = all_role_list filterNot(_ == RoleEnum.LILIA)
      if (room.has_flag(RoomFlagEnum.NO_CLOUDBOW))
        all_role_list = all_role_list filterNot(_ == RoleEnum.CLOUDBOW)
      if (room.has_flag(RoomFlagEnum.NO_JUDGMENT))
        all_role_list = all_role_list filterNot(_ == RoleEnum.JUDGMENT)
      if ((room.has_flag(RoomFlagEnum.NO_SHAHEART)) || (room.has_flag(RoomFlagEnum.ALL_NEUTRAL)))
        all_role_list = all_role_list filterNot(_ == RoleEnum.SHAHEART)
      if ((room.has_flag(RoomFlagEnum.NO_HUNSOUL)) || (room.has_flag(RoomFlagEnum.ALL_NEUTRAL)))
        all_role_list = all_role_list filterNot(_ == RoleEnum.HUNSOUL)
      if (room.has_flag(RoomFlagEnum.NO_MICAH))
        all_role_list = all_role_list filterNot(_ == RoleEnum.MICAH)
      if ((room.has_flag(RoomFlagEnum.NO_SETH)) || (room.hasnt_flag(RoomFlagEnum.BLACKCARD_MASK)) || (room.hasnt_flag(RoomFlagEnum.BLACKCARD_DAGGER)))
        all_role_list = all_role_list filterNot(_ == RoleEnum.SETH)
      if (room.has_flag(RoomFlagEnum.NO_DRAGON))
        all_role_list = all_role_list filterNot(_ == RoleEnum.DRAGON)
      if ((room.has_flag(RoomFlagEnum.NO_LEON)) || (room.has_flag(RoomFlagEnum.ALL_NEUTRAL)))
        all_role_list = all_role_list filterNot(_ == RoleEnum.LEON)
      if (room.has_flag(RoomFlagEnum.NO_PUZZLE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.PUZZLE)
      if ((room.has_flag(RoomFlagEnum.NO_ADRIATIC)) || (userentrys.length < 7))
        all_role_list = all_role_list filterNot(_ == RoleEnum.ADRIATIC)
      if (room.has_flag(RoomFlagEnum.NO_WESTLOBE))
        all_role_list = all_role_list filterNot(_ == RoleEnum.WESTLOBE)
      if ( (room.has_flag(RoomFlagEnum.NO_LUBE))
        || (userentrys.length < 6)
        || ( (room.hasnt_flag(RoomFlagEnum.WHITECARD_EARTHQUAKE)) 
          && (room.hasnt_flag(RoomFlagEnum.WHITECARD_VOLCANIC))
          && (room.hasnt_flag(RoomFlagEnum.WHITECARD_TSUNAMI)) ) )
        all_role_list = all_role_list filterNot(_ == RoleEnum.LUBE)
      if (room.has_flag(RoomFlagEnum.NO_TEL))
        all_role_list = all_role_list filterNot(_ == RoleEnum.TEL)
    }
    
    if (all_role_list.length == 0) {
      all_role_list = all_role_list ++ RoleEnum.STANDARD_ROLE_LIST
    }
    
   
    /* var all_role_list = List(
      RoleEnum.VAMPIRE, RoleEnum.WEREWOLF, RoleEnum.EMI, RoleEnum.FRANKLIN, RoleEnum.GEORGE,
      RoleEnum.ALLIE, RoleEnum.BOB, RoleEnum.DANIEL, RoleEnum.ULTRASOUL, RoleEnum.VALKYRIE,
      RoleEnum.AGNES, RoleEnum.BRYAN, RoleEnum.CATHERINE, RoleEnum.ELLEN, RoleEnum.FUKA,
      RoleEnum.WIGHT, RoleEnum.GREGOR, RoleEnum.CHARLES, RoleEnum.UNSEEN, RoleEnum.VENGEFUL_GHOST,
      RoleEnum.GINGER, RoleEnum.UNKNOWN, RoleEnum.DAVID, RoleEnum.FATHER_OCONNEL, RoleEnum.WITCH,
      RoleEnum.ANGEL,  RoleEnum.BELLANDONA, RoleEnum.EVAN, RoleEnum.CASSANDRA, RoleEnum.DESPAIR,
      RoleEnum.BOMB, RoleEnum.EMMA, RoleEnum.FENG, RoleEnum.GODFAT,
          RoleEnum.UNDEAD, RoleEnum.VIPER, RoleEnum.WICKED, RoleEnum.ADECOY, RoleEnum.CHESHIRE,
        RoleEnum.DETECTIVE)
    */
    
    var java_shadow_list: java.util.List[RoleEnum.Value] = 
      ListBuffer(all_role_list.filter(RoleEnum.get_role(_).role_side == RoleSideEnum.SHADOW): _*)
    var java_hunter_list: java.util.List[RoleEnum.Value] = 
      ListBuffer(all_role_list.filter(RoleEnum.get_role(_).role_side == RoleSideEnum.HUNTER): _*)
    var java_neutral_list: java.util.List[RoleEnum.Value] = 
      ListBuffer(all_role_list.filter(RoleEnum.get_role(_).role_side == RoleSideEnum.NEUTRAL): _*)
      
    //println("all_role_list : " + all_role_list.size())
    //println("shadow_list : " + java_shadow_list.size())
    //println("hunter_list : " + java_hunter_list.size())
    //println("neutral_list : " + java_neutral_list.size())
    
    /*
    var shadow_onrolenum = java_shadow_list.size()
    var hunter_onrolenumt = java_hunter_list.size()
    var neutral_onrolenum = java_neutral_list.size()
    var all_onrolenum = (java_shadow_list.size() + java_hunter_list.size() + java_neutral_list.size())
    */
    
    /* val talk_debug1 = Talk.create.roomround_id(0).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message("all_role_list:" + (all_role_list.size().toString) + " "
                          + "java_shadow_list:" + (java_shadow_list.size().toString) + " "
                          + "java_hunter_list:" + (java_hunter_list.size().toString) + " "
                          + "java_neutral_list:" + (java_neutral_list.size().toString) + " " )
    talk_debug1.save
    talk_debug1.send(room.id.is) */
    
    if ((java_shadow_list.size() != 0) && (shadow_hunter_number != 0)) {
      var addback_shadow : Int = math.ceil(shadow_hunter_number / java_shadow_list.size()).toInt
      /* val talk_s = Talk.create.roomround_id(0).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                        .message("addback_shadow:" + (addback_shadow.toString) + " " )
      talk_s.save
      talk_s.send(room.id.is) */
      for (i <- 1 to addback_shadow) {
        java_shadow_list.addAll(java_shadow_list)
        /* val talk_debug2 = Talk.create.roomround_id(0).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message("for java_shadow_list:" + (java_shadow_list.size().toString) + " " )
        talk_debug2.save
        talk_debug2.send(room.id.is) */
      }
    }
    
    if ((java_hunter_list.size() != 0) && (shadow_hunter_number != 0)) {
      var addback_hunter : Int = math.ceil(shadow_hunter_number / java_hunter_list.size()).toInt
      /* val talk_h = Talk.create.roomround_id(0).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                        .message("addback_hunter:" + (addback_hunter.toString) + " " )
      talk_h.save
      talk_h.send(room.id.is) */
      for (i <- 1 to addback_hunter) {
        java_hunter_list.addAll(java_hunter_list)
        /* val talk_debug3 = Talk.create.roomround_id(0).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message("for java_hunter_list:" + (java_hunter_list.size().toString) + " " )
        talk_debug3.save
        talk_debug3.send(room.id.is) */
      }
    }
    
    if ((java_neutral_list.size() != 0) && (neutral_number != 0)) {
      var addback_neutral : Int = math.ceil(neutral_number / java_neutral_list.size()).toInt
      /* val talk_n = Talk.create.roomround_id(0).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                        .message("addback_neutral:" + (addback_neutral.toString) + " " )
      talk_n.save
      talk_n.send(room.id.is) */
      for (i <- 1 to addback_neutral) {
        java_neutral_list = java_neutral_list ++ java_neutral_list
        /* val talk_debug4 = Talk.create.roomround_id(0).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message("for java_neutral_list:" + (java_neutral_list.size().toString) + " " )
        talk_debug4.save
        talk_debug4.send(room.id.is) */
      }
    }
    
    java.util.Collections.shuffle(java_shadow_list)
    java.util.Collections.shuffle(java_hunter_list)
    java.util.Collections.shuffle(java_neutral_list)
    
    java_shadow_list = java_shadow_list.take(shadow_hunter_number)
    java_hunter_list = java_hunter_list.take(shadow_hunter_number)
    java_neutral_list = java_neutral_list.take(neutral_number)
    
    /* val talk_debug5 = Talk.create.roomround_id(0).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message("all_role_list:" + (all_role_list.size().toString) + " "
                          + "take java_shadow_list:" + (java_shadow_list.size().toString) + " "
                          + "take java_hunter_list:" + (java_hunter_list.size().toString) + " "
                          + "take java_neutral_list:" + (java_neutral_list.size().toString) + " " )
    talk_debug5.save
    talk_debug5.send(room.id.is) */
    
    
    // 設定玩家優先順位
    var java_user_no_array : java.util.LinkedList[Int] = new java.util.LinkedList()
    for (i <- 1 to userentrys.length)
      java_user_no_array.add(i)
      
    java.util.Collections.shuffle(java_user_no_array)
    
    userentrys.foreach { userentry =>
      userentry.user_no(java_user_no_array.removeFirst()).room_flags(userentry.role.is).role("")
      //println(user_entry.user_no.is + " " + user_entry.user_flags.is)
    }

    val userentrys_ordered = userentrys.sortBy(_.user_no.is)

    // 第一次先看看有沒有希望陣營
    userentrys_ordered.foreach { userentry =>
      val role  = try { RoleEnum.withName(userentry.role.is) }
        catch { case e: Exception => RoleEnum.NONE }
      val align = try { RoleSideEnum.withName(userentry.subrole.is) }
        catch { case e: Exception => RoleSideEnum.NONE }
      if (role != RoleEnum.NONE) {  
        val align_array = RoleEnum.get_role(role).role_side match {
          case RoleSideEnum.SHADOW => java_shadow_list
          case RoleSideEnum.HUNTER => java_hunter_list
          case RoleSideEnum.NEUTRAL => java_neutral_list
        }
        if (align_array.contains(role)) {
          userentry.role(role.toString)
          align_array.remove(role)
        }
      } else if (align != RoleSideEnum.NONE) {
        val align_array = align match {
          case RoleSideEnum.SHADOW => java_shadow_list
          case RoleSideEnum.HUNTER => java_hunter_list
          case RoleSideEnum.NEUTRAL => java_neutral_list
        }
        if (align_array.size() != 0) {
          userentry.role(align_array.get(0).toString)
          align_array.remove(0)
        }
      }
    }

    // 然後設定剩下的職業
    var java_role_array : java.util.LinkedList[RoleEnum.Value] = new java.util.LinkedList()
    java_role_array.addAll(java_shadow_list)
    java_role_array.addAll(java_hunter_list)
    java_role_array.addAll(java_neutral_list)
    
    java.util.Collections.shuffle(java_role_array)
    
    /*
    userentrys_ordered.foreach { userentry =>
      if (userentry.role.is == "") {
        userentry.role(java_role_array.removeFirst().toString)
      }
    }
    */

    userentrys_ordered.foreach { userentry =>
      if (userentry.role.is == "") {
        val haterole  = try { RoleEnum.withName(userentry.haterole.is) }
                        catch { case e: Exception => RoleEnum.NONE }
        if ((haterole != RoleEnum.NONE) && (java_role_array.size() != 0)) {
          if (java_role_array.contains(haterole)) {
            //userentry.role(role.toString)
            var hateroleof = java_role_array.indexOf(haterole)
            java_role_array.addLast(java_role_array.get(hateroleof))
            java_role_array.remove(java_role_array.get(hateroleof))
          }
        }
        userentry.role(java_role_array.removeFirst().toString)
      }
    }
    
    if (room.has_flag(RoomFlagEnum.INIT_LOCATION)) {
      userentrys_ordered.foreach { userentry =>
        val index = random.nextInt(LocationEnum.LOCATION_LIST.length)
        userentry.location(LocationEnum.LOCATION_LIST(index).toString)
      }
    }
    
    //java_user_no_array : java.util.LinkedList[Int] = new java.util.LinkedList()
    for (i <- 1 to userentrys.length)
      java_user_no_array.add(i)
    
    if (room.has_flag(RoomFlagEnum.RANDOM_POSITION))
      java.util.Collections.shuffle(java_user_no_array)
    
    userentrys.foreach { userentry =>
      userentry.user_no(java_user_no_array.removeFirst()).room_flags("").damaged(0).subrole("")
      if (userentry.role.is != RoleEnum.UNKNOWN.toString)
        userentry.role_flags("")
      //println(user_entry.user_no.is + " " + user_entry.user_flags.is)
      userentry.save
    }
    //龍珠分配
    val dragons = userentrys.filter (x => x.get_role == RoleDragon)
    if (dragons.length > 0) {
      for (i <- 1 to 6) {
        val nobeads_users = userentrys.filter (x => (x.beads.is <= 2))
        val random_no = random.nextInt(nobeads_users.length)
        nobeads_users(random_no).beads(nobeads_users(random_no).beads.is + 1)
        nobeads_users(random_no).save
      }
      if (random.nextInt(1) > 0) {
        val nobeads_users = userentrys.filter (x => (x.beads.is <= 2))
        val random_no = random.nextInt(nobeads_users.length)
        nobeads_users(random_no).beads(nobeads_users(random_no).beads.is + 1)
        nobeads_users(random_no).save
        
      } else {
        val yesbeads_users = userentrys.filter (x => (x.beads.is > 0))
        val random_no = random.nextInt(yesbeads_users.length)
        yesbeads_users(random_no).beads(yesbeads_users(random_no).beads.is + 1)
        yesbeads_users(random_no).save
      }
    }
  }
  
  def process_start_game(room : Room) = {
    // val room = Room.find(By(Room.id, room_id)).get
    var userentrys_rr = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                        By(UserEntry.revoked, false))
    
    dispatch_role(room, userentrys_rr)
    
    // 產生亂數地形
    val location_list = new java.util.LinkedList[LocationEnum.Value]()
    LocationEnum.LOCATION_LIST.foreach { location =>
      location_list.add(location)
    }
    java.util.Collections.shuffle(location_list)
    room.room_arrange(location_list.toList.map(_.toString).mkString(""))
    
    if (room.has_flag(RoomFlagEnum.RANDOM_POSITION))
      userentrys_rr = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                        By(UserEntry.revoked, false),
                                        OrderBy(UserEntry.user_no, Ascending))
    
    // 加入第一回合
    val new_round = RoomRound.create.room_id(room.id.is).round_no(1)
    new_round.save
    
    val new_phase = RoomPhase.create.roomround_id(new_round.id.is)
                    .phase_type(RoomPhaseEnum.MOVEMENT.toString)
                    .player(userentrys_rr(0).id.is)
                    .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.move_time))
    new_phase.save

    // 產生人數字串
    val align_text = new StringBuffer("陣營分布：")
    align_text.append("　暗影：")
    align_text.append(userentrys_rr.filter(x => RoleEnum.get_role(x.role.is).role_side == RoleSideEnum.SHADOW).length)
    align_text.append("　獵人：")
    align_text.append(userentrys_rr.filter(x => RoleEnum.get_role(x.role.is).role_side == RoleSideEnum.HUNTER).length)
    align_text.append("　中立：")
    align_text.append(userentrys_rr.filter(x => RoleEnum.get_role(x.role.is).role_side == RoleSideEnum.NEUTRAL).length)

    val talk = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message("第 " + (new_round.round_no.is.toString) + " 回合 "+ (new java.util.Date).toString)
    talk.save
    
    val talk2 = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message(align_text.toString).cssclass("normal")
    talk2.save
    //RoomActor ! NewMessageNoSend(talk)
    
    if (room.has_flag(RoomFlagEnum.INIT_GREEN)) {
      userentrys_rr.foreach { userentry =>
        val index = userentrys_rr.indexOf(userentry)
        val prev_index = (index + userentrys_rr.length - 1) % (userentrys_rr.length)
        val targetuserentry = userentrys_rr(prev_index)
        
        val card = draw_card(room, CardTypeEnum.GREEN)
        
        val talk3 = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.ACTION_GREENCARD.toString)
                       .actioner_id(userentry.id.is).actionee_id(targetuserentry.id.is).message_flags(card.card.is.toString)
        talk3.save
        
        val talk4 = CardHelper.process_green_internal(userentry, targetuserentry, CardEnum.get_card(card.card.is.toString), userentrys_rr)
        talk4.roomround_id(new_round.id.is).save
        
        targetuserentry.save
      }
    }
    RoomActor.sendUserEntryMessage(userentrys_rr(0).id.is, UserEntryForceUpdate(userentrys_rr(0).id.is ,List(ForceUpdateEnum.MUSIC)))
    
    room.status(RoomStatusEnum.PLAYING.toString)
    room.save
    
    //RoomActor ! NewRoomRound(room_i, new_round)
  }
  //死亡處理
  def check_death(actionee : UserEntry, actioner : UserEntry, action : Action, userentrys : List[UserEntry]) : Boolean = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is), OrderBy(RoomPhase.phase_no, Descending)).get
    //val roomphase = RoomPhase_R.get
    val role = actionee.get_role
    val skill_role = actionee.get_skill_role
    //val actioner = userentrys.filter(_.id.is == action.actioner_id.is)(0)
    var actioner_role = actioner.get_role
    var actioner_skill_role = actioner.get_skill_role
    
    val is_dead =
      if ((skill_role == RoleUnknown) && (!actionee.revealed.is) && (actionee.damaged.is < 14) &&
          (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK)))
        false
      else if (actionee.damaged.is >= role.life)
        if ((skill_role == RoleAnimalBones) && (actionee.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED))) {
          actionee.damaged(0)
          actionee.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
          actionee.save
          val talk1 = Talk.create.roomround_id(action.roomround_id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                            .message(actionee.handle_name.is + " 復活，且損傷設定為 0 (獸骸)")
          talk1.save
          talk1.send(actioner.room_id.is)
          if (!actionee.revealed.is)
            GameProcessor.flip(actionee, action, userentrys)
          false
        } else if (actionee.has_user_flag(UserEntryFlagEnum.REVIVE)) {
          actionee.damaged(0)
          actionee.remove_user_flag(UserEntryFlagEnum.REVIVE)
          actionee.save
          val talk1 = Talk.create.roomround_id(action.roomround_id.is).mtype(MTypeEnum.RESULT_DRAGON.toString)
                            .message(actionee.handle_name.is + " 復活，且損傷設定為 0 (許願)")
          talk1.save
          talk1.send(actioner.room_id.is)
          if (!actionee.revealed.is)
            GameProcessor.flip(actionee, action, userentrys)
          false
        } else true
      else false
      
    if (is_dead) {
      val actionee_equips = actionee.items
      if (actionee_equips.length > 0) {
        if (actioner == actionee) {
          // 剩下的裝備丟至墓地
          actionee.items.foreach { actionee_item =>
            val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                     By(CardPool.card, actionee_item.card_enum.toString)).get
            card.owner_id(0).discarded(true).save
          }
      
          actionee.item_flags("").save
        } else {
          rob(room, roomround, actioner, actionee)
        //check_item_victory(room, roomround, actioner)
        }
      }
      val userentrys_r  = userentrys.filter(x => (!x.revoked.is))
      if (actioner_skill_role == RoleAdriatic) {
        val index_r = userentrys_r.indexOf(actioner)
        val index_e = userentrys_r.indexOf(actionee)
        val equiv_index1 = (index_r+1) % (userentrys_r.length)
        val equiv_index2 = (index_r + userentrys_r.length - 1) % (userentrys_r.length)
            
        if ((index_e == equiv_index1) || (index_e == equiv_index2)) {
          actioner.action_point(actioner.action_point.is + 1)
        }
      }
      // <!--[波姆勝利判定]-->
      if (actioner_role == RoleBomb) {
        actioner.action_point(actioner.action_point.is + 1)
      }
      
      val dead_userentrys = userentrys.filter (x => (!x.revoked.is) && (!x.live.is) && (x.get_role != RoleAngel))
      if ((skill_role == RoleAngel) && (dead_userentrys.length != 0)) {
        val target_player_index = 
          dead_userentrys.map(_.id.is).indexOf(actionee.target_user.is)
        val dead_useretrys_index = 
          if (target_player_index == -1)
            random.nextInt(dead_userentrys.length)
          else
            target_player_index
          
        val dead_userentry = dead_userentrys(dead_useretrys_index)
        
        actionee.role(dead_userentry.role.is.substring(0,2) + actionee.role.is)
        actionee.target_user(dead_userentry.target_user.is) // role_flags(dead_userentry.role_flags.is).
        actionee.damaged(7)
        
        if (!actionee.revealed.is)
          GameProcessor.flip(actionee, action, userentrys)
      } else {
        if ((actioner_skill_role == RoleBellandona) && (actioner != actionee)) {
          actioner.role(actionee.role.is.substring(0,2) + actioner.role.is)
          actioner.target_user(actionee.target_user.is) // role_flags(actionee.role_flags.is)
          actioner_role = actionee.get_role
        
          val actionee_role = actionee.get_role
          if (actioner.damaged.is >= actionee_role.role_life)
            actioner.damaged(actionee_role.role_life - 1)

          //GameProcessor.check_death(actioner, actioner, action, userentrys)
          actioner.save
        }
        
        if ((role == RoleDaniel) || (role == RoleCatherine)) {
          val userentrys_dead = userentrys.filter(x => (!x.revoked.is) && (!x.live.is) && (x.hasnt_user_flag(UserEntryFlagEnum.REVIVED)))
          if (userentrys_dead.length == 0) {
            actionee.add_user_flag(UserEntryFlagEnum.VICTORY) 
          }
        } else if ((role == RoleSeth) && (actionee.action_point.is == 0) && (actionee.location.is == LocationEnum.ERSTWHILE_ALTER.toString) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SLOW))) {
            actionee.add_user_flag(UserEntryFlagEnum.VICTORY) 
        } else if (role == RoleDespair) {
          userentrys.foreach { userentry =>
            check_item_victory(room, roomround, userentry)
            if (userentry.live.is && userentry.has_user_flag(UserEntryFlagEnum.VICTORY2))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY)
          }
        } else if (skill_role == RoleBomb) {
          val death_number = userentrys.filter(x => !x.live.is).length
          userentrys.foreach { userentry1 =>
            if ((userentry1.location.is == actionee.location.is) &&
                (userentry1.id.is != actionee.id.is) &&
                (userentry1.get_role != RoleBomb)) {
              val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                                 .mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(userentry1.handle_name.is + " 的損傷增加 3 點(波姆)")
              talk1.save
              talk1.send(actioner.room_id.is)
              if ((userentry1.get_skill_role == RoleLion) && (userentry1.revealed) && (userentry1.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (userentry1.hasnt_item(CardEnum.B_MASK))) {
                val talk_l = Talk.create.roomround_id(action.roomround_id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(actioner.id.is).actionee_id(userentry1.id.is)
                talk_l.save
                talk_l.send(userentry1.room_id.is)
              }
              if (userentry1.inflict_damage(3, actionee))
              GameProcessor.check_death(userentry1, actioner, action, userentrys)
            }
          }
          val death_number2 = userentrys.filter(x => !x.live.is).length
          if ((death_number2 - death_number >= 1) && (role == RoleBomb))
            actionee.add_user_flag(UserEntryFlagEnum.VICTORY)
        } else if (skill_role == RoleHunsoul) {
          // 多堤
          val live_hunter_rs = userentrys.filter(x => (x.live.is) && (x.get_role.role_side == RoleSideEnum.HUNTER) && (x.revealed.is))
          if (live_hunter_rs.length > 0) {
            val random_hunter = random.nextInt(live_hunter_rs.length)
            val saved_damaged = live_hunter_rs(random_hunter).damaged.is
            //live_hunter_rs(random_hunter).damaged(99)
            live_hunter_rs(random_hunter).inflict_damage(99, actionee)
            GameProcessor.check_death(live_hunter_rs(random_hunter), actioner, action, userentrys)
            //live_hunter_rs(random_hunter).damaged(saved_damaged).save
          
            val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is).actionee_id(live_hunter_rs(random_hunter).id.is)
                               .mtype(MTypeEnum.ACTION_HUNSOULACK.toString)
            talk.save
            talk.send(live_hunter_rs(random_hunter).room_id.is)
          }
        } else if (skill_role == RoleShaHeart) {
          // 朵伊
          val live_shadow_rs = userentrys.filter(x => (x.live.is) && (x.get_role.role_side == RoleSideEnum.SHADOW) && (x.revealed.is))
          live_shadow_rs.foreach { live_shadow_r =>
            val heal1d6 = GameProcessor.random.nextInt(6) + 1
            live_shadow_r.inflict_damage(heal1d6, actionee)
            GameProcessor.check_death(live_shadow_r, actioner, action, userentrys)

            val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is).actionee_id(live_shadow_r.id.is)
                                 .mtype(MTypeEnum.ACTION_SHAHEARTACK.toString).message_flags(heal1d6.toString)
            talk.save
            talk.send(live_shadow_r.room_id.is)
          }
        }
        // 布萊恩
        if (actioner_role == RoleBryan) {
          if (role.role_life >= 13) {
            val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                                 .mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(actioner.handle_name.is + " 達成主動勝利條件(布萊恩)")
            talk1.save
            talk1.send(actioner.room_id.is)
            actioner.add_user_flag(UserEntryFlagEnum.VICTORY) 
            actioner.save
          } else if ((!actioner.revealed) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
            val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                                 .mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(actioner.handle_name.is + " 錯殺(布萊恩)")
            talk1.save
            talk1.send(actioner.room_id.is)
            flip(actioner, action, userentrys)
          }
        } else if (actioner_role == RoleCharles) {
          // 查理斯
          val userentrys_length = userentrys.filter(x => !x.revoked.is).length
          val dead_char_length =
            if (userentrys_length <=5)
              1
            else if (userentrys_length >=9)
              3
            else
              2
          
          val userentrys_dead = userentrys.filter(x => (!x.revoked.is) && (!x.live.is))
          if (userentrys_dead.length >= dead_char_length) {
            val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                                 .mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(actioner.handle_name.is + " 達成主動勝利條件(查理斯)")
            talk1.save
            talk1.send(actioner.room_id.is)
            actioner.add_user_flag(UserEntryFlagEnum.VICTORY) 
          }
        }
        // 丹尼爾
        val live_daniels = userentrys.filter(x => (x.id.is != actionee.id.is) &&
          (!x.revoked.is) && (x.live.is) && (x.get_skill_role == RoleDaniel) && (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (x.hasnt_item(CardEnum.B_MASK) && (!x.revealed.is)))
        live_daniels.foreach { live_daniel =>
          val action1 = Action.create.roomround_id(action.roomround_id.is).actioner_id(live_daniel.id.is)
                              .mtype(MTypeEnum.ACTION_FLIP.toString)
          action1.save
          val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(live_daniel.id.is)
                                 .mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(live_daniel.handle_name.is + " 尖叫(丹尼爾)")
          talk1.save
          talk1.send(live_daniel.room_id.is)
          val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(live_daniel.id.is)
                                 .mtype(MTypeEnum.ACTION_FLIP.toString)
          talk.save
          talk.send(live_daniel.room_id.is)
        
          if (live_daniel.damaged.is < 5)
            live_daniel.damaged(5)
          
          live_daniel.revealed(true)
          live_daniel.save
        }
        // 死亡處理
        actionee.live(false).location("")
        if (!actionee.revealed.is)
          GameProcessor.flip(actionee, action, userentrys)
        // 死亡系統訊息
        val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actionee.id.is)
                               .mtype(MTypeEnum.MESSAGE_DEATH.toString)
        talk1.save
        talk1.send(actionee.room_id.is)
        // 柴郡貓追隨中立陣營主人自殺
        val live_cheshires = userentrys.filter(x => (x.get_role == RoleCheshire) && (x.live.is) &&
                                                 (x.target_user.is == actionee.id.is))
        if (live_cheshires.length > 0) {
            live_cheshires.foreach { live_cheshire =>
              if (role.role_side == RoleSideEnum.NEUTRAL) {
                val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(live_cheshire.id.is)
                                 .mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(live_cheshire.handle_name.is + " 的損傷增加 99 點(柴郡貓)")
                talk.save
                talk.send(live_cheshire.room_id.is)
                val saved_damaged = live_cheshire.damaged.is
                //live_cheshire.damaged(99)
                live_cheshire.inflict_damage(99, actionee)
                GameProcessor.check_death(live_cheshire, live_cheshire, action, userentrys)
                //live_cheshire.damaged(saved_damaged).save
              } else if (!live_cheshire.revealed.is)
                GameProcessor.flip(live_cheshire, action, userentrys)
            }
        }
        // 莉可勝利無望
        val live_unrevealed = userentrys.filter(x => (!x.revoked.is) && (x.get_role != RoleDetective) && (x.live.is) && (!x.revealed.is))
        if (live_unrevealed.length == 0) {
          val live_detectives = userentrys.filter(x =>(x.get_role == RoleDetective) && (x.live.is))
          live_detectives.foreach { live_detective =>
            val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(live_detective.id.is)
                                 .mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(live_detective.handle_name.is + " 的損傷增加 99 點(莉可)")
            talk.save
            talk.send(live_detective.room_id.is)
            val saved_damaged = live_detective.damaged.is
            //live_detective.damaged(99)
            live_detective.inflict_damage(99, live_detective)
            GameProcessor.check_death(live_detective, live_detective, action, userentrys)
            //live_detective.damaged(saved_damaged).save
          }
        }
        // 審判獲勝判定
        val live_unrevealed2 = userentrys.filter(x => (x.live.is) && (!x.revealed.is) && (!x.revoked.is))
        if (live_unrevealed2.length == 0) {
          val live_judgments = userentrys.filter(x =>(x.get_role == RoleJudgment) && (x.live.is))
          live_judgments.foreach { live_judgment =>
            live_judgment.add_user_flag(UserEntryFlagEnum.VICTORY)
            live_judgment.save
          }
        }

      }
      
      // 龍珠移動
      if (actioner == actionee) {
        val live_userentrys = userentrys.filter (x => (!x.revoked.is) && (x.live.is) && (x.id.is != actionee.id.is))
        val random_no = random.nextInt(live_userentrys.length)
        live_userentrys(random_no).beads(live_userentrys(random_no).beads.is + actionee.beads.is).save
        actionee.beads(0).save
      } else {
        actioner.beads(actioner.beads.is + actionee.beads.is).save
        actionee.beads(0).save
      }
      
    }  
    actionee.save
    
    !actionee.live.is
  }
  
  def rob_single(room: Room, roomround: RoomRound, actioner: UserEntry, actionee: UserEntry) = {
    // 注意: actionee 須手動 save
    val actioner_role = actioner.get_skill_role
    val actionee_equips = actionee.items
    
    val equip_index = random.nextInt(actionee_equips.length)
    val equip =
      if (actioner_role == RoleDavid) {
        val david_equips1 = actionee_equips.filter( x=> RoleDavid.WIN_EQUIPMENT_LIST.contains(x))
        val david_equips2 = actionee_equips.filter( x=> RoleDavid.WIN_EQUIPMENT_LIST2.contains(x))
        if (david_equips1.length != 0)
          david_equips1(0)
        else if (david_equips2.length != 0)
          david_equips2(0)
        else
          actionee_equips(equip_index)
      }  
      else actionee_equips(equip_index)

    actioner.add_item(equip.card_enum)
    actionee.remove_item(equip.card_enum)
    
    // 從 CardPool 修改 owner 資訊
    val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                             By(CardPool.card, equip.card_enum.toString)).get
    card.owner_id(actioner.id.is).discarded(false).save
    
    check_item_victory(room, roomround, actioner)
    
    equip
  }
  
  def rob_specific(room: Room, roomround: RoomRound, actioner: UserEntry, actionee: UserEntry, item : Card) = {
    // 注意: actionee 須手動 save

    actioner.add_item(item.card_enum)
    actionee.remove_item(item.card_enum)
    
    
    // 從 CardPool 修改 owner 資訊
    val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                             By(CardPool.card, item.card_enum.toString)).get
    card.owner_id(actioner.id.is).discarded(false).save
    
    check_item_victory(room, roomround, actioner)
    
    item
  }
  
  def rob(room: Room, roomround: RoomRound, actioner: UserEntry, actionee: UserEntry) {
    // 注意: actionee 須手動 save
    val actioner_role = actioner.get_skill_role
    val actionee_equips = actionee.items
    
    if (!actioner.live.is) {
    } else if (((actioner_role == RoleBob) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) ||
        (actioner.has_item(CardEnum.W_SILVER_ROSARY))) {

      actioner.item_flags(actioner.item_flags.is.toString + actionee.item_flags.is.toString).save
      
      // 從 CardPool 修改 owner 資訊
      actionee.items.foreach { actionee_item =>
        val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                 By(CardPool.card, actionee_item.card_enum.toString)).get
        card.owner_id(actioner.id.is).save
      }
      actionee.item_flags("").save
      check_item_victory(room, roomround, actioner)
    } else {
      rob_single(room, roomround, actioner, actionee)
      
      if (actionee.items.length > 0) {
        // 剩下的裝備丟至墓地
        actionee.items.foreach { actionee_item =>
          val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                   By(CardPool.card, actionee_item.card_enum.toString)).get
          card.owner_id(0).discarded(true).save
        }
      }
      actionee.item_flags("").save
    }
      
  }
  
  def check_item_victory(room: Room, roomround: RoomRound, actioner : UserEntry) = {
    var result = false
    val actioner_role = actioner.get_skill_role
    if (actioner_role == RoleBob) {
      val actioner_equips = actioner.items
      if ((actioner_equips.length >= 5) ||
          ((actioner_equips.length >= 4) && (!actioner.revealed.is))) {
        val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).actioner_id(actioner.id.is)
                                .message(actioner.handle_name.is + " 達成主動勝利條件(鮑伯)")
        talk_a.save
        talk_a.send(actioner.room_id.is)
        actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save 
        result = true
      }
    } else if (actioner_role == RoleDavid) {
      val win_items = actioner.items.filter( x=> RoleDavid.WIN_EQUIPMENT_LIST.contains(x))
      if (win_items.length >= 3) {
        val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).actioner_id(actioner.id.is)
                                .message(actioner.handle_name.is + " 達成主動勝利條件(大衛)")
        talk_a.save
        talk_a.send(actioner.room_id.is)
        actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save 
        result = true
      } else {
        val win_items2 = actioner.items.filter( x=> RoleDavid.WIN_EQUIPMENT_LIST2.contains(x))
        if (win_items2.length >= 4) {
          val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).actioner_id(actioner.id.is)
                                .message(actioner.handle_name.is + " 達成主動勝利條件(大衛)")
          talk_a.save
          talk_a.send(actioner.room_id.is)
          actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save 
          result = true
        }
      }
    }
    result
  }
  
  
  def check_victory(room: Room, roomround: RoomRound, userentrys : List[UserEntry]) : Boolean = {
    if (room.status.is == RoomStatusEnum.ENDED)
      return true
      
    val userentrys_r  = userentrys.filter(x => (!x.revoked.is))
    val userentrys_rl = userentrys_r.filter(x => (x.live.is))
    val userentrys_rd = userentrys_r.filter(x => (!x.live.is))
    
    val live_despairs = userentrys_rl.filter(x => (x.get_role == RoleDespair) && (x.revealed.is))
    
    // 龍珠移動
    userentrys_rd.foreach { userentry_rd =>
      if (userentry_rd.beads.is > 0) {
          val random_no = random.nextInt(userentrys_rl.length)
          userentrys_rl(random_no).beads(userentrys_rl(random_no).beads.is + userentry_rd.beads.is).save
          userentry_rd.beads(0).save
      }
    }
    
    // 神龍 勝利判定
    val live_dragons = userentrys.filter (x => (x.get_role == RoleDragon) && (x.live.is) && (x.beads.is >= 7) && (x.hasnt_user_flag(UserEntryFlagEnum.VICTORY)))
    if ((live_dragons.length > 0) && (live_despairs.length == 0)) {
        live_dragons.foreach { live_dragon =>
        live_dragon.add_user_flag(UserEntryFlagEnum.VICTORY)
        live_dragon.save
        val talk_1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_DRAGON.toString).actioner_id(live_dragon.id.is)
                                .message(live_dragon.handle_name.is + " 達成主動勝利條件(神龍)")
        talk_1.save
        talk_1.send(live_dragon.room_id.is)
      }
    }
    // 夏彌加 勝利判定
    val cardpools = CardPool.findAll(By(CardPool.room_id, room.id.is), OrderBy(CardPool.card_no, Ascending))
    val room_users = userentrys.filter (x => (!x.revoked.is))
    val live_micahs = userentrys.filter (x => (x.get_role == RoleMicah) && (x.live.is) && (x.revealed.is) && (x.hasnt_user_flag(UserEntryFlagEnum.VICTORY)))
    val black_discards = cardpools.filter(x => (x.discarded.is == true) && (x.card_type.is == CardTypeEnum.BLACK.toString))
    val white_discards = cardpools.filter(x => (x.discarded.is == true) && (x.card_type.is == CardTypeEnum.WHITE.toString))
    if(((black_discards.length + white_discards.length) > (room_users.length * 2.5)) && (live_micahs.length > 0) && (live_despairs.length == 0)){
        live_micahs.foreach { live_micah =>
        live_micah.add_user_flag(UserEntryFlagEnum.VICTORY)
        live_micah.save
        val talk_1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).actioner_id(live_micah.id.is)
                                .message("黑白棄牌合計 大於 玩家人數 2.5 倍： (" + (black_discards.length + white_discards.length) + " > " + (room_users.length * 2.5) + ")")
        talk_1.save
        talk_1.send(live_micah.room_id.is)
        
        val talk_2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).actioner_id(live_micah.id.is)
                                .message(live_micah.handle_name.is + " 達成主動勝利條件(夏彌加)")
        talk_2.save
        talk_2.send(live_micah.room_id.is)
      }
    }
    // 塞特 勝利判定
    val live_seths = userentrys.filter (x => (x.get_role == RoleSeth) && (x.live.is) && (x.hasnt_user_flag(UserEntryFlagEnum.VICTORY)) &&
                                             (x.has_item(CardEnum.B_EVILSWORD)) && (x.has_item(CardEnum.W_LANCE_OF_LONGINUS)) && (x.action_point.is == 0))
    if ((live_seths.length > 0) && (live_despairs.length == 0)) {
      live_seths.foreach { live_seth =>
        live_seth.add_user_flag(UserEntryFlagEnum.VICTORY)
        live_seth.save
        val talk_1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).actioner_id(live_seth.id.is)
                         .message(live_seth.handle_name.is + " 達成主動勝利條件(塞特)")
        talk_1.save
        talk_1.send(live_seth.room_id.is)
      }
    }
    // 亞德利斯 勝利判定
    val live_Adriatics = userentrys.filter (x => (x.get_role == RoleAdriatic) && (x.live.is) && (x.hasnt_user_flag(UserEntryFlagEnum.VICTORY)))
    if ((live_Adriatics.length > 0) && (live_despairs.length == 0)) {
      live_Adriatics.foreach { live_Adriatic =>
        if (live_Adriatic.action_point.is >= 2) {
          live_Adriatic.add_user_flag(UserEntryFlagEnum.VICTORY)
          live_Adriatic.save
          val talk_1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).actioner_id(live_Adriatic.id.is)
                           .message(live_Adriatic.handle_name.is + " 達成主動勝利條件(亞德利斯)")
          talk_1.save
          talk_1.send(live_Adriatic.room_id.is)
        }
      }
    }
    // 波姆
    val live_Bombs = userentrys.filter (x => (x.get_role == RoleBomb) && (x.live.is) && (x.hasnt_user_flag(UserEntryFlagEnum.VICTORY)))
    if ((live_Bombs.length > 0) && (live_despairs.length == 0)) {
      live_Bombs.foreach { live_Bomb =>
        if (live_Bomb.action_point.is >= 2) {
          live_Bomb.add_user_flag(UserEntryFlagEnum.VICTORY)
          live_Bomb.save
          val talk_1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).actioner_id(live_Bomb.id.is)
                           .message(live_Bomb.handle_name.is + " 達成主動勝利條件(波姆)")
          talk_1.save
          talk_1.send(live_Bomb.room_id.is)
        }
      }
    }
    // 檢查遊戲是否結束
    val userentrys_r_shadow = userentrys_rl.filter(x => (x.get_role.role_side == RoleSideEnum.SHADOW))
    val userentrys_r_hunter = userentrys_rl.filter(x => (x.get_role.role_side == RoleSideEnum.HUNTER))
    val userentrys_d_neutral = userentrys_r.filter(x =>  (!x.live.is) && (x.get_role.role_side == RoleSideEnum.NEUTRAL))
    val userentrys_l         = userentrys_rl.filter(x => x.has_user_flag(UserEntryFlagEnum.LOVER))
    val userentrys_v = userentrys_r.filter(x => (x.has_user_flag(UserEntryFlagEnum.VICTORY)))
    
    val is_shadow_victory_nd =
      if (room.has_flag(RoomFlagEnum.FOUR_NEUTRAL) && (userentrys_r.length == 10))
        (userentrys_d_neutral.length >= 4)
      else
        (userentrys_d_neutral.length >= 3)
        
    var is_hunter_victory = (userentrys_r_shadow.length == 0)
    var is_shadow_victory = (userentrys_r_hunter.length == 0) || is_shadow_victory_nd
    var is_neutral_victory = (userentrys_v.length > 0)
    var is_lover_victory = (userentrys_l.length >= 2) && 
      (userentrys_rl.filter(x => ((x.get_role.role_side == RoleSideEnum.SHADOW) ||
                                  (x.get_role.role_side == RoleSideEnum.HUNTER))).length == userentrys_l.length)
    var is_draw_victory = false
    
    var nowin_neutrals = 0
    // 全中立模式，排除暗影獵人勝利
    if (room.has_flag(RoomFlagEnum.ALL_NEUTRAL)) {
      is_hunter_victory = false
      is_shadow_victory = false
      is_lover_victory  = false
      if (userentrys_rl.length == 0) {
        is_neutral_victory = true
      } else {
        
        // 判斷剩餘的角色是否有能力結束遊戲
        userentrys_rl.foreach { l_userentry =>
          if (l_userentry.get_role == RoleBob) {
          } else if (l_userentry.get_role == RoleCharles) {
            if (userentrys_rl.length <= 1)
              nowin_neutrals += 1
          } else if (l_userentry.get_role == RoleDavid) {
            // 大衛應該也沒問題
          } else if (l_userentry.get_role == RoleBomb) {
            if (userentrys_rl.length <= 1)
              nowin_neutrals += 1
          } else if (l_userentry.get_role == RoleAngel) {
            if (userentrys_rl.length <= 1)
              nowin_neutrals += 1
          } else if (l_userentry.get_role == RoleADecoy) {
            if (userentrys_rl.length <= 1)
              nowin_neutrals += 1
          } else if (l_userentry.get_role == RoleDespair) {
            // 絕望會另外處理
          } else if (l_userentry.get_role == RoleBob) {
            // 鮑伯
          } else if (l_userentry.get_role == RoleDetective) {
            // 莉可敗北會直接死
          } else if (l_userentry.get_role == RoleJudgment) {
            // 審判沒有不會勝利的可能性
          } else if (l_userentry.get_role == RoleMicah) {
            // 夏彌加沒有不會勝利的可能性
          } else if (l_userentry.get_role == RoleSeth) {
            // 塞特還有可能得到道具嗎？
            val card = CardPool.findAll(By(CardPool.room_id, l_userentry.room_id.is),
                                 By(CardPool.card, CardEnum.W_LANCE_OF_LONGINUS.toString),
                                 By(CardPool.discarded, true))
            val card2 = CardPool.findAll(By(CardPool.room_id, l_userentry.room_id.is),
                                 By(CardPool.card, CardEnum.B_EVILSWORD.toString),
                                 By(CardPool.discarded, true))
            if ((l_userentry.has_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED))
             && ((card.length > 0) || (card2.length > 0))) {
              nowin_neutrals += 1
            }
          } else if (l_userentry.get_role == RoleDragon) {
            // 神龍沒有不會勝利的可能性
          } else if (l_userentry.get_role == RoleAdriatic) {
            // 亞德的上下家還可能擊殺嗎？
            var deadusers = 0
            val index_r = userentrys_r.indexOf(l_userentry)
            val equiv_index1 = (index_r+1) % (userentrys_r.length)
            val equiv_index2 = (index_r + userentrys_r.length - 1) % (userentrys_r.length)
            userentrys_r.foreach { r_userentry =>
              val index_r2 = userentrys_r.indexOf(r_userentry)
              if (((equiv_index1 == index_r2) || (equiv_index2 == index_r2)) && (!r_userentry.live.is)) {
                deadusers += 1
              }
            }
            if (deadusers > l_userentry.action_point.is) {
              nowin_neutrals += 1
            }
          } else if (l_userentry.get_role == RolePuzzle) {
            if (userentrys_rl.length <= 1)
              nowin_neutrals += 1
          } else {
            nowin_neutrals += 1
          }
          if ((userentrys_rl.length == nowin_neutrals) && (!is_neutral_victory)) {
            is_neutral_victory = true
            val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).actioner_id(l_userentry.id.is)
                             .message("已經沒有持有主動勝利條件的角色，無法繼續進行遊戲")
            talk_a.save
            talk_a.send(l_userentry.room_id.is)
          }
        }
        
      }
    }
    
    // 絕望封鎖勝利條件
    
    if (live_despairs.length != 0) {
      is_hunter_victory = false
      is_shadow_victory = false
      is_lover_victory  = false
      
      if (userentrys_rl.length == 1) {
        val live_despair = live_despairs(0)
        live_despair.add_user_flag(UserEntryFlagEnum.VICTORY)
        live_despair.save
        is_neutral_victory = true
      } else {
        userentrys_v.foreach { userentry =>
          userentry.remove_user_flag(UserEntryFlagEnum.VICTORY)
          userentry.save
          is_neutral_victory = false
        }
      }
    }
    
    if (is_hunter_victory || is_shadow_victory || is_neutral_victory || is_lover_victory || is_draw_victory) {
      val victory = 
        if (is_lover_victory)
          RoomVictoryEnum.LOVER_WIN
        else if (is_hunter_victory && is_shadow_victory)
          RoomVictoryEnum.DUAL_WIN
        else if (is_hunter_victory)
          RoomVictoryEnum.HUNTER_WIN
        else if (is_shadow_victory)
          RoomVictoryEnum.SHADOW_WIN
        else if (is_neutral_victory)
          RoomVictoryEnum.NEUTRAL_WIN
        else
          RoomVictoryEnum.DRAW
        
      // 再次測試是否有中立者勝利
      userentrys_r.foreach { userentry =>
        val role = userentry.get_role
        role match {
          case RoleAllie =>
            if ((userentry.live.is))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleBryan =>
            if ((userentry.live.is) && (userentry.location.is == LocationEnum.ERSTWHILE_ALTER.toString))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleDaniel => 
            if ((userentry.live.is) && (victory == RoomVictoryEnum.HUNTER_WIN))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleCatherine =>
            val userentrys_live = userentrys_r.filter(_.live.is)
            if ((userentry.live.is) && (userentrys_live.length <= 2))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleShaHeart =>
            if (userentrys_r_hunter.length == 0)
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleAdriatic =>
            if ((room.hasnt_flag(RoomFlagEnum.ALL_NEUTRAL)) && (userentry.live.is) && (userentrys_r_hunter.length == 0) && (userentry.hasnt_user_flag(UserEntryFlagEnum.VICTORY)))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RolePuzzle =>
            if ((userentry.live.is) && (userentry.location.is == LocationEnum.GRAVEYARD.toString) && (userentry.hasnt_user_flag(UserEntryFlagEnum.VICTORY)))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleHunsoul =>
            if (userentrys_r_shadow.length == 0)
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleWestLobe =>
            if ( (userentry.live.is)
              || ((room.hasnt_flag(RoomFlagEnum.ALL_NEUTRAL)) && (userentrys_r_shadow.length == 0)) )
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleLube =>
            if ( (userentry.live.is) && (room.hasnt_flag(RoomFlagEnum.ALL_NEUTRAL)) && (userentrys_r_shadow.length == 0) )
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case xs => ;
        }
      }
      
      var is_done = true
      do {
        is_done = true
        userentrys_r.foreach { userentry =>
          // Agnes 要最後
          val role = userentry.get_role
          if ((role == RoleAgnes) && (userentry.hasnt_user_flag(UserEntryFlagEnum.VICTORY))) {
            val index = userentrys_r.indexOf(userentry)
            val equiv_index = 
              if (!userentry.revealed.is)
                (index+1) % (userentrys_r.length)
              else
                (index + userentrys_r.length - 1) % (userentrys_r.length)
            
            val equiv_userentry = userentrys_r(equiv_index)
            //val equiv_role      = equiv_userentry.get_role
            if (check_user_victory(equiv_userentry, victory.toString)) {
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
              is_done = false
            } 
          } else if ((role == RoleTel) && (userentry.hasnt_user_flag(UserEntryFlagEnum.VICTORY))) {
            val index = userentrys_r.indexOf(userentry)
            val equiv_index = (index + userentrys_r.length - 1) % (userentrys_r.length)
            
            val equiv_userentry = userentrys_r(equiv_index)
            //val equiv_role      = equiv_userentry.get_role
            if (check_user_victory(equiv_userentry, victory.toString)) {
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
              is_done = false
            } 
          } else if (((role == RoleCassandra) || (role == RoleCheshire))
                     && (userentry.hasnt_user_flag(UserEntryFlagEnum.VICTORY))) {
            val target_users = userentrys.filter(_.id.is == userentry.target_user.is)
            if (target_users.length != 0) {
              if (check_user_victory(target_users(0), victory.toString)) {
                userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
                is_done = false
              } 
            }
          }
        }
      } while(!is_done)
      
      process_victory(room, roomround, userentrys, victory)
      true
    } else
      false
  }
  
  def check_user_victory(userentry : UserEntry, victory_str : String) : Boolean = {
    val role = userentry.get_role
    
    if ((victory_str == RoomVictoryEnum.SHADOW_WIN.toString) && (role.role_side == RoleSideEnum.SHADOW)) 
      true
    else if ((victory_str == RoomVictoryEnum.HUNTER_WIN.toString) && (role.role_side == RoleSideEnum.HUNTER)) 
      true
    else if ((victory_str == RoomVictoryEnum.DUAL_WIN.toString) && (role.role_side != RoleSideEnum.NEUTRAL)) 
      true
    else if ((victory_str == RoomVictoryEnum.LOVER_WIN.toString) && (userentry.has_user_flag(UserEntryFlagEnum.LOVER)))
      true
    else if (userentry.has_user_flag(UserEntryFlagEnum.VICTORY))
      true
    else 
      false
  }
  
  def process_victory(room : Room, roomround: RoomRound, userentrys: List[UserEntry], victory : RoomVictoryEnum.Value) = {
    val new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                        .last_round(roomround.id.is)
    new_roomround.save
    val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("遊戲結束 "+ (new java.util.Date).toString)
    talk.save
    
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(0)
                                     .phase_type(RoomPhaseEnum.ENDED.toString)
    new_phase.save
    
    val userentrys_v = userentrys.filter(x => (!x.revoked.is) && (x.has_user_flag(UserEntryFlagEnum.VICTORY)))
    val userentrys_v_role = userentrys_v.map(_.get_real_role.role_enum.toString)
    
    room.status(RoomStatusEnum.ENDED.toString).victory(victory.toString).victory_all(userentrys_v_role.mkString(","))
    room.save

    RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, roomround = new_roomround, roomphase = new_phase, userentrys = userentrys))
    RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
  }
  
  def next_player(room: Room, roomround: RoomRound, roomphase: RoomPhase, userentrys: List[UserEntry]) : Unit = {
    val userentrys_rl = userentrys.filter(x => (!x.revoked.is) && 
                                           (((x.live.is) && (x.hasnt_room_flag(UserEntryRoomFlagEnum.SKIPPED)))
                                            || (x.id.is == roomphase.player.is)
                                            || ((x.get_skill_role == RoleUndead) && (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (x.hasnt_item(CardEnum.B_MASK)) && (x.hasnt_room_flag(UserEntryRoomFlagEnum.SUDDENDEATH)))
                                            ))
    val currentplayer = userentrys.filter(x => x.id.is == roomphase.player.is)(0)
    val old_player_index = userentrys_rl.map(_.id.is).indexOf(roomphase.player.is)
    var player_index = old_player_index
    var new_phase_no = roomphase.phase_no.is + 1
    var new_roomround = roomround
    var additional    = roomphase.additional.is

    var while_bound   = 0
    
    if ((additional <= 0) || (!currentplayer.live.is)) { // && (currentplayer.live.is)) {
      additional = 0
      do {
        while_bound = while_bound + 1
        player_index = player_index + 1
        if (player_index  >= userentrys_rl.length) {
          // 新回合
          player_index = 0
          new_phase_no = 0

          new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                   .last_round(roomround.id.is)
          new_roomround.save

          val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                         .message("第 " + (new_roomround.round_no.is.toString) + " 回合 "+ (new java.util.Date).toString)
          talk.save
        }
        
        val next_player1 = userentrys_rl(player_index)
        if ((!next_player1.live.is) && (next_player1.get_skill_role == RoleUndead) &&
            (next_player1.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (next_player1.hasnt_item(CardEnum.B_MASK))) {
          next_player1.lower_damage(1, userentrys)
          
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                              .message(next_player1.handle_name.is + " 的損傷減少 1 點(不死族)")
          talk1.save
          talk1.send(next_player1.room_id.is)
          if (next_player1.damaged.is < RoleUndead.life) {
            next_player1.damaged(8).live(true)
                        .add_user_flag(UserEntryFlagEnum.REVIVED)
                        .add_role_flag(UserEntryRoleFlagEnum.REVIVED_AURA)
            val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString)
                              .message(next_player1.handle_name.is + " 復活，且損傷設定為 8 ，並添加闇之靈氣狀態(不死族)")
            talk2.save
            talk2.send(next_player1.room_id.is)
          }
          next_player1.save
        }
        
      } while ((!userentrys_rl(player_index).live.is) && (while_bound < 100))
      
      val next_player1 = userentrys_rl(player_index)
      val talk_nextuser = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                         .message("輪替為 " + next_player1.handle_name.is + " 的回合")
      talk_nextuser.save
      talk_nextuser.send(next_player1.room_id.is)
    } else {
      val next_player2 = userentrys_rl(player_index)
      val talk_nextuser2 = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                         .message(next_player2.handle_name.is + " 尚有額外回合 " + additional + " 次，請繼續行動")
      talk_nextuser2.save
      talk_nextuser2.send(next_player2.room_id.is)
      additional = additional - 1
    }
    
    val next_player = userentrys_rl(player_index)
    if ((player_index == old_player_index) && (!next_player.live.is)) {
      warn("warn : next_player dead same player")
      warn("room : " + room.id.is)
      warn("roomround : " + roomround.id.is)
      warn("roomphase : " + roomphase.id.is)
      //warn("currentuserentry : " + currentuserentry.id.is)
    } 

    if (while_bound >= 100) {
      warn("warn : while_bound >= 100")
      warn("room : " + room.id.is)
      warn("roomround : " + roomround.id.is)
      warn("roomphase : " + roomphase.id.is)
    }
    
    next_player.remove_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
    if (next_player.get_skill_role == RoleWerewolf)
      next_player.remove_role_flag(UserEntryRoleFlagEnum.AMBUSH)
    next_player.remove_user_flag(UserEntryFlagEnum.GUARDIAN)
    next_player.remove_user_flag(UserEntryFlagEnum.ENCHANTMENT)
    next_player.remove_user_flag(UserEntryFlagEnum.BARRIER)
    currentplayer.remove_user_flag(UserEntryFlagEnum.ADVENT)
    currentplayer.remove_user_flag(UserEntryFlagEnum.CHOCOLATE)
    currentplayer.remove_user_flag(UserEntryFlagEnum.DIABOLIC)
    currentplayer.remove_user_flag(UserEntryFlagEnum.FROG)
    currentplayer.remove_user_flag(UserEntryFlagEnum.TAUNT)
    currentplayer.remove_user_flag(UserEntryFlagEnum.FIREWORK)
    currentplayer.remove_user_flag(UserEntryFlagEnum.STICKY)
    currentplayer.remove_user_flag(UserEntryFlagEnum.FAITH)
    currentplayer.card_flags("")
    if (currentplayer.get_skill_role == RoleEmi)
      currentplayer.remove_role_flag(UserEntryRoleFlagEnum.ENHANCED)
    if ((currentplayer.get_skill_role == RoleSeth) && (currentplayer.action_point.is > 0))
      currentplayer.action_point(math.max(0, currentplayer.action_point.is - 1))
    if ((currentplayer.get_skill_role == RoleSeth) && (currentplayer.action_point.is == 0))
      currentplayer.remove_user_flag(UserEntryFlagEnum.SLOW)
    if ((currentplayer.get_skill_role == RoleBomb) && (currentplayer.action_point.is > 0))
      currentplayer.action_point(0)
    currentplayer.save
    
    val live_seths = userentrys.filter (x => (x.get_role == RoleSeth) && (x.live.is) && (x.revealed.is) && (x.hasnt_user_flag(UserEntryFlagEnum.VICTORY)) &&
                                             (x.has_item(CardEnum.B_EVILSWORD)) && (x.has_item(CardEnum.W_LANCE_OF_LONGINUS)))
    if(live_seths.length > 0){
        live_seths.foreach { live_seth =>
        if ((live_seth.action_point.is > 0) && (currentplayer.id.is == live_seth.id.is)) {
            val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_NEUTRAL.toString).actioner_id(live_seth.id.is)
                                .message("距離塞特獲勝還有 " + live_seth.action_point.is + " 回合")
            talk_a.save
            talk_a.send(live_seth.room_id.is)
        }
      }
    }
    
    if ((next_player.revealed) && (next_player.get_skill_role == RoleCatherine) && (next_player.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (next_player.hasnt_item(CardEnum.B_MASK)))
      next_player.lower_damage(1, userentrys)
    if ((next_player.revealed) && (next_player.get_skill_role == RoleGinger) && (next_player.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (next_player.hasnt_item(CardEnum.B_MASK))) {
      val shadow_rl = userentrys_rl.filter(x => (x.live.is) && (x.get_role.role_side == RoleSideEnum.SHADOW)) // (x.revealed.is) && 
      val hunter_rl = userentrys_rl.filter(x => (x.live.is) && (x.get_role.role_side == RoleSideEnum.HUNTER)) // (x.revealed.is) && 
      if (shadow_rl.length > hunter_rl.length)
        if (room.has_flag(RoomFlagEnum.GINGER_REUSE)) {
          val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_HUNTER.toString).actioner_id(next_player.id.is)
                                .message(next_player.handle_name.is + " 的損傷減少 4 點(金格)")
          talk_a.save
          talk_a.send(next_player.room_id.is)
          next_player.lower_damage(4, userentrys)
        } else {
          val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_HUNTER.toString).actioner_id(next_player.id.is)
                                .message(next_player.handle_name.is + " 的損傷減少 2 點(金格)")
          talk_a.save
          talk_a.send(next_player.room_id.is)
          next_player.lower_damage(2, userentrys)
        }
    }
    if (next_player.has_user_flag(UserEntryFlagEnum.POISON)) {
      val talk_a = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_SHADOW.toString).actioner_id(next_player.id.is)
                                .message(next_player.handle_name.is + " 的損傷增加 " + next_player.user_flags.is.count(_.toString == UserEntryFlagEnum.POISON.toString) + " 點(毒蛇)")
      talk_a.save
      talk_a.send(next_player.room_id.is)
      if ((next_player.get_skill_role == RoleLion) && (next_player.revealed) && (next_player.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (next_player.hasnt_item(CardEnum.B_MASK))) {
        val talk_l = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LION.toString).actioner_id(next_player.id.is)
        talk_l.save
        talk_l.send(next_player.room_id.is)
      }
      next_player.inflict_damage(next_player.user_flags.is.count(_.toString == UserEntryFlagEnum.POISON.toString), 
                                 next_player)
    }
    next_player.save
    
    val deadline =
      if (next_player.has_room_flag(UserEntryRoomFlagEnum.AUTOVOTED))
        math.max(15, room.move_time.is * 3 / 4)
      else
        room.move_time.is
    
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(new_phase_no).additional(additional)
                             .phase_type(RoomPhaseEnum.MOVEMENT.toString).player(next_player.id.is)
                             .deadline(PlummUtil.dateAddSecond(new java.util.Date(), deadline))
    new_phase.save
    
    GameProcessor.check_death(next_player, next_player, Action.create.roomround_id(new_roomround.id.is), userentrys)
    if (!GameProcessor.check_victory(room, new_roomround, userentrys)) {
      
      if (!next_player.live.is)  {
        GameProcessor.next_player(room, new_roomround, new_phase, userentrys) 
      } else {
        RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, roomround = new_roomround, roomphase = new_phase, userentrys = userentrys))
        RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
        RoomActor.sendUserEntryMessage(next_player.id.is, UserEntryForceUpdate(next_player.id.is ,List(ForceUpdateEnum.MUSIC)))
      }
    }
  }
  //翻開處理
  def flip(actioner : UserEntry, action : Action, userentrys : List[UserEntry]) : Unit = {
    val action1 = Action.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                               .mtype(MTypeEnum.ACTION_FLIP.toString)
    action1.save
    val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                               .mtype(MTypeEnum.ACTION_FLIP.toString)
    talk.save
    talk.send(actioner.room_id.is)
    
    val role = actioner.get_role
    if ((role.role_side == RoleSideEnum.SHADOW) && 
       (userentrys.filter(x => (x.get_role.role_side == RoleSideEnum.SHADOW) && (x.revealed.is)).length == 0)) {
      val live_evans = userentrys.filter(x => (x.live.is) && (!x.revoked.is) && (x.get_role == RoleEvan))
      if (live_evans.length != 0) {
        val live_evan = live_evans(0)
        live_evan.add_user_flag(UserEntryFlagEnum.LOVER)
        live_evan.target_user(actioner.id.is)
        actioner.add_user_flag(UserEntryFlagEnum.LOVER)
        
        //println("Before Find")
        Room.find(By(Room.id, actioner.room_id.is)) match {
          case Full(room) if (room.has_flag(RoomFlagEnum.EVAN_HEAL)) =>
            if (actioner.live.is) {
              //println("Lover Flip : Actioner Healed " + actioner.handle_name.is)
              actioner.damaged(0)
            }
            live_evan.damaged(0)
          case xs => error("No Room Found (Lover Flip)");
        }
        //println("After Find")
        
        if (!live_evan.revealed.is)
          flip(live_evan, action, userentrys)
        else
          live_evan.save
      }
    }
    
    actioner.revealed(true).save
    
/*     //柴郡貓認主
    if (actioner.get_role != RoleCheshire) {
      val cheshires = userentrys.filter(_.get_role == RoleCheshire)
      cheshires.foreach { cheshire =>
        if (cheshire.target_user.is == 0)
          cheshire.target_user(actioner.id.is).save
      }
    } */
    
    val live_unrevealed = userentrys.filter(x =>(!x.revoked.is) && (x.get_role != RoleDetective) && (x.live.is) && (!x.revealed.is))
    if (live_unrevealed.length == 0) {
      val live_detectives = userentrys.filter(x =>(x.get_role == RoleDetective) && (x.live.is))
      live_detectives.foreach { live_detective =>
        val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(live_detective.id.is)
                                 .mtype(MTypeEnum.RESULT_NEUTRAL.toString).message(live_detective.handle_name.is + " 的損傷增加 99 點(莉可)")
        talk.save
        talk.send(live_detective.room_id.is)
        val saved_damaged = live_detective.damaged.is
        live_detective.inflict_damage(99, live_detective)
        GameProcessor.check_death(live_detective, live_detective, action, userentrys)
        //live_detective.damaged(saved_damaged).save
      }
    }
    val live_unrevealed2 = userentrys.filter(x => (x.live.is) && (!x.revealed.is) && (!x.revoked.is))
    if (live_unrevealed2.length == 0) {
      val live_judgments = userentrys.filter(x =>(x.get_role == RoleJudgment) && (x.live.is))
      live_judgments.foreach { live_judgment =>
        live_judgment.add_user_flag(UserEntryFlagEnum.VICTORY)
        live_judgment.save
      }
    }
    
    actioner
  }
  
  def attack(actioner : UserEntry, actionee : UserEntry, action : Action, userentrys : List[UserEntry], is_ambush : Boolean = false, ginger_power : Int = 0) = {
    val actioner_role = actioner.get_skill_role
    
    val random1d6 = GameProcessor.random.nextInt(6) + 1
    val random1d4 = GameProcessor.random.nextInt(4) + 1
        
    var attack_power = 0
    var attack_str   = ""
        
    if (actionee.has_user_flag(UserEntryFlagEnum.BARRIER)) {
      attack_power = 0
      attack_str   = "防護罩防禦住攻擊"
    } else if (actionee.has_user_flag(UserEntryFlagEnum.GUARDIAN)) {
      attack_power = 0
      attack_str   = "守護天使防禦住攻擊"
    } else if (actioner.has_user_flag(UserEntryFlagEnum.MISSED) && actioner.has_item(CardEnum.B_DAGGER)) {
      if ((actioner.revealed) && (actioner.get_role.role_side == RoleSideEnum.HUNTER)) 
        attack_power = 6
      else 
        attack_power = 5
      attack_str = ("攻擊力：" + attack_power)
    } else if (is_ambush) {
      attack_power = 5
      attack_str = ("攻擊力：" + attack_power)
    } else if (((actioner.revealed.is) && (actioner.get_skill_role == RoleValkyrie) 
               && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) ||
              (actioner.has_item(CardEnum.B_MASAMUNE))) {
      Room.find(By(Room.id, actioner.room_id.is)) match {
        case Full(room) if (room.has_flag(RoomFlagEnum.VALKYRIE_ENHANCE) &&
                            (actioner.revealed.is) && (actioner.get_skill_role == RoleValkyrie)) =>
          attack_power = random1d6
          attack_str = ("1D6=" + random1d6 + " 攻擊力：" + attack_power)
        case xs =>  
          attack_power = random1d4
          attack_str = ("1D4=" + random1d4 + " 攻擊力：" + attack_power)
      }  
    } else {
      attack_power = math.abs(random1d6 - random1d4)
      attack_str = ("1D6=" + random1d6 + ",1D4=" + random1d4 + " 攻擊力：" + attack_power)
    }
        
    var is_append = false
    var is_viper = false
    var is_shinai = false
    val room = Room.find(By(Room.id, actioner.room_id.is)).get
    val roomround = RoomRound_R.get
    val neighbor = LocationHelper.neighbor(room, actioner.location.is)
    
    
    
    if ((room.has_flag(RoomFlagEnum.GODFAT_REUSE)) && (actioner.revealed.is) && (actioner.get_skill_role == RoleGodfat) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) &&
               ((actioner.location.is != actionee.location.is) && (actionee.location.is != neighbor)) &&
               (actioner.hasnt_item(CardEnum.B_HANDGUN))) {
      attack_power = math.max(0, attack_power - 1)
      attack_str += "-1(摺)"
      is_append = true
    }
    
    if ((actioner.has_item(CardEnum.W_HOLY_ROBE) || actionee.has_item(CardEnum.W_HOLY_ROBE)) && (attack_power != 0)) {
      attack_power = math.max(0, attack_power - 1)
      attack_str += "-1(袍)"
      is_append = true
    }
    
    if ((actionee.get_role.role_side == RoleSideEnum.SHADOW) &&
        (actionee.revealed)){
      val reviveds = userentrys.filter(x => (x.live.is) &&
        (x.get_skill_role == RoleUndead) &&
        (x.has_role_flag(UserEntryRoleFlagEnum.REVIVED_AURA)))
      if (reviveds.length != 0) {
        attack_power = math.max(0, attack_power - 1)
        attack_str += "-1(闇)"
        is_append = true
      }
    }
    
    
    if ((actioner.revealed.is) && (actioner.get_skill_role == RoleAmetsuki) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) &&
               (actionee.has_user_flag(UserEntryFlagEnum.SEAL))) {
      attack_power += 1
      attack_str += "+1(訃)"
      is_append = true
    }
    
    if ((actionee.get_skill_role == RoleLion) && (actionee.revealed) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))){
        attack_power = math.max(0, attack_power - 1)
        attack_str += "-1(王)"
        is_append = true
    }
    
    if ((actioner.revealed.is) && (actioner.get_role.role_side == RoleSideEnum.SHADOW) &&
               (actioner.has_item(CardEnum.B_EVILSWORD))) {
        attack_power += 1
        attack_str += "+1(骸)"
        is_append = true
    }
    
    if ((attack_power > 0) && (actioner.has_item(CardEnum.B_CHAINSAW))) {
      attack_power += 1
      attack_str += "+1(鋸)"
      is_append = true
    }
    if ((attack_power > 0) && (actioner.has_item(CardEnum.B_RUSTED_BROAD_AXE))) {
      attack_power += 1
      attack_str += "+1(斧)"
      is_append = true
    }
    if ((attack_power > 0) && (actioner.has_item(CardEnum.B_BUTCHER_KNIFE))) {
      attack_power += 1
      attack_str += "+1(刀)"
      is_append = true
    }
    if (((actioner.revealed.is) && (actioner.get_skill_role == RoleValkyrie) 
          && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) &&
         (actioner.has_item(CardEnum.B_MASAMUNE))) {
      attack_power += 1
      attack_str += "+1(妖)"
      is_append = true
    }
    if ((attack_power > 0) && (actioner.has_item(CardEnum.W_LANCE_OF_LONGINUS)) && (actioner.get_role.role_side == RoleSideEnum.HUNTER) && (actioner.revealed.is)) {
      attack_power += 2
      attack_str += "+2(槍)"
      is_append = true
    }
    //if ((attack_power > 0) && (actionee.location.is == LocationEnum.WIERD_WOODS.toString) &&
    //    (actioner.get_role == RoleWitch) && (actioner.revealed.is) &&
    //    (!actioner.has_user_flag(UserEntryFlagEnum.SEALED))) 
    //  actionee.add_user_flag(UserEntryFlagEnum.FROG)
    if ((attack_power > 0) && (actioner.has_user_flag(UserEntryFlagEnum.FAITH))) {
      attack_power += 1
      attack_str += "+1(鳴)"
      actioner.save
      is_append = true
    }
    
    if ((attack_power > 0) && (actioner.get_role.role_side == RoleSideEnum.HUNTER) && (actioner.revealed.is)){
      val Arsisr = userentrys.filter(x => (x.live.is) && (x.get_skill_role == RoleArsis) && (x.revealed.is) && (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (x.hasnt_item(CardEnum.B_MASK)))
      if(Arsisr.length != 0){
        attack_power += 1
        attack_str += "+1(澄)"
        is_append = true
      }
    }
    
    if ((attack_power > 0) && (actioner.revealed.is) && (actioner != actionee) &&
        (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
      if ((actioner.get_skill_role == RoleFeng) && (actioner.items.length == 0)){
        if (actionee.items.length <= 1) {
          attack_power += 2
          attack_str += "+2(鬥)"
        } else {
          attack_power += 1
          attack_str += "+1(鬥)"
        }
        is_append = true
      } else if ((actioner.get_skill_role == RoleEmi) && (actioner.has_role_flag(UserEntryRoleFlagEnum.ENHANCED))){
        attack_power += 1
        attack_str += "+1(靈)"
        is_append = true
      } else if ((actioner.get_skill_role == RoleMars) && (actioner.items.length != 0)){
        val itemsl = actioner.items.length
        attack_power += itemsl
        attack_str += "+" + itemsl + "(萬)"
        is_append = true
      } else if ((actioner.has_user_flag(UserEntryFlagEnum.RESENTFUL)) && (actioner.get_skill_role == RoleGinger)){
        attack_str += "+" + attack_power + "(憤)"
        attack_power += attack_power
        is_append = true
        actioner.remove_user_flag(UserEntryFlagEnum.RESENTFUL).save
      } else if ((actioner.get_skill_role == RoleAdriatic) && (ginger_power > 0)){
        attack_power += ginger_power
        attack_str += "+" + ginger_power + "(狂)"
        is_append = true
      } else if ((actioner.get_skill_role == RoleBorogove)){
        if(!actionee.revealed.is){
          attack_power += 3
          attack_str += "+3(悲)"
          is_append = true
        }
      } else if (actioner.get_skill_role == RoleWicked) {
        if ((actionee.location.is == LocationEnum.GRAVEYARD.toString) || 
            (actionee.location.is == LocationEnum.WIERD_WOODS.toString)) {
          attack_power += 1
          attack_str += "+1(潔)"
          is_append = true
        }
        if (((actionee.get_role.role_side == RoleSideEnum.HUNTER) ||
            (actionee.get_role.role_side == RoleSideEnum.NEUTRAL)) &&
            (actionee.revealed.is)) {
          attack_power += 2
          attack_str += "+2(亂)"
          is_append = true
        }
      } else if ((attack_power > 1) && (actioner.get_skill_role == RoleBellandona) && 
                 (actionee.revealed.is)) {
      
        Room.find(By(Room.id, actioner.room_id.is)) match {
          case Full(room) =>
            if (room.has_flag(RoomFlagEnum.BELLANDONA_CHOOSE)) {
              actioner.role(actionee.role.is.substring(0,2) + actioner.role.is)
              actioner.target_user(actionee.target_user.is)
              //actioner_role = actionee.get_role
      
              val actionee_role = actionee.get_role
              if (actioner.damaged.is >= actionee_role.role_life)
                actioner.damaged(actionee_role.role_life - 1)
      
              actioner.save
              //GameProcessor.check_death(actioner, actioner, action, userentrys)
              //actioner.save
            }
          
          case xs => ;
        }
      } else if ((actioner.get_skill_role == RoleViper)) { //&& 
                 // (actionee.hasnt_user_flag(UserEntryFlagEnum.POISON))) {
        is_viper = true
        
      }
    }
    var attack_power_text = attack_power - 1
    if ((actionee.has_user_flag(UserEntryFlagEnum.BARRIER)) || (actionee.has_user_flag(UserEntryFlagEnum.GUARDIAN))) {
      is_append = false
    }
    if (is_append){
      attack_str += "=" + attack_power
    }
    
    // 希奈迷蹤
    if ((!is_ambush) && (attack_power != 0) && (actionee.revealed.is) && (actionee.get_skill_role == RoleShinai) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actionee.hasnt_item(CardEnum.B_MASK))) {
        val heal1d6r = GameProcessor.random.nextInt(6) + 1
        val heal1d6e = GameProcessor.random.nextInt(6) + 1
        val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                 .mtype(MTypeEnum.RESULT_HUNTER.toString).message("擲骰結果：攻1D6=" + heal1d6r + "、守1D6=" + heal1d6e)
        talk1.save
        talk1.send(actioner.room_id.is)
        if (heal1d6e >= heal1d6r) {
          val talk2 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                 .mtype(MTypeEnum.RESULT_HUNTER.toString).message(actionee.handle_name.is + " 消失蹤影(佐藤希奈)")
          talk2.save
          talk2.send(actioner.room_id.is)
          attack_power = 0
          actionee.location("")
          actioner.save
          is_shinai = true
        } else {
          val talk2 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                 .mtype(MTypeEnum.RESULT_HUNTER.toString).message(actionee.handle_name.is + " 迴避失敗(佐藤希奈)")
          talk2.save
          talk2.send(actioner.room_id.is)
        }
    }
    
    //毒蛇
    if ((!is_shinai) && (is_viper)) {
      actionee.add_user_flag(UserEntryFlagEnum.POISON)
      val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                             .mtype(MTypeEnum.RESULT_SHADOW.toString).message(actionee.handle_name.is + " 添加 中毒 狀態(毒蛇)")
      talk1.save
      talk1.send(actioner.room_id.is)
    }
        
    // 吸血鬼回復 HP
    if ((actioner.revealed.is) && (actioner.get_skill_role == RoleVampire) && (attack_power != 0) &&
        (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
      Room.find(By(Room.id, actioner.room_id.is)) match {
        case Full(room) if (room.has_flag(RoomFlagEnum.VAMPIRE_WEAKEN) &&
          actioner.has_item(CardEnum.B_MACHINE_GUN)) => 
          actioner.lower_damage(1, userentrys)
          attack_str += "，並回復 1 點損傷"
        case xs =>  
          actioner.lower_damage(2, userentrys)
          attack_str += "，並回復 2 點損傷"
      }
      actioner.save
    }
    
    // 波若哥夫回復 HP
    val live_evans = userentrys.filter(x => (x.live.is) && (x.revealed.is) && (!x.revoked.is) && (x.get_role == RoleEvan) && (x.has_user_flag(UserEntryFlagEnum.LOVER)))
    if ((actioner.revealed.is) && (actioner.get_skill_role == RoleBorogove) && (attack_power != 0) &&
        (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK)) && ((actioner.hasnt_user_flag(UserEntryFlagEnum.LOVER)) && (live_evans.length == 0))) {
      if ((actionee.get_role.role_side == RoleSideEnum.SHADOW) && (actionee.revealed.is) && ((actionee.hasnt_user_flag(UserEntryFlagEnum.LOVER)) && (live_evans.length == 0))) {
        var lower_power = attack_power / 2
        attack_str += "，將攻擊力轉化為回復 " + lower_power + " 點(波若哥夫)"
        //傷害回復在最後階段處理
      }
    }
    
    // 鮑伯搶奪
    if (((actioner_role == RoleBob) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) &&
       // (actioner.has_item(CardEnum.W_SILVER_ROSARY)) && 
       (attack_power != 0) ) {
      if (actionee.items.length > 0) {
        val robbed_item = rob_single(room, roomround, actioner, actionee)
        actioner.beads(actioner.beads.is + actionee.beads.is).save
        actionee.beads(0).save
        attack_str += "，並搶奪 " + robbed_item.card_name
      }
      actioner.save
    }
    // 寶珠獲得
    if ((attack_power != 0) && (actioner.live.is)) {
        var power_beads : Int = attack_power / 3
        var give_beads = math.min(actionee.beads.is, power_beads)
        actionee.beads(math.max(0, (actionee.beads.is - give_beads))).save
        actioner.beads(actioner.beads.is + give_beads).save
    }
    
    // 物攻嘲諷
    /*
    if (((actioner_role == RoleADecoy) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) &&
       // (actioner.has_item(CardEnum.W_SILVER_ROSARY)) && 
       (attack_power == 0) ) {
      actionee.add_user_flag(UserEntryFlagEnum.TAUNT)
    } */
   
    if ((attack_power == 0) && (actioner.hasnt_user_flag(UserEntryFlagEnum.MISSED))) {
      actioner.add_user_flag(UserEntryFlagEnum.MISSED).save
    } else if (attack_power > 0) {
      actioner.remove_user_flag(UserEntryFlagEnum.MISSED).save
    }
    
    // 魔渦
    if ((attack_power > 0) && (actioner_role == RoleBane) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (actioner.hasnt_item(CardEnum.B_MASK))) {
      val live_hunters = userentrys.filter(x => (x.live.is) && (x.revealed.is) && (!x.revoked.is) && (x.get_skill_role.role_side == RoleSideEnum.HUNTER) && (x.id.is != actionee.id.is))
      if (live_hunters.length != 0) {
        live_hunters.foreach { live_hunter =>
          if (actioner.hasnt_item(CardEnum.B_MACHINE_GUN)) {
            val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is).actionee_id(live_hunter.id.is)
                                 .mtype(MTypeEnum.RESULT_SHADOW.toString).message(live_hunter.handle_name.is + " 受到 2 點損傷(弒魔)")
            talk1.save
            talk1.send(actioner.room_id.is)
            live_hunter.inflict_damage(2, actioner)
          } else {
            val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is).actionee_id(live_hunter.id.is)
                                 .mtype(MTypeEnum.RESULT_SHADOW.toString).message(live_hunter.handle_name.is + " 受到 1 點損傷(弒魔)")
            talk1.save
            talk1.send(actioner.room_id.is)
            live_hunter.inflict_damage(1, actioner)
          }
          live_hunter.save
          GameProcessor.check_death(live_hunter, actioner, action, userentrys)
          is_append = true
        }
      }
    }
    
    // 咒封之假面轉移
    if ((attack_power > 0) && (actioner.has_item(CardEnum.B_MASK))) {
      actioner.remove_item(CardEnum.B_MASK)
      actionee.add_item(CardEnum.B_MASK)
      /*
      val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                 By(CardPool.card, CardEnum.B_MASK.toString)).get
      card.owner_id(actionee.id.is).discarded(true).save
      */
      actioner.save
      actionee.save
      GameProcessor.check_item_victory(room, roomround, actionee)
      attack_str += "，並給予 咒封之假面"
      is_append = true
    }
        
    (attack_power, attack_str)
  }
  
  //def next_phase = {
  //  
  //}
  
  def abandon(room : Room) = {
    val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is), OrderBy(RoomRound.round_no, Descending)).get
    val new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                        .last_round(roomround.id.is)
    new_roomround.save
    val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("遊戲結束 "+ (new java.util.Date).toString)
    talk.save
     
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(0)
                            .phase_type(RoomPhaseEnum.ENDED.toString)
    new_phase.save
             
    room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
    room.save
            
    RoomActor ! SessionVarSet(room = room, roomround = new_roomround, roomphase = new_phase)
    //RoomActor.sendRoomMessage(room_id, RoomForceUpdate(room_id ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
    RoomActor.sendRoomMessage(room.id.is, RoomForceOut(room.id.is))
  }
}
