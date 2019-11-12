package org.plummtw.shadowhunter.enum

object RoomPhaseEnum extends Enumeration {
  type RoomPhaseEnum    = Value
  
  val NONE              = Value("")
  
  val GAMEHALL          = Value("GH")
  val MOVEMENT          = Value("MV")
  //val MOVEMENT_SKILL  = Value("MS")
  val LOCATION          = Value("LO")
  val LOCATION_CHOOSE   = Value("LC")
  val CARD              = Value("CA")
  val CARD_CHOOSE       = Value("CC")
  val CARD_SKILL        = Value("CS")
  val CARD_AKI          = Value("CK")
  val GREEN_REACTION    = Value("GR")
  val ATTACK            = Value("AT")
  val REACTION          = Value("RE")
  val POST_ATTACK       = Value("PA")
  val ENDED             = Value("EN")
  
  val CNAME_MAP     = scala.collection.immutable.TreeMap(
    NONE              -> "無",
    GAMEHALL          -> "房間階段",
    MOVEMENT          -> "移動階段",
    LOCATION          -> "區域階段",
    CARD              -> "卡片階段",
    CARD_SKILL        -> "卡片階段",
    CARD_CHOOSE       -> "卡片階段",
    CARD_AKI          -> "卡片階段",
    ATTACK            -> "攻擊階段",
    POST_ATTACK       -> "後置行動",
    ENDED             -> "結束階段"
  )
  
  def get_cname(phase : RoomPhaseEnum.Value) : String =
    CNAME_MAP.get(phase).getOrElse("無")
  
  def get_cname(phase : String ) : String =
    get_cname(try {RoomPhaseEnum.withName(phase)}
      catch {case e : Exception => NONE})
  
  implicit def gamephaseenum2String (en : RoomPhaseEnum.Value) : String = en.toString
}
