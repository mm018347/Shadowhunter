package org.plummtw.shadowhunter.enum

import org.plummtw.shadowhunter.data._

object RoleSideEnum extends Enumeration {
  type RoleSideEnum = Value

  val NONE     = Value("")
  val SHADOW   = Value("S")
  val HUNTER   = Value("H")
  val NEUTRAL  = Value("N")
  
  val ROLESIDE_COLOR_MAP   = scala.collection.immutable.TreeMap(
    NONE   -> "none",
    SHADOW -> "shadow",
    HUNTER -> "hunter",
    NEUTRAL -> "neutral"
  )
  
  val ROLESIDE_CNAME_MAP   = scala.collection.immutable.TreeMap(
    SHADOW -> "暗影",
    HUNTER -> "獵人",
    NEUTRAL -> "中立"
  )
  
  val ROLESIDE_LIST = List(
    SHADOW,
    HUNTER,
    NEUTRAL
  )
  
  def get_roleside_color(roleside : RoleSideEnum.Value) : String = {
    ROLESIDE_COLOR_MAP.get(roleside).getOrElse("")
  }
  
  def get_roleside_cname(roleside : RoleSideEnum.Value) : String = {
    ROLESIDE_CNAME_MAP.get(roleside).getOrElse("")
  }
  
  def get_roleside_cname(roleside : String) : String = {
    try { get_roleside_cname(withName(roleside)) }
    catch { case e : Exception => ""}
  }
  
  implicit def rolesideenum2String (en : RoleSideEnum.Value) : String = en.toString
}

object RoleEnum extends Enumeration {
  type RoleEnum = Value

  val NONE      = Value("")
  val NOEFFECT  = Value("NE")
  
  val ULTRASOUL = Value("US")
  val UNKNOWN  = Value("UN")
  val UNSEEN    = Value("UE")
  val UNDEAD     = Value("UD")
  val VALKYRIE  = Value("VK")
  val VAMPIRE   = Value("VM")
  val VENGEFUL_GHOST = Value("VG")
  val VIPER     = Value("VI")
  val WEREWOLF = Value("WW")
  val WIGHT     = Value("WI")
  val WITCH      = Value("WT")
  val WICKED     = Value("WC")
  val BANE     = Value("BN")
  val STARS     = Value("SA")
  val FIGHTER     = Value("FH")
  val BOROGOVE     = Value("BG")
  val CLACKEN     = Value("CC")

  val ELLEN      = Value("EL")
  val EMI        = Value("EM")
  val EMMA       = Value("EA")
  val EVAN        = Value("EV")
  val FRANKLIN  = Value("FR")
  val FUKA      = Value("FK")
  val FATHER_OCONNEL = Value("FO")
  val FENG      = Value("FE")
  val GEORGE    = Value("GE")
  val GREGOR    = Value("GR")
  val GINGER     = Value("GI")
  val GODFAT     = Value("GF")
  val MARS      = Value("MR")
  val LION      = Value("LO")
  val ARSIS     = Value("AR")
  val SHINAI    = Value("SI")
  val AICHA     = Value("AI")

  val AGNES     = Value("AG")
  val ALLIE      = Value("AL")
  val ANGEL      = Value("AN")
  val ADECOY     = Value("AD")
  val BOB       = Value("BO")
  val BOMB      = Value("BM")
  val BRYAN    = Value("BR")
  val BELLANDONA = Value("BE")
  val CATHERINE= Value("CA")
  val CHARLES  = Value("CH")
  val CASSANDRA = Value("CS")
  val CHESHIRE  = Value("CR")
  val DAVID    = Value("DV")
  val DANIEL   = Value("DN")
  val DESPAIR  = Value("DE")
  val DETECTIVE = Value("DT")
  val JUDGMENT = Value("JM")
  val SHAHEART = Value("SH")
  val HUNSOUL = Value("HS")
  val ADRIATIC = Value("AA")
  val MICAH = Value("MA")
  
  val ROLE_MAP   = scala.collection.immutable.TreeMap(
    NOEFFECT  -> RoleNoEffect,
    
    ULTRASOUL -> RoleUltraSoul,
    UNKNOWN   -> RoleUnknown,
    UNSEEN    -> RoleUnseen,
    UNDEAD     -> RoleUndead,
    VALKYRIE  -> RoleValkyrie,
    VAMPIRE   -> RoleVampire,
    VENGEFUL_GHOST -> RoleVengefulGhost,
    VIPER     -> RoleViper, 
    WEREWOLF  -> RoleWerewolf,
    WIGHT     -> RoleWight,
    WITCH     -> RoleWitch,
    WICKED    -> RoleWicked,
    BANE      -> RoleBane,
    STARS     -> RoleStars,
    FIGHTER   -> RoleFighter,
    BOROGOVE  -> RoleBorogove,
    CLACKEN   -> RoleClacken,

    ELLEN     -> RoleEllen,
    EMI       -> RoleEmi,
    EMMA      -> RoleEmma,
    EVAN      -> RoleEvan,
    FATHER_OCONNEL -> RoleFatherOconnel,
    FENG      -> RoleFeng,
    FRANKLIN  -> RoleFranklin,
    FUKA      -> RoleFuka,
    GEORGE    -> RoleGeorge,
    GREGOR    -> RoleGregor,
    GINGER    -> RoleGinger,
    GODFAT    -> RoleGodfat,
    MARS      -> RoleMars,
    LION      -> RoleLion,
    ARSIS     -> RoleArsis,
    SHINAI    -> RoleShinai,
    AICHA     -> RoleAicha,

    AGNES     -> RoleAgnes,
    ALLIE     -> RoleAllie,
    ANGEL     -> RoleAngel,
    ADECOY    -> RoleADecoy,
    BELLANDONA -> RoleBellandona,
    BOB       -> RoleBob,
    BOMB       -> RoleBomb,
    BRYAN     -> RoleBryan,
    CATHERINE -> RoleCatherine,
    CHARLES   -> RoleCharles,
    CASSANDRA -> RoleCassandra,
    CHESHIRE  -> RoleCheshire,
    DAVID     -> RoleDavid,
    DANIEL    -> RoleDaniel,
    DESPAIR   -> RoleDespair,
    DETECTIVE -> RoleDetective,
    JUDGMENT   -> RoleJudgment,
    SHAHEART   -> RoleShaHeart,
    HUNSOUL   -> RoleHunsoul,
    ADRIATIC   -> RoleAdriatic,
    MICAH      ->RoleMicah
  )
  //不明欺騙
  val UNKNOWN_DECEIVE_LIST = List(
    UNKNOWN, NOEFFECT, VAMPIRE, WEREWOLF, EMI, FRANKLIN,
     GEORGE, ALLIE, BOB, CHARLES, DANIEL,
     ULTRASOUL, VALKYRIE, WIGHT, ELLEN, FUKA, GREGOR, 
     AGNES, BRYAN, CATHERINE, DAVID,
     
     UNSEEN, UNDEAD, VENGEFUL_GHOST, VIPER, WITCH, WICKED, BANE, STARS, FIGHTER, BOROGOVE, CLACKEN,
     
     EVAN, EMMA, FATHER_OCONNEL, FENG, GINGER, GODFAT, MARS, LION, ARSIS, SHINAI, AICHA,
     
     ANGEL, BELLANDONA, CASSANDRA, DESPAIR, 
     ADECOY, BOMB, CHESHIRE, DETECTIVE, JUDGMENT, SHAHEART, HUNSOUL, MICAH)
  //標準職業
  val STANDARD_ROLE_LIST = List(
    UNKNOWN, VAMPIRE, WEREWOLF, EMI, FRANKLIN, GEORGE,
    ALLIE, BOB, CHARLES, DANIEL)
  //擴充職業
  val EXPANSION_ROLE_LIST = List(
    ULTRASOUL, VALKYRIE, WIGHT, ELLEN, FUKA, GREGOR, 
    AGNES, BRYAN, CATHERINE, DAVID)
  //自製職業
  val CUSTOM_ROLE_LIST = List(
    UNSEEN, UNDEAD, VENGEFUL_GHOST, VIPER, WITCH, WICKED,
    EVAN, EMMA, FATHER_OCONNEL, FENG, GINGER, GODFAT,
    ANGEL, BELLANDONA, CASSANDRA, DESPAIR, 
    ADECOY, BOMB, CHESHIRE, DETECTIVE)
  //參考職業
  val CUSTOM_CAT_ROLE_LIST = List(
    BANE, STARS, FIGHTER, BOROGOVE, CLACKEN,
    MARS, LION, ARSIS, SHINAI, AICHA,
    JUDGMENT, SHAHEART, HUNSOUL, MICAH)
  //全部職業
  val ALL_ROLE_LIST = List(
    UNKNOWN, VAMPIRE, WEREWOLF, 
    ULTRASOUL, VALKYRIE, WIGHT, 
    UNSEEN, VENGEFUL_GHOST, WITCH, 
    UNDEAD, VIPER, WICKED, BANE, STARS, FIGHTER, BOROGOVE, CLACKEN,
    
    EMI, FRANKLIN, GEORGE,
    ELLEN, FUKA, GREGOR, 
    EVAN, FATHER_OCONNEL, GINGER,
    EMMA, FENG, GODFAT, MARS, LION, ARSIS, SHINAI, AICHA,
    
    ALLIE, BOB, CHARLES, DANIEL,
    AGNES, BRYAN, CATHERINE, DAVID,
    ANGEL, BELLANDONA, CASSANDRA, DESPAIR, 
    ADECOY, BOMB, CHESHIRE, DETECTIVE, JUDGMENT, SHAHEART, HUNSOUL, MICAH)
  //希望職業
  val WISH_ROLE_LIST = List(
    NONE, UNKNOWN, VAMPIRE, WEREWOLF, 
    ULTRASOUL, VALKYRIE, WIGHT, 
    UNSEEN, VENGEFUL_GHOST, WITCH, 
    UNDEAD, VIPER, WICKED, BANE, STARS, FIGHTER, BOROGOVE, CLACKEN,
    
    EMI, FRANKLIN, GEORGE,
    ELLEN, FUKA, GREGOR, 
    EVAN, FATHER_OCONNEL, GINGER,
    EMMA, FENG, GODFAT, MARS, LION, ARSIS, SHINAI, AICHA,
    
    ALLIE, BOB, CHARLES, DANIEL,
    AGNES, BRYAN, CATHERINE, DAVID,
    ANGEL, BELLANDONA, CASSANDRA, DESPAIR, 
    ADECOY, BOMB, CHESHIRE, DETECTIVE, JUDGMENT, SHAHEART, HUNSOUL, MICAH)
  //不包含中立
  val NO_NEUTRAL_LIST = List(
    UNKNOWN, VAMPIRE, WEREWOLF, 
    ULTRASOUL, VALKYRIE, WIGHT, 
    UNSEEN, VENGEFUL_GHOST, WITCH, 
    UNDEAD, VIPER, WICKED, BANE, STARS, FIGHTER, CLACKEN,
    
    EMI, FRANKLIN, GEORGE,
    ELLEN, FUKA, GREGOR, 
    EVAN, FATHER_OCONNEL, GINGER,
    EMMA, FENG, GODFAT, MARS, LION, ARSIS, SHINAI, AICHA)
  //僅有中立
   val NEUTRAL_LIST = List(
    ALLIE, BOB, CHARLES, DANIEL,
    AGNES, BRYAN, CATHERINE, DAVID,
    ANGEL, BELLANDONA, CASSANDRA, DESPAIR, 
    ADECOY, BOMB, CHESHIRE, DETECTIVE, JUDGMENT, SHAHEART, HUNSOUL, MICAH)
  
  def get_role(role : RoleEnum.Value) : RoleData = {
    val result = ROLE_MAP.get(role) 
    //if (result.isEmpty)
    //  println(role.toString + "is null")
    return result.getOrElse(RoleNone)
  }
  
  def get_role(role_string : String) : RoleData = {
    try {get_role(withName(role_string)) }
    catch {case e : Exception => RoleNone}
  }
  
  implicit def roleenum2String (en : RoleEnum.Value) : String = en.toString
}

