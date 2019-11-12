package org.plummtw.shadowhunter.enum

object RoomFlagEnum extends Enumeration {
  val TEST_MODE               = Value("TM_")
  val WISH_ALIGN              = Value("WA_")
  val WISH_ROLE               = Value("WR_")
  val HATE_ROLE               = Value("HR_")
  //val DEATH_LOOK            = Value("DL_")
  val EXPANSION_ROLE          = Value("R1_")
  val CUSTOM_ROLE             = Value("R2_")
  val CUSTOM_CAT_ROLE         = Value("RC_")
  val NO_HUNSOUL_SHAHEART     = Value("NHS")

  val NO_ULTRASOUL            = Value("NUS")
  val NO_UNKNOWN              = Value("NUN")
  val NO_UNSEEN               = Value("NUE")
  val NO_UNDEAD               = Value("NUD")
  val NO_VALKYRIE             = Value("NVK")
  val NO_VAMPIRE              = Value("NVM")
  val NO_VENGEFUL_GHOST       = Value("NVG")
  val NO_VIPER                = Value("NVI")
  val NO_WEREWOLF             = Value("NWW")
  val NO_WIGHT                = Value("NWI")
  val NO_WITCH                = Value("NWT")
  val NO_WICKED               = Value("NWC")
  val NO_BANE                 = Value("NBN")
  val NO_STARS                = Value("NSA")
  val NO_FIGHTER              = Value("NFH")
  val NO_BOROGOVE             = Value("NBG")
  val NO_CLACKEN              = Value("NCC")
  val NO_ANIMALBONES          = Value("NAB")
  val NO_CONCUBINE            = Value("NCU")
  val NO_MOSSEN               = Value("NMO")
  val NO_MAGICIAN             = Value("NMI")
  val NO_FALLOMEN             = Value("NFM")

  val NO_ELLEN                = Value("NEL")
  val NO_EMI                  = Value("NEM")
  val NO_EMMA                 = Value("NEA")
  val NO_EVAN                 = Value("NEV")
  val NO_FRANKLIN             = Value("NFR")
  val NO_FUKA                 = Value("NFK")
  val NO_FATHER_OCONNEL       = Value("NFO")
  val NO_FENG                 = Value("NFE")
  val NO_GEORGE               = Value("NGE")
  val NO_GREGOR               = Value("NGR")
  val NO_GINGER               = Value("NGI")
  val NO_GODFAT               = Value("NGF")
  val NO_MARS                 = Value("NMR")
  val NO_LION                 = Value("NLO")
  val NO_ARSIS                = Value("NAR")
  val NO_SHINAI               = Value("NSI")
  val NO_AICHA                = Value("NAI")
  val NO_YAKALI               = Value("NYK")
  val NO_AKI                  = Value("NAK")
  val NO_AMETSUKI             = Value("NAS")
  val NO_LILIA                = Value("NLL")
  val NO_CLOUDBOW             = Value("NCB")

  val NO_AGNES                = Value("NAG")
  val NO_ALLIE                = Value("NAL")
  val NO_ANGEL                = Value("NAN")
  val NO_ADECOY               = Value("NAD")
  val NO_BOB                  = Value("NBO")
  val NO_BOMB                 = Value("NBM")
  val NO_BRYAN                = Value("NBR")
  val NO_BELLANDONA           = Value("NBE")
  val NO_CATHERINE            = Value("NCA")
  val NO_CHARLES              = Value("NCH")
  val NO_CASSANDRA            = Value("NCS")
  val NO_CHESHIRE             = Value("NCR")
  val NO_DAVID                = Value("NDV")
  val NO_DANIEL               = Value("NDN")
  val NO_DESPAIR              = Value("NDE")
  val NO_DETECTIVE            = Value("NDT")
  val NO_JUDGMENT             = Value("NJM")
  val NO_SHAHEART             = Value("NSH")
  val NO_HUNSOUL              = Value("NHU")
  val NO_ADRIATIC             = Value("NAA")
  val NO_MICAH                = Value("NMA")
  val NO_SETH                 = Value("NST")
  val NO_DRAGON               = Value("NDR")
  val NO_LEON                 = Value("NLE")
  val NO_PUZZLE               = Value("NPU")
  val NO_WESTLOBE             = Value("NWL")
  val NO_LUBE                 = Value("NLB")
  val NO_TEL                  = Value("NTE")


  val NEUTRAL_BACK_1          = Value("NB1")
  val NEUTRAL_BACK_2          = Value("NB2")

  val INIT_LOCATION           = Value("IL_")
  val INIT_GREEN              = Value("IG_")
  val RANDOM_POSITION         = Value("RP_")
  val FOUR_NEUTRAL            = Value("FN_")
  val ALL_NEUTRAL             = Value("AN_")

  val ULTRASOUL_RAY           = Value("UL1")
  val UNSEEN_RESIST           = Value("UE1")
  val VAMPIRE_WEAKEN          = Value("VM1")
  val VALKYRIE_ENHANCE        = Value("VK1")
  val VGHOST_EXPAND           = Value("VG1")
  val EMI_ENHANCE             = Value("EM1")
  val EMI_SEND                = Value("EM2")
  val ELLEN_HEAL              = Value("EL1")
  val EVAN_HEAL               = Value("EV1")
  val FRANKLIN_REUSE          = Value("FR1")
  val GEORGE_REUSE            = Value("GE1")
  val GEORGE_REUSE2           = Value("GE2")
  val GODFAT_REUSE            = Value("GF1")
  val EMMA_REUSE              = Value("EA1")
  val GINGER_REUSE            = Value("GI1")
  val ANGEL_CHOOSE            = Value("AN1")
  val ADECOY_INTIMATE         = Value("AD1")
  val BELLANDONA_CHOOSE       = Value("BE1")

  val BLACKCARD_DAGGER        = Value("B01")
  val BLACKCARD_LAMIRROR      = Value("B02")
  val BLACKCARD_MASK          = Value("B03")
  val BLACKCARD_DECLINE       = Value("B04")
  val BLACKCARD_FIREHORSE     = Value("B05")
  val BLACKCARD_SPLINTERED    = Value("B06")
  val BLACKCARD_EVILSWORD     = Value("B07")
  val BLACKCARD_GEMWAND       = Value("B08")
  val BLACKCARD_SUPPLYBOMB    = Value("B09")
  val BLACKCARD_EXPLODE       = Value("B10")

  val WHITECARD_TEA           = Value("W01")
  val WHITECARD_BALANCE       = Value("W02")
  val WHITECARD_FIREWORK      = Value("W03")
  val WHITECARD_FLYHIGH       = Value("W04")
  val WHITECARD_ENCHANTMENT   = Value("W05")
  val WHITECARD_MAGICSPIRIT   = Value("W06")
  val WHITECARD_EARTHQUAKE    = Value("W07")
  val WHITECARD_VOLCANIC      = Value("W08")
  val WHITECARD_TSUNAMI       = Value("W09")

  val GREENCARD_HUNTERHEAL2   = Value("G01")
  val GREENCARD_LIFEUNDER11_2 = Value("G02")

  // 已停用
  val NOUSE_ROOMFLAG_LIST = List(
    NO_HUNSOUL_SHAHEART
  )
  
  // 系統 
  val SYSTEM_LIST = List(
    TEST_MODE,
    WISH_ALIGN,
    WISH_ROLE,
    HATE_ROLE,
    ALL_NEUTRAL,
    NEUTRAL_BACK_1,
    NEUTRAL_BACK_2,
    INIT_LOCATION,
    INIT_GREEN,
    RANDOM_POSITION,
    FOUR_NEUTRAL
  )
  
  // 角色集合
  val ROLE_CLUMP_LIST = List(
    EXPANSION_ROLE,
    CUSTOM_ROLE,
    CUSTOM_CAT_ROLE
  )
  
  // 角色設定
  val ROLE_SETTING_LIST = List(
    ULTRASOUL_RAY,
    UNSEEN_RESIST,
    VAMPIRE_WEAKEN,
    VALKYRIE_ENHANCE,
    VGHOST_EXPAND,
    EMI_ENHANCE,
    EMI_SEND,
    ELLEN_HEAL,
    EVAN_HEAL,
    FRANKLIN_REUSE,
    GEORGE_REUSE,
    GEORGE_REUSE2,
    GODFAT_REUSE,
    EMMA_REUSE,
    GINGER_REUSE,
    ANGEL_CHOOSE,
    ADECOY_INTIMATE,
    BELLANDONA_CHOOSE
  )
  // 禁用角色
  val ROLE_CLOSE_LIST = List(
    NO_ULTRASOUL,
    NO_UNKNOWN,
    NO_UNSEEN,
    NO_UNDEAD,
    NO_VALKYRIE,
    NO_VAMPIRE,
    NO_VENGEFUL_GHOST,
    NO_VIPER,
    NO_WEREWOLF,
    NO_WIGHT,
    NO_WITCH,
    NO_WICKED,
    NO_BANE,
    NO_STARS,
    NO_FIGHTER,
    NO_BOROGOVE,
    NO_CLACKEN,
    NO_ANIMALBONES,
    NO_CONCUBINE,
    NO_MOSSEN,
    NO_MAGICIAN,
    NO_FALLOMEN,
    NO_ELLEN,
    NO_EMI,
    NO_EMMA,
    NO_EVAN,
    NO_FATHER_OCONNEL,
    NO_FENG,
    NO_FRANKLIN,
    NO_FUKA,
    NO_GEORGE,
    NO_GREGOR,
    NO_GINGER,
    NO_GODFAT,
    NO_MARS,
    NO_LION,
    NO_ARSIS,
    NO_SHINAI,
    NO_AICHA,
    NO_YAKALI,
    NO_AKI,
    NO_AMETSUKI,
    NO_LILIA,
    NO_CLOUDBOW,
    NO_AGNES,
    NO_ALLIE,
    NO_ANGEL,
    NO_ADECOY,
    NO_BELLANDONA,
    NO_BOB,
    NO_BOMB,
    NO_BRYAN,
    NO_CATHERINE,
    NO_CHARLES,
    NO_CASSANDRA,
    NO_CHESHIRE,
    NO_DAVID,
    NO_DANIEL,
    NO_DESPAIR,
    NO_DETECTIVE,
    NO_JUDGMENT,
    NO_SHAHEART,
    NO_HUNSOUL,
    NO_ADRIATIC,
    NO_MICAH,
    NO_SETH,
    NO_DRAGON,
    NO_LEON,
    NO_PUZZLE,
    NO_WESTLOBE,
    NO_LUBE,
    NO_TEL
  )
  // 綠卡
  val GREENCARD_LIST = List(
    GREENCARD_HUNTERHEAL2,
    GREENCARD_LIFEUNDER11_2
  )
  // 黑卡
  val BLACKCARD_LIST = List(
    BLACKCARD_DAGGER,
    BLACKCARD_LAMIRROR,
    BLACKCARD_MASK,
    BLACKCARD_DECLINE,
    BLACKCARD_FIREHORSE,
    BLACKCARD_SPLINTERED,
    BLACKCARD_EVILSWORD,
    BLACKCARD_GEMWAND,
    BLACKCARD_SUPPLYBOMB,
    BLACKCARD_EXPLODE
  )
  // 白卡
  val WHITECARD_LIST = List(
    WHITECARD_TEA,
    WHITECARD_BALANCE,
    WHITECARD_FIREWORK,
    WHITECARD_FLYHIGH,
    WHITECARD_ENCHANTMENT,
    WHITECARD_MAGICSPIRIT,
    WHITECARD_EARTHQUAKE,
    WHITECARD_VOLCANIC,
    WHITECARD_TSUNAMI
  )
  // 管理員測試房間不使用的選項
  val TESTMODE_CLOSE_OPTION_LIST = List(
      ALL_NEUTRAL,
      NEUTRAL_BACK_1,
      NEUTRAL_BACK_2,
      NO_ULTRASOUL,
      NO_UNKNOWN,
      NO_UNSEEN,
      NO_UNDEAD,
      NO_VALKYRIE,
      NO_VAMPIRE,
      NO_VENGEFUL_GHOST,
      NO_VIPER,
      NO_WEREWOLF,
      NO_WIGHT,
      NO_WITCH,
      NO_WICKED,
      NO_BANE,
      NO_STARS,
      NO_FIGHTER,
      NO_BOROGOVE,
      NO_CLACKEN,
      NO_ANIMALBONES,
      NO_CONCUBINE,
      NO_MOSSEN,
      NO_MAGICIAN,
      NO_FALLOMEN,
      NO_ELLEN,
      NO_EMI,
      NO_EMMA,
      NO_EVAN,
      NO_FATHER_OCONNEL,
      NO_FENG,
      NO_FRANKLIN,
      NO_FUKA,
      NO_GEORGE,
      NO_GREGOR,
      NO_GINGER,
      NO_GODFAT,
      NO_MARS,
      NO_LION,
      NO_ARSIS,
      NO_SHINAI,
      NO_AICHA,
      NO_YAKALI,
      NO_AKI,
      NO_AMETSUKI,
      NO_LILIA,
      NO_CLOUDBOW,
      NO_AGNES,
      NO_ALLIE,
      //NO_ANGELNO_ANGEL,
      NO_ADECOY,
      NO_BELLANDONA,
      NO_BOB,
      NO_BOMB,
      NO_BRYAN,
      NO_CATHERINE,
      NO_CHARLES,
      NO_CASSANDRA,
      NO_CHESHIRE,
      NO_DAVID,
      //NO_DANIEL,
      NO_DESPAIR,
      //NO_DETECTIVE,
      NO_JUDGMENT,
      NO_SHAHEART,
      NO_HUNSOUL,
      //NO_ADRIATIC,
      NO_MICAH,
      //NO_SETH,
      //NO_DRAGON,
      //NO_LEON,
      NO_PUZZLE,
      NO_WESTLOBE,
      NO_LUBE,
      NO_TEL,
      BLACKCARD_LAMIRROR,
      BLACKCARD_DECLINE,
      BLACKCARD_SPLINTERED,
      WHITECARD_TEA,
      WHITECARD_FIREWORK,
      WHITECARD_FLYHIGH
    )
  // 選項名稱
  val FLAGNAME_MAP   = Map(
    TEST_MODE               -> <code class="purple">測試</code>,
    //DEATH_LOOK            -> <code class="purple">靈</code>,

    EXPANSION_ROLE          -> <code class="blue">擴充</code>,
    CUSTOM_ROLE             -> <code class="blue">自製</code>,
    CUSTOM_CAT_ROLE         -> <code class="blue">參角</code>,

    NO_HUNSOUL_SHAHEART     -> <code class="red">多朵</code>,
    NO_ULTRASOUL            -> <code class="red">究極</code>,
    NO_UNKNOWN              -> <code class="red">不明</code>,
    NO_UNSEEN               -> <code class="red">隱形</code>,
    NO_UNDEAD               -> <code class="red">不死</code>,
    NO_VALKYRIE             -> <code class="red">女武</code>,
    NO_VAMPIRE              -> <code class="red">吸血</code>,
    NO_VENGEFUL_GHOST       -> <code class="red">復仇</code>,
    NO_VIPER                -> <code class="red">毒蛇</code>,
    NO_WEREWOLF             -> <code class="red">狼人</code>,
    NO_WIGHT                -> <code class="red">巫妖</code>,
    NO_WITCH                -> <code class="red">青蛙</code>,
    NO_WICKED               -> <code class="red">邪惡</code>,
    NO_BANE                 -> <code class="red">魔君</code>,
    NO_STARS                -> <code class="red">妖星</code>,
    NO_FIGHTER              -> <code class="red">鬥魂</code>,
    NO_BOROGOVE             -> <code class="red">儀轉</code>,
    NO_CLACKEN              -> <code class="red">深海</code>,
    NO_ANIMALBONES          -> <code class="red">亙古</code>,
    NO_CONCUBINE            -> <code class="red">傾城</code>,
    NO_MOSSEN               -> <code class="red">博士</code>,
    NO_MAGICIAN             -> <code class="red">夢饜</code>,
    NO_FALLOMEN             -> <code class="red">雷怒</code>,
    NO_ELLEN                -> <code class="red">精獵</code>,
    NO_EMI                  -> <code class="red">巫女</code>,
    NO_EMMA                 -> <code class="red">卡片</code>,
    NO_EVAN                 -> <code class="red">戀人</code>,
    NO_FATHER_OCONNEL       -> <code class="red">神父</code>,
    NO_FENG                 -> <code class="red">獵魔</code>,
    NO_FRANKLIN             -> <code class="red">護士</code>,
    NO_FUKA                 -> <code class="red">武術</code>,
    NO_GEORGE               -> <code class="red">武士</code>,
    NO_GREGOR               -> <code class="red">發明</code>,
    NO_GINGER               -> <code class="red">女戰</code>,
    NO_GODFAT               -> <code class="red">哥德</code>,
    NO_MARS                 -> <code class="red">戰神</code>,
    NO_LION                 -> <code class="red">獅王</code>,
    NO_ARSIS                -> <code class="red">光王</code>,
    NO_SHINAI               -> <code class="red">影忍</code>,
    NO_AICHA                -> <code class="red">神光</code>,
    NO_YAKALI               -> <code class="red">亞卡</code>,
    NO_AKI                  -> <code class="red">銀麟</code>,
    NO_AMETSUKI             -> <code class="red">影術</code>,
    NO_LILIA                -> <code class="red">黑魔</code>,
    NO_CLOUDBOW             -> <code class="red">穿雲</code>,
    NO_AGNES                -> <code class="red">善變</code>,
    NO_ALLIE                -> <code class="red">學生</code>,
    NO_ANGEL                -> <code class="red">天使</code>,
    NO_ADECOY               -> <code class="red">詛偶</code>,
    NO_BELLANDONA           -> <code class="red">懸賞</code>,
    NO_BOB                  -> <code class="red">盜賊</code>,
    NO_BOMB                 -> <code class="red">炸彈</code>,
    NO_BRYAN                -> <code class="red">拳擊</code>,
    NO_CATHERINE            -> <code class="red">魔瞳</code>,
    NO_CHARLES              -> <code class="red">殺人</code>,
    NO_CASSANDRA            -> <code class="red">賞金</code>,
    NO_CHESHIRE             -> <code class="red">柴貓</code>,
    NO_DAVID                -> <code class="red">盜墓</code>,
    NO_DANIEL               -> <code class="red">老人</code>,
    NO_DESPAIR              -> <code class="red">絕望</code>,
    NO_DETECTIVE            -> <code class="red">偵探</code>,
    NO_JUDGMENT             -> <code class="red">審判</code>,
    NO_SHAHEART             -> <code class="red">暗心</code>,
    NO_HUNSOUL              -> <code class="red">獵魂</code>,
    NO_ADRIATIC             -> <code class="red">野性</code>,
    NO_MICAH                -> <code class="red">墮牌</code>,
    NO_SETH                 -> <code class="red">掌管</code>,
    NO_DRAGON               -> <code class="red">寶珠</code>,
    NO_LEON                 -> <code class="red">課金</code>,
    NO_PUZZLE               -> <code class="red">隱禍</code>,
    NO_WESTLOBE             -> <code class="red">心靈</code>,
    NO_LUBE                 -> <code class="red">花仙</code>,
    NO_TEL                  -> <code class="red">倒影</code>,

    WISH_ALIGN              -> <code class="yellow">希陣</code>,
    WISH_ROLE               -> <code class="yellow">希角</code>,
    HATE_ROLE               -> <code class="yellow">厭角</code>,
    ALL_NEUTRAL             -> <code class="yellow">全中</code>,
    NEUTRAL_BACK_1          -> <code class="yellow">暗獵+1</code>,
    NEUTRAL_BACK_2          -> <code class="yellow">暗獵+2</code>,
    INIT_LOCATION           -> <code class="yellow">初位</code>,
    INIT_GREEN              -> <code class="yellow">初綠</code>,
    RANDOM_POSITION         -> <code class="yellow">亂位</code>,
    FOUR_NEUTRAL            -> <code class="yellow">四中</code>,

    ULTRASOUL_RAY           -> <code class="orange">究</code>,
    UNSEEN_RESIST           -> <code class="orange">隱</code>,
    VAMPIRE_WEAKEN          -> <code class="orange">吸</code>,
    VALKYRIE_ENHANCE        -> <code class="orange">武</code>,
    VGHOST_EXPAND           -> <code class="orange">復</code>,
    EMI_ENHANCE             -> <code class="orange">米1</code>,
    EMI_SEND                -> <code class="orange">米2</code>,
    ELLEN_HEAL              -> <code class="orange">蓮</code>,
    EVAN_HEAL               -> <code class="orange">伊</code>,
    FRANKLIN_REUSE          -> <code class="orange">弗</code>,
    GEORGE_REUSE            -> <code class="orange">喬</code>,
    GEORGE_REUSE2           -> <code class="orange">喬2</code>,
    GODFAT_REUSE            -> <code class="orange">哥</code>,
    EMMA_REUSE              -> <code class="orange">瑪</code>,
    GINGER_REUSE            -> <code class="orange">金</code>,
    ANGEL_CHOOSE            -> <code class="orange">天</code>,
    ADECOY_INTIMATE         -> <code class="orange">詛</code>,
    BELLANDONA_CHOOSE       -> <code class="orange">貝</code>,

    GREENCARD_HUNTERHEAL2   -> <code class="green">綠獵</code>,
    GREENCARD_LIFEUNDER11_2 -> <code class="green">綠弱</code>,

    BLACKCARD_DAGGER        -> <code class="black">匕首</code>,
    BLACKCARD_LAMIRROR      -> <code class="black">拉鏡</code>,
    BLACKCARD_MASK          -> <code class="black">封面</code>,
    BLACKCARD_DECLINE       -> <code class="black">殞落</code>,
    BLACKCARD_FIREHORSE     -> <code class="black">煉馬</code>,
    BLACKCARD_SPLINTERED    -> <code class="black">漩破</code>,
    BLACKCARD_EVILSWORD     -> <code class="black">邪骸</code>,
    BLACKCARD_GEMWAND       -> <code class="black">血杖</code>,
    BLACKCARD_SUPPLYBOMB    -> <code class="black">供炸</code>,
    BLACKCARD_EXPLODE       -> <code class="black">爆炸</code>,

    WHITECARD_TEA           -> <code class="white">分解</code>,
    WHITECARD_BALANCE       -> <code class="white">天秤</code>,
    WHITECARD_FIREWORK      -> <code class="white">煙火</code>,
    WHITECARD_FLYHIGH       -> <code class="white">平衡</code>,
    WHITECARD_ENCHANTMENT   -> <code class="white">結界</code>,
    WHITECARD_MAGICSPIRIT   -> <code class="white">幻靈</code>,
    WHITECARD_EARTHQUAKE    -> <code class="white">地震</code>,
    WHITECARD_VOLCANIC      -> <code class="white">火山</code>,
    WHITECARD_TSUNAMI       -> <code class="white">海嘯</code>
  )

  def flag_name(flag : RoomFlagEnum.Value) = {
    FLAGNAME_MAP.get(flag)
  }
  
  implicit def roomflagenum2String (en : RoomFlagEnum.Value) : String = en.toString
}

object RoomStatusEnum extends Enumeration {
  type RoomStatusEnum         = Value

  val WAITING                 = Value("W")
  val PLAYING                 = Value("P")
  val ENDED                   = Value("E")
  implicit def roomstatusenum2String (en : RoomStatusEnum.Value) : String = en.toString
}

object RoomVictoryEnum extends Enumeration {
  type RoomVictoryEnum        = Value

  val NONE                    = Value("")

  val SHADOW_WIN              = Value("S")
  val HUNTER_WIN              = Value("H")
  val DUAL_WIN                = Value("D")
  val NEUTRAL_WIN             = Value("N")
  val LOVER_WIN               = Value("L")

  val DRAW                    = Value("0")
  val ABANDONED               = Value("1")

  val VICTORY_MAP = Map(
    NONE                    -> "無",
    SHADOW_WIN              -> "暗影",
    HUNTER_WIN              -> "獵人",
    DUAL_WIN                -> "雙重",
    NEUTRAL_WIN             -> "中立",
    LOVER_WIN               -> "戀人",
    DRAW                    -> "平手",
    ABANDONED               -> "廢棄"
  )

  def victory_name(flag : RoomVictoryEnum.Value) : String = {
    VICTORY_MAP.get(flag).getOrElse("")
  }

  def victory_name(flag_str : String) : String = {
    val flag = try {RoomVictoryEnum.withName(flag_str)}
      catch {case e: Exception => NONE}
    victory_name(flag)
  }

  implicit def roomvictoryenum2String (en : RoomVictoryEnum.Value) : String = en.toString
}

object ForceUpdateEnum extends Enumeration {
  type ForceUpdateEnum        = Value

  val NONE                    = Value("")

  val GO_OUT_LINK             = Value("G")
  val ACTION_BAR              = Value("A")
  val USER_TABLE              = Value("U")
  val TIME_TABLE              = Value("I")
  val LOCATION_TABLE          = Value("L")
  val TALK_TABLE              = Value("T")
  val CARD_TABLE              = Value("C")
  val MUSIC                   = Value("M")

  implicit def forceupdateenum2String (en : ForceUpdateEnum.Value) : String = en.toString
}