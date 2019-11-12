package org.plummtw.shadowhunter.data

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.card._

class RoleData(val enum : RoleEnum.Value, val name : String, val life : Int, val side : RoleSideEnum.Value) {
  def role_enum = enum
  def role_name = name
  def role_life = life
  def role_side = side

  def cfield = {
    <span class={RoleSideEnum.get_roleside_color(role_side)}>[{role_name}]</span>
  }

  def simple_cfield = {
    <span class={RoleSideEnum.get_roleside_color(role_side)}>[{role_name.substring(0,1)}]</span>
  }

  def movement_skill : List[ActionData] = List(ActionNoAction)
  def attack_skill     : ActionData = ActionNoAction
  def free_skill      : ActionData = ActionNoAction
  def post_skill      : ActionData = ActionNoAction
}

class RoleShadow(override val enum : RoleEnum.Value, override val name : String, override val life : Int) extends RoleData(enum, name, life, RoleSideEnum.SHADOW)

class RoleHunter(override val enum : RoleEnum.Value, override val name : String, override val life : Int) extends RoleData(enum, name, life, RoleSideEnum.HUNTER)

class RoleNeutral(override val enum : RoleEnum.Value, override val name : String, override val life : Int) extends RoleData(enum, name, life, RoleSideEnum.NEUTRAL)

object RoleNone extends RoleData(RoleEnum.NONE, "不指定", 20, RoleSideEnum.NONE )
object RoleNoEffect extends RoleData(RoleEnum.NOEFFECT, "無效果", 20, RoleSideEnum.NONE )

// Shadow
object RoleUltraSoul extends RoleShadow(RoleEnum.ULTRASOUL, "究極靈魂", 11) {
  override def movement_skill = List(ActionUltrasoulRay, ActionUltrasoulUray)
}
object RoleUnknown extends RoleShadow(RoleEnum.UNKNOWN, "不明", 11) {
  override def free_skill = ActionUnknownDeceive
}
object RoleUnseen extends RoleShadow(RoleEnum.UNSEEN, "隱形人", 11)
object RoleUndead extends RoleShadow(RoleEnum.UNDEAD, "不死族", 11)

object RoleValkyrie extends RoleShadow(RoleEnum.VALKYRIE, "女武神", 13)
object RoleVampire extends RoleShadow(RoleEnum.VAMPIRE, "吸血鬼", 13)
object RoleVengefulGhost extends RoleShadow(RoleEnum.VENGEFUL_GHOST, "復仇鬼", 13)
object RoleViper extends RoleShadow(RoleEnum.VIPER, "毒蛇", 13)
object RoleWerewolf extends RoleShadow(RoleEnum.WEREWOLF, "狼人", 14) {
  override def free_skill = ActionWerewolfAmbush
}
object RoleWight extends RoleShadow(RoleEnum.WIGHT, "巫妖", 14) {
  override def post_skill = ActionWightManipulate
}
object RoleWitch extends RoleShadow(RoleEnum.WITCH, "女巫", 14)



object RoleWicked extends RoleShadow(RoleEnum.WICKED, "邪惡", 14)

object RoleBane extends RoleShadow(RoleEnum.BANE, "弒魔", 13)

object RoleStars extends RoleShadow(RoleEnum.STARS, "菲爾特", 11)

object RoleFighter extends RoleShadow(RoleEnum.FIGHTER, "伽門羅", 13) {
  override def movement_skill = List(ActionFighterStrike)
}

object RoleBorogove extends RoleShadow(RoleEnum.BOROGOVE, "波若哥夫", 13)

object RoleClacken extends RoleShadow(RoleEnum.CLACKEN, "克拉肯", 11) {
  override def movement_skill = List(ActionClackenCapture)
}

//object RoleBorogove extends RoleShadow(RoleEnum.BOROGOVE, "魔女", 13)

object RoleAnimalBones extends RoleShadow(RoleEnum.ANIMALBONES, "獸骸", 11)

object RoleConcubine extends RoleShadow(RoleEnum.CONCUBINE, "妖妃", 11)

object RoleMossen extends RoleShadow(RoleEnum.MOSSEN, "魔森", 13)

object RoleMagician extends RoleShadow(RoleEnum.MAGICIAN, "魔魂師", 11)

object RoleFallOmen extends RoleShadow(RoleEnum.FALLOMEN, "墮天魔", 13)

// Hunter
object RoleEllen extends RoleHunter(RoleEnum.ELLEN, "艾蓮", 10) {
  override def movement_skill = List(ActionEllenCurseChain)
}
object RoleEmi extends RoleHunter(RoleEnum.EMI, "艾米", 10)
object RoleEmma extends RoleHunter(RoleEnum.EMMA, "艾瑪", 10)
object RoleEvan extends RoleHunter(RoleEnum.EVAN, "伊凡", 10) {
  override def movement_skill = List(ActionEvanBraceup)
}
object RoleFatherOconnel extends RoleHunter(RoleEnum.FATHER_OCONNEL, "歐肯奈", 12) {
  override def movement_skill = List(ActionFatherOconnelPray)
}

object RoleFranklin extends RoleHunter(RoleEnum.FRANKLIN, "弗蘭克林", 12) {
  override def movement_skill = List(ActionFranklinLightning)
}
object RoleFuka extends RoleHunter(RoleEnum.FUKA, "楓花", 12) {
  override def movement_skill = List(ActionFukaDynamiteHeal)
}

object RoleFeng extends RoleHunter(RoleEnum.FENG, "馮大師", 12) {
  override def attack_skill = ActionFengKikou
}

object RoleGeorge extends RoleHunter(RoleEnum.GEORGE, "喬治", 12) {
  override def movement_skill = List(ActionGeorgeDemolish)
}
object RoleGregor extends RoleHunter(RoleEnum.GREGOR, "葛瑞格", 14) {
  override def post_skill = ActionGregorBarrier
}

object RoleGinger extends RoleHunter(RoleEnum.GINGER, "金格", 14) {
  override def movement_skill = List(ActionGingerResentful)
}

object RoleGodfat extends RoleHunter(RoleEnum.GODFAT, "哥德法", 12) {
  override def movement_skill = List(ActionGodfatExchange)
}

object RoleMars extends RoleHunter(RoleEnum.MARS, "修特", 14)

object RoleLion extends RoleHunter(RoleEnum.LION, "特羅修", 12)

object RoleArsis extends RoleHunter(RoleEnum.ARSIS, "阿爾西斯", 12)

object RoleShinai extends RoleHunter(RoleEnum.SHINAI, "希奈", 12)

object RoleAicha extends RoleHunter(RoleEnum.AICHA, "艾夏", 12) {
  override def post_skill = ActionAichaGrasp
}

object RoleYakali extends RoleHunter(RoleEnum.YAKALI, "亞卡莉", 12)

object RoleAki extends RoleHunter(RoleEnum.AKI, "亞希", 12)

object RoleAmetsuki extends RoleHunter(RoleEnum.AMETSUKI, "雨月", 10)  {
  override def movement_skill = List(ActionAmetsukiCurse)
}


object RoleLilia extends RoleHunter(RoleEnum.LILIA, "莉莉雅", 12)


object RoleCloudBow extends RoleHunter(RoleEnum.CLOUDBOW, "小彩", 12)

// Neutral
object RoleAgnes extends RoleNeutral(RoleEnum.AGNES, "愛格妮絲", 8)
object RoleAllie extends RoleNeutral(RoleEnum.ALLIE, "愛莉", 8)  {
  override def movement_skill = List(ActionAllieMotherLove)
}
object RoleAngel extends RoleNeutral(RoleEnum.ANGEL, "天使", 8)  {
  override def free_skill = ActionAngelReincarnate
}
object RoleADecoy extends RoleNeutral(RoleEnum.ADECOY, "詛咒人偶", 8) {
  override def movement_skill = List(ActionADecoyTaunt)
}


object RoleBellandona extends RoleNeutral(RoleEnum.BELLANDONA, "貝爾多娜", 10)  {
}
object RoleBob extends RoleNeutral(RoleEnum.BOB, "鮑伯", 10)
object RoleBomb extends RoleNeutral(RoleEnum.BOMB, "波姆", 10) {
  override def movement_skill = List(ActionBombBomb)
}
object RoleBryan extends RoleNeutral(RoleEnum.BRYAN, "布萊恩", 10)
object RoleCatherine extends RoleNeutral(RoleEnum.CATHERINE, "凱瑟琳", 11)
object RoleCharles extends RoleNeutral(RoleEnum.CHARLES, "查理斯", 11) {
  override def post_skill = ActionCharlesBloodfeast
}
object RoleCassandra extends RoleNeutral(RoleEnum.CASSANDRA, "卡珊卓", 11) {
  override def movement_skill = List(ActionCassandraFateChange)
}
object RoleCheshire extends RoleNeutral(RoleEnum.CHESHIRE, "柴郡貓", 11)


object RoleDavid extends RoleNeutral(RoleEnum.DAVID, "大衛", 13) {
  override def free_skill = ActionDavidGravedig

  def WIN_EQUIPMENT_LIST = List(WCardTalisman,
    WCardHolyRobe, WCardSilverRosary, WCardLanceOfLonginus)

  def WIN_EQUIPMENT_LIST2 = List(WCardFortuneBrooch, WCardTalisman,
    WCardHolyRobe, WCardSilverRosary, WCardLanceOfLonginus, WCardMysticCompass,
    WCardBalance)
}


object RoleDaniel extends RoleNeutral(RoleEnum.DANIEL, "丹尼爾", 13)

object RoleDespair extends RoleNeutral(RoleEnum.DESPAIR, "絕望", 13)

object RoleDetective extends RoleNeutral(RoleEnum.DETECTIVE, "莉可", 13) {
  override def movement_skill = List(ActionDetectiveReasonA, ActionDetectiveReasonR)
}
//自製 Judgment
object RoleJudgment extends RoleNeutral(RoleEnum.JUDGMENT, "審判", 12)

object RoleShaHeart extends RoleNeutral(RoleEnum.SHAHEART, "朵伊", 9)

object RoleHunsoul extends RoleNeutral(RoleEnum.HUNSOUL, "多提", 11)

object RoleAdriatic extends RoleNeutral(RoleEnum.ADRIATIC, "亞德利斯", 11)

object RoleMicah extends RoleNeutral(RoleEnum.MICAH, "夏彌加", 10) {
  override def movement_skill = List(ActionMicahConfused)
}

object RoleSeth extends RoleNeutral(RoleEnum.SETH, "塞特", 12) {
  override def movement_skill = List(ActionSethControl)
}

object RoleDragon extends RoleNeutral(RoleEnum.DRAGON, "神龍", 16)

object RoleLeon extends RoleNeutral(RoleEnum.LEON, "萊昂", 11) {
  override def movement_skill = List(ActionLeonCharges, ActionLeonUse)
}

object RolePuzzle extends RoleNeutral(RoleEnum.PUZZLE, "謎", 11) {
  override def movement_skill = List(ActionPuzzleSpike)
}

object RoleWestLobe extends RoleNeutral(RoleEnum.WESTLOBE, "西露貝貝", 11) {
  override def movement_skill = List(ActionWestLobePry)
}

object RoleLube extends RoleNeutral(RoleEnum.LUBE, "露比", 11) {
  override def movement_skill = List(ActionLubeResponse)
  // ActionFatherOconnelPray
}

object RoleTel extends RoleNeutral(RoleEnum.TEL, "特爾", 10) {
  override def movement_skill = List(ActionTelWaterMirror)
}

/*
<a href="#role_none">不指定</a>
<a href="#role_noeffect">無效果</a>

// shadow
<a href="#role_unknown">不明</a>
<a href="#role_vampire">吸血鬼</a>
<a href="#role_werewolf">狼人</a>

<a href="#role_ultrasoul">究極靈魂</a>
<a href="#role_valkyrie">女武神</a>
<a href="#role_wight">巫妖</a>

<a href="#role_unseen">隱形人</a>
<a href="#role_vengefulghost">復仇鬼</a>
<a href="#role_witch">女巫</a>

<a href="#role_undead">不死族</a>
<a href="#role_viper">毒蛇</a>
<a href="#role_wicked">邪惡</a>

<a href="#role_bane">弒魔</a>
<a href="#role_stars">菲爾特</a>
<a href="#role_fighter">伽門羅</a>

<a href="#role_borogove">波若哥夫</a>
<a href="#role_clacken">克拉肯</a>
<a href="#role_animalbones">獸骸</a>

<a href="#role_concubine">妖妃</a>
<a href="#role_magician">魔魂師</a>
<a href="#role_fallomen">墮天魔</a>

// hunter

<a href="#role_emi">艾米</a>
<a href="#role_franklin">弗蘭克林</a>
<a href="#role_george">喬治</a>

<a href="#role_ellen">艾蓮</a>
<a href="#role_fuka">楓花</a>
<a href="#role_gregor">葛瑞格</a>

<a href="#role_evan">伊凡</a>
<a href="#role_fatheroconnel">歐肯奈</a>
<a href="#role_ginger">金格</a>

<a href="#role_emma">艾瑪</a>
<a href="#role_feng">馮大師</a>
<a href="#role_godfat">哥德法</a>

<a href="#role_mars">修特</a>
<a href="#role_lion">特羅修</a>
<a href="#role_arsis">阿爾西斯</a>

<a href="#role_shinai">希奈</a>
<a href="#role_aicha">艾夏</a>
<a href="#role_aki">亞希</a>

<a href="#role_ametsuki">雨月</a>
<a href="#role_lilia">莉莉雅</a>
<a href="#role_cloudbow">小彩</a>


//<a href="#role_yakali">亞卡莉</a>

// neutral
<a href="#role_allie">愛莉</a>
<a href="#role_bob">鮑伯</a>
<a href="#role_charles">查理斯</a>
<a href="#role_daniel">丹尼爾</a>

<a href="#role_agnes">愛格妮絲</a>
<a href="#role_bryan">布萊恩</a>
<a href="#role_catherine">凱瑟琳</a>
<a href="#role_david">大衛</a>

<a href="#role_angel">天使</a>
<a href="#role_bellandona">貝爾多娜</a>
<a href="#role_cassandra">卡珊卓</a>
<a href="#role_despair">絕望</a>

<a href="#role_bomb">波姆</a>
<a href="#role_adecoy">詛咒人偶</a>
<a href="#role_cheshire">柴郡貓</a>
<a href="#role_detective">莉可</a>

<a href="#role_judgment">審判</a>
<a href="#role_shaheart">朵伊</a>
<a href="#role_hunsoul">多提</a>
<a href="#role_adriatic">亞德利斯</a>

<a href="#role_micah">夏彌加</a>
<a href="#role_seth">塞特</a>
<a href="#role_dragon">神龍</a>
<a href="#role_leon">萊昂</a>

<a href="#role_puzzle">謎</a>
<a href="#role_westlobe">西露貝貝</a>
<a href="#role_lube">露比</a>
<a href="#role_tel">特爾</a>

object Role(.*) extends Role(.*)\(RoleEnum.(.*), \"(.*)\", (.*)\)
*/