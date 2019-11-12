package org.plummtw.shadowhunter.enum

object UserEntryRoomFlagEnum extends Enumeration {
  type UserEntryRoomFlagEnum = Value
  
  val  VOTED                 = Value("V")
  val  AUTOVOTED             = Value("A")
  val  SKIPPED               = Value("S")
  val  SUDDENDEATH           = Value("D")
}

object UserEntryRoleFlagEnum extends Enumeration {
  type UserEntryRoleFlagEnum = Value
  
  val  AMBUSH                = Value("A")
  val  ROLE_SKILL_USED       = Value("U") // 永久用過技能
  val  ROLE_MOVESKILL_USED   = Value("M") // 該回合用過技能
  val  REVIVED_AURA          = Value("R") // 不死族靈氣
  val  ENHANCED              = Value("E")
}

object UserEntryFlagEnum extends Enumeration {
  type UserEntryFlagEnum     = Value
  
  val  SEALED                = Value("S")
  val  VICTORY               = Value("V")
  val  VICTORY2              = Value("W")
  
  val  ADVENT                = Value("A") // 降臨
  val  CHOCOLATE             = Value("C") // 巧克力
  val  DIABOLIC              = Value("D") // 魔鬼儀式
  
  val  GUARDIAN              = Value("G") // 守護天使
  val  BARRIER               = Value("B") // 防護罩
  
  val  LOVER                 = Value("L") // 戀人
  val  FROG                  = Value("F") // 青蛙術
  
  val  POISON                = Value("P") // 毒蛇
  val  TAUNT                 = Value("T") // 嘲諷
  
  val  REASONAED             = Value("Z") // 推理陣營
  val  REVIVED               = Value("R") // 
  
  val  MISSED                = Value("M") // 匕首
  val  ENCHANTMENT           = Value("E") // 結界
  val  FIREWORK              = Value("I") // 煙火
  val  STICKY                = Value("K") // 黏稠
  val  SEAL                  = Value("J") // 訃影
  val  SLOW                  = Value("O") // 遲緩
  
  val  FAITH                 = Value("H") // 精靈共鳴
  val  MAGICSPIRIT           = Value("X") // 幻靈
  val  REVIVE                = Value("N") // 復活
  val  RESENTFUL             = Value("U") // 憤槌
  val  SOULHUNT              = Value("Y") // 魂狩
}


