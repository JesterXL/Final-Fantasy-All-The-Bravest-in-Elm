module BattleUtils exposing (..)
import Random

PERFECT_HIT_RATE : Int
PERFECT_HIT_RATE = 255

getRandomNumberFromRange : Int -> Int -> Int -> (Int, Random.Seed)
getRandomNumberFromRange start end seed =
  Random.step (Random.int start end) (Random.initialSeed seed)

divide: Int -> Int -> Int
divide a b = (//) a b

-- Step 1

type Attack = 
    PlayerPhysicalAttack
    | PlayerPhysicalMultipleAttack
    | PlayerMagicalAttack 
    | PlayerMagicalMultipleAttack
    | PlayerHealingAttack
    | MonsterPhysicalAttack
    | MonsterPhysicalMultipleAttack
    | MonsterMagicalAttack
    | MonsterMagicalMultipleAttack

getDamageStep1A : Attack -> Int -> Int -> Int -> Int -> Int
getDamageStep1A attack spellPower magicPower level damage =
    case attack of
        PlayerMagicalAttack ->
            spellPower * 4 + (level * magicPower * spellPower // 32) + damage
        _ -> 
            damage

getDamageStep1B : Attack -> Int -> Int -> Int -> Int -> Int
getDamageStep1B attack spellPower magicPower level damage =
    case attack of
        MonsterMagicalAttack ->
            spellPower * 4 + (level * (magicPower * 3//2) * spellPower // 32) + damage
        _ -> 
            damage

-- Step 1C
clampVigor : Int -> Int
clampVigor vigor =
    if vigor >= 128 then
        255
    else
        vigor

doubleVigor : Int -> Int
doubleVigor vigor = vigor * 2 |> clampVigor

getAttackPower : Int -> Int -> Int
getAttackPower battlePower vigor =
    clampVigor(battlePower + doubleVigor(vigor))

gauntletIncreasedAttack: Int -> Int -> Bool -> Int
gauntletIncreasedAttack attackPower battlePower equippedWithGauntlet =
    if equippedWithGauntlet == True then
        battlePower * 3 //4 + attackPower
    else
        attackPower

getAttackPowerFromEquipment : Int -> Int -> Bool -> Int
getAttackPowerFromEquipment battlePower vigor equippedWithGauntlet =
    gauntletIncreasedAttack ( getAttackPower battlePower vigor ) battlePower equippedWithGauntlet

getDamageStep1CBasicDamage: Int -> Int -> Int -> Int
getDamageStep1CBasicDamage battlePower level attackPower =
    battlePower + ( (level * level * attackPower) // 256) * 3 // 2

getOfferingDecreasedDamage : Bool -> Int -> Int
getOfferingDecreasedDamage equippedWithOffering damage =
    if equippedWithOffering == True then
        damage // 2
    else
        damage

getDamageFromGenjiGlove : Bool -> Bool -> Bool -> Int -> Int
getDamageFromGenjiGlove standardFightAttack equippedWithGenjiGlove oneOrZeroWeapons damage =
    case standardFightAttack of
        True -> case equippedWithGenjiGlove of
                    True -> case oneOrZeroWeapons of
                                True -> Basics.ceiling(toFloat(damage) * (3 / 4))
                                False -> damage
                    False -> damage
        False -> damage

addDamage : Int -> Int -> Int
addDamage a b = a + b

getDamageStep1C : Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> Int -> Int
getDamageStep1C vigor battlePower level equippedWithGauntlet equippedWithOffering equippedWithGenjiGlove oneOrZeroWeapons damage =
    let
        attackPower = getAttackPowerFromEquipment battlePower vigor equippedWithGauntlet
        getOfferingDecreasedDamageF = getOfferingDecreasedDamage equippedWithOffering
        getDamageFromGenjiGloveF = getDamageFromGenjiGlove True equippedWithGenjiGlove oneOrZeroWeapons
    in
        getDamageStep1CBasicDamage battlePower level attackPower
        |> getOfferingDecreasedDamageF
        |> getDamageFromGenjiGloveF
        |> addDamage damage

getDamageStep1D: Attack -> Int -> Int -> Int -> Int -> Int -> Int
getDamageStep1D attack vigor battlePower level attackPower damage =
    case attack of
        MonsterPhysicalAttack ->
            level * level * (battlePower * 4 + vigor) // 256 + damage
        _ -> 
            damage


getDamageStep1 : Attack -> Int -> Int -> Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> Int
getDamageStep1 attack vigor battlePower spellPower magicPower level equippedWithGauntlet equippedWithOffering equippedWithGenjiGlove oneOrZeroWeapons =
    let
        attackPower = getAttackPowerFromEquipment battlePower vigor equippedWithGauntlet
        getDamageStep1AP = getDamageStep1A attack spellPower magicPower level 0
        getDamageStep1BP = getDamageStep1B attack spellPower magicPower level
        getDamageStep1CP = getDamageStep1C vigor battlePower level equippedWithGauntlet equippedWithOffering equippedWithGenjiGlove oneOrZeroWeapons
        getDamageStep1DP = getDamageStep1D attack vigor battlePower level attackPower
    in
        getDamageStep1AP
        |> getDamageStep1BP
        |> getDamageStep1CP
        |> getDamageStep1DP

getRandomMonsterVigor : Int -> Int
getRandomMonsterVigor seed =
    Tuple.first (getRandomNumberFromRange 56 63 1)

-- Step 2

getStep2SingleDamageBonus : Int -> Int
getStep2SingleDamageBonus damage = damage * 5 // 4

getStep2DoubleDamageBonus : Int -> Int
getStep2DoubleDamageBonus damage = (damage // 4) + (damage // 4)

getDamageStep2 : Attack -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> Int
getDamageStep2 attack equippedWithAtlasArmlet equippedWith1HeroRing equippedWith2HeroRings equippedWith1Earring equippedWith2Earrings damage =
    case attack of
        PlayerPhysicalAttack ->
            if equippedWithAtlasArmlet || equippedWith1HeroRing then
                getStep2SingleDamageBonus damage
            else
                damage
        PlayerMagicalAttack ->
            if equippedWith1Earring || equippedWith1HeroRing then
                getStep2SingleDamageBonus damage
            else if equippedWith2Earrings || equippedWith2HeroRings then
                getStep2DoubleDamageBonus damage
            else
                damage
        _ -> 
            damage

-- Step 3

getDamageStep3 : Attack -> Int -> Int
getDamageStep3 attack damage =
    case attack of
        PlayerMagicalMultipleAttack ->
            damage // 2
        _ ->
            damage

-- Step 4

getDamageStep4 : Bool -> Int -> Int
getDamageStep4 attackerIsInBackRow damage =
    if attackerIsInBackRow == True then 
        damage // 2 
    else
        damage


-- Step 5

getCrtiicalHit : Int -> Bool
getCrtiicalHit seed = Tuple.first (getRandomNumberFromRange 1 32 seed) == 32

getMorphStatusMultiplier : Bool -> number -> number
getMorphStatusMultiplier hasMorphStatus multiplier = 
    if hasMorphStatus == True then
        multiplier + 3
    else
        multiplier

getBerserkStatusAndPhysicalAttackMultiplier : Attack -> Bool -> number -> number
getBerserkStatusAndPhysicalAttackMultiplier attack hasBerserkStatus multiplier =
    if hasBerserkStatus == True then
        case attack of
            PlayerPhysicalAttack -> multiplier + 2
            _ -> multiplier
    else
        multiplier

getCriticalHitMultiplier : Bool -> number -> number
getCriticalHitMultiplier isCriticalHit multiplier =
    if isCriticalHit == True then
        multiplier + 3
    else
        multiplier

getStep5Damage : Int -> Int -> Int
getStep5Damage multiplier damage = (damage // 2) * multiplier

getStep5DamageMultiplier : Attack -> Bool -> Bool -> Bool -> number
getStep5DamageMultiplier attack hasMorphStatus hasBerserkStatus isCriticalHit =
    let
        multiplier = 0
    in
        getMorphStatusMultiplier hasMorphStatus multiplier
        |> getBerserkStatusAndPhysicalAttackMultiplier attack hasBerserkStatus
        |> getCriticalHitMultiplier isCriticalHit

getDamageStep5 : Attack -> Bool -> Bool -> Bool -> Int -> Int
getDamageStep5 attack hasMorphStatus hasBerserkStatus isCriticalHit damage =
    getStep5Damage (
        getStep5DamageMultiplier attack hasMorphStatus hasBerserkStatus isCriticalHit
    ) damage

-- Step 6

-- 224 was what I had defaulted to
getDamageModificationsVariance : Int -> Int
getDamageModificationsVariance seed = Tuple.first (getRandomNumberFromRange 224 255 seed)

getDamageStep6Basic : Int -> Int -> Int
getDamageStep6Basic variance damage = damage * variance // 256 + 1

getStep6DefenseModification : Attack -> Int -> Int -> Int -> Int
getStep6DefenseModification attack defense magicalDefense damage =
    case attack of
        PlayerPhysicalAttack ->
            damage * (255 - defense) // 256 + 1
        PlayerMagicalAttack ->
            damage * (255 - magicalDefense) // 256 + 1
        _ ->
            damage

getStep6SafeShellModificationDamage : Int -> Int
getStep6SafeShellModificationDamage damage = damage * 170 // 256 + 1

getStep6SafeShellModification : Attack -> Bool -> Bool -> Int -> Int
getStep6SafeShellModification attack targetHasSafeStatus targetHasShellStatus damage =
    case attack of
        PlayerPhysicalAttack ->
            if targetHasSafeStatus == True then
                getStep6SafeShellModificationDamage damage
            else
                damage
        PlayerMagicalAttack ->
            if targetHasShellStatus == True then
                getStep6SafeShellModificationDamage damage
            else
                damage
        _ ->
            damage

getStep6DefendingModification : Attack -> Bool -> Int -> Int
getStep6DefendingModification attack targetDefending damage = 
    case attack of
        PlayerPhysicalAttack ->
            if targetDefending == True then
                damage // 2
            else
                damage
        _ ->
            damage

getStep6BackRowModification : Attack -> Bool -> Int -> Int
getStep6BackRowModification attack targetIsInBackRow damage =
    case attack of
        PlayerPhysicalAttack ->
            if targetIsInBackRow == True then
                damage // 2
            else
                damage
        _ ->
            damage

getStep6MorphModification : Attack -> Bool -> Int -> Int
getStep6MorphModification attack targetHasMorphStatus damage = 
    case attack of
        PlayerMagicalAttack ->
            if targetHasMorphStatus == True then
                damage // 2
            else
                damage
        _ ->
            damage

getStep6HealingAttack : Attack -> Bool -> Bool -> Int -> Int
getStep6HealingAttack attack targetIsSelf targetIsCharacter damage =
    case attack of
        PlayerHealingAttack ->
            if targetIsSelf == True && targetIsCharacter == True then
                damage // 2
            else
                damage
        _ ->
            damage

getStep6Damage
    : Attack
    -> Int
    -> Int
    -> Int
    -> Bool
    -> Bool
    -> Bool
    -> Bool
    -> Bool
    -> Bool
    -> Bool
    -> Int
    -> Int
getStep6Damage attack variance defense magicalDefense targetHasSafeStatus targetHasShellStatus targetDefending targetIsInBackRow targetHasMorphStatus targetIsSelf targetIsCharacter damage =
    getDamageStep6Basic variance damage
    |> getStep6DefenseModification attack defense magicalDefense
    |> getStep6SafeShellModification attack targetHasSafeStatus targetHasShellStatus
    |> getStep6DefendingModification attack targetDefending 
    |> getStep6BackRowModification attack targetIsInBackRow
    |> getStep6MorphModification attack targetHasMorphStatus
    |> getStep6HealingAttack attack targetIsSelf targetIsCharacter


-- Step 7

getStep7Damage : Attack -> Bool -> Int -> Int
getStep7Damage attack hittingTargetsBack damage =
    case attack of
        PlayerPhysicalAttack ->
            if hittingTargetsBack == True then
                (damage // 2) * 1 + damage
            else
                damage
        _ ->
            damage

-- Step 8

getDamageStep8 : Bool -> Int -> Int
getDamageStep8 targetHasPetrifyStatus damage =
    if targetHasPetrifyStatus == True then
        0
    else
        damage


-- Step 9

type ElementEffect =
    ElementHasBeenNullified
    | TargetAbsorbsElement
    | TargetIsImmuneToElement
    | TargetIsResistantToElement
    | TargetIsWeakToElement

getDamageStep9 : ElementEffect -> Int -> Int
getDamageStep9 elementEffect damage =
    case elementEffect of
        ElementHasBeenNullified -> 0
        TargetAbsorbsElement -> damage * -1
        TargetIsImmuneToElement -> 0
        TargetIsResistantToElement -> damage // 2
        TargetIsWeakToElement -> damage * 2


--

getRandomHitOrMissvalue : Int -> Int
getRandomHitOrMissvalue seed = Tuple.first (getRandomNumberFromRange 0 99 seed)

getRandomStaminaHitOrMissValue : Int -> Int
getRandomStaminaHitOrMissValue seed = Tuple.first (getRandomNumberFromRange 0 127 seed)

getRandomImageStatusRemoval : Int -> Int
getRandomImageStatusRemoval seed = Tuple.first (getRandomNumberFromRange 0 3 seed)

getRemoveImageStatus : Int -> Bool
getRemoveImageStatus seed = Tuple.first (getRandomNumberFromRange 1 4 seed) == 4

getMonsterStamina : Int -> Int
getMonsterStamina maxHitPoints = (maxHitPoints // 512) + 16 |> clamp 0 40

-- Get Hit

type AttackResult removeImageStatus = Hit | Miss | Unknown

physicalAttackAgainstClearTarget targetHasClearStatus = 
    if targetHasClearStatus then
        Miss
    else
        Unknown

magicalAttackAgainstClearTarget targetHasClearStatus = 
    if targetHasClearStatus then
        Hit
    else
        Unknown

anyAttackWoundProtectMissDeath protectedFromWound attackMissesDeathProtectedTargets =
    if protectedFromWound && attackMissesDeathProtectedTargets then
        Miss
    else
        Unknown

magicalAttackSpellUnblockable spellUnblockable = if spellUnblockable then Hit else Unknown

-- No Special Attacks --

attackUnmissableAgainstTarget targetHasSleepStatus targetHasPetrifyStatus targetHasFreezeStatus targetHasStopStatus =
    if targetHasSleepStatus || targetHasPetrifyStatus || targetHasFreezeStatus || targetHasStopStatus then
        Hit
    else
        Unknown

physicalAttackBack backOfTarget = if backOfTarget then Hit else Unknown

perfectHitRate hitRate = if hitRate == PERFECT_HIT_RATE then Hit else Unknown

physicalAttackAgainstImageStatus targetHasImageStatus seed = 
    if targetHasImageStatus then
        if getRemoveImageStatus seed == True then
            Hit True
        else
            Miss
    else
        Unknown

getStep4eDefenseType isPhysicalAttack magicBlock defense = 
    if isPhysicalAttack then defense else magicBlock
getStep4eBaseBlockValueFromBlock block = (255 - block * 2) + 1
clampStep4eBlockValue block = clamp 1 255 block
getHitFromBlock isPhysicalAttack magicBlock defense hitRate randomHitOrMissValue =
    let
        block = getStep4eDefenseType isPhysicalAttack magicBlock defense
        clampedBlock = clampStep4eBlockValue block
    in
        if ( (hitRate * clampedBlock) // 256) > randomHitOrMissValue then
            Hit
        else
            Miss

-- Special Attacks --

getStep5bStaminaHitOrMiss targetStamina randomStaminaHitOrMiss hitInStep5a =
    if targetStamina >= randomStaminaHitOrMiss then
        Miss
    else if hitInStep5a then
        Hit
    else
        Miss

getHit randomHitOrMissValue
    randomStaminaHitOrMissValue
    attack
    targetHasClearStatus
    protectedFromWound
    attackMissesDeathProtectedTargets
    attackCanBeBlockedByStamina
    spellUnblockable
    targetHasSleepStatus
    targetHasPetrifyStatus
    targetHasFreezeStatus
    targetHasStopStatus
    hittingTargetsBack
    targetHasImageStatus
    hitRate
    targetDefense
    targetMagicBlock
    targetStamina
    specialAttackType =

    case attack of
        PlayerPhysicalAttack ->
            if physicalAttackAgainstClearTarget targetHasClearStatus == Miss then
                Miss
            else if magicalAttackAgainstClearTarget targetHasClearStatus == Hit then
                Hit
            else if anyAttackWoundProtectMissDeath protectedFromWound attackMissesDeathProtectedTargets == Miss then
                Miss
            else if 

