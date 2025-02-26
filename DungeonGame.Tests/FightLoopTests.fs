module DungeonGame.FightLoopTests

open NUnit.Framework

let adventurer: Adventurer = {
    GloriousName = "Tester!"
    Health = 30
    Strength = 8
    Defense = 3
    HealingPotionCount = 3
    HealingPotionStrength = 8
}

let zombieMonster: Monster = {
    Type = Zombie
    Health = 15
    Strength = 5
    Defense = 2
}

let defaultDiceRoll: DiceRoll = {
    MonsterDamageMultiplier = 1
    MonsterAction = MonsterAction.Defend
    AdventurerDamageMultiplier = 1
    AdventurerDodgeSuccessful = false
}

[<Test>]
let ``Test attack`` () =
    let diceRoll = defaultDiceRoll
    let _, monster, _ =
        handleFightTurn adventurer zombieMonster false AdventurerAction.Attack diceRoll

    Assert.That(monster.Health, Is.EqualTo(zombieMonster.Health - adventurer.Strength + zombieMonster.Defense))

[<Test>]
let ``Test attack with random`` () =
    let diceRoll = { defaultDiceRoll with AdventurerDamageMultiplier = 1.5 }
    let _, monster, _ =
        handleFightTurn adventurer zombieMonster false AdventurerAction.Attack diceRoll

    Assert.That(monster.Health, Is.EqualTo(zombieMonster.Health - (int (float adventurer.Strength * 1.5)) + zombieMonster.Defense))

[<Test>]
let ``Test attack when monster defends`` () =
    let diceRoll = defaultDiceRoll
    let _, monster, _ =
        handleFightTurn adventurer zombieMonster true AdventurerAction.Attack diceRoll

    Assert.That(monster.Health, Is.EqualTo(zombieMonster.Health - (int (float adventurer.Strength * 0.5)) + zombieMonster.Defense))

[<Test>]
let ``Test defend`` () =
    let diceRoll = { defaultDiceRoll with MonsterAction = MonsterAction.Attack }
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer zombieMonster false AdventurerAction.Defend diceRoll

    Assert.That(updatedAdventurer.Health, Is.GreaterThanOrEqualTo(0))
    Assert.That(updatedAdventurer.Health, Is.EqualTo(adventurer.Health - (zombieMonster.Strength / 2) + adventurer.Defense))

[<Test>]
let ``Test defend with random multiplier`` () =
    let diceRoll = { defaultDiceRoll with MonsterAction = MonsterAction.Attack; MonsterDamageMultiplier = 2.0 }
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer zombieMonster false AdventurerAction.Defend diceRoll

    Assert.That(updatedAdventurer.Health, Is.GreaterThanOrEqualTo(0))
    Assert.That(updatedAdventurer.Health, Is.EqualTo(adventurer.Health - zombieMonster.Strength + adventurer.Defense))

[<Test>]
let ``Test dodge`` () =
    let diceRoll = { defaultDiceRoll with AdventurerDodgeSuccessful = true }
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer zombieMonster false AdventurerAction.Dodge diceRoll

    Assert.That(updatedAdventurer, Is.EqualTo(adventurer))

[<Test>]
let ``Test dodge fail`` () =
    let diceRoll = { defaultDiceRoll with AdventurerDodgeSuccessful = false }
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer zombieMonster false AdventurerAction.Dodge diceRoll

    Assert.That(updatedAdventurer, Is.EqualTo(adventurer))

[<Test>]
let ``Test heal`` () =
    let diceRoll = { defaultDiceRoll with AdventurerDodgeSuccessful = false }
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer zombieMonster false AdventurerAction.Heal diceRoll

    Assert.That(updatedAdventurer.Health, Is.EqualTo(adventurer.Health + adventurer.HealingPotionStrength))
    Assert.That(updatedAdventurer.HealingPotionCount, Is.EqualTo(adventurer.HealingPotionCount - 1))

[<Test>]
let ``Test fight ends when monster dies`` () =
    let monster = { zombieMonster with Health = 1 }
    let diceRoll = { defaultDiceRoll with MonsterAction = MonsterAction.Attack }
    let updatedAdventurer, _, _ = handleFightTurn adventurer monster false AdventurerAction.Attack diceRoll

    Assert.That(updatedAdventurer, Is.EqualTo(adventurer))
