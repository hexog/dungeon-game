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

[<Test>]
let ``Test attack`` () =
    let _, monster, _ =
        handleFightTurn adventurer zombieMonster false (AdventurerAction.Attack 1) MonsterAction.Defend

    Assert.That(monster.Health, Is.EqualTo(zombieMonster.Health - adventurer.Strength + zombieMonster.Defense))

[<Test>]
let ``Test attack with random`` () =
    let _, monster, _ =
        handleFightTurn adventurer zombieMonster false (AdventurerAction.Attack 1.5) MonsterAction.Defend

    Assert.That(monster.Health, Is.EqualTo(zombieMonster.Health - (int (float adventurer.Strength * 1.5)) + zombieMonster.Defense))

[<Test>]
let ``Test attack when monster defends`` () =
    let _, monster, _ =
        handleFightTurn adventurer zombieMonster true (AdventurerAction.Attack 1) MonsterAction.Defend

    Assert.That(monster.Health, Is.EqualTo(zombieMonster.Health - (int (float adventurer.Strength * 0.5)) + zombieMonster.Defense))

[<Test>]
let ``Test defend`` () =
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer zombieMonster false AdventurerAction.Defend (MonsterAction.Attack 1)

    Assert.That(updatedAdventurer.Health, Is.GreaterThanOrEqualTo(0))
    Assert.That(updatedAdventurer.Health, Is.EqualTo(adventurer.Health - (zombieMonster.Strength / 2) + adventurer.Defense))

[<Test>]
let ``Test defend with random multiplier`` () =
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer zombieMonster false AdventurerAction.Defend (MonsterAction.Attack 2)

    Assert.That(updatedAdventurer.Health, Is.GreaterThanOrEqualTo(0))
    Assert.That(updatedAdventurer.Health, Is.EqualTo(adventurer.Health - zombieMonster.Strength + adventurer.Defense))

[<Test>]
let ``Test dodge`` () =
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer zombieMonster false (AdventurerAction.Dodge true) MonsterAction.Defend

    Assert.That(updatedAdventurer, Is.EqualTo(adventurer))

[<Test>]
let ``Test dodge fail`` () =
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer zombieMonster false (AdventurerAction.Dodge false) MonsterAction.Defend

    Assert.That(updatedAdventurer, Is.EqualTo(adventurer))

[<Test>]
let ``Test heal`` () =
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer zombieMonster false AdventurerAction.Heal MonsterAction.Defend

    Assert.That(updatedAdventurer.Health, Is.EqualTo(adventurer.Health + adventurer.HealingPotionStrength))
    Assert.That(updatedAdventurer.HealingPotionCount, Is.EqualTo(adventurer.HealingPotionCount - 1))

[<Test>]
let ``Test fight ends when monster dies`` () =
    let monster = { zombieMonster with Health = 1 }
    let updatedAdventurer, _, _ =
        handleFightTurn adventurer monster false (AdventurerAction.Attack 1) (MonsterAction.Attack 1)

    Assert.That(updatedAdventurer, Is.EqualTo(adventurer))
