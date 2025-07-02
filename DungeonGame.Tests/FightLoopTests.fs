module DungeonGame.FightLoopTests

open DungeonGame.Program
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

let state = {
    Status = Playing
    Adventurer = adventurer
    AdventurerBlocks = false
    Monster = zombieMonster
    MonsterBlocks = false
    AdventurerAttackFactor = 1
    AdventurerDodgeSuccessful = false
    MonsterAttackFactor = 1
    MonsterActionDecider = fun _ -> MonsterAction.Attack
}

[<Test>]
let ``Test attack`` () =
    let state = processFrame state AdventurerAction.Attack
    Assert.That(state.Monster.Health, Is.EqualTo(zombieMonster.Health - adventurer.Strength + zombieMonster.Defense))

[<Test>]
let ``Test attack with random`` () =
    let state = processFrame { state with AdventurerAttackFactor = 1.5 } AdventurerAction.Attack
    Assert.That(state.Monster.Health, Is.EqualTo(zombieMonster.Health - (int (float adventurer.Strength * 1.5)) + zombieMonster.Defense))

[<Test>]
let ``Test attack when monster defends`` () =
    let state = processFrame { state with MonsterBlocks = true } AdventurerAction.Attack

    Assert.That(state.Monster.Health, Is.EqualTo(zombieMonster.Health - (int (float adventurer.Strength * 0.5)) + zombieMonster.Defense))

[<Test>]
let ``Test defend`` () =
    let state = processFrame state AdventurerAction.Block

    Assert.That(state.Adventurer.Health, Is.GreaterThanOrEqualTo(0))
    Assert.That(state.Adventurer.Health, Is.EqualTo(adventurer.Health - (zombieMonster.Strength / 2) + adventurer.Defense))

[<Test>]
let ``Test defend with random multiplier`` () =
    let state = processFrame { state with MonsterBlocks = true; AdventurerAttackFactor = 2 } AdventurerAction.Attack

    Assert.That(state.Monster.Health, Is.GreaterThanOrEqualTo(0))
    Assert.That(state.Monster.Health, Is.EqualTo(zombieMonster.Health - adventurer.Strength + zombieMonster.Defense))

[<Test>]
let ``Test dodge`` () =
    let state = processFrame { state with AdventurerDodgeSuccessful = true } AdventurerAction.Dodge

    Assert.That(state.Adventurer, Is.EqualTo(adventurer))

[<Test>]
let ``Test dodge fail`` () =
    let state = processFrame state AdventurerAction.Dodge

    Assert.That(state.Adventurer.Health, Is.EqualTo(adventurer.Health - zombieMonster.Strength + adventurer.Defense))

[<Test>]
let ``Test heal`` () =
    let state = processFrame state AdventurerAction.Heal

    Assert.That(state.Adventurer.Health, Is.EqualTo(adventurer.Health + adventurer.HealingPotionStrength))
    Assert.That(state.Adventurer.HealingPotionCount, Is.EqualTo(adventurer.HealingPotionCount - 1))

[<Test>]
let ``Test fight ends when monster dies`` () =
    let monster = { zombieMonster with Health = 1 }
    let state = processFrame { state with Monster = monster } AdventurerAction.Attack

    Assert.That(state.Status, Is.EqualTo(GameStatus.Won))
