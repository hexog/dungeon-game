module [<AutoOpen>] DungeonGame.Program

open System

type MonsterType =
    | Slime
    | Zombie
    | Phantom

let getMonsterTypeName monsterType =
    match monsterType with
    | Slime -> "Слизь"
    | Zombie -> "Зомби"
    | Phantom -> "Фантом"

type Monster = {
    Type: MonsterType
    Health: int
    Strength: int
    Defense: int
}

type Adventurer = {
    GloriousName: string
    Health: int
    Strength: int
    Defense: int
    HealingPotionCount: int
    HealingPotionStrength: int
}

let monsters: Monster list = [
    { Type = Slime; Health = 10; Strength = 2; Defense = 2 }
    { Type = Zombie; Health = 20; Strength = 3; Defense = 5 }
    { Type = Phantom; Health = 15; Strength = 5; Defense = 1 }
]

let getNextMonster () =
    let nextIndex = Random.Shared.Next(0, monsters.Length)
    monsters[nextIndex]

type AdventurerAction =
    | Attack
    | Defend
    | Dodge
    | Heal

type MonsterAction =
    | Attack
    | Defend

let monsterActions: MonsterAction array = [| Attack; Defend |]
let getNextMonsterAction () =
    monsterActions[Random.Shared.Next(0, monsterActions.Length)]

type DiceRoll = {
    MonsterDamageMultiplier: double
    MonsterAction: MonsterAction

    AdventurerDamageMultiplier: double
    AdventurerDodgeSuccessful: bool
}

let getNextDiceRoll () =
    {
        MonsterDamageMultiplier = Random.Shared.NextDouble() * 1.5 + 0.5
        MonsterAction = getNextMonsterAction()

        AdventurerDamageMultiplier = Random.Shared.NextDouble() * 1.5 + 0.5
        AdventurerDodgeSuccessful = Random.Shared.NextDouble() < 0.6
    }

let calculateDamage strength defense defendsSelf damageMultiplier =
    let damageMultiplierBase = if defendsSelf then 0.5 else 1
    let damageMultiplierTotal = damageMultiplierBase * damageMultiplier
    let damage = (int (float strength * damageMultiplierTotal) - defense)
    damage

let handleAdventurerAttack (adventurer: Adventurer) (monster: Monster) monsterDefends (diceRoll: DiceRoll) =
    let damage = calculateDamage adventurer.Strength monster.Defense monsterDefends diceRoll.AdventurerDamageMultiplier
    let monster = { monster with Health = monster.Health - damage }
    printfn $"{adventurer.GloriousName} нанес {damage} урона. У врага осталось {monster.Health} ед. здоровья."
    monster

let handleMonsterAttack (monster: Monster) (adventurer: Adventurer) adventurerDefends (diceRoll: DiceRoll) =
    let damage = calculateDamage monster.Strength adventurer.Defense adventurerDefends diceRoll.MonsterDamageMultiplier
    let adventurer = { adventurer with Health = adventurer.Health - damage }
    printfn $"{getMonsterTypeName monster.Type} атакует! Он нанес {damage} урона, у тебя осталось {adventurer.Health} ед. здоровья."
    adventurer

let handleFightTurn adventurer monster monsterDefends adventurerAction diceRoll =
    let monster =
        match adventurerAction with
        | AdventurerAction.Attack ->
            handleAdventurerAttack adventurer monster monsterDefends diceRoll
        | AdventurerAction.Defend ->
            monster
        | AdventurerAction.Dodge -> failwith "Not implemented!"
        | AdventurerAction.Heal -> failwith "Not implemented!"

    let adventurerDefends = adventurerAction = AdventurerAction.Defend

    match diceRoll.MonsterAction with
    | MonsterAction.Attack ->
        let adventurer = handleMonsterAttack monster adventurer adventurerDefends diceRoll
        adventurer, monster, false
    | MonsterAction.Defend ->
        printfn $"{getMonsterTypeName monster.Type} защищается."
        adventurer, monster, true


let rec fightMonster (adventurer: Adventurer) (monster: Monster) monsterDefends =
    if adventurer.Health <= 0 then
        printfn $"{adventurer.GloriousName} был побежден!"
        adventurer
    elif monster.Health <= 0 then
        printfn "Монстр побежден!"
        adventurer
    else

    printfn $"Что сделает {adventurer.GloriousName}?"
    printfn "A - атаковать | D - защищаться | F - уклоняться | S - лечиться"
    printf "> "
    let input = Console.ReadKey()
    printfn ""

    let adventurerAction =
        match input.Key with
        | ConsoleKey.A -> Some AdventurerAction.Attack
        | ConsoleKey.D -> Some AdventurerAction.Defend
        | ConsoleKey.F -> Some AdventurerAction.Dodge
        | ConsoleKey.S -> Some AdventurerAction.Heal
        | _ ->
           printfn "Неизвестное действие. Попробуй снова."
           None

    match adventurerAction with
    | None ->
        fightMonster adventurer monster monsterDefends
    | Some adventurerAction ->
        let diceRoll = getNextDiceRoll ()
        let adventurer, monster, monsterDefends = handleFightTurn adventurer monster monsterDefends adventurerAction diceRoll
        fightMonster adventurer monster monsterDefends

let runGameLoop adventurer =
    printfn "--------------------------------"
    let monster = getNextMonster ()
    let monsterName = getMonsterTypeName monster.Type
    printfn $"{adventurer.GloriousName} встретил... {monsterName}!"
    printfn $"У {monsterName} {monster.Health} ед. здоровья."
    let adventurer = fightMonster adventurer monster false
    adventurer

let runGame () =
    printfn "Добро пожаловать в Подземелье. Введи имя героя, который войдет в историю!"
    printf "> "
    let name = defaultIfNull "Приключенец" (Console.ReadLine())

    let mutable adventurer = {
        GloriousName = name
        Health = 35
        Strength = 7
        Defense = 1
        HealingPotionCount = 3
        HealingPotionStrength = 8
    }

    while adventurer.Health >= 0 do
        adventurer <- runGameLoop adventurer

runGame ()