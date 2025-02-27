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
    | Attack of damageMultiplier: double
    | Defend
    | Dodge of dodgeSuccessful: bool
    | Heal

type MonsterAction =
    | Attack of damageMultiplier: double
    | Defend

let getNextMonsterAction () =
    match Random.Shared.Next(0, 2) with
    | 0 -> MonsterAction.Attack (Random.Shared.NextDouble() * 1.5 + 0.5)
    | 1 -> MonsterAction.Defend
    | _ -> failwith "Невалидное случайное значение"

let calculateDamage strength defense defendsSelf damageMultiplier =
    let damageMultiplierBase = if defendsSelf then 0.5 else 1
    let damageMultiplierTotal = damageMultiplierBase * damageMultiplier
    let damage = (int (float strength * damageMultiplierTotal) - defense)
    damage

let handleAdventurerAttack (adventurer: Adventurer) (monster: Monster) monsterDefends adventurerDamageMultiplier =
    let damage = calculateDamage adventurer.Strength monster.Defense monsterDefends adventurerDamageMultiplier
    let monster = { monster with Health = monster.Health - damage }
    printfn $"{adventurer.GloriousName} нанес {damage} урона. У врага осталось {monster.Health} ед. здоровья."
    monster

let handleMonsterAttack (monster: Monster) (adventurer: Adventurer) adventurerDefends monsterDamageMultiplier =
    let damage = calculateDamage monster.Strength adventurer.Defense adventurerDefends monsterDamageMultiplier
    let adventurer = { adventurer with Health = adventurer.Health - damage }
    printfn $"{getMonsterTypeName monster.Type} атакует! Он нанес {damage} урона, у тебя осталось {adventurer.Health} ед. здоровья."
    adventurer

let handleFightTurn adventurer monster monsterDefends adventurerAction monsterAction =
    let monster =
        match adventurerAction with
        | AdventurerAction.Attack damageMultiplier ->
            handleAdventurerAttack adventurer monster monsterDefends damageMultiplier
        | AdventurerAction.Defend ->
            monster
        | AdventurerAction.Dodge dodgeSuccessful -> failwith "Not implemented!"
        | AdventurerAction.Heal -> failwith "Not implemented!"

    let adventurerDefends = adventurerAction = AdventurerAction.Defend

    match monsterAction with
    | MonsterAction.Attack damageMultiplier ->
        let adventurer = handleMonsterAttack monster adventurer adventurerDefends damageMultiplier
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
        | ConsoleKey.A -> Some (AdventurerAction.Attack (Random.Shared.NextDouble() * 1.5 + 0.5))
        | ConsoleKey.D -> Some AdventurerAction.Defend
        | ConsoleKey.F -> Some (AdventurerAction.Dodge (Random.Shared.NextDouble() < 0.6))
        | ConsoleKey.S -> Some AdventurerAction.Heal
        | _ ->
           printfn "Неизвестное действие. Попробуй снова."
           None

    match adventurerAction with
    | None ->
        fightMonster adventurer monster monsterDefends
    | Some adventurerAction ->
        let monsterAction = getNextMonsterAction ()
        let adventurer, monster, monsterDefends =
            handleFightTurn adventurer monster monsterDefends adventurerAction monsterAction
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