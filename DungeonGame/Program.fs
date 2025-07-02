module DungeonGame.Program

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

let monsters: Monster array = [|
    { Type = Slime; Health = 10; Strength = 2; Defense = 2 }
    { Type = Zombie; Health = 20; Strength = 3; Defense = 3 }
    { Type = Phantom; Health = 15; Strength = 5; Defense = 1 }
|]

let getNextMonster () =
    let nextIndex = Random.Shared.Next(0, monsters.Length)
    monsters[nextIndex]

type AdventurerAction =
    | Attack
    | Block
    | Dodge
    | Heal

type MonsterAction =
    | Attack
    | Defend

type GameStatus =
    | Playing
    | Won
    | GameOver

type BattleContext = {
    Status: GameStatus

    Adventurer: Adventurer
    AdventurerBlocks: bool
    Monster: Monster
    MonsterBlocks: bool

    AdventurerAttackFactor: float
    AdventurerDodgeSuccessful: bool
    MonsterAttackFactor: float

    MonsterActionDecider: BattleContext -> MonsterAction
}

let getNextMonsterAction () =
    match Random.Shared.Next(0, 2) with
    | 0 -> MonsterAction.Attack
    | 1 -> MonsterAction.Defend
    | _ -> failwith "Невалидное случайное значение"

let calculateDamage strength defense targetBlocks damageMultiplier =
    let damageMultiplierBase = if targetBlocks then 0.5 else 1
    let damageMultiplierTotal = damageMultiplierBase * damageMultiplier
    let damage = (int (float strength * damageMultiplierTotal) - defense)
    damage

let printMonsterIntroduction adventurer monster =
    printfn "--------------------------------"
    printfn $"{adventurer.GloriousName} встретил... {getMonsterTypeName monster.Type}!"

let readAdventurerActionInput adventurer =
    printfn $"Что сделает {adventurer.GloriousName}?"
    printfn "A - атаковать | D - защищаться | F - уклоняться | S - лечиться"

    let rec readInput adventurer =
        printf "> "
        let input = Console.ReadKey()
        printfn ""

        match input.Key with
        | ConsoleKey.A ->
            AdventurerAction.Attack
        | ConsoleKey.D ->
            AdventurerAction.Block
        | ConsoleKey.F ->
            AdventurerAction.Dodge
        | ConsoleKey.S ->
            if adventurer.HealingPotionCount > 0 then
                AdventurerAction.Heal
            else
                printfn $"У {adventurer.GloriousName} не осталось зелий. Попробуй снова."
                readInput adventurer
        | _ ->
           printfn "Неизвестное действие. Попробуй снова."
           readInput adventurer

    readInput adventurer

let handleAttackAction adventurer (monster: Monster) monsterBlocks randomFactor =
    let monsterDamage = calculateDamage adventurer.Strength monster.Defense monsterBlocks randomFactor
    let newMonsterHealth = monster.Health - monsterDamage
    printfn $"{adventurer.GloriousName} наносит {monsterDamage} ед. урона {getMonsterTypeName monster.Type}."
    { monster with Health = newMonsterHealth }

let handleAdventurerAction state adventurerAction monster monsterBlocks =
    let adventurer = state.Adventurer
    match adventurerAction with
        | AdventurerAction.Attack ->
            printfn $"{adventurer.GloriousName} атакует."
            let monster = handleAttackAction adventurer monster monsterBlocks state.AdventurerAttackFactor
            if monster.Health <= 0 then
                printfn $"{getMonsterTypeName monster.Type} побежден!"
            else
                printfn $"У {getMonsterTypeName monster.Type} осталось {monster.Health} ед. здоровья"
            { state with Monster = monster }
        | AdventurerAction.Block ->
            printfn $"{adventurer.GloriousName} защищается."
            { state with AdventurerBlocks = true }
        | AdventurerAction.Dodge ->
            failwith "Not implemented"
        | Heal ->
            failwith "Not implemented"

let handleMonsterAction state (monster: Monster) monsterAction =
    printfn $"{getMonsterTypeName monster.Type} атакует!"

    let adventurer = state.Adventurer
    let adventurerDamage =
        calculateDamage monster.Strength adventurer.Defense state.AdventurerBlocks state.MonsterAttackFactor

    let adventurerHealth = adventurer.Health - adventurerDamage
    if adventurerHealth <= 0 then
        printfn $"{getMonsterTypeName monster.Type} наносит критический удар {adventurerDamage} ед. урона и побеждает {adventurer.GloriousName}."
        { state with Status = GameOver }
    else
        printfn $"{getMonsterTypeName monster.Type} наносит {adventurerDamage} ед. урона {adventurer.GloriousName}."
        printfn $"У {adventurer.GloriousName} осталось {adventurerHealth} ед. здоровья."
        { state with Adventurer = { adventurer with Health = adventurerHealth } }

let processFrame state adventurerAction =
    let monster = state.Monster
    let monsterBlocks = state.MonsterBlocks

    let state = handleAdventurerAction state adventurerAction monster monsterBlocks

    let monsterAction = state.MonsterActionDecider state
    let state = handleMonsterAction state monster monsterAction

    state

let gameLoop adventurer =
    let rec gameLoop state =
        let adventurerAction = readAdventurerActionInput state.Adventurer
        let state = processFrame state adventurerAction

        if state.Status = GameOver then
            printfn "Проиграл."
        else
            if state.Monster.Health <= 0 then
                let monster = getNextMonster ()
                printMonsterIntroduction state.Adventurer monster
                gameLoop { state with Monster = monster }
            else
                gameLoop state

    let monster = getNextMonster ()
    printMonsterIntroduction adventurer monster
    let state = {
        Status = Playing
        Adventurer = adventurer
        AdventurerBlocks = false
        Monster = monster
        MonsterBlocks = false
        AdventurerAttackFactor = Random.Shared.NextDouble() * 1.5 + 1.0
        AdventurerDodgeSuccessful = Random.Shared.NextDouble() <= 0.6
        MonsterAttackFactor = Random.Shared.NextDouble() * 1.5 + 1.0
        MonsterActionDecider = fun _ -> getNextMonsterAction ()
    }

    gameLoop state


let runGame () =
    printfn "Добро пожаловать в Подземелье. Введи имя героя, который войдет в историю!"
    printf "> "
    let input = Console.ReadLine()
    let name = if String.IsNullOrWhiteSpace(input) then "Приключенец" else input

    let adventurer = {
        GloriousName = name
        Health = 35
        Strength = 7
        Defense = 1
        HealingPotionCount = 3
        HealingPotionStrength = 8
    }

    gameLoop adventurer

runGame ()