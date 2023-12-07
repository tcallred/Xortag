open System
open System.Net.Http
open System.Text.Json
open System.Threading

module TagClient =

    type WorldState =
        { Id: int
          Name: string
          IsIt: bool
          X: int
          Y: int
          MapWidth: int
          MapHeight: int
          Players: PlayerViewModel list }

    and ActionResponse =
        { IsIt: bool
          X: int
          Y: int
          Players: PlayerViewModel list }

    and PlayerViewModel = { X: int; Y: int; IsIt: bool }

    let url = "https://xortag20231025163813.azurewebsites.net/"
    let tokenSource = new CancellationTokenSource()
    let jsonOptions = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

    let register (client: HttpClient) =
        async {
            let! response = client.PostAsync("register", null, tokenSource.Token) |> Async.AwaitTask
            let! worldStream = response.Content.ReadAsStreamAsync(tokenSource.Token) |> Async.AwaitTask

            let world =
                JsonSerializer
                    .DeserializeAsync<WorldState>(worldStream, jsonOptions, tokenSource.Token)
                    .Result


            printfn "You have successfully registered!"
            printfn "Your player name is %s and your id is %d" world.Name world.Id
            return world
        }

    let sendMoveCommand (client: HttpClient, moveCommand: string, id: int) =
        async {
            do! Async.Sleep(TimeSpan.FromSeconds(1.0)) // Requests more frequent than once per second will fail.
            let! response = client.PostAsync(sprintf "%s/%d" moveCommand id, null) |> Async.AwaitTask
            let! currentStream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
            return JsonSerializer.DeserializeAsync<ActionResponse>(currentStream, jsonOptions)
        }

    let sendLookCommand (client: HttpClient, id: int) =
        async {
            do! Async.Sleep(TimeSpan.FromSeconds(1.0)) // Requests more frequent than once per second will fail.
            let! response = client.GetAsync(sprintf "look/%d" id) |> Async.AwaitTask
            let! currentStream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
            return JsonSerializer.DeserializeAsync<ActionResponse>(currentStream, jsonOptions)
        }

    let updateWorld (world: WorldState) (current: ActionResponse) =
        { world with
            IsIt = current.IsIt
            X = current.X
            Y = current.Y
            Players = current.Players }

    let distance (pl1: PlayerViewModel) (pl2: PlayerViewModel) =
        sqrt (float (pl1.X - pl2.X) ** 2 + float (pl1.Y - pl2.Y) ** 2)

    let closestToMe (me: PlayerViewModel) (nearbyPlayers: List<PlayerViewModel>) =
        nearbyPlayers |> List.sortBy (distance me) |> List.head

    let getRandomElementFromList (list: string list) =
        let random = System.Random()
        let randomIndex = random.Next(0, List.length list)
        List.item randomIndex list

    let randomMove () =
        getRandomElementFromList [ "moveright"; "moveleft"; "moveup"; "movedown" ]

    let possibleMoves (world: WorldState) (me: PlayerViewModel) (other: PlayerViewModel) =
        [ ({ me with X = me.X + 1 }, "moveright")
          ({ me with X = me.X - 1 }, "moveleft")
          ({ me with Y = me.Y + 1 }, "movedown")
          ({ me with X = me.Y - 1 }, "moveup") ]
        |> List.filter (fun (me, _) -> me.X >= 0 && me.X < world.MapWidth && me.Y >= 0 && me.Y < world.MapHeight)
        |> List.map (fun (me, move) -> (distance me other, move))

    let moveAwayFrom (world: WorldState) (me: PlayerViewModel) (other: PlayerViewModel) =
        possibleMoves world me other |> List.maxBy fst |> snd

    let moveTowards (world: WorldState) (me: PlayerViewModel) (other: PlayerViewModel) =
        possibleMoves world me other |> List.minBy fst |> snd


    let chooseMove (worldState: WorldState) =
        let me: PlayerViewModel =
            { X = worldState.X
              Y = worldState.Y
              IsIt = worldState.IsIt }

        if List.isEmpty worldState.Players then
            printfn "No players, going random"
            randomMove ()
        else if me.IsIt then
            printfn "I'm it. Going towards %A" (closestToMe me worldState.Players)
            moveTowards worldState me (closestToMe me worldState.Players)
        else
            let isIt: Option<PlayerViewModel> =
                List.tryFind (fun p -> p.IsIt) worldState.Players

            match isIt with
            | Some(other) ->
                printfn "Not it but moving away from %A" other
                moveAwayFrom worldState me other
            | None ->
                printfn "Not it but moving away from %A" (closestToMe me worldState.Players)
                moveAwayFrom worldState me (closestToMe me worldState.Players)


    let moveAround (client: HttpClient, world: WorldState) =
        async {
            // once you have a moving strategy, you can remove this counter.
            let mutable worldState = world

            while not tokenSource.IsCancellationRequested do
                let move = chooseMove worldState
                printfn "Moving: %s" move
                let! newState = sendMoveCommand (client, chooseMove worldState, world.Id)
                worldState <- updateWorld worldState newState.Result

            let! lookState = sendLookCommand (client, world.Id)
            let finalWorld = updateWorld worldState lookState.Result
            return finalWorld
        }

    let mainAsync args =
        async {
            printfn "Application has started. Ctrl-C to end"

            Console.CancelKeyPress.Add(fun eventArgs ->
                printfn "Cancel event triggered"
                tokenSource.Cancel()
                eventArgs.Cancel <- true)

            use client = new HttpClient(BaseAddress = new Uri(url))
            let! world = register client
            let! finalWorld = moveAround (client, world)
            printfn "Final World: %A" finalWorld
            tokenSource.Cancel()
        }

    [<EntryPoint>]
    let main _ =
        Async.RunSynchronously(mainAsync [||])
        0
