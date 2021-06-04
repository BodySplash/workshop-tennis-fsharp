namespace Tennis

(*
    Two players

    Game

    Fifteen, Thirty, Forty -> Game
    Deuce -> Advantage -> Deuce / Game

*)


type Player =
    | A
    | B


type PlayerPoints =
    | Zero
    | Fifteen
    | Thirty

type Score =
    | Points of PlayerPoints * PlayerPoints
    | Forty of Player * PlayerPoints
    | Advantage of Player
    | Deuce
    | Game of Player

[<RequireQualifiedAccess>]
module Game =


    let private incrementPoint =
        function
        | Zero -> Some Fifteen
        | Fifteen -> Some Thirty
        | Thirty -> None

    let private pointsOf playerAPoints playerBPoints player =
        match player with
        | A -> playerAPoints
        | B -> playerBPoints

    let private pointTo player point playerAPoints playerBPoints =
        match player with
        | A -> Points(point, playerBPoints)
        | B -> Points(playerAPoints, point)

    let private otherPlayer =
        function
        | A -> B
        | B -> A

    let scoreWithPoints winner playerAPoints playerBPoints =
        let currentPointOf = pointsOf playerAPoints playerBPoints

        match currentPointOf winner |> incrementPoint with
        | Some v -> pointTo winner v playerAPoints playerBPoints
        | None -> Forty(winner, (currentPointOf (otherPlayer winner)))

    let scoreWithForty winner player otherPlayer =
        if player = winner then
            Game winner
        else
            match incrementPoint otherPlayer with
            | Some v -> Forty(player, v)
            | None -> Deuce

    let scoreWithAdvantage winner player =
        if winner = player then
            Game winner
        else
            Deuce

    let scoreWithDeuce winner = Advantage winner

    let score points =
        points
        |> Seq.fold
            (fun score player ->
                match score, player with
                | Advantage player, winner -> scoreWithAdvantage winner player
                | Deuce, winner -> scoreWithDeuce winner
                | Forty (player, other), winner -> scoreWithForty winner player other
                | Points (a, b), winner -> scoreWithPoints winner a b
                | _, _ -> score)
            (Points(Zero, Zero))
