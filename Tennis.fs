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
    | Forty

type Score =
    | Points of PlayerPoints * PlayerPoints
    | Advantage of Player
    | Deuce
    | Game of Player

[<RequireQualifiedAccess>]
module Game =


    let private nextPoint =
        function
        | Zero -> Fifteen
        | Fifteen -> Thirty
        | Thirty -> Forty
        | _ -> failwith "Laul what"

    let normalizeScore =
        function
        | Points (Forty, Forty) -> Deuce
        | s -> s

    let score points =
        points
        |> Seq.fold
            (fun score player ->
                match score, player with
                | Advantage p1, p2 when p1 = p2 -> Game p1
                | Advantage _, _ -> Deuce
                | Deuce, p -> Advantage p
                | Points (Forty, _), A -> Game A
                | Points (_, Forty), B -> Game B
                | Points (a, b), A -> Points(nextPoint a, b) |> normalizeScore
                | Points (a, b), B -> Points(a, nextPoint b) |> normalizeScore
                | _, _ -> score)
            (Points(Zero, Zero))
