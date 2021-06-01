module ``Tennis test``

open Tennis
open Expecto

[<Tests>]
let tests =
    testList
        "Game"
        [ test "With no points" {
            let result = Game.score []

            "should have 0/0 score"
            |> Expect.equal result (Points(Zero, Zero))
          }

          test "With score from Player a" {
              let result = Game.score [ A ]

              "should have Fifteen/Zero score"
              |> Expect.equal result (Points(Fifteen, Zero))
          }

          test "With two scores from Player a" {
              let result = Game.score [ A; A ]

              "should have Fifteen/Zero score"
              |> Expect.equal result (Points(Thirty, Zero))
          }

          test "With player A scoring everything" {
              let result = Game.score [ A; A; A; A ]

              ""
              |> Expect.equal result (Game A)
          }

          test "With A and B scoring the same thing" {
              let result = Game.score [ A; A; A; B; B; B ]

              "score is deuce"
              |> Expect.equal result Deuce
          }

          test "With A and B in deuce, A can take advantage" {
              let result = Game.score [ A; A; A; B; B; B ; A]

              "score is advantage A"
              |> Expect.equal result (Advantage A)
          }

          ]
