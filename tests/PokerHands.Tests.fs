module PokerHandsTests

open Expecto
open PokerHands

let assertThat actual expected = Expect.equal actual expected ""

[<Tests>]
let tests = testList "Poker Hands tests" [
  
  testList "Rank hand of cards scenarios" [
    let testCases = 
      [ ("2H 3D 5S 9C KD", HighCard)
        ("2H 2D 5S 9C KD", Pair)
        ("2H 2D 5S 5C KD", TwoPairs)
        ("2H 2D 6S 5C 2S", ThreeOfAKind)
        ]
    for cards, expectedRank in testCases do
      test $"Should rank {cards} with a {expectedRank}" {
        assertThat (rank cards) expectedRank 
      }
  ]
]  
  