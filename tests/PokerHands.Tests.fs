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
        ("AD AS 5C 5S AH", FullHouse)
        ("KD AS KC KS KH", FourOfAKind)
        ("2D 3D 4D 5D 6H", Straight)
        ("7D 8D 9D 10D JH", Straight)
        ("AD 2D 3D 4D 5H", Straight)
        ("AD KD QD JD 10H", Straight)
        ("2D 3D 5D 9D KD", Flush)
        ("2D 2D 5D 9D KD", Flush)
        ("2D 2D 2D 9D KD", Flush)
        ]
    for cards, expectedRank in testCases do
      test $"Should rank {cards} with a {expectedRank}" {
        assertThat (rank cards) expectedRank 
      }
  ]
]  
  