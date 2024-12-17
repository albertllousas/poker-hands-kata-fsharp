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
        ("AD KD QD JD 10D", StraightFlush)
        ]
    for cards, expectedRank in testCases do
      test $"Should rank {cards} with a {expectedRank}" {
        assertThat (rank cards) expectedRank 
      }
  ]
  
  testList "Compare hands scenarios" [
    let testCases = 
      [ ("2H 3D 5S 9C 2D", "2H 3D 5S 9C KD", (Winner(player = P1, rank = Pair, hand = "2H 3D 5S 9C 2D")))
        ("2H 3D 5S 9C 2D", "2H 3D 2S 3C KD", (Winner(player = P2, rank = TwoPairs, hand = "2H 3D 2S 3C KD")))
        ]
    for p1Hand, p2Hand, expectedResult in testCases do
      test $"Should compare two hands p1 '{p1Hand}' and p2 '{p2Hand}', with the result of {expectedResult}" {
        assertThat (compare p1Hand p2Hand) expectedResult 
      }
  ]
]  
  