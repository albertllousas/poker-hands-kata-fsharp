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
  
  testList "Decide winner of two different hands scenarios" [
    let testCases = 
      [ ("2H 3D 5S 9C 2D", "2H 3D 5S 9C KD", Winner(P1, Pair, "2H 3D 5S 9C 2D", None))
        ("2H 3D 5S 9C 2D", "2H 3D 2S 3C KD", Winner(P2, TwoPairs, "2H 3D 2S 3C KD", None))
        ("2H 3D 2S 9C 2D", "2H 3D 2S 3C KD", Winner(P1, ThreeOfAKind, "2H 3D 2S 9C 2D", None))
        ("2H 3D 2S 9C 2D", "AD 2D 3D 4D 5H", Winner(P2, Straight, "AD 2D 3D 4D 5H", None))
        ("AD AS 5C 4S AH", "AD AS 5C 5S AH", Winner(P2, FullHouse, "AD AS 5C 5S AH", None))
        ("KD AS KC KS KH", "2H 3D 2S 3C KD", Winner(P1, FourOfAKind, "KD AS KC KS KH", None))
        ("2H 3D 5S 9C 2D", "AD KD QD JD 10D", Winner(P2, StraightFlush, "AD KD QD JD 10D", None))
        ("2H 3D 5S 9C KD", "2C 3H 4S 8C AH", Winner(P2, HighCard, "2C 3H 4S 8C AH", None))
        ("2H 3D 5S 9C KD", "2C 3H 4S 8C KH", Winner(P1, HighCard, "2H 3D 5S 9C KD", kicker = Some "9"))
        ]
    for p1Hand, p2Hand, expectedResult in testCases do
      test $"Should decide the winner two hands p1 '{p1Hand}' and p2 '{p2Hand}', with the result of {expectedResult}" {
        assertThat (decideWinner p1Hand p2Hand) expectedResult 
      }
  ]
]  
  