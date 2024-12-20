module PokerHandsTests

open Expecto
open PokerHands

let assertThat actual expected = Expect.equal actual expected ""

[<Tests>]
let tests = testList "Poker Hands tests" [
  
  testList "Rank hand of cards scenarios" [
    let testCases = 
      [ ([(2,H);(3,D);(5,S);(9,C);(13,D)], HighCard)
        ([(2,H);(2,D);(5,S);(9,C);(13,D)], Pair)
        ([(2,H);(2,D);(5,S);(5,C);(13,D)], TwoPairs)
        ([(2,H);(2,D);(6,S);(5,C);(2,S)], ThreeOfAKind)
        ([(14,D);(14,S);(5,C);(5,S);(14,H)], FullHouse)
        ([(13,D);(14,S);(13,C);(13,S);(13,H)], FourOfAKind)
        ([(2,D);(3,D);(4,D);(5,D);(6,H)], Straight)
        ([(7,D);(8,D);(9,D);(10,D);(11,H)], Straight)
        ([(14,D);(2,D);(3,D);(4,D);(5,H)], Straight)
        ([(14,D);(13,D);(12,D);(11,D);(10,H)], Straight)
        ([(2,D);(3,D);(5,D);(9,D);(13,D)], Flush)
        ([(2,D);(2,D);(5,D);(9,D);(13,D)], Flush)
        ([(2,D);(2,D);(2,D);(9,D);(13,D)], Flush)
        ([(14,D);(13,D);(12,D);(11,D);(10,D)], StraightFlush)
        ]
    for cards, expectedRank in testCases do
      test $"Should rank {cards} with a {expectedRank}" {
        assertThat (Hands.rank cards) expectedRank 
      }
  ]

  testList "Decide winner of two different hands scenarios" [
    let testCases = 
      [ ([(2,H);(3,D);(5,S);(9,C);(2,D)],
         [(2,H);(3,D);(5,S);(9,C);(13,D)],
         Winner(P1, Pair, [(2,H);(3,D);(5,S);(9,C);(2,D)], None))
        ([(2,H);(3,D);(5,S);(9,C);(2,D)],
         [(2,H);(3,D);(2,S);(3,C);(13,D)],
         Winner(P2, TwoPairs, [(2,H);(3,D);(2,S);(3,C);(13,D)], None))
        ([(2,H);(3,D);(2,S);(9,C);(2,D)],
         [(2,H);(3,D);(2,S);(3,C);(13,D)],
         Winner(P1, ThreeOfAKind, [(2,H);(3,D);(2,S);(9,C);(2,D)], None))
        ([(2,H);(3,D);(2,S);(9,C);(2,D)],
         [(14,D);(2,D);(3,D);(4,D);(5,H)],
         Winner(P2, Straight, [(14,D);(2,D);(3,D);(4,D);(5,H)], None))
        ([(14,D);(14,S);(5,C);(4,S);(14,H)],
         [(14,D);(14,S);(5,C);(5,S);(14,H)],
         Winner(P2, FullHouse, [(14,D);(14,S);(5,C);(5,S);(14,H)], None))
        ([(13,D);(14,S);(13,C);(13,S);(13,H)],
         [(2,H);(3,D);(2,S);(3,C);(13,D)],
         Winner(P1, FourOfAKind, [(13,D);(14,S);(13,C);(13,S);(13,H)], None))
        ([(2,H);(3,D);(5,S);(9,C);(2,D)],
         [(14,D);(13,D);(12,D);(11,D);(10,D)],
         Winner(P2, StraightFlush, [(14,D);(13,D);(12,D);(11,D);(10,D)], None))
        ([(2,H);(3,D);(5,S);(9,C);(13,D)],
         [(2,C);(3,H);(4,S);(8,C);(14,H)],
         Winner(P2, HighCard, [(2,C);(3,H);(4,S);(8,C);(14,H)], None))
        ([(2,H);(3,D);(5,S);(9,C);(13,D)],
         [(2,C);(3,H);(4,S);(8,C);(13,H)],
         Winner(P1, HighCard, [(2,H);(3,D);(5,S);(9,C);(13,D)], kicker = Some 9))
        ]
    for p1Hand, p2Hand, expectedResult in testCases do
      test $"Should decide the winner two hands p1 '{p1Hand}' and p2 '{p2Hand}', with the result of {expectedResult}" {
        assertThat (Hands.decideWinner p1Hand p2Hand) expectedResult 
      }
  ]
]  
  