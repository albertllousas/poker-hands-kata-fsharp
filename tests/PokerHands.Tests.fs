module PokerHandsTests

open Expecto
open PokerHands

let assertThat actual expected = Expect.equal actual expected ""

[<Tests>]
let tests = testList "Poker Hands tests" [
  
  test "Should rank a hand with High Card" {
    let hand = "2H 3D 5S 9C KD"
   
    let result = rank hand
    
    assertThat result HighCard
  }
  
  test "Should rank a hand with a Pair" {
    let hand = "2H 2D 5S 9C KD"
    
    let result = rank hand
    
    assertThat result Pair
  }
  
  test "Should rank a hand with Two Pairs" {
    let hand = "2H 2D 5S 5C KD"
    
    let result = rank hand
    
    assertThat result TwoPairs
  }
]  
  