module PokerHandsTests

open Expecto

let assertThat actual expected = Expect.equal actual expected ""

[<Tests>]
let tests = testList "Poker Hands tests" [
  
  test "Should work" {
    assertThat 1 0
  } 
]  
  