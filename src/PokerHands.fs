module PokerHands

type HandRank = HighCard | Pair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush

type Suit = H | D | S | C 

type Card = int * Suit

type Player = P1 | P2

type Winner = Winner of player: Player * rank: HandRank * hand: Card list * kicker: int option | Tie

module private Cards =
  
  let groupByValueAndCount (cards: Card list) =
    cards |> List.map fst |> List.groupBy id |> List.map (fun (_, group) -> group.Length) |> List.sortDescending

  let haveConsecutiveValues (cards: Card list) = 
    let consecutiveCards aceValue =
      cards
      |> List.map (fun (value, _) -> if value = 14 then aceValue else value)
      |> List.sort
      |> List.pairwise
      |> List.forall (fun (a, b) -> b = a + 1)
    consecutiveCards 1 || consecutiveCards 14
    
  let haveSameSuit (cards: Card list) = cards |> List.map snd |> List.distinct |> List.length = 1
    
  let remove (cards: Card list) (toRemove: Card list ) =
    let valuesToRemove = List.map fst toRemove
    List.filter (fun (value, _) -> not (List.contains value valuesToRemove)) cards
  
  let discardSingleCards (hand: Card list) =
    hand |> List.groupBy fst |> List.filter (fun (_, group) -> not (group.Length = 1)) |> List.map snd |> List.concat
    
  let highestVal (hand: Card list) = hand |> List.map fst |> fun l -> if List.isEmpty l then -1 else List.max l

module private TieBreaker =
  
  open Cards
    
  let private breakWithKicker (p1Hand, p1Rank) (p2Hand, p2Rank) = 
    let p1Kicker = remove p1Hand p2Hand |> highestVal
    let p2Kicker = remove p2Hand p1Hand |> highestVal
    if p1Kicker > p2Kicker then Winner(P1, p1Rank, p1Hand, Some p1Kicker) |> Some
    elif p1Kicker < p2Kicker then Winner(P2, p2Rank, p2Hand, Some p2Kicker) |> Some
    else None
  
  let private breakWithHighestCard (p1Hand, p1Rank) (p2Hand, p2Rank) =
    if highestVal p1Hand > highestVal p2Hand then Winner(P1, p1Rank, p1Hand, None) |> Some
    elif highestVal p1Hand < highestVal p2Hand then Winner(P2, p2Rank, p2Hand, None) |> Some
    else None
    
  let private breakWithHighestGroup (p1Hand, p1Rank) (p2Hand, p2Rank) =
    let p1Groups = remove (discardSingleCards p1Hand) (discardSingleCards p2Hand) 
    let p2Groups = remove (discardSingleCards p2Hand) (discardSingleCards p1Hand)
    if  highestVal p1Groups > highestVal p2Groups then Winner(P1, p1Rank, p1Hand, None) |> Some
    elif highestVal p1Groups < highestVal p2Groups then Winner(P2, p2Rank, p2Hand, None) |> Some
    else None
    
  let breakTie (p1Hand, p1Rank) (p2Hand, p2Rank) =
    let winner =
      match p1Rank with
      | HighCard | Straight | Flush | StraightFlush -> breakWithHighestCard (p1Hand, p1Rank) (p2Hand, p2Rank)
      | Pair | TwoPairs | ThreeOfAKind -> breakWithHighestGroup (p1Hand, p1Rank) (p2Hand, p2Rank)
      | _ -> breakWithHighestCard (discardSingleCards p1Hand, p1Rank) (discardSingleCards p2Hand, p2Rank)
    let winnerWithKicker = breakWithKicker (p1Hand, p1Rank) (p2Hand, p2Rank)
    if Option.isSome winner then Option.get winner
    elif Option.isSome winnerWithKicker then Option.get winnerWithKicker
    else Tie
      
module Hands =
  
  let rank (hand: Card list) =
    if Cards.haveSameSuit hand && Cards.haveConsecutiveValues hand then StraightFlush
    elif Cards.groupByValueAndCount hand = [4; 1] then FourOfAKind
    elif Cards.groupByValueAndCount hand = [3; 2] then FullHouse
    elif Cards.haveSameSuit hand then Flush
    elif Cards.haveConsecutiveValues hand then Straight
    elif Cards.groupByValueAndCount hand = [3; 1; 1] then ThreeOfAKind
    elif Cards.groupByValueAndCount hand = [2; 2; 1] then TwoPairs
    elif Cards.groupByValueAndCount hand = [2; 1; 1; 1] then Pair
    else HighCard
    
  let decideWinner (p1Hand: Card list) (p2Hand: Card list) : Winner =
    let idx rank = List.findIndex (fun r -> r = rank) [HighCard; Pair; TwoPairs; ThreeOfAKind; Straight; Flush; FullHouse; FourOfAKind; StraightFlush]
    let p1Rank = rank p1Hand  
    let p2Rank = rank p2Hand
    if idx p1Rank > idx p2Rank then Winner(P1, p1Rank, p1Hand, None)
    elif idx p1Rank < idx p2Rank then  Winner(P2, p2Rank, p2Hand, None)
    else TieBreaker.breakTie (p1Hand, p1Rank) (p2Hand, p2Rank)
  