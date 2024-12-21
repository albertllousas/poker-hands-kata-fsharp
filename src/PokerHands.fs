module PokerHands

type HandRank = HighCard | Pair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush

type Suit = H | D | S | C 

type Card = int * Suit

type Player = P1 | P2

type Winner = Winner of player: Player * rank: HandRank * hand: Card list * kicker: int option

module private Cards =
  
  let groupByValueAndCount (cards: Card list) =
    cards |> List.map fst |> List.groupBy id |> List.map (fun (_, group) -> group.Length) |> List.sortDescending

  let mapCardValues (aceValue: int) (hand: Card list)  =
    List.map (fun (value, _) -> if value = 14 then aceValue else value) hand

  let haveConsecutiveValues (cards: Card list) = 
    let consecutiveCards aceValue =
      cards
      |> mapCardValues aceValue
      |> List.sort
      |> List.pairwise
      |> List.forall (fun (a, b) -> b = a + 1)
    consecutiveCards 1 || consecutiveCards 14
    
  let haveSameSuit (cards: Card list) =
    cards |> List.map snd |> List.distinct |> List.length = 1
    
  let rmDuplicateValues (hand1: Card list) (hand2: Card list ) =
    let hand2Values = mapCardValues 14 hand2
    List.filter (fun (value, _) -> not (List.contains value hand2Values)) hand1
    
  let getPairs (hand: Card list) =
    hand |> List.groupBy fst |> List.filter (fun (_, group) -> group.Length = 2) |> List.map snd |> List.concat

module private TieBreaker =
  
  let valueToCard (value: int) =
    match value with | 14 -> "A" | 1 -> "A" | 13 -> "K" | 12 -> "Q" | 11 -> "J" | _ -> value.ToString()
   
  let decideWinner (p1Hand, p1Rank) (p2Hand, p2Rank) =
    if (p1Rank = HighCard && p2Rank = HighCard) then
      let highestCard (hand: Card list) = hand |> Cards.mapCardValues 14 |> List.max
      if highestCard p1Hand > highestCard p2Hand then Winner(player = P1, rank = p1Rank, hand = p1Hand, kicker = None)
      elif highestCard p1Hand < highestCard p2Hand then Winner(player = P2, rank = p2Rank, hand = p2Hand, kicker = None)
      else
        let p1Kicker = highestCard (Cards.rmDuplicateValues p1Hand p2Hand)
        let p2Kicker = highestCard (Cards.rmDuplicateValues p2Hand p1Hand)
        if p1Kicker > p2Kicker then Winner(player = P1, rank = p1Rank, hand = p1Hand, kicker = Some p1Kicker)
        else Winner(player = P2, rank = p2Rank, hand = p2Hand, kicker = Some p2Kicker)
    else
      let p1MaxPair = Cards.getPairs p1Hand |> List.max
      let p2MaxPair = Cards.getPairs p2Hand |> List.max
      if p1MaxPair > p2MaxPair then Winner(player = P1, rank = p1Rank, hand = p1Hand, kicker = None)
      else Winner(player = P2, rank = p2Rank, hand = p2Hand, kicker = None)

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
    if idx p1Rank > idx p2Rank then Winner(player = P1, rank = p1Rank, hand = p1Hand, kicker = None)
    elif idx p1Rank < idx p2Rank then  Winner(player = P2, rank = p2Rank, hand = p2Hand, kicker = None)
    else TieBreaker.decideWinner (p1Hand, p1Rank) (p2Hand, p2Rank)
  