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
    
  let highestVal (hand: Card list) = hand |> List.max |> fst

module private TieBreaker =
  
  let private valueToCard (value: int) =
    match value with | 14 -> "A" | 1 -> "A" | 13 -> "K" | 12 -> "Q" | 11 -> "J" | _ -> value.ToString()
    
  let private breakWithKicker (p1Hand, p1Rank) (p2Hand, p2Rank) = 
    let p1Kicker = Cards.remove p1Hand p2Hand |> Cards.highestVal
    let p2Kicker = Cards.remove p2Hand p1Hand |> Cards.highestVal
    if p1Kicker > p2Kicker then Winner(P1, p1Rank, p1Hand, Some p1Kicker)
    else Winner(P2, p2Rank, p2Hand, Some p2Kicker)
    
  let private maxOrNone list = if List.isEmpty list then None else Some (List.max list)
    
  let breakTie (p1Hand, p1Rank) (p2Hand, p2Rank) =
    match (p1Rank, p2Rank) with
      | HighCard, HighCard | Straight, Straight | Flush, Flush ->
        if Cards.highestVal p1Hand > Cards.highestVal p2Hand then Winner(P1, p1Rank, p1Hand, None)
        elif Cards.highestVal p1Hand < Cards.highestVal p2Hand then Winner(P2, p2Rank, p2Hand, None)
        else breakWithKicker (p1Hand, p1Rank) (p2Hand, p2Rank)
      | Pair, Pair | TwoPairs, TwoPairs | ThreeOfAKind, ThreeOfAKind ->
        let p1PairsOrTrios = Cards.discardSingleCards p1Hand  
        let p2PairsOrTrios = Cards.discardSingleCards p2Hand 
        let p1MaxPair = Cards.remove p1PairsOrTrios p2PairsOrTrios |> List.map fst |> maxOrNone
        let p2MaxPair = Cards.remove p2PairsOrTrios p1PairsOrTrios |> List.map fst |> maxOrNone
        if p1MaxPair.IsSome && p1MaxPair > p2MaxPair then Winner(P1, p1Rank, p1Hand, None)
        elif p1MaxPair.IsSome && p1MaxPair < p2MaxPair then Winner(P2, p2Rank, p2Hand, None)
        else breakWithKicker (p1Hand, p1Rank) (p2Hand, p2Rank)
      | FullHouse, FullHouse | FourOfAKind , FourOfAKind ->
         let p1Max = Cards.discardSingleCards p1Hand |> List.map fst |> List.max
         let p2Max = Cards.discardSingleCards p2Hand |> List.map fst |> List.max
         if p1Max > p2Max then Winner(P1, p1Rank, p1Hand, None)
         else Winner(P2, p2Rank, p2Hand, None)
      | _ -> failwith "Not implemented"

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
  