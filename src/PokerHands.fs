module PokerHands

type HandRank = HighCard | Pair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush

type Player = P1 | P2

type ComparisonResult = Winner of player: Player * rank: HandRank * hand: string

let private groupByValueAndCount (cards: string list) =
  cards
  |> List.map (fun card -> card.Substring(0, card.Length - 1))
  |> List.groupBy id
  |> List.map (fun (_, group) -> group.Length)
  |> List.sortDescending
  
let private haveConsecutiveValues (cards: string list) = 
  let consecutiveCards aceValue =
    cards
    |> List.map (fun card -> card.Substring(0, card.Length - 1))
    |> List.map (fun card -> match card with "A" -> aceValue | "K" -> 13 | "Q" -> 12 | "J" -> 11 | _ -> int card )
    |> List.sort
    |> List.pairwise
    |> List.forall (fun (a, b) -> b = a + 1)
  consecutiveCards 1 || consecutiveCards 14
  
let private haveSameSuit (cards: string list) =
  cards
  |> List.map (fun card -> card[card.Length - 1])
  |> List.distinct
  |> List.length = 1

let rank (hand: string) =
  let cards = hand.Split(' ') |> Array.toList
  if haveSameSuit cards && haveConsecutiveValues cards then StraightFlush
  elif groupByValueAndCount cards = [4; 1] then FourOfAKind
  elif groupByValueAndCount cards = [3; 2] then FullHouse
  elif haveSameSuit cards then Flush
  elif haveConsecutiveValues cards then Straight
  elif groupByValueAndCount cards = [3; 1; 1] then ThreeOfAKind
  elif groupByValueAndCount cards = [2; 2; 1] then TwoPairs
  elif groupByValueAndCount cards = [2; 1; 1; 1] then Pair
  else HighCard
  
let compare (p1Hand: string) (p2Hand: string) : ComparisonResult =
  let idx rank = List.findIndex (fun r -> r = rank) [HighCard; Pair; TwoPairs; ThreeOfAKind; Straight; Flush; FullHouse; FourOfAKind; StraightFlush]
  let p1Rank = rank p1Hand  
  let p2Rank = rank p2Hand
  if idx p1Rank > idx p2Rank
  then Winner(player = P1, rank = p1Rank, hand = p1Hand)
  else Winner(player = P2, rank = p2Rank, hand = p2Hand) 
  
  
