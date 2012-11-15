type suit = Spades | Hearts | Diamonds | Clubs

type card =
  | Joker
  | King of suit
  | Queen of suit
  | Jack of suit
  | Minor of suit * int

let rec interval s e =
  if s >= e then [e] else s :: (interval (s + 1) e)

let deck =
  let suits = [Spades; Hearts; Diamonds; Clubs] in
  let face suit = [King suit; Queen suit; Jack suit] in
  Joker :: List.concat (List.map face suits)
    @ List.concat
        (List.map (fun n -> List.map (fun s -> Minor (s, n)) suits)
           (interval 1 9))

let pick i deck =
  let rec aux i deck_pre deck =
    match deck with
    | []     -> failwith "oops"
    | h :: t ->
        if i = 0 then
          (h, deck_pre @ t)
        else
          aux (i - 1) (h :: deck_pre) t
  in
  aux i [] deck

let shuffle deck =
  let rec aux shuffled deck =
    match deck with
    | [] -> shuffled
    | _  ->
        let (card, deck) = pick (Random.int (List.length deck)) deck in
        aux (card :: shuffled) deck
  in
  aux [] deck

let draw deck = (List.hd deck, List.tl deck)

let point = function
  | Joker        -> 13
  | King _       -> 12
  | Queen _      -> 11
  | Jack _       -> 10
  | Minor (_, n) -> n

let string_of_suit = function
  | Spades   -> "♠"
  | Hearts   -> "♥"
  | Diamonds -> "♦"
  | Clubs    -> "♣"

let string_of_card = function
  | Joker      -> "☻"
  | King suit  -> (string_of_suit suit) ^ "K"
  | Queen suit -> (string_of_suit suit) ^ "Q"
  | Jack suit  -> (string_of_suit suit) ^ "J"
  | Minor (suit, n) -> (string_of_suit suit) ^ (string_of_int n)

let game n =
  Random.self_init ();
  let rec aux deck i win lose =
    if i = n then (
      Printf.printf "Your points: %d\n" win;
      Printf.printf "OCaml's points: %d\n" lose;
      Printf.printf "%s\n"
        (if win > lose then "You win!"
         else if win = lose then "Tie."
         else "You lose..."
        )
    ) else (
      let (your_card, deck) = draw deck in
      let (ocaml_card, deck) = draw deck in
      let your_point = point your_card in
      let ocaml_point = point ocaml_card in
      Printf.printf "\nGame #%d. Ready?" (i + 1);
      ignore (read_line ());
      Printf.printf "Your card: %s\n" (string_of_card your_card);
      Printf.printf "OCaml's card: %s\n" (string_of_card ocaml_card);
      if your_point > ocaml_point then (
        Printf.printf "You win!\n";
        aux deck (i + 1) (win + 1) lose
      ) else if your_point = ocaml_point then (
        Printf.printf "Tie.\n";
        aux deck (i + 1) win lose
      ) else (
        Printf.printf "You lose...\n";
        aux deck (i + 1) win (lose + 1)
      )
    )
  in
  aux (shuffle deck) 0 0 0
