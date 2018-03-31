(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/03/30 21:43:42 by snadaras          #+#    #+#             *)
(*   Updated: 2018/03/30 21:43:46 by snadaras         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let main () =
    print_string "----------------"; 
    print_newline ();
    print_string "random 1 - short output for all 52 cards : "; 
    print_newline ();
    let rec displayDeck deck = match deck with
            | []            -> ()
            | head :: tail  -> print_string ((head) ^ " "); displayDeck tail
    in
    displayDeck (Deck.toStringList (Deck.newDeck ()));
    print_newline ();
    print_string "----------------"; 
    print_newline ();
    print_string "random 2 - full output for all 52 cards : ";
    print_newline ();
    let deck = Deck.newDeck () in
    let rec displayDeck deck = match deck with
            | []            -> ()
            | head :: tail  -> print_string ((head) ^ " "); displayDeck tail
    in
    displayDeck (Deck.toStringListVerbose deck);
    print_newline ();
    print_string "----------------"; 
    print_newline ();
    let (n, c) as card = Deck.drawCard deck in
        print_string "Tirage 1 : ";
        print_endline (Deck.Card.toStringVerbose n);
    print_newline ();
    print_string "----------------"; 
    print_newline ();
    let (n, c) as card = Deck.drawCard c in
        print_string "Tirage 2 : ";
        print_endline (Deck.Card.toStringVerbose n);
    print_newline ();
    print_string "----------------"; 
    print_newline ();
    let (n, c) as card = Deck.drawCard c in
        print_string "Tirage 3: "; 
        print_endline (Deck.Card.toStringVerbose n);
    print_newline ();
    print_string "----------------"; 
    print_newline ()

let () = main ()
