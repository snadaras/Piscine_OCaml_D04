(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/03/30 21:42:54 by snadaras          #+#    #+#             *)
(*   Updated: 2018/03/30 21:42:58 by snadaras         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let main () = 
	print_endline "booleen : Color As Diamond is Diamond";
    print_endline 
    (string_of_bool	(Card.isDiamond (Card.newCard Card.Value.As Card.Color.Diamond)));
    print_endline "booleen : Color As Heart is Spade";
    print_endline 
    (string_of_bool (Card.isSpade (Card.newCard Card.Value.As Card.Color.Heart)));
    print_endline "booleen : As Color Diamond is a card of Diamond's Cards";
    print_endline 
    (string_of_bool (Card.isOf (Card.newCard Card.Value.As Card.Color.Diamond) Card.Color.Diamond));
    print_endline "booleen : As Color Club is a card of Diamond's Cards";
    print_endline 
    (string_of_bool (Card.isOf (Card.newCard Card.Value.As Card.Color.Club) Card.Color.Diamond));
    print_endline "max between King Spade and Queen Heart";
    print_endline 
    ( Card.toString (Card.max (Card.newCard Card.Value.King Card.Color.Spade) (Card.newCard Card.Value.Queen Card.Color.Heart)));
    print_endline "min between As Spade and Jack Diamond";
    print_endline 
    ( Card.toString (Card.min (Card.newCard Card.Value.As Card.Color.Spade) (Card.newCard Card.Value.Jack Card.Color.Diamond)));
    print_endline "Compare value between As Spade and As Diamond";
    print_endline 
    ( string_of_int (Card.compare (Card.newCard Card.Value.As Card.Color.Spade) (Card.newCard Card.Value.As Card.Color.Diamond)));
    print_endline "Compare value between Queen Heart and As Diamond";
    print_endline 
    ( string_of_int (Card.compare (Card.newCard Card.Value.Queen Card.Color.Heart) (Card.newCard Card.Value.As Card.Color.Diamond)));
    print_endline "Compare value between As Spade and 9 Diamond";
    print_endline 
    ( string_of_int (Card.compare (Card.newCard Card.Value.As Card.Color.Spade) (Card.newCard Card.Value.T9 Card.Color.Diamond)));
    print_endline "-------------";
    let rec display all = match all with
        | []            -> ()
        | head :: tail  -> print_endline (Card.toStringVerbose head); display tail
    in
    display Card.all;

    print_endline "-------------";
    let rec display l = match l with
        | []            -> ()
        | head :: tail  -> print_endline (Card.toString head); display tail
    in
    display Card.allSpades;

    print_endline "-------------";
    print_endline 
    	( Card.toStringVerbose (Card.best 
    	[(Card.newCard Card.Value.King Card.Color.Spade); 
    	(Card.newCard Card.Value.As Card.Color.Diamond)]));
    print_endline 
    	( Card.toStringVerbose (Card.best 
    	[(Card.newCard Card.Value.King Card.Color.Spade); 
    	(Card.newCard Card.Value.As Card.Color.Diamond) ;
        (Card.newCard Card.Value.King Card.Color.Diamond)]));
    print_endline 
   		( Card.toStringVerbose (Card.best 
   		[(Card.newCard Card.Value.As Card.Color.Spade); 
   		 (Card.newCard Card.Value.As Card.Color.Heart);
    	(Card.newCard Card.Value.As Card.Color.Diamond)]));
    print_endline " - empty list : ";
    print_endline ( Card.toStringVerbose (Card.best []))

let () = main ()
