(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/03/30 21:41:19 by snadaras          #+#    #+#             *)
(*   Updated: 2018/03/30 21:41:24 by snadaras         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let main () =
    
    let rec loop lst = match lst with
        | []        	-> 	()
        | head :: tail  -> 	print_endline 	
        					("Card : "
    						^ Value.toStringVerbose head
                           	^ " ("
                            ^ Value.toString head
                            ^ ") | Value : "
                            ^ string_of_int (Value.toInt head));
                            if head <> Value.T2
                            then print_endline 
                            (" | Previous " 
                            ^ Value.toString (Value.previous head));
                            if head <> Value.As
                            then print_endline
                            (" | Next " 
                            ^ Value.toString (Value.next head));
                            print_newline ();
        					loop tail
    in
    loop Value.all 

(* ************************************************************************** *)

let () = main ()