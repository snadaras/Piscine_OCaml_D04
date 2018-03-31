(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/03/30 21:40:52 by snadaras          #+#    #+#             *)
(*   Updated: 2018/03/30 21:40:56 by snadaras         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let main () =

    let rec loop all = match all with
        | []      		  -> 	()
        | head :: tail    ->	print_endline ((Color.toString head)^ " : " ^ 
        						(Color.toStringVerbose head) ^ "\n"); 
        						loop tail
    in 
    loop Color.all

(* ************************************************************************** *)

let () = main ()
