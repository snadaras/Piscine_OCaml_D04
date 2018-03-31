(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Color.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/03/30 21:40:39 by snadaras          #+#    #+#             *)
(*   Updated: 2018/03/30 21:40:46 by snadaras         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


type t = Spade | Heart | Diamond | Club

let all = [Spade; Heart; Diamond; Club]

let toString t = match t with
        | Spade     -> "S"
        | Heart     -> "H"
        | Diamond   -> "D"
        | Club      -> "C"

let toStringVerbose t = match t with
        | Spade     -> "Spade"
        | Heart     -> "Heart"
        | Diamond   -> "Diamond"
        | Club      -> "Club"

