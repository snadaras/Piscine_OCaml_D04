(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Card.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/03/30 21:42:26 by snadaras          #+#    #+#             *)
(*   Updated: 2018/03/30 21:42:35 by snadaras         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


module Color =
    struct
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
    end

module Value =
    struct
        type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

        let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

		let toInt t = match t with
        		  | T2      -> 1
		          | T3      -> 2
 		          | T4      -> 3
      			  | T5      -> 4
    		      | T6      -> 5
          		  | T7      -> 6
          		  | T8      -> 7
          		  | T9      -> 8
          		  | T10     -> 9
            	  | Jack    -> 10
          		  | Queen   -> 11
          		  | King    -> 12
          		  | As      -> 13


        let toString t = match t with
                  | T2      -> "2"
                  | T3      -> "3"
                  | T4      -> "4"
                  | T5      -> "5"
                  | T6      -> "6"
                  | T7      -> "7"
                  | T8      -> "8"
                  | T9      -> "9"
                  | T10     -> "10"
                  | Jack    -> "J"
                  | Queen   -> "Q"
                  | King    -> "K"
                  | As      -> "A"

        let toStringVerbose t = match t with
                  | T2      -> "2"
                  | T3      -> "3"
                  | T4      -> "4"
                  | T5      -> "5"
                  | T6      -> "6"
                  | T7      -> "7"
                  | T8      -> "8"
                  | T9      -> "9"
                  | T10     -> "10"
                  | Jack    -> "Jack"
                  | Queen   -> "Queen"
                  | King    -> "King"
                  | As      -> "As"

        let next t = match t with 
                  | T2      -> T3
                  | T3      -> T4
                  | T4      -> T5
                  | T5      -> T6
                  | T6      -> T7
                  | T7      -> T8
                  | T8      -> T9
                  | T9      -> T10
                  | T10     -> Jack
                  | Jack    -> Queen
                  | Queen   -> King
                  | King    -> As
                  | As      -> invalid_arg "don't have next value after As"
          
        let previous t = match t with
                  | T2      -> invalid_arg "don't have previous before T2"
                  | T3      -> T2
                  | T4      -> T3
                  | T5      -> T4
                  | T6      -> T5
                  | T7      -> T6
                  | T8      -> T7
                  | T9      -> T8
                  | T10     -> T9
                  | Jack    -> T10
                  | Queen   -> Jack
                  | King    -> Queen
                  | As      -> King
    end

type t =  { v:Value.t; c:Color.t }

let newCard value color = { v = value; c = color }

let getValue t = t.v
let getColor t = t.c

let toString t = (Value.toString t.v) ^ (Color.toString t.c)
let toStringVerbose t = Printf.sprintf "Card (%s, %s)" (Value.toStringVerbose t.v) (Color.toStringVerbose t.c)

let allSpades =
    let rec all l = match l with
        | []            -> []
        | head :: tail  -> (newCard head Color.Spade) :: all tail
    in
    all  Value.all 

let allHearts =
    let rec all l = match l with
        | []            -> []
        | head :: tail  -> (newCard head Color.Heart) :: all tail
    in
    all  Value.all 

let allDiamonds =
    let rec all l = match l with
        | []            -> []
        | head :: tail  -> (newCard head Color.Diamond) :: all tail
    in
    all  Value.all 

let allClubs =
    let rec all l = match l with
        | []            -> []
        | head :: tail  -> (newCard head Color.Club) :: all tail
    in
    all  Value.all 

let all = allSpades @ allHearts @ allDiamonds @ allClubs

let isSpade t   = t.c = Color.Spade
let isHeart t   = t.c = Color.Heart
let isDiamond t = t.c = Color.Diamond
let isClub t    = t.c = Color.Club
let isOf t c    = t.c = c 

let max t1 t2 = if getValue t1 >= getValue t2
                then t1
                else t2

let min t1 t2 = if getValue t1 <= getValue t2
                then t1
                else t2

let compare t1 t2 = if getValue t1 > getValue t2
                    then 1
                    else if getValue t1 < getValue t2
                    then -1
                    else 0

let best l = 
    let rec is_in_list lst value = match lst with
        | []            -> false
        | head :: tail  -> if getValue head = value
                           then true
                           else is_in_list tail value
    in
    let rec check_list l = match l with
        | []            -> invalid_arg "List is empty"
        | head :: []    -> head
        | head :: tail  -> if is_in_list tail (getValue head)
                           then head
                           else check_list tail
    in
    check_list l
