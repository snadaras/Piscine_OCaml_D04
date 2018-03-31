(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Deck.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/03/30 21:44:33 by snadaras          #+#    #+#             *)
(*   Updated: 2018/03/30 21:44:37 by snadaras         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Color =
    struct
        type t = Spade | Heart | Diamond | Club

        let all = [Spade; Heart; Diamond; Club]

        let toString color = match color with
            | Spade     -> "S"
            | Heart     -> "H"
            | Diamond   -> "D"
            | Club      -> "C"
    
        let toStringVerbose color = match color with
            | Spade     -> "Spade"
            | Heart     -> "Heart"
            | Diamond   -> "Diamond"
            | Club      -> "Club"
    end

module Value =
    struct
        type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

        let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

        let toInt value = match value with
            | T2    -> 1
            | T3    -> 2
            | T4    -> 3
            | T5    -> 4
            | T6    -> 5
            | T7    -> 6
            | T8    -> 7
            | T9    -> 8
            | T10   -> 9
            | Jack  -> 10
            | Queen -> 11
            | King  -> 12
            | As    -> 13

        let toString value = match value with
            | T2    -> "2"
            | T3    -> "3"
            | T4    -> "4"
            | T5    -> "5"
            | T6    -> "6"
            | T7    -> "7"
            | T8    -> "8"
            | T9    -> "9"
            | T10   -> "10"
            | Jack  -> "J"
            | Queen -> "Q"
            | King  -> "K"
            | As    -> "A"

        let toStringVerbose value = match value with
            | T2    -> "2"
            | T3    -> "3"
            | T4    -> "4"
            | T5    -> "5"
            | T6    -> "6"
            | T7    -> "7"
            | T8    -> "8"
            | T9    -> "9"
            | T10   -> "10"
            | Jack  -> "Jack"
            | Queen -> "Queen"
            | King  -> "King"
            | As    -> "As"

        let next value = match value with
            | T2    -> T3
            | T3    -> T4
            | T4    -> T5
            | T5    -> T6
            | T6    -> T7
            | T7    -> T8
            | T8    -> T9
            | T9    -> T10
            | T10   -> Jack
            | Jack  -> Queen
            | Queen -> King
            | King  -> As
            | As    -> invalid_arg "Nothing after As"

        let previous value = match value with
            | T2    -> invalid_arg "Nothing before 2"
            | T3    -> T2
            | T4    -> T3
            | T5    -> T4
            | T6    -> T5
            | T7    -> T6
            | T8    -> T7
            | T9    -> T8
            | T10   -> T9
            | Jack  -> T10
            | Queen -> Jack
            | King  -> Queen
            | As    -> King
    end
module Card =
    struct
        type t = (Value.t * Color.t)

        let newCard value color =
            (value, color)

        let flip f a b = f b a 

        let allSpades = List.map (flip newCard Color.Spade) Value.all
        let allHearts = List.map (flip newCard Color.Heart) Value.all
        let allDiamonds = List.map (flip newCard Color.Diamond) Value.all
        let allClubs = List.map (flip newCard Color.Club) Value.all

        let all = List.flatten [allSpades; allHearts; allDiamonds; allClubs]

        let getValue (a, _) = a
        let getColor (_, b) = b
        
        let toString card = Value.toString(getValue card) ^ Color.toString(getColor card)
        let toStringVerbose card = "Card(" ^ Value.toStringVerbose(getValue card)
                                ^ ", " ^ Color.toStringVerbose(getColor card) ^ ")"
        
        let compare (a, _) (b, _) =
            Value.toInt a - Value.toInt b
        
        let max a b =
            if b > a then   b
            else            a
        
        let min a b =
            if b < a then   b
            else            a
        
        let best lst = match lst with
            | []        		-> invalid_arg "List is empty"
            | head	::	tail    -> List.fold_left max head lst
        
        let isOf (_, a) color = a = color

        let isSpade a = isOf a Color.Spade
        let isHeart a = isOf a Color.Heart
        let isDiamond a = isOf a Color.Diamond
        let isClub a = isOf a Color.Club
    end

type t = Card.t list

let newDeck () =
    let random x y =
        Random.self_init (); 
        if Random.bool () then 1 else -1
        in
        List.sort random Card.all

let toStringList deck = List.map Card.toString deck
let toStringListVerbose deck = List.map Card.toStringVerbose deck

let drawCard deck =
    if List.length deck > 0 then (List.hd deck, List.tl deck)
    else invalid_arg "Deck is empty"
    

(* ************************************************************************** *)
