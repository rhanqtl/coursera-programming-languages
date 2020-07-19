(* Problem 1 *)

(*
 * if you use this function to compare two strings (returns true if the same
 * string), then you avoid several of the functions in problem 1 having
 * polymorphic types that may be confusing
 *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (opt: string, strings: string list) =
    let fun helper (xs: string list, acc: string list) =
            case xs of
                []     => NONE
              | x::xs' => if same_string(opt, x) then
                              SOME (acc @ xs')
                          else
                              helper(xs', acc @ [x])
    in helper(strings, [])
    end

fun get_substitutions1 (subs: string list list, s: string) =
    case subs of
        []      => []
      | x :: xs => let val ans = all_except_option (s, x);
                   in case ans of
                          NONE   => get_substitutions1 (xs, s)
                        | SOME y => get_substitutions1 (xs, s) @ y
                   end

fun get_substitutions2 (subs: string list list, s: string) =
    let fun helper (xs: string list list, acc: string list) =
            case xs of
                [] => acc
              | (x :: xs') => let val ans = all_except_option (s, x);
                              in case ans of
                                     NONE   => helper (xs', acc)
                                   | SOME y => helper (xs', acc @ y)
                              end
    in helper(subs, [])
    end

fun similar_names (subs: string list list,
                   full_name: {first:string, middle:string, last:string}) =
    let val alternatives = case full_name of
                               {first = s, middle = _, last = _} => get_substitutions2 (subs, s);
        fun helper (xs: string list) =
            case xs of
                []        => []
              | x' :: xs' => case full_name of
                                 {first = x, middle = y, last = z} =>  {first = x', middle = y, last = z} :: helper (xs')
    in full_name :: helper (alternatives)
    end

(* Problem 2 *)

(*
 * you may assume that Num is always used with values
 *     2, 3, ..., 10
 * though it will not really come up
 *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (c) =
    case c of
        (Spades,   _) => Black
      | (Clubs,    _) => Black
      | (Diamonds, _) => Red
      | (Hearts,   _) => Red

fun card_value (c) =
    case c of
        (_, Num n) => n
      | (_, Ace)   => 11
      | _          => 10

fun remove_card (cs: card list, c: card, e) =
    case cs of
        []        => raise e
      | c' :: cs' => if c' = c then
                         cs'
                     else
                         c' :: remove_card (cs', c, e)

fun all_same_color (cs: card list) =
    case cs of
        []            => true
      | c :: []       => true
      | c :: d :: cs' => if card_color (c) = card_color (d) then
                             all_same_color (d :: cs')
                         else
                             false

fun sum_cards (cs: card list) =
    let fun helper (cs: card list, acc) =
            case cs of
                []       => acc
              | c :: cs' => helper (cs', acc + card_value (c))
    in helper (cs, 0)
    end

fun score (held_cards: card list, goal: int) =
    let val sum = sum_cards (held_cards);
        val pre_score = if sum > goal then
                            3 * (sum - goal)
                        else
                            goal - sum
    in if all_same_color (held_cards) then
           pre_score div 2
       else
           pre_score
    end

fun officiate (cards: card list, moves: move list, goal: int) =
    let fun helper (cs: card list, ms: move list, held_cards: card list) =
            case ms of
                []       => held_cards
              | m :: ms' => case m of
                                Discard c => helper (cs, ms', remove_card (held_cards, c, IllegalMove))
                              | Draw      => case cs of
                                                 [] => held_cards
                                               | c :: cs' => let val held_cards_new = c :: held_cards;
                                                             in if sum_cards (held_cards_new) > goal then
                                                                    held_cards_new
                                                                else
                                                                    helper (cs', ms', held_cards_new)
                                                             end
    in score (helper (cards, moves, []), goal)
    end

fun card_value_challenge (c, ace_val) =
    case c of
        (_, Num n) => n
      | (_, Ace)   => ace_val
      | _          => 10

fun sum_cards_challenge (cs: card list, ace_val: int) =
    let fun helper (cs: card list, acc) =
            case cs of
                []       => acc
              | c :: cs' => helper (cs', acc + card_value_challenge (c, ace_val))
    in helper (cs, 0)
    end

fun score_challenge (held_cards: card list, goal: int) =
    let exception EmptyList;
        fun min (xs) =
            case xs of
                []            => raise EmptyList
              | x :: []       => x
              | x :: y :: xs' => min (Int.min(x, y) :: xs')
        val sums = [sum_cards_challenge (held_cards, 1), sum_cards_challenge (held_cards, 11)];
        val pre_scores = List.map (fn (sum) => if sum > goal then 3 * (sum - goal) else (goal - sum)) sums;
        val min_pre_score = min (pre_scores);
    in if all_same_color (held_cards) then
           min_pre_score div 2
       else
           min_pre_score
    end

fun officiate_challenge (cards: card list, moves: move list, goal: int) =
    let fun helper (cs: card list, ms: move list, held_cards: card list) =
            case ms of
                []       => held_cards
              | m :: ms' => case m of
                                Discard c => helper (cs, ms', remove_card (held_cards, c, IllegalMove))
                              | Draw      => case cs of
                                                 [] => held_cards
                                               | c :: cs' => let val held_cards_new = c :: held_cards;
                                                             in if sum_cards_challenge (held_cards_new, 1) > goal andalso sum_cards_challenge (held_cards_new, 11) > goal then
                                                                    held_cards_new
                                                                else
                                                                    helper (cs', ms', held_cards_new)
                                                             end
    in score_challenge (helper (cards, moves, []), goal)
    end

(*
 * Rules:
 * - The value of the held cards never exceeds the goal.
 * - A card is drawn whenever the goal is more than 10 greater than the value of the held cards.
 *+  As a detail, you should (attempt to) draw, even if no cards remain in the card-list.
 * - If it is possible to reach a score of 0 by discarding a card followed by drawing a card,
 *+  then this must be done. Note careful_player will have to look ahead to the next card,
 *+  which in many card games is considered “cheating.” Also note that the previous requirement
 *+  takes precedence: There must be no more moves after a score of 0 is reached even if
 *+  there is another way to get back to 0.
 *)
fun careful_player (cs: card list, goal: int) =
    let fun helper (cs: card list, held_cards: card list, ms: move list) =
            ms
    in []
    end
