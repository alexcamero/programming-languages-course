(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* ---------------------------------------------------------------------------------------- *)

(* 1(a) *)

fun all_except_option(s: string, sl: string list) =
   case sl of
      [] => NONE
      | x::xl => if same_string(s,x)
                  then SOME xl
                  else case all_except_option(s,xl) of
                     NONE => NONE
                     | SOME yl => SOME (x::yl)


(* 1(b) *)

fun get_substitutions1(substitutions: string list list, s: string) =
   case substitutions of
      [] => []
      | sl::sll => case all_except_option(s,sl) of
                     NONE => get_substitutions1(sll,s)
                     | SOME l => l @ get_substitutions1(sll,s)



(* 1(c) *)

fun get_substitutions2(substitutions: string list list, s: string) =
   let
      fun helper_fun(subs: string list list, acc: string list, st: string) =
         case subs of
            [] => acc
            | sl::sll => case all_except_option(st,sl) of
                           NONE => helper_fun(sll,acc,st)
                           | SOME nl => helper_fun(sll,acc @ nl,st)
   in
      helper_fun(substitutions, [], s)
   end
   



(* 1(d) *)

fun similar_names(substitutions: string list list, full_name: {first:string,middle:string,last:string}) =
   case full_name of
         {first=F, middle=M, last=L} =>
         let
            fun helper(subs_list: string list, names_list: {first:string,middle:string,last:string} list) =
               case subs_list of
                  [] => names_list
                  | sub::sl => helper(sl, {first = sub, middle = M, last = L}::names_list)
         in
            full_name::helper(get_substitutions2(substitutions,F),[])
         end



(* ---------------------------------------------------------------------------------------- *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* ---------------------------------------------------------------------------------------- *)

(* 2(a) *)

fun card_color(c: card) =
   case c of
      (Clubs,_) => Black
      | (Spades,_) => Black
      | _ => Red



(* 2(b) *)

fun card_value(c: card) =
   case c of
      (_,Num(x)) => x
      | (_,Ace) => 11
      | _ => 10



(* 2(c) *)

fun remove_card(cs: card list, c: card, e: exn) =
   case cs of
      [] => raise e
      | x::cl => if x = c
                  then cl
                  else x::remove_card(cl,c,e)



(* 2(d) *)

fun all_same_color(cs: card list) =
   case cs of
      [] => true
      | c::[] => true
      | c1::c2::more => if card_color(c1) = card_color(c2)
                        then all_same_color(c2::more)
                        else false



(* 2(e) *)

fun sum_cards(cs: card list) =
   let
      fun sum_helper(sum_so_far: int, cards: card list) =
         case cards of
            [] => sum_so_far
            | c::more => sum_helper(sum_so_far+card_value(c),more)
   in
      sum_helper(0,cs)
   end



(* 2(f) *)

fun score(held: card list, goal: int) =
   let
      val preliminary = 
         let
            val total = sum_cards(held)
         in
            if total > goal
            then 3 * (total - goal)
            else goal - total
         end
   in
      if all_same_color(held)
      then preliminary div 2
      else preliminary
   end



(* 2(g) *)

fun officiate(card_list: card list, move_list: move list, goal: int) =
   let
      fun process_state(deck: card list, hand: card list, moves: move list, current_sum: int) =
         case moves of
            [] => score(hand,goal)
            | m::more_moves => case m of
                                 Discard(c) => process_state(deck, remove_card(hand, c, IllegalMove), more_moves, current_sum - card_value(c))
                                 | Draw => case deck of
                                    [] => score(hand,goal)
                                    | c::more_cards => if current_sum + card_value(c) > goal
                                                      then score(c::hand,goal)
                                                      else process_state(more_cards,c::hand,more_moves,current_sum + card_value(c))
   in
      process_state(card_list, [], move_list, 0)
   end


(* ---------------------------------------------------------------------------------------- *)


(* 3(a) *)

(* A quick check shows that lowering the sum by 10 results in a better score if and only if 
the current sum is at least three more than the goal. So we minimize the score by starting 
with the sum with all aces counting 11, and then reducing the sum by 10 for each ace
(switching from 11 to 1) until either we have no more aces or the sum falls below goal + 2 *)

fun score_challenge(held: card list, goal: int) =
   let
      fun preliminary_score(sum: int) =
         if sum > goal
         then 3*(sum - goal)
         else goal - sum

      fun preliminary_sum(cards: card list, current_sum: int, aces: int) =
            case cards of
               [] => if aces = 0
                     then current_sum
                     else if current_sum > goal + 2
                           then preliminary_sum([],current_sum - 10, aces - 1)
                           else current_sum
               | c::more => case c of
                              (_,Ace) => preliminary_sum(more,current_sum + 11,aces+1)
                              | _ => preliminary_sum(more,current_sum + card_value(c), aces)
   in
      let
         val preliminary = preliminary_score(preliminary_sum(held,0,0))
      in
         if all_same_color(held)
         then preliminary div 2
         else preliminary
      end
   end

(* Helper function to only add one for aces instead of 11  in the officiate challenge function *)

fun card_value_challenge(c: card) =
   case c of
      (_,Num(x)) => x
      | (_,Ace) => 1
      | _ => 10

fun officiate_challenge(card_list: card list, move_list: move list, goal: int) =
   let
      fun process_state(deck: card list, hand: card list, moves: move list, current_sum: int) =
         case moves of
            [] => score_challenge(hand,goal)
            | m::more_moves => case m of
                                 Discard(c) => process_state(deck, remove_card(hand, c, IllegalMove), more_moves, current_sum - card_value_challenge(c))
                                 | Draw => case deck of
                                    [] => score_challenge(hand,goal)
                                    | c::more_cards => if current_sum + card_value_challenge(c) > goal
                                                      then score_challenge(c::hand,goal)
                                                      else process_state(more_cards,c::hand,more_moves,current_sum + card_value_challenge(c))
   in
      process_state(card_list, [], move_list, 0)
   end


(* 3(b) *)

(* Helper function to reverse lists *)

fun reverse_list(X) =
   let
      fun reverse_aux(remaining,so_far) =
         case remaining of
            [] => so_far
            | y::more => reverse_aux(more,y::so_far)
   in
      reverse_aux(X,[])
   end

fun careful_player(card_list: card list, goal: int) =
   let
      fun careful_helper(cards: card list, current_hand: card list, current_value: int, current_moves: move list) =
         if goal - current_value > 10
         then case cards of
                  [] => Draw::current_moves
                  | c::more => careful_helper(more,c::current_hand,current_value+card_value(c),Draw::current_moves)
         else if current_value = goal
               then current_moves
               else case cards of
                     [] => current_moves
                     | c::more => let
                                    val c_diff = goal - current_value;
                                    val ncv = card_value(c);

                                    fun check_against_hand(held: card list, new_card_val: int, current_diff: int) =
                                       case held of
                                          [] => []
                                          | h::more_hand => if current_diff + card_value(h) - new_card_val = 0
                                                            then [Discard(h),Draw]
                                                            else check_against_hand(more_hand, new_card_val, current_diff)
                                 in
                                    case check_against_hand(current_hand,ncv,c_diff) of
                                       [] => current_moves
                                       | m1::m2::rest => m2::m1::current_moves
                                 end

   in
      reverse_list(careful_helper(card_list,[],0,[]))
   end