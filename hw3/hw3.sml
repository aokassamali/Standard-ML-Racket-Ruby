(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs  = List.filter (fn x => Char.isUpper (String.sub (x,0))) xs

fun longest_string1 xs = List.foldl (fn (x ,acc) => if String.size x > String.size acc then x else acc) "" xs;

fun longest_string2 xs = List.foldl (fn (x ,acc) => if String.size x >= String.size acc then x else acc) "" xs;
		  
fun longest_string_helper f xs =  List.foldl (fn (x ,acc) => if f (String.size x, String.size acc) then x else acc) "" xs

val longest_string3 = longest_string_helper (fn (x, acc) => x > acc)

val longest_string4 = longest_string_helper (fn (x, acc) => x >= acc)

val longest_capitalized  = longest_string1 o only_capitals 

fun rev_string s = (String.implode o List.rev o String.explode) s
	      
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME v => v 

fun all_answers f xs =
    let
	fun aux xs acc =
	    case xs of
		[] => SOME acc
	      | x::xs =>  case f x of
			      NONE => NONE
			    | SOME v => aux xs (v @ acc)	
    in
	aux xs []
    end

val count_wildcards = g (fn () => 1) (fn _ => 0)
    
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

fun count_some_var (s,p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p
			     
fun check_pat p =
    let
	fun var_list p  =
	    case p of
		Wildcard          => []
	      | Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (p ,acc) => var_list p @ acc) [] ps
	      | ConstructorP(_,p) => var_list p
	      | _                 => []		     
	fun exists xs =
	    case xs of
		[] => true
	      | x::xs' =>  if List.exists (fn y => x = y) xs' then false else exists xs'
    in	       
	exists(var_list p)
    end

fun match (v, p) =
    case (v,p) of
	(_, Wildcard) => SOME []
      | (_, Variable x) => SOME [(x, v)]
      | (Unit, UnitP) => SOME []
      | (Const n, ConstP i)  => if i=n then SOME [] else NONE
      | (Tuple xs, TupleP ys) =>
	if List.length xs = List.length ys
	then all_answers match (ListPair.zip(xs,ys))
	else NONE
      | (Constructor(s1,a), ConstructorP(s2,b)) =>
	if s1 = s2 then match(a,b) else NONE
      | _ => NONE 


fun first_match v ps =
    SOME (first_answer (fn x => match(v , x)) ps)
    handle NoAnswer => NONE



fun typecheck_patterns (constructors, patterns) =
    let
        val find_constructor_type = 
            fn c_name => fn [] => NONE
                | ((c, d, t) :: cs) => if c_name = c then SOME (d, t) else find_constructor_type c_name cs

        val unify_types = 
            fn (Anything, t) => t
                | (t, Anything) => t
                | (UnitT, UnitT) => UnitT
                | (IntT, IntT) => IntT
                | (TupleT ts1, TupleT ts2) => TupleT (ListPair.map (unify_types) (ts1, ts2))
                | (Datatype d1, Datatype d2) => if d1 = d2 then Datatype d1 else Anything
                | (_, _) => Anything

        val pattern_type = fn _ => Wildcard => SOME Anything
                | _ => (Variable _) => SOME Anything
                | _ => UnitP => SOME UnitT
                | _ => (ConstP _) => SOME IntT
                | constructors => (TupleP ps) =>
                    let
                        val patterns_types = 
                            fn [] => SOME []
                                | (p::ps) => (case pattern_type constructors p of
                                                NONE => NONE
                                                | SOME t => (case patterns_types ps of
                                                                NONE => NONE
                                                                | SOME ts => SOME (t::ts)))
                    in
                        case patterns_types ps of
                            NONE => NONE
                            | SOME ts => SOME (TupleT ts)
                    end
                | constructors => (ConstructorP (c_name, p)) =>
                    (case find_constructor_type c_name constructors of
                        NONE => NONE
                        | SOME (_, typ) => pattern_type constructors p)
        
        val check = fn [] => fn acc => acc
                | (p::ps) => fn acc =>
                    (case pattern_type constructors p of
                        NONE => NONE
                        | SOME t => (case acc of
                                        NONE => check ps (SOME t)
                                        | SOME t_acc => check ps (SOME (unify_types t t_acc))))

    in
        check patterns NONE
    end
