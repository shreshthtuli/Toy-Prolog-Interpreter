(* COL226 - Assignment 6. Shreshth Tuli - 2016CS10680 *)

open Unix;;
open Printf;;

type term = Var of string | Const of int | Sym of string * term list;;
type atom = Atom of string * term list | Cut | Fail;;
type head = atom;;
type body = atom list;;
type clause = Fact of head | Rule of head * body;;
type program = clause list;;
type goal = atom list;;


type substitution = (term * term) list;;
type answer = true | false | Ans of substitution | ManyAns of answer list;;

exception Error;;
exception NOT_UNIFIABLE;;

let rec map f l = match l with
	  [] -> []
  | x::xs -> (f x)::(map f xs);;

let rec foldl f e l = match l with
	  [] -> e
  | x::xs -> foldl f (f(x,e)) xs;;

let rec foldr f e l = match l with
	[] -> e
  | x::xs -> f(x,(foldr f e xs));;

let rec vars t = 
  (** [val vars : term -> term list = <fun>]  *)
  match t with
	Var x -> [Var x]
  | Const n -> []
  | Sym(sym, []) -> []
  | Sym(sym, l) -> let rec union (x,y) = match x with
                        [] -> y
                      | x::xs -> if (List.mem x y) then union (xs, y) else x::(union (xs, y)) in
  			foldl union [] (map vars l);; 

let rec vars_atom g = 
	(** [val vars_atom : atom -> term list = <fun>] *)
	match g with
	Atom(s, t) -> 
	let rec v t = match t with
		[] -> []
	  | (t1::tr) -> (vars t1)@(v tr) in
	v t;;

let rec subst (s:substitution) (x:term) = 
  (** [val subst : substitution -> term -> term = <fun>]  *)
  match x with
    Const n -> Const n
  | Var v -> let rec find v s = match s with
              [] -> Var v
            | (Var a,b)::xs -> if (v = a) then b else find v xs in
           find v s
  | Sym(sym, l) -> if (l = []) then Sym(sym, l)
  		   else let subst1 x = subst s x in
  		   let l' = map subst1 l in 
  		   Sym(sym, l');;


let compose (s1:substitution) (s2:substitution) : substitution = 
  (** [val compose : substitution -> substitution -> substitution = <fun>]  *)
  let s1' (a,b) = (a, subst s2 b) in
  let s1s2 = map s1' s1 in 
  let rec sigma2 l = match l with
     [] -> []
   | (a,b)::xs -> let rec member a l : bool = match l with
                    [] -> false
                  | (v,t)::xs -> if (a = v) then true else member a xs in
  if member a s1s2 then sigma2 xs else [(a,b)]@(sigma2 xs) in
  let rec rm_id l = match l with
     [] -> []
   | (a,b)::xs -> if (a = b) then (rm_id xs) else (a,b)::(rm_id xs) in
  rm_id (s1s2 @ (sigma2 s2));;


let rec mgu t u : substitution = 
  (** [val mgu : term -> term -> substitution = <fun>]  *) 
  match (t, u) with
    (Var x, Const n) -> [(Var x, Const n)]
  | (Const n, Var x) -> [(Var x, Const n)]
  |	(Var x, Var y) -> if (x = y) then [] else [(Var x, Var y)]
  | (Var x, Sym(sym, [])) -> [(Var x, Sym(sym, []))]
  | (Sym(sym, []), Var x) -> [(Var x, Sym(sym, []))]
  | (Var x, Sym(sym, l)) -> if (List.mem (Var x) (vars (Sym(sym, l)))) then raise NOT_UNIFIABLE 
                           else [(Var x, Sym(sym, l))]
  | (Sym(sym, l), Var x) -> if (List.mem (Var x) (vars (Sym(sym, l)))) then raise NOT_UNIFIABLE
                           else [(Var x, Sym(sym, l))]
  | (Sym(sym, []), Sym(sym', [])) -> if (sym = sym') then [] else raise NOT_UNIFIABLE
  | (Sym(sym, t'), Sym(sym', u')) -> if (List.length t' = List.length u' && sym = sym') then
  				let rec fold sigma t u = match (t,u) with
                  ([],[]) -> sigma
                | (t1::tr, u1::ur) -> fold (compose sigma (mgu (subst sigma t1) (subst sigma u1))) tr ur
                | _ -> raise Error in
          fold [] t' u'
   		  else raise NOT_UNIFIABLE;; 

let subst_atom sub a = 
	(** [val subst_atom : substitution -> atom -> atom = <fun>]  *)
	match a with
	Atom(s, l) -> 
	let subst_temp t = subst sub t in
	Atom(s, (map subst_temp l))
  | Cut -> Cut
  | Fail -> Fail;;


let rec keep (l:substitution) (atom:atom) : substitution =
	(** [val keep : substitution -> atom -> substitution = <fun>]  *) 
	match l with
	[] -> []
  | (v, v')::lr -> if (List.mem v (vars_atom atom)) then (v,v')::(keep lr atom) 
				   else (keep lr atom);;

let rec remove_vars (l:substitution) : substitution =
	(** [val remove_vars : substitution -> substitution = <fun>]  *)
	match l with
	[] -> []
  | (v, v')::lr -> if vars(v') = [] then (v,v')::(remove_vars lr) else (remove_vars lr);;


let mgu_union (s:term list) (t:term list) : substitution =
	(** [val mgu_union : term list -> term list -> substitution = <fun>]  *)
	mgu (Sym("test", s)) (Sym("test", t));;


let atom_list a = 
	(** [val atom_list : atom -> term list = <fun>]  *)
	match a with
	Atom(s, l) -> l;;

let union (a,b) = 
	(** [val union : 'a list * 'a list -> 'a list = <fun>]  *)
	a@b;;

let rec intersect ((s:substitution),(t:substitution)) : substitution =
	match s with
	[] -> []
  |	((s1,s2)::sr) -> let rec member v1 v2 y : bool = match y with
  						[] -> false
  					  | (a,b)::ys -> if a = v1 && b = v2 then true else member v1 v2 ys in
  					 if (member s1 s2 t) then (s1,s2)::(intersect(sr,t))
  					 else (intersect(sr,t));; 

let rec uniq l l1 = 
	(** [val uniq : 'a list -> 'a list -> 'a list = <fun>]  *)
	match l with
	[] -> l1
  | x::xs -> if (List.mem x l1) then uniq xs l1 else uniq xs (x::l1);;

exception Done;;
exception False;;

let rec intermediate (program:program) (goal:atom) : substitution = 
	(** [val intermediate : program -> atom -> substitution = <fun>]  *)
	match goal with Atom(s, l) -> 
	let rec find s p (b:bool) = match (p,b) with
		(Fact(Atom(s', l'))::pr,b) -> if (s=s') then 
									(try ((keep (mgu_union l l') goal)@(try (find s pr false) with Error -> [])) with NOT_UNIFIABLE -> find s pr false)
									else find s pr false
	  | (Rule(Atom(s', l'), l2)::pr,b) -> if (s=s') then 
	  								(try
	  								let substitute atom = 
	  								try (subst_atom (mgu_union l' l) atom) with NOT_UNIFIABLE -> raise Error in
	  								let transformed = if (List.mem Fail l2) then raise False else map substitute l2 in
	  								let run atom = 
	  									try(if(subst_atom (mgu_union l (atom_list(atom))) goal = atom) then raise Done
	  									else intermediate program atom) with NOT_UNIFIABLE -> intermediate program atom in
	  								let answers = try (if(List.mem Cut transformed) then [mgu_union l l'] 
	  												   else map run (List.rev(transformed)))
	  									with Done -> [] 
	  									| Error -> [] in
	  								let final = 
	  								if answers = [] then [] else if (foldr intersect (List.hd(answers)) answers)=[] then [] else
	  								foldr union [] answers in
	  									(try 
	  									if (final=[] &&answers=[]) then find s pr false 
	  									else if (keep (mgu_union l l') goal)@final@(try (find s pr false) with Error -> []) = [] then raise Error 
	  									else(keep (mgu_union l l') goal)@final@(try (find s pr false) with Error -> [])
	  									with NOT_UNIFIABLE -> find s pr false)
	  								with Error -> find s pr false)
									else find s pr false
	  | ([], true) -> []
	  | ([], false) -> raise Error in
	find s program false;;


let execute (program:program) (goal:atom) : answer = 
	(** [val execute : program -> atom -> answer = <fun>]  *)
	match goal with
	Atom(s, l) ->
    let result = try(Ans(List.rev(uniq (remove_vars (keep (intermediate program goal) goal)) []))) 
	with Error -> false | False -> false in
    if (result=Ans([])) then true else result;;

let execute2 (program:program) (goal:atom) : answer =
	match goal with
	Atom(s, l) ->
	let vars = vars_atom goal in
	let rec keep_var (l:substitution) (t:term) : substitution = match l with
		[] -> []
	  | (a,b)::xs -> if (a = t) then (a,b)::(keep_var xs t) else (keep_var xs t) in
	let rec map_vars l = match l with
		[] -> []
	  | x::xs -> let e = (execute program goal) in 
	  			 let sub a = match a with Ans(l)-> l in
	  	(keep_var (sub e) x)::(map_vars xs) in
	let var_answers = map_vars vars in
	let rec permute_2 l1 l2 = match l1 with
		[] -> []
	  | x::xs -> let rec form l = match l with
	  				[] -> []
	  			  | y::ys -> [x;y]::(form ys) in
	  			(form l2)@(permute_2 xs l2) in
	let rec permute_2' (l1,l2) = match l1 with
		[] -> []
	  | x::xs -> let rec form' l = match l with
	  				[] -> []
	  			  | y::ys -> (x@[y])::(form' ys) in
	  			(form' l2)@(permute_2' (xs , l2)) in
 	let tail2 l = match l with x1::x2::xs -> xs in
 	let rec fold_sp l1 l2 = match l2 with
 		[] -> l1
 	  | x::xs -> fold_sp (permute_2'(l1,x)) xs in
	let permute_n = fold_sp (permute_2 (List.hd(var_answers)) (List.nth var_answers 1)) (tail2 var_answers) in
	let rec filter l = match l with
		[] -> []
	  | x::xs -> if (execute program (subst_atom x goal)) = true then (Ans(x))::(filter xs)
	  			else filter xs in
	ManyAns (List.rev(uniq (filter permute_n) []));;


let prolog_debug_one (program:program) (goal:atom) : answer = 
	if (List.length(vars_atom goal) <= 1) then execute program goal
	else execute2 program goal;;

let rec prolog_debug (program:program) (goal:goal) : answer list = 
	(** [val prolog : program -> goal -> answer list = <fun>]  *)
	match goal with
	[] -> []
  | (g1::gr) -> [prolog_debug_one program g1]@(prolog_debug program gr);;

let print_var (v,a) = match (v,a) with
	(Var x, Sym(a, b)) -> print_string (x^" = "^a^"  ");; 

let print_table l = map print_var (List.rev l);;

let rec print (answer_list:answer) = let read = read_line() in (match read, answer_list with
	("", Ans([])) -> print_string ("false\n")
  | ("", Ans(x::xl)) -> (print_var x);print (Ans(xl))
  | (".", a) -> () );;

let rec printM (answer_list:answer) = let read = read_line() in (match read, answer_list with
	("", ManyAns([])) -> print_string ("false\n")
  | ("", ManyAns(Ans(x)::xl)) -> (print_table x);printM (ManyAns(xl))
  | (".", a) -> () );;

let prolog_one (program:program) (goal:atom) = 
	let debug = prolog_debug_one program goal in
	match debug with
	Ans(a) -> print debug
  | ManyAns(a) -> printM debug
  | true ->  print_string ("true\n")
  | false ->  print_string ("false\n");;

let rec prolog (program:program) (goal:goal) : answer list = 
	match goal with
	[] -> []
  | (g1::gr) -> let r = read_line() in prolog_one program g1 ; prolog program gr;;

(* TESTCASES *)



let fact_edge_ab =
Fact(Atom("edge", [Sym("a", []); Sym("b", [])]));;

let fact_edge_bc =
Fact(Atom("edge", [Sym("b", []); Sym("c", [])]));;

let fact_edge_cd =
Fact(Atom("edge", [Sym("c", []); Sym("d", [])]));;

let fact_edge_ad =
Fact(Atom("edge", [Sym("a", []); Sym("d", [])]));;

let fact_path_xx =
Fact(Atom("path", [Var("X"); Var("X")]));;

let rule_path_xy =
Rule(Atom("path", [Var("X"); Var("Y")]), [Atom("edge", [Var("X"); Var("Y")])]);;

let rule_path_xy' =
Rule(Atom("path", [Var("X"); Var("Y")]), 
	[
		Atom("edge", [Var("X"); Var("Z")]);
		Atom("path", [Var("Z"); Var("Y")])
	]);;

let goal_aap =
Atom("path", [Sym("a", []); Sym("a", [])]);;

let goal_ace =
Atom("edge", [Sym("a", []); Sym("c", [])]);;

let goal_aie =
Atom("edge", [Sym("a", []); Sym("i", [])]);;

let goal_Wcp =
Atom("path", [Var("W"); Sym("c", [])]);;

let goal_acp =
Atom("path", [Sym("a", []); Sym("c", [])]);;

let goal_aWp =
Atom("path", [Sym("a", []); Var("W")]);;

let goal_aWe =
Atom("edge", [Sym("a", []); Var("W")]);;

let goal_aep =
Atom("path", [Sym("a", []); Sym("e", [])]);;

let goal_aip =
Atom("path", [Sym("a", []); Sym("i", [])]);;

let goal_Wip =
Atom("path", [Var("W"); Sym("i", [])]);;

let goal_ABe =
Atom("edge", [Var("A"); Var("B")]);;

let goal_ABp = 
Atom("path", [Var("A"); Var("B")]);;

(* Given testcases*)

let goal_acp =
Atom("path", [Sym("a", []); Sym("c", [])]);;

let goal_aXe =
Atom("edge", [Sym("a", []); Var("X")]);;

let goal_aXp =
Atom("path", [Sym("a", []); Var("X")]);;

let prog = [fact_edge_ab; fact_edge_bc; fact_edge_cd; fact_edge_ad;
fact_path_xx; rule_path_xy; rule_path_xy'];;


intermediate prog goal_acp;;
execute prog goal_acp;;

intermediate prog goal_aXe;;
execute prog goal_aXe;;

intermediate prog goal_aXp;;
execute prog goal_aXp;;


intermediate prog goal_ace;;
execute prog goal_ace;;

intermediate prog goal_aie;;
execute prog goal_aie;;

intermediate prog goal_aap;;
execute prog goal_aap;;

intermediate prog goal_Wcp;;
execute prog goal_Wcp;;

intermediate prog goal_acp;;
execute prog goal_acp;;

intermediate prog goal_aWe;;
execute prog goal_aWe;;

intermediate prog goal_aWp;;
execute prog goal_aWp;;

intermediate prog goal_aep;;
execute prog goal_aep;;

intermediate prog goal_aip;;
execute prog goal_aip;;

intermediate prog goal_ABe;;
execute2 prog goal_ABe;;

intermediate prog goal_ABp;;
execute2 prog goal_ABp;;

prolog prog ([goal_ace;goal_aie;goal_aap;goal_Wcp;goal_acp;goal_aWe;goal_aWp;goal_aep;goal_aip;goal_ABe;goal_ABp]);;
prolog_debug prog ([goal_ace;goal_aie;goal_aap;goal_Wcp;goal_acp;goal_aWe;goal_aWp;goal_aep;goal_aip;goal_ABe;goal_ABp]);;







male(john).
parent(john).
male(andrewdoe).
female(mary).
father(X) :- male(X), parent(X).

let fact_mjohn = 
Fact(Atom("male", [Sym("john", [])]));;

let fact_pjohn =
Fact(Atom("parent", [Sym("john", [])]));;

let fact_mandrew =
Fact(Atom("male", [Sym("john", [])]));;

let fact_fmary =
Fact(Atom("female", [Sym("mary", [])]));;

let rule =
Rule(Atom("father", [Var("X")]), [
	Atom("male", [Var("X")]);
	Atom("father", [Var("X")])
]);;

let prog = [fact_mjohn; fact_mandrew; fact_pjohn; fact_fmary; rule];;

let goal_fjohn = 
Atom("father", [Sym("john", [])]);;

intermediate prog goal_fjohn;;













mgu (Sym("const", [Var("N")])) (Sym("const", [Const(3)]));;

let fact_const =
Fact(Atom("hastype", [Var("Gamma"); Sym("const",[Var("N")]); Sym("intT", [])]));;

let rule_neg = 
Rule(Atom("hastype", [Var("Gamma"); Sym("neg", [Var("E")]); Sym("intT", [])]),
	[
		Atom("hastype", [Var("Gamma"); Var("E"); Sym("intT", [])])	
	]);;

let rule_plus =
Rule(Atom("hastype", 
	[Var("Gamma"); Sym("plus", [Var("E1"); Var("E2")]); Sym("intT", [])]),
	[
		Atom("hastype", [Var("Gamma"); Var("E1"); Sym("intT", [])]);
		Atom("hastype", [Var("Gamma"); Var("E2"); Sym("intT", [])])
	]);;


let goal_const =
Atom("hastype", [Sym("Gamma", []); Sym("const", [Const(3)]); Var("T")]);;

let goal_const2 =
Atom("hastype", [Sym("Gamma", []); Sym("const", [Const(3)]); Sym("intT", [])]);;

let goal_const_wrong =
Atom("hastype", [Sym("Gamma", []); Sym("const", [Const(3)]); Sym("boolT", [])]);;

let goal_neg = 
Atom("hastype", [Sym("Gamma", []); Sym("neg", [Sym("const", [Const 3])]); Var("T")]);;

let goal_plus = 
Atom("hastype", [Sym("Gamma", []); Sym("plus", [Sym("const", [Const 2]); Sym("const", [Const 3])]); Var("T")]);;


intermediate ([fact_const]) (goal_const);;
execute ([fact_const]) (goal_const);;
intermediate ([fact_const]) (goal_const2);;
execute ([fact_const]) (goal_const2);;
intermediate ([fact_const]) (goal_const_wrong);;
execute ([fact_const]) (goal_const_wrong);;


intermediate ([fact_const; rule_neg]) (goal_neg);;
execute ([fact_const; rule_neg]) (goal_neg);;
intermediate ([fact_const;rule_neg;rule_plus]) (goal_plus);;
execute ([fact_const;rule_neg;rule_plus]) (goal_plus);;

prolog ([fact_const;rule_neg;rule_plus]) ([goal_const;goal_neg;goal_plus; goal_const_wrong; goal_const2]);;
prolog_debug ([fact_const;rule_neg;rule_plus]) ([goal_const;goal_neg;goal_plus; goal_const_wrong; goal_const2]);;

