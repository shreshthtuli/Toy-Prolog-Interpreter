open Parser;;
open Printf;;

let parse_string str =
	let lb = Lexing.from_string str
	in
		Parser.main Lexer.token lb
;;

let predicate fl= 
    let filename = fl in 
    let file = open_in filename in
    let lexbuf = Lexing.from_channel file in
    let result = Parser.main Lexer.token lexbuf in
    result
;;

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)
;;

let token_list_of_string s =
	let lb = Lexing.from_string s in
	let rec helper l =
		try
			let t = Lexer.token lb in
			if t = Parser.EOF then List.rev l else helper (t::l)
		with _ -> List.rev l
	in 
		helper []
;;

let token fl =
   token_list_of_string (load_file fl)
;;

let parse fl =
   parse_string (load_file fl)
;;


(*parse_string (load_file(Sys.argv.(1)));;*)
