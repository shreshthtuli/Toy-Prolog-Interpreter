open Parser;;
open Printf;;

let predicate fl= 
    let filename = fl in 
    let file = open_in filename in
    let lexbuf = Lexing.from_channel file in
    let result = Parser.main Lexer.token lexbuf in
    result
;;

let solve prog str = 
  let lexbuf = Lexing.from_string str in
  let goals = Parser.goal Lexer.token lexbuf in
  let ans = Solver.prolog_debug prog goals in match ans with
      [Ans(a)] -> Solver.print (Ans(a))
    | [ManyAns(a)] -> Solver.printM (ManyAns(a))
    | [true] ->  print_string ("true\n")
    | [false] ->  print_string ("false\n")
;;


let rec maino prog =
    Printf.printf "?- ";
    let inp = try read_line() with End_of_file -> Printf.printf "\nExiting\n" ;exit 0 | _ -> "" in
    let () = try flush stdout with _ -> () in            
    let len = try String.length inp with _ -> 0 in
    if (try (inp.[(len-1)] <> '.') with _ -> false) then
      let () = (Printf.printf "Every instruction must end with \'.\'\n") in
      maino prog
    else if (len = 0) then maino prog
    else
      if (inp = "exit.") then exit 0
      else
          let () = try (solve prog inp)
          with
          | Parsing.Parse_error -> Printf.printf "Invalid querry.\n"
          in maino prog
;;

let main prog = let name = try Sys.argv.(1) with _ -> Printf.printf("No procedure defined. Exiting.\n");exit 0 in
          let new_prog = try let prog = predicate name in let () = Printf.printf "Procedure loaded.\n" in prog with
            | Parsing.Parse_error -> let () = Printf.printf("Parse error in file. Exiting.\n"); exit 0 in prog 
            | _ -> let () = Printf.printf("Incorrect file path provided. Exiting.\n"); exit 0 in prog
          in maino new_prog;;


if !Sys.interactive then () else main [];;

