{
	open Parser
	exception Eof
}

rule token = parse
| '.' {(*let () = Printf.printf "." in*) END}
| "fail" {FAIL}
| ":-" {(*let () = Printf.printf ":-" in*) SEP}
| '%' {(*let () = Printf.printf "" in*) single_line lexbuf}
| "/*" {(*let () = Printf.printf "" in*) multi_line lexbuf}
| (['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*) as var {(*let () = print_string var in*) VAR(var)}
| (['a'-'z']['a'-'z''A'-'Z''0'-'9''_']*) as const {(*let () = print_string const in*) CONST(const)}
| "\""[^'\"']+"\"" as const {(*let () = print_string const in*) CONST(const)}
| (['0'-'9']['0'-'9']*) as n {(*let () = print_string n in*) NUM(n)}
| '+' {PLUS}
| '-' {MIN}
| '*' {MUL}
| '/' {DIV}
| "=" {(*let () = Printf.printf "=" in*) EQUAL}
| ( "\\==" | "\\=" ) {(*let () = Printf.printf "\\=" in*) NOT_EQUAL}
| ',' {(*let () = Printf.printf "," in*) COMMA}
| '(' {(*let () = Printf.printf "(" in*) O_PAREN}
| ')' {(*let () = Printf.printf ")" in*) C_PAREN}
| '[' {(*let () = Printf.printf "[" in*) O_SQ}
| ']' {(*let () = Printf.printf "]" in*) C_SQ}
| '|' {(*let () = Printf.printf "|" in*) CONS}
| '!' {(*let () = Printf.printf "!" in*) CUT}
| "Fail"|"fail" {(*let () = Printf.printf "fail" in*) FAIL}
| "\n" {(*let () = Printf.printf "\n" in*) token lexbuf}
| [' ''\t']+ {(*let () = Printf.printf "" in*) token lexbuf}
| eof {EOF}
| _ {token lexbuf}

and single_line = parse
| eof { token lexbuf }
| "\n" { token lexbuf }
| _ { single_line lexbuf }

and multi_line = parse
| "*/" {token lexbuf}
| eof {raise Eof}
| _ {multi_line lexbuf}

 

