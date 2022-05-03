open LL_Parser.Cfg

(*
let () = print_endline "Hello, World!"

let cfg1 =
  CFG.(
    let grammar =
      [ "E" ==: "T E"
      ; "E'" ==: "+ T E" ||| ""
      ; "T" ==: "F T"
      ; "T'" ==: "* F T" ||| ""
      ; "F" ==: "id"
      ]
    in
    cfg_of_grammar grammar "E")
;;

let cfg_left_recur =
  CFG.(
    let grammar = [ "S" ==: "A a" ||| "b"; "A" ==: "A c" ||| "S d" ||| "" ] in
    cfg_of_grammar grammar "S")
;;
let cfg_project =
  let grammar =
    [ "program" ==: "compoundstmt"
    ; "stmt" ==: "ifstmt" ||| "whilestmt" ||| "assgstmt" ||| "compoundstmt"
    ; "compoundstmt" ==: "{ stmts }"
    ; "stmts" ==: "stmt stmts" ||| ""
    ; "ifstmt" ==: "if ( boolexpr ) then stmt else stmt"
    ; "whilestmt" ==: "while ( boolexpr ) stmt"
    ; "assgstmt" ==: "ID = arithexpr ;"
    ; "boolexpr" ==: "arithexpr boolop arithexpr"
    ; "boolop" ==: "<" ||| ">" ||| "<=" ||| ">=" ||| "=="
    ; "arithexpr" ==: "multexpr arithexprprime"
    ; "arithexprprime"
      ==: "+ multexpr arithexprprime"
      ||| "- multexpr arithexprprime"
      ||| ""
    ; "multexpr" ==: "simpleexpr multexprprime"
    ; "multexprprime"
      ==: "* simpleexpr multexprprime"
      ||| "/ simpleexpr multexprprime"
      ||| ""
    ; "simpleexpr" ==: "ID" ||| "NUM" ||| "( arithexpr )"
    ]
  in
  cfg_of_grammar grammar "program"
;;
*)

let homework =
  let grammar =
    [ "S" ==: "a A B" ||| "b A" ||| ""; "A" ==: "a A b" ||| ""; "B" ==: "b B" ||| "" ]
  in
  cfg_of_grammar grammar "S"
;;

let _ =
  let cfg = homework |> eliminate_left_recur in
  let first = cfg |> first in
  let follow = follow first cfg in
  print_first_set first;
  print_follow_set follow;
  ()
;;
