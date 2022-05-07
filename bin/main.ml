open LL_Parser.Cfg

(*
let () = print_endline "Hello, World!"

let cfg1 =
  CFG.(
    let grammar =
      [ "E" ==> "T E"
      ; "E'" ==> "+ T E" ||| ""
      ; "T" ==> "F T"
      ; "T'" ==> "* F T" ||| ""
      ; "F" ==> "id"
      ]
    in
    cfg_of_grammar grammar "E")
;;

let cfg_left_recur =
  CFG.(
    let grammar = [ "S" ==> "A a" ||| "b"; "A" ==> "A c" ||| "S d" ||| "" ] in
    cfg_of_grammar grammar "S")
;;
*)
(*let cfg =
  let grammar =
    [ "program" ==> "compoundstmt"
    ; "stmt" ==> "ifstmt" ||| "whilestmt" ||| "assgstmt" ||| "compoundstmt"
    ; "compoundstmt" ==> "{ stmts }"
    ; "stmts" ==> "stmt stmts" ||| ""
    ; "ifstmt" ==> "if ( boolexpr ) then stmt else stmt"
    ; "whilestmt" ==> "while ( boolexpr ) stmt"
    ; "assgstmt" ==> "ID = arithexpr ;"
    ; "boolexpr" ==> "arithexpr boolop arithexpr"
    ; "boolop" ==> "<" ||| ">" ||| "<=" ||| ">=" ||| "=="
    ; "arithexpr" ==> "multexpr arithexprprime"
    ; "arithexprprime"
      ==> "+ multexpr arithexprprime"
      ||| "- multexpr arithexprprime"
      ||| ""
    ; "multexpr" ==> "simpleexpr multexprprime"
    ; "multexprprime"
      ==> "* simpleexpr multexprprime"
      ||| "/ simpleexpr multexprprime"
      ||| ""
    ; "simpleexpr" ==> "ID" ||| "NUM" ||| "( arithexpr )"
    ]
  in
  cfg_of_grammar grammar "program"
;;*)

(*let cfg =
  let grammar =
    [ "S" ==> "a A B" ||| "b A" ||| ""; "A" ==> "a A b" ||| ""; "B" ==> "b B" ||| "" ]
  in
  cfg_of_grammar grammar "S"
;;*)

(*let grammar =*)
(*[ "E" ==> "T E'"*)
(*; "E'" ==> "+ T E'" ||| ""*)
(*; "T" ==> "F T'"*)
(*; "T'" ==> "* F T'" ||| ""*)
(*; "F" ==> "( E )" ||| "id"*)
(*]*)
(*;;*)
let grammar = [ "S" ==> "i E t S S'" ||| "a"; "S'" ==> "e S" ||| ""; "E" ==> "b" ]
let cfg = cfg_of_grammar grammar "S"
let cfg = cfg |> eliminate_left_recur
let first = cfg |> first
let follow = follow first cfg;;

let tb = pred_analysis_tb cfg first follow in
(*print_first_set first;*)
(*print_follow_set follow;*)
(*print_string @@ string_of_predict_analysis_tb cfg tb*)
print_string @@ string_of_predict_analysis_tb cfg tb
