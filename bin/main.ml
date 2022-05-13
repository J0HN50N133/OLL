open LL_Parser.Cfg

type jStmt = JStmt of string
type jType = JType of string
type jArg = jType * string

let jVoid = JType "void"
let jPrivate = "private"
let jPublic = "public"
let jStatic = "static"
let jFinal = "final"

type jAttr =
  { (* field descriptor, like: static private public and type*)
    decr : string list
  ; t : jType
  ; name : string
  }

let string_of_jType (JType s) = s
let string_of_decr = String.concat " "

let string_of_jAttr { decr; t; name } =
  Printf.sprintf "%s %s %s;" (string_of_decr decr) (string_of_jType t) name
;;

type jMethod =
  { name : string
  ; decr : string list
  ; arg_list : jArg list
  ; ret_type : jType
  ; body : jStmt list
  }

let string_of_jStmt (JStmt s) = s
let indent strs = List.map (( ^ ) "    ") strs
let string_of_jArg (jtype, name) = string_of_jType jtype ^ " " ^ name

let string_of_jMethod { name; decr; arg_list; ret_type; body } =
  Printf.sprintf
    "%s %s %s(%s) {\n%s\n}"
    (string_of_decr decr)
    (string_of_jType ret_type)
    name
    (arg_list |> List.map string_of_jArg |> String.concat ", ")
    (body |> List.map string_of_jStmt |> indent |> String.concat "\n")
  |> String.trim
;;

type jClass =
  { import : string list
  ; name : string
  ; decr : string list
  ; attr : jAttr list
  ; mthd : jMethod list
  }

let indent_string str = str |> String.split_on_char '\n' |> indent |> String.concat "\n"

let string_of_jClass { import; name; decr; attr; mthd } =
  Printf.sprintf
    "%s\n%s class %s{\n%s\n%s\n}"
    (import |> List.map (fun x -> "import " ^ x ^ ";") |> String.concat "\n")
    (string_of_decr decr)
    name
    (attr |> List.map string_of_jAttr |> indent |> String.concat "\n")
    (mthd |> List.map string_of_jMethod |> String.concat "\n" |> indent_string)
;;

let declVar jtype name = JStmt (string_of_jType jtype ^ " " ^ name ^ ";")
let jAssign a b = JStmt (a ^ " = " ^ b ^ ";")

let newObj ?(arg_list = []) obj =
  "new " ^ obj ^ "(" ^ (arg_list |> List.map string_of_jArg |> String.concat ", ") ^ ")"
;;

let newJSet = newObj "HashSet<>"
let newJMap = newObj "HashMap<>"
let jSetAdd set var = JStmt (set ^ ".add(" ^ var ^ ");")

let grammar =
  [ "program" ==> "compoundstmt"
  ; "stmt" ==> "ifstmt" ||| "whilestmt" ||| "assgstmt" ||| "compoundstmt"
  ; "compoundstmt" ==> "{ stmts }"
  ; "stmts" ==> "stmt stmts" ||| ""
  ; "ifstmt" ==> "if ( boolexpr ) then stmt else stmt"
  ; "whilestmt" ==> "while ( boolexpr ) stmt"
  ; "assgstmt" ==> "id = arithexpr ;"
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
  ; "simpleexpr" ==> "id" ||| "num" ||| "( arithexpr )"
  ]
;;

let parserName = "Parser"

let parser cfg =
  let first = first cfg in
  let follow = follow first cfg in
  let pred_tb = pred_analysis_tb cfg first follow in
  let name = parserName in
  let terminal =
    { decr = [ jPrivate; jFinal ]; t = JType "Set<String>"; name = "terminal" }
  in
  let non_terminal =
    { decr = [ jPrivate; jFinal ]; t = JType "Set<String>"; name = "non_terminal" }
  in
  let predict_table =
    { decr = [ jPrivate; jFinal ]
    ; t = JType "Map<String, Map<String, String[]>>"
    ; name = "predict_table"
    }
  in
  let prod_to_java_list prod =
    let elems =
      prod
      |> List.map (fun x -> "\"" ^ string_of_symbol x ^ "\"")
      |> List.rev
      |> String.concat ", "
    in
    "new String[]{" ^ elems ^ "}"
  in
  let add_to_predict =
    declVar (JType "Map<String, String[]>") "oldMap"
    :: (pred_tb
       |> SymbolTupleMap.to_seq
       |> Seq.map (fun ((n, t), prods) ->
              let prod = snd (List.hd prods) in
              match prod with
              | [ Epsilon ] -> []
              | prod ->
                let n_str = "\"" ^ string_of_symbol n ^ "\"" in
                let t_str = "\"" ^ string_of_symbol t ^ "\"" in
                let getOldMap =
                  jAssign
                    "oldMap"
                    (predict_table.name ^ ".getOrDefault(" ^ n_str ^ ",new HashMap<>())")
                in
                let addToOldMap =
                  JStmt ("oldMap.put(" ^ t_str ^ ", " ^ prod_to_java_list prod ^ ");")
                in
                let updatePredTb =
                  JStmt (predict_table.name ^ ".put(" ^ n_str ^ ", oldMap);")
                in
                [ getOldMap; addToOldMap; updatePredTb ])
       |> List.of_seq
       |> List.flatten)
  in
  let constructor =
    { name
    ; decr = []
    ; arg_list = []
    ; ret_type = JType ""
    ; body =
        [ jAssign terminal.name newJSet
        ; jAssign non_terminal.name newJSet
        ; jAssign predict_table.name newJMap
        ]
        @ List.map
            (fun x -> jSetAdd terminal.name ("\"" ^ string_of_symbol x ^ "\""))
            cfg.ts
        @ List.map
            (fun x -> jSetAdd non_terminal.name ("\"" ^ string_of_symbol x ^ "\""))
            cfg.nts
        @ add_to_predict
    }
  in
  let main =
    { name = "main"
    ; decr = [ jPublic; jStatic ]
    ; ret_type = jVoid
    ; arg_list = [ JType "String[]", "args" ]
    ; body = [ declVar (JType parserName) "parser"; jAssign "parser" (newObj parserName) ]
    }
  in
  let callback_list =
    cfg.nts
    |> List.map (fun s ->
           let func_name = string_of_symbol s in
           { name = func_name
           ; ret_type = jVoid
           ; decr = [ jPrivate ]
           ; arg_list = []
           ; body = []
           })
  in
  { import = [ "java.util.*" ]
  ; name
  ; decr = [ jPublic ]
  ; attr = [ terminal; non_terminal; predict_table ]
  ; mthd = [ constructor; main ] @ callback_list
  }
;;

let write_jclass jclass =
  let fo = Out_channel.open_text (jclass.name ^ ".java") in
  Printf.fprintf fo "%s\n" (string_of_jClass jclass);
  Out_channel.close fo
;;

let cfg = cfg_of_grammar grammar "program"
let cfg = cfg |> eliminate_left_recur
let _ = write_jclass (parser cfg)
