open LL_Parser.Cfg

type jStmt =
  | JStmt of string
  | JRaw of string
  | JRet of string
  | JEmptyStmt

type jType = JType of string
type jArg = jType * string

let jVoid = JType "void"
let jBoolean = JType "boolean"
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

let string_of_jStmt st =
  match st with
  | JStmt s -> s ^ ";"
  | JRaw s -> s
  | JRet s -> "return " ^ s ^ ";"
  | JEmptyStmt -> ""
;;

let indent strs = List.map (( ^ ) "    ") strs
let string_of_jArg (jtype, name) = string_of_jType jtype ^ " " ^ name

let string_of_jMethod { name; decr; arg_list; ret_type; body } =
  Printf.sprintf
    "%s %s %s(%s) {\n%s\n}"
    (string_of_decr decr)
    (string_of_jType ret_type)
    name
    (arg_list |> List.map string_of_jArg |> String.concat ", ")
    (body
    |> List.filter (( <> ) JEmptyStmt)
    |> List.map string_of_jStmt
    |> indent
    |> String.concat "\n")
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

let jDeclVar jtype name = JStmt (string_of_jType jtype ^ " " ^ name)
let jAssign a b = JStmt (a ^ " = " ^ b)

let jCall (mthd : jMethod) arg_list =
  JStmt (mthd.name ^ "(" ^ (arg_list |> String.concat ", ") ^ ")")
;;

let newObj ?(arg_list = []) obj =
  "new " ^ obj ^ "(" ^ (arg_list |> List.map string_of_jArg |> String.concat ", ") ^ ")"
;;

let newJSet = newObj "HashSet<>"
let newJMap = newObj "HashMap<>"
let jSetAdd set var = JStmt (set ^ ".add(" ^ var ^ ")")

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

let ll_parser cfg =
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
  let line = { decr = [ jPrivate ]; t = JType "int"; name = "line" } in
  let level = { decr = [ jPrivate ]; t = JType "int"; name = "level" } in
  let predict_table =
    { decr = [ jPrivate; jFinal ]
    ; t = JType "Map<String, Map<String, String[]>>"
    ; name = "predict_table"
    }
  in
  let stack =
    { decr = [ jPrivate; jFinal ]; t = JType "Deque<String>"; name = "stack" }
  in
  let tokens = { decr = [ jPrivate; jFinal ]; t = JType "String[]"; name = "tokens" } in
  let prog = { decr = [ jPrivate; jFinal ]; t = JType "StringBuilder"; name = "prog" } in
  let cursor = { decr = [ jPrivate ]; t = JType "int"; name = "cursor" } in
  let prod_to_java_list prod =
    let elems =
      prod
      |> List.filter (( <> ) Epsilon)
      |> List.map (fun x -> "\"" ^ string_of_symbol x ^ "\"")
      |> List.rev
      |> String.concat ", "
    in
    "new String[]{" ^ elems ^ "}"
  in
  let add_to_predict_helper =
    { name = "insert_into_predict_table"
    ; decr = [ jPrivate ]
    ; arg_list = [ JType "String", "n"; JType "String", "t"; JType "String[]", "prod" ]
    ; ret_type = jVoid
    ; body =
        [ jDeclVar (JType "Map<String, String[]>") "oldMap"
        ; jAssign "oldMap" (predict_table.name ^ ".getOrDefault(n, new HashMap<>());")
        ; JStmt "oldMap.put(t, prod)"
        ; JStmt (predict_table.name ^ ".put(n, oldMap)")
        ]
    }
  in
  let add_to_predict =
    pred_tb
    |> SymbolTupleMap.to_seq
    |> Seq.map (fun ((n, t), prods) ->
           let prod = snd (List.hd prods) in
           let n_str = "\"" ^ string_of_symbol n ^ "\"" in
           let t_str = "\"" ^ string_of_symbol t ^ "\"" in
           jCall add_to_predict_helper [ n_str; t_str; prod_to_java_list prod ])
    |> List.of_seq
  in
  let error =
    { name = "error"
    ; decr = [ jPrivate ]
    ; arg_list = [ JType "String", "msg" ]
    ; ret_type = jVoid
    ; body = [ JStmt "System.err.println(\"语法错误,第\" + line + \"行,\" + msg)" ]
    }
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
        ; jAssign level.name "0"
        ; jAssign line.name "1"
        ; jAssign cursor.name "0"
        ; jAssign stack.name (newObj "LinkedList<>")
        ; jDeclVar prog.t "prog"
        ; jAssign prog.name (newObj (string_of_jType prog.t))
        ]
        @ List.map
            (fun x -> jSetAdd terminal.name ("\"" ^ string_of_symbol x ^ "\""))
            cfg.ts
        @ List.map
            (fun x -> jSetAdd non_terminal.name ("\"" ^ string_of_symbol x ^ "\""))
            cfg.nts
        @ add_to_predict
        @ [ JRaw
              "Scanner sc = new Scanner(System.in);\n\
              \    while (sc.hasNextLine()) {\n\
              \          prog.append(sc.nextLine().toLowerCase()).append(\" NL \");\n\
              \    }"
          ; JStmt "sc.close()"
          ; jAssign tokens.name "prog.toString().split(\" \")"
          ]
    }
  in
  let peek =
    { name = "peek"
    ; decr = [ jPrivate ]
    ; ret_type = JType "String"
    ; arg_list = []
    ; body = [ JRet "tokens[cursor]" ]
    }
  in
  let advance =
    { name = "advance"
    ; decr = [ jPrivate ]
    ; ret_type = jVoid
    ; arg_list = []
    ; body = [ JStmt "cursor++" ]
    }
  in
  let main =
    { name = "main"
    ; decr = [ jPublic; jStatic ]
    ; ret_type = jVoid
    ; arg_list = [ JType "String[]", "args" ]
    ; body =
        [ jDeclVar (JType parserName) "parser"
        ; jAssign "parser" (newObj parserName)
        ; JStmt "parser.analysis()"
        ]
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
  let isT =
    { name = "isT"
    ; decr = [ jPrivate ]
    ; ret_type = jBoolean
    ; arg_list = [ JType "String", "tk" ]
    ; body = [ JRet (terminal.name ^ ".contains(tk)") ]
    }
  in
  let isEnd =
    { name = "isEnd"
    ; decr = [ jPrivate ]
    ; ret_type = jBoolean
    ; arg_list = []
    ; body = [ JRet (cursor.name ^ " == " ^ tokens.name ^ ".length") ]
    }
  in
  let indent_print =
    { name = "indent_print"
    ; decr = [ jPrivate ]
    ; ret_type = jVoid
    ; arg_list = [ JType "String", "s" ]
    ; body =
        [ JRaw
            "for(int i = 0; i < level; i++) System.out.print(\"\\t\");\n\
            \    System.out.println(s);"
        ]
    }
  in
  let analysis =
    { name = "analysis"
    ; decr = [ jPrivate ]
    ; ret_type = jVoid
    ; arg_list = []
    ; body =
        [ JStmt "stack.push(\"$\")"
        ; JStmt ("stack.push(\"" ^ string_of_symbol cfg.start ^ "\")")
        ; JRaw "while(!\"$\".equals(stack.peek())){"
        ; JRaw "    String tk = peek().trim();"
        ; JRaw
            "    if(\"NL\".equals(tk)){\n\
            \        advance();\n\
            \        line++;\n\
            \        continue;\n\
            \    }else if(tk.isEmpty()){\n\
            \        advance();\n\
            \        continue;\n\
            \    }else if(stack.peek().equals(tk)){\n\
            \        indent_print(tk.toUpperCase());\n\
            \        stack.pop();\n\
            \        advance();\n\
            \        continue;\n\
            \    }else if(isT(stack.peek())){\n\
            \        error(\"\");\n\
            \        continue;\n\
            \    }\n\
            \    String[] prod = predict_table.get(stack.peek()).get(peek());\n\
            \    if(prod == null){\n\
            \        error(\"\");\n\
            \        continue;\n\
            \    }\n\
            \    indent_print(stack.peek());\n\
            \    stack.pop();\n\
            \    level++;\n\
            \    for(String s: prod){\n\
            \        stack.push(s);\n\
            \    }\n\
            \    if(prod.length == 0){\n\
            \        indent_print(\"E\");\n\
            \        level -= 2;\n\
            \    } \n\
            \             "
        ; JRaw "}"
        ]
    }
  in
  { import = [ "java.util.*" ]
  ; name
  ; decr = [ jPublic ]
  ; attr = [ line; level; terminal; non_terminal; predict_table; stack; tokens; cursor ]
  ; mthd =
      [ constructor
      ; main
      ; add_to_predict_helper
      ; error
      ; peek
      ; advance
      ; analysis
      ; isT
      ; isEnd
      ; indent_print
      ]
      @ callback_list
  }
;;

let write_jclass jclass =
  let fo = Out_channel.open_text (jclass.name ^ ".java") in
  Printf.fprintf fo "%s\n" (string_of_jClass jclass);
  Out_channel.close fo
;;

let cfg = cfg_of_grammar grammar "program"
let cfg = cfg |> eliminate_left_recur
let first = cfg |> first
let follow = cfg |> follow first

let _ =
  print_endline @@ string_of_predict_analysis_tb cfg (pred_analysis_tb cfg first follow)
;;

let _ = write_jclass (ll_parser cfg)
