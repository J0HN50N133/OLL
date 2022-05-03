type symbol =
  | T of string (* terminal symbol *)
  | N of string (* nonterminal symbol *)
  | Epsilon (* empty string *)
  | EOF (* eof mark *)

type production = (symbol * symbol list) list
type grammar = (string * string list) list

type cfg =
  { nts : symbol list
  ; ts : symbol list
  ; start : symbol
  ; prods : production
  }

let ( ==: ) a b = a, [ b ]
let ( ||| ) (a, b) c = a, b @ [ c ]
let empty_grammar = []

(* [cfg_of_grammar cfg] return the [cfg] records of the grammar *)
let cfg_of_grammar (grammar : grammar) (entry : string) =
  let open List in
  let nts_str_list = map (fun x -> fst x |> String.trim) grammar in
  let ts_str_list =
    grammar
    |> concat_map (fun x -> x |> snd |> concat_map (String.split_on_char ' '))
    |> map String.trim
    |> filter (fun x -> String.empty <> x)
    |> sort_uniq Stdlib.compare
    |> filter (fun x -> not (mem x nts_str_list))
  in
  let symbol_of_string = function
    | "" -> Epsilon
    | "<<EOF>>" -> EOF
    | s when mem s nts_str_list -> N s
    | s when mem s ts_str_list -> T s
    | _ -> failwith "unknown symbol"
  in
  let symbol_list_of_string_list = map symbol_of_string in
  let symbol_list_of_string s =
    String.split_on_char ' ' s |> symbol_list_of_string_list
  in
  { nts = map (fun x -> N x) nts_str_list
  ; ts = map (fun x -> T x) ts_str_list
  ; start = N entry
  ; prods =
      grammar
      |> concat_map (fun (lhs, rhs) -> map (fun x -> N lhs, symbol_list_of_string x) rhs)
  }
;;

let groupby (f : 'a -> 'b) (l : 'a list) : ('b * 'a list) list =
  let open List in
  l
  |> fold_left
       (fun acc item ->
         let key = f item in
         if mem_assoc key acc
         then (
           let l = assoc key acc in
           let l' = cons item l in
           remove_assoc key acc |> cons (key, l'))
         else cons (key, [ item ]) acc)
       []
  |> rev
;;

let string_of_symbol = function
  | N s -> s
  | T s -> s
  | Epsilon -> "ε"
  | EOF -> "$"
;;

let string_of_symbol_list (l : symbol list) =
  l |> List.map string_of_symbol |> String.concat ""
;;

let string_of_cfg { nts = _; ts = _; start; prods } =
  prods
  |> groupby fst
  |> List.map (fun (lhs, rhs) ->
         let left = (if lhs = start then "*" else "") ^ string_of_symbol lhs in
         let left_len = String.length left in
         left
         ^ " ::= "
         ^ (rhs
           |> List.map (fun x -> string_of_symbol_list @@ snd x)
           |> fun l ->
           let indent = String.init left_len (Fun.const ' ') in
           match l with
           | [] -> failwith "body could not be empty"
           | h :: t -> List.fold_left (fun acc x -> acc ^ "\n" ^ indent ^ " |   " ^ x) h t
           ))
  |> List.fast_sort Stdlib.compare
  |> String.concat "\n"
;;

let new_name sym nts =
  let name = ref (string_of_symbol sym) in
  while List.mem (N name.contents) nts do
    name := name.contents ^ "'"
  done;
  name.contents
;;

let has_left_recur sym bodys =
  let recur, _ = List.partition (fun x -> List.hd x = sym) bodys in
  recur <> []
;;

let eliminate_direct_left_recursion sym bodys nts =
  let open List in
  let recur, non_recur = partition (fun x -> hd x = sym) bodys in
  let new_name = new_name sym nts in
  let replaced_recur = map (fun x -> tl x @ [ N new_name ]) recur @ [ [ Epsilon ] ] in
  let replaced_nonrecur = map (fun x -> x @ [ N new_name ]) non_recur in
  new_name, replaced_recur, replaced_nonrecur
;;

let eliminate_left_recur { nts; ts; start; prods } =
  let open List in
  let nts = ref nts in
  let bodys = ref (prods |> groupby fst |> map (fun x -> fst x, map snd (snd x))) in
  let n = length bodys.contents in
  for i = 0 to n - 1 do
    let ai, ai_prods = nth bodys.contents i in
    let ai_prods_ref = ref ai_prods in
    for j = 0 to i - 1 do
      let aj, aj_prods = nth bodys.contents j in
      let recur, non_recur = ai_prods |> partition (fun x -> hd x = aj) in
      let replaced =
        recur
        |> map tl
        |> concat_map (fun t ->
               map (fun x -> if x = [ Epsilon ] then t else x @ t) aj_prods)
      in
      ai_prods_ref := replaced @ non_recur
    done;
    if has_left_recur ai ai_prods_ref.contents
    then (
      let new_name, replaced_recur, replaced_nonrecur =
        eliminate_direct_left_recursion ai ai_prods_ref.contents nts.contents
      in
      nts := cons (N new_name) nts.contents;
      bodys
        := bodys.contents
           |> mapi (fun k item -> if i = k then ai, replaced_nonrecur else item)
           |> fun x -> append x [ N new_name, replaced_recur ])
    else
      bodys
        := bodys.contents
           |> mapi (fun k item -> if i = k then ai, ai_prods_ref.contents else item)
  done;
  let prods =
    bodys.contents (* symbol * symbol list list *)
    |> map (fun (sym, sym_list_list) ->
           sym_list_list |> map (fun sym_list -> sym, sym_list))
    |> concat
    (* (symbol * symbol list) list *)
  in
  { nts = nts.contents; ts; start; prods }
;;

(* [first cfg] return the first set of [cfg].
     require: [cfg] is LL(1)
  *)
let print_set prompt set =
  set
  |> List.iter (fun (sym, sym_set) ->
         print_string @@ prompt ^ "(" ^ string_of_symbol sym ^ ")";
         print_string "= { ";
         sym_set |> List.map string_of_symbol |> String.concat ", " |> print_string;
         print_string " }";
         print_newline ())
;;

let print_first_set = print_set "first"
let print_follow_set = print_set "follow"
let update_set k v l = l |> List.remove_assoc k |> List.cons (k, v)
let union_set s1 s2 = s1 @ s2 |> List.sort_uniq Stdlib.compare

let first cfg =
  let open List in
  let first = ref [] in
  cfg.ts @ [ EOF; Epsilon ] |> iter (fun ts -> first := cons (ts, [ ts ]) first.contents);
  cfg.nts |> iter (fun nt -> first := cons (nt, []) first.contents);
  let changing = ref true in
  while changing.contents do
    changing := false;
    cfg.prods
    |> iteri (fun _ (symbol, prod) ->
           let rhs =
             ref (first.contents |> assoc (hd prod) |> filter (fun x -> x <> Epsilon))
           in
           let k = length prod in
           let break = ref false in
           let j = ref 0 in
           while j.contents < k - 1 && not break.contents do
             let bj = first.contents |> assoc (nth prod j.contents) in
             if not @@ mem Epsilon bj
             then break := true
             else rhs := rhs.contents |> append (filter (( <> ) Epsilon) bj)
           done;
           let bk = assoc (nth prod (k - 1)) first.contents in
           if j.contents = k - 1 && mem Epsilon bk then rhs := cons Epsilon rhs.contents;
           let first_a = assoc symbol first.contents in
           let old_size = length first_a in
           let first_a = first_a @ rhs.contents |> sort_uniq Stdlib.compare in
           let new_size = length first_a in
           first := first.contents |> update_set symbol first_a;
           changing := changing.contents || old_size <> new_size)
  done;
  first.contents
;;

let follow first cfg =
  let open List in
  let follow =
    ref (cfg.nts |> map (fun nt -> if nt = cfg.start then nt, [ EOF ] else nt, []))
  in
  let changing = ref true in
  while changing.contents do
    changing := false;
    cfg.prods
    |> List.iter (fun (a, prod) ->
           let trailer = ref (assoc a follow.contents) in
           prod
           |> List.rev
           |> List.iter (fun b ->
                  if mem b cfg.nts
                  then (
                    let old_set = assoc b follow.contents in
                    let new_set = union_set old_set trailer.contents in
                    follow := follow.contents |> update_set b new_set;
                    changing
                      := changing.contents || List.length old_set <> List.length new_set;
                    let first_b = assoc b first in
                    if mem Epsilon first_b
                    then
                      trailer
                        := union_set trailer.contents (filter (( <> ) Epsilon) first_b)
                    else trailer := first_b)
                  else trailer := [ b ]))
  done;
  follow.contents
;;
