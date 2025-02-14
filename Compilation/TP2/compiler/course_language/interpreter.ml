open Ast
open Abstract_machine

exception Return_exp of value option
exception Non_variable_reference of expr
exception Non_bool_test of expr
exception Non_integer_array_position of expr

type type_basic = TInt | TFloat | TBool | TNull

let get_tab_pos name pos = name ^ "#" ^ string_of_int pos
let get_struct_field name field = name ^ "~" ^ field

let operation_of_binop (op : binop) (v1 : value) (v2 : value) =
  match (op, v1, v2) with
  | (Add, VInt(x), VInt(y)) -> VInt(x+y)
  | (Add, VFloat(x), VFloat(y)) -> VFloat(x+.y)
  | (Sub, VInt(x), VInt(y)) -> VInt(x-y)
  | (Sub, VFloat(x), VFloat(y)) -> VFloat(x-.y)
  | (Mul, VInt(x), VInt(y)) -> VInt(x*y)
  | (Mul, VFloat(x), VFloat(y)) -> VFloat(x*.y)
  | (Div, VInt(x), VInt(y)) -> VInt(x/y)
  | (Div, VFloat(x), VFloat(y)) -> VFloat(x/.y)
  | (Mod, VInt(x), VInt(y)) -> VInt(x mod y)
  | (Mod, VFloat(x), VFloat(y)) -> VFloat(let (ex, dx) = Float.modf x in let (ey, dy) = Float.modf y in (((float_of_int ((int_of_float ex) mod (int_of_float ey))) +. dy +.  dx)))
  | (And, VBool(x), VBool(y)) -> VBool(x && y)
  | (Or, VBool(x), VBool(y)) -> VBool(x || y)
  | (Eq, VInt(x), VInt(y)) -> VBool(x=y)
  | (Eq, VFloat(x), VFloat(y)) -> VBool(x=y)
  | (Eq, VBool(x), VBool(y)) -> VBool(x=y)
  | (Eq, VNone, VNone) -> VBool(true)
  | (Neq, VInt(x), VInt(y)) -> VBool(x!=y)
  | (Neq, VFloat(x), VFloat(y)) -> VBool(x!=y)
  | (Neq, VBool(x), VBool(y)) -> VBool(x!=y)
  | (Neq, VNone, VNone) -> VBool(false)
  | (Lt, VInt(x), VInt(y)) -> VBool(x<y)
  | (Lt, VFloat(x), VFloat(y)) -> VBool(x<y)
  | (Gt, VInt(x), VInt(y)) -> VBool(x>y)
  | (Gt, VFloat(x), VFloat(y)) -> VBool(x>y)
  | (Leq, VInt(x), VInt(y)) -> VBool(x<=y)
  | (Leq, VFloat(x), VFloat(y)) -> VBool(x<=y)
  | (Leq, VBool(x), VBool(y)) -> VBool(x<=y)
  | (Geq, VInt(x), VInt(y)) -> VBool(x>=y)
  | (Geq, VFloat(x), VFloat(y)) -> VBool(x>=y)
  | (Geq, VBool(x), VBool(y)) -> VBool(x>=y)
  | _ -> failwith("Type operation error")


let operation_of_unop (op : unop) (v : value) =
  match (op, v) with
  | (UMin, VInt(x)) -> VInt(-x)
  | (UMin, VFloat(x)) -> VFloat(-.x)
  | (Not, VBool(x)) -> VBool(not x)
  | _ -> failwith("Va faire du python stp (c'est mieux pour la société)")

let rec execute_func name args
          (map : value Util.Environment.t)
          (map_type : ((string * type_basic) list) Util.Environment.t)
          (map_function : (Ast.argument list * Ast.instruction) Util.Environment.t) = 
  let rec get_environment map new_map args expected =
    match (args, expected) with
    | ([], []) -> ()
    | (a::rargs, (ta, _, name)::rexpected) ->
        (match ta with
         | Value -> Util.Environment.add new_map name (interpret_expr map map_type map_function a)
         | Reference -> Util.Environment.add_ref new_map name (match Util.Environment.get_ref map name with
                         | Some v -> v
                         | None -> failwith("Unbound value"));
       get_environment map new_map rargs rexpected)
    | _ -> failwith("Number of args and expected args doesn't match")
  in
  match (Util.Environment.get map_function name) with
  | Some (expected, instruction) ->
     let new_map = Util.Environment.new_environment () in
     get_environment map new_map args expected;
     (try
        (interpret_instruction new_map map_type map_function instruction);
        VNone
      with
      | Return_exp(None) -> VNone
      | Return_exp(Some v) -> v)
  | None -> failwith("Function does not exist")

and interpret_expr
(map : value Util.Environment.t)
(map_type : ((string * type_basic) list) Util.Environment.t)
(map_function : (Ast.argument list * Ast.instruction) Util.Environment.t)
(expr : Ast.expr) =
  match expr with
  | Cst_i(x, _) -> VInt(x)
  | Cst_f(x, _) -> VFloat(x)
  | Cst_b(x, _) -> VBool(x)
  | Var(name, _) ->
     (match (Util.Environment.get map name) with
      | Some(x) -> x
      | None -> failwith("Unbound value"))
  | Binop(op, left, right, _) -> operation_of_binop op (interpret_expr map map_type map_function left) (interpret_expr map map_type map_function right)
  | Unop(op, x, _) -> operation_of_unop op (interpret_expr map map_type map_function x)
  | Array_val(name, exp, _) -> let i = match (interpret_expr map map_type map_function exp) with
                                 | VInt(x) -> x
                                 | _ -> failwith("Can only index arrays with Int") in
                               (match Util.Environment.get map name with
                               | Some VArray(_, arr_env) -> (match Util.Environment.get arr_env (get_tab_pos name i) with
                                                            | Some v -> v
                                                            | _ -> failwith("Invalid index"))
                               | Some _ -> failwith("Array was expected")
                               | _  -> failwith("Unbound value"))
  | Size_tab(name, a) ->  interpret_expr map map_type map_function (Var((name ^ "#size"), a))
  | Func(name, args, _) -> execute_func name args map map_type map_function


and interpret_instruction
(map : value Util.Environment.t)
(map_type : ((string * type_basic) list) Util.Environment.t)
(map_function : (Ast.argument list * Ast.instruction) Util.Environment.t)
(instruction : Ast.instruction) =
  let get_tab_pos name exp =
    get_tab_pos name (
        match (interpret_expr map map_type map_function exp) with
        | VInt(x) -> x
        | _ -> failwith("Can't index an array with anything else than boolean"))
  in
  match instruction with
      | Affect(name, expr, _) -> Util.Environment.modify map name (interpret_expr map map_type map_function expr)
      | Block (block, _) -> let rec int_block block = match block with
                              | [] -> ()
                              | l::r -> interpret_instruction map map_type map_function l; int_block r
                            in int_block block
      | IfThenElse (expr, yes, no, _) ->
         interpret_instruction map map_type map_function (match (interpret_expr map map_type map_function expr) with
         | VBool(b) when b -> yes
         | VBool(_) -> no
         | _ -> failwith("Expected boolean, found something else"))
      | While (expr, instr, _) -> let rec int_while () =
                                 match (interpret_expr map map_type map_function expr) with
                                  | VBool(b) when b -> interpret_instruction map map_type map_function instr; int_while ()
                                  | VBool(_) -> ()
                                  | _ -> failwith("Expected boolean, found something else")
                               in int_while ()
      | Affect_array(name, index, value, _) ->
         (match Util.Environment.get map name with
         | Some VArray(_, arr_env) -> Util.Environment.modify arr_env (get_tab_pos name index) (interpret_expr map map_type map_function value)
         | Some _ -> failwith("Expected array")
         | None -> failwith("Unbound value"))
      | Array_decl(_, name, exp, _) ->
         Util.Environment.add map name (VArray(name, map));
         Util.Environment.add map (name ^ "#size") (interpret_expr map map_type map_function exp)

      | Affect_custom(_, _, _) -> failwith("todo")
      | Custom_decl(name, var_name, fields) -> 
         
      | Proc (func, args, _) -> let _ = execute_func func args map map_type map_function in ()
      | Return (res, _) -> raise (Return_exp (match res with
                        | Some res -> Some (interpret_expr map map_type map_function res)
                        | None -> None));
      | Print_str(s, _) -> print_string s
      | Print_expr(expr, _) -> print_endline (string_of_value (interpret_expr map map_type map_function expr))
      | Var_decl(_, name, _) ->
         Util.Environment.add map name VNone


let interpret_func_decl
    (functions : (Ast.argument list * Ast.instruction) Util.Environment.t)
    (func_decl : Ast.function_decl) =
  match func_decl with
  | Func_decl(_, name, args, first_instr, _) -> Util.Environment.add functions name (args, first_instr)


let interpret_custom_decl
    (customs : (string * type_basic) list Util.Environment.t)
    (custom_decl : Ast.custom_decl) =
  match custom_decl with
  | Struct_decl(name, fields) -> Util.Environment.add customs

let normalize_arg_list args vars =
  if List.length args < List.length vars then
    args @ List.init (List.length vars - List.length args) (fun _ -> "")
  else if List.length args > List.length vars then
    List.filteri (fun i _ -> i < List.length vars) args
  else args

let interpret_prg prg args =
  let functions = Util.Environment.new_environment () in
  List.iter (interpret_func_decl functions) prg;
  let environnement = Util.Environment.new_environment () in
  let map_type = Util.Environment.new_environment () in
  let params, body =
    try Option.get (Util.Environment.get functions "main")
    with _ -> failwith "Function main not defined!"
  in
  let vars = List.map (fun (_, _, v) -> v) params in
  let args = normalize_arg_list args vars in
  List.iter2
    (fun v a ->
      Abstract_machine.parse_complex_argument_and_affect environnement v a)
    vars args;
  try interpret_instruction environnement map_type functions body
  with Return_exp _ -> ()
