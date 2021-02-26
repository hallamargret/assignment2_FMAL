// T-501-FMAL, Spring 2021, Assignment 2

(*
STUDENT NAMES HERE: Eva Sol Petursdottir and Halla Margret Jonsdottir

*)

module Assignment2


(* Various type and function definitions, do not edit *)

type iexpr =
    | IVar of string
    | INumI of int
    | INumF of float
    | IPlus of iexpr * iexpr
    | ITimes of iexpr * iexpr
    | INeg of iexpr
    | IIfPositive of iexpr * iexpr * iexpr

type expr =
    | Var of string
    | NumI of int
    | NumF of float
    | Plus of expr * expr
    | Times of expr * expr
    | Neg of expr
    | IfPositive of expr * expr * expr
    | IntToFloat of expr
    | Match of expr * string * expr * string * expr

type value =
    | I of int
    | F of float

type envir = (string * value) list

type typ =
    | Int
    | Float

type tyenvir = (string * typ) list

let rec lookup (x : string) (env : (string * 'a) list) : 'a =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env

let paren b s = if b then  "(" + s + ")" else s

let iprettyprint (e : iexpr) : string =
    let rec iprettyprint' e acc =
        match e with
        | IVar x -> x
        | INumI i -> string i
        | INumF f -> sprintf "%A" f
        | IPlus  (e1, e2) ->
              paren (4 <= acc) (iprettyprint' e1 3 + " + " + iprettyprint' e2 4)
        | ITimes (e1, e2) ->
              paren (7 <= acc) (iprettyprint' e1 6 + " * " + iprettyprint' e2 7)
        | INeg e ->
              paren (10 <= acc) ("-" + iprettyprint' e 9)
        | IIfPositive (e, et, ef) ->
              paren (2 <= acc) ("if " + iprettyprint' e 3 + " > 0 then " + iprettyprint' et 2 + " else " + iprettyprint' ef 1)
    iprettyprint' e 0

let prettyprint (e : expr) : string =
    let rec prettyprint' e acc =
        match e with
        | Var x -> x
        | NumI i -> string i
        | Plus  (e1, e2) ->
             paren (4 <= acc) (prettyprint' e1 3 + " + " + prettyprint' e2 4)
        | Times (e1, e2) ->
             paren (7 <= acc) (prettyprint' e1 6 + " * " + prettyprint' e2 7)
        | Neg e ->
             paren (10 <= acc) ("-" + prettyprint' e 9)
        | IfPositive (e, et, ef) ->
             paren (2 <= acc) ("if " + prettyprint' e 3 + " > 0 then " + prettyprint' et 2 + " else " + prettyprint' ef 1)
        | NumF f -> sprintf "%A" f
        | IntToFloat e ->
             paren (10 <= acc) ("float " + prettyprint' e 10)
        | Match (e, xi, ei, xf, ef) ->
             paren (2 <= acc) ("match " + prettyprint' e 1 + " with"
               + " I " + xi + " -> " + prettyprint' ei 2
               + " | F " + xf + " -> " + prettyprint' ef 1)
    prettyprint' e 0

let plus_value (v1 : value, v2 : value) : value =
    match v1, v2 with
    | I x1, I x2 -> I (x1 + x2)
    | F x1, I x2 -> F (x1 + float x2)
    | I x1, F x2 -> F (float x1 + x2)
    | F x1, F x2 -> F (x1 + x2)

let times_value (v1 : value, v2 : value) : value =
    match v1, v2 with
    | I x1, I x2 -> I (x1 * x2)
    | F x1, I x2 -> F (x1 * float x2)
    | I x1, F x2 -> F (float x1 * x2)
    | F x1, F x2 -> F (x1 * x2)

let neg_value (v : value) : value =
    match v with
    | I x -> I (-x)
    | F x -> F (-x)

let is_positive_value (v : value) : bool =
    match v with
    | I x -> x > 0
    | F x -> x > 0.

type rinstr =
    | RLoad of int            // load from environment
    | RStore                  // move value from top of stack to
                              // 0th pos of the environment,
                              // shifting all others down
    | RErase                  // remove 0th value from environment,
                              // shifting all others up
    | RNum of int
    | RAdd
    | RSub
    | RMul
    | RPop
    | RDup
    | RSwap

type rcode = rinstr list
type stack = int list          // intermediate values
type renvir = int list         // values of numbered variables

let rec reval (inss : rcode) (stk : stack) (renv : renvir) =
    match inss, stk with
    | [], i :: _ -> i
    | [], []     -> failwith "reval: No result on stack!"
    | RLoad n :: inss,             stk ->
          reval inss (List.item n renv :: stk) renv
    | RStore  :: inss,        i :: stk -> reval inss stk (i :: renv)
    | RErase  :: inss,             stk -> reval inss stk (List.tail renv)
    | RNum i  :: inss,             stk -> reval inss (i :: stk) renv
    | RAdd    :: inss, i2 :: i1 :: stk -> reval inss ((i1+i2) :: stk) renv
    | RSub    :: inss, i2 :: i1 :: stk -> reval inss ((i1-i2) :: stk) renv
    | RMul    :: inss, i2 :: i1 :: stk -> reval inss ((i1*i2) :: stk) renv
    | RPop    :: inss,        i :: stk -> reval inss stk renv
    | RDup    :: inss,        i :: stk -> reval inss ( i ::  i :: stk) renv
    | RSwap   :: inss, i2 :: i1 :: stk -> reval inss (i1 :: i2 :: stk) renv
    | _ -> failwith "reval: too few operands on stack"





// Problem 1

let rec modifiedLookup (x : string) (env : (string * 'a) list) : 'a =
    match env with
    | []          -> I 0
    | (y, v)::env -> if x = y then v else modifiedLookup x env



let rec ieval (e : iexpr) (env : envir) : value =
    match e with
    | IVar x -> modifiedLookup x env                       // to modify - Modified and implemented above
    | INumI i -> I i
    | INumF f -> F f
    | IPlus (e1, e2) -> plus_value (ieval e1 env, ieval e2 env)
    | ITimes (e1, e2) -> times_value (ieval e1 env, ieval e2 env)
    | INeg e -> neg_value (ieval e env)
    | IIfPositive (e, et, ef) ->
        if is_positive_value (ieval e env)
        then ieval et env
        else ieval ef env


// Problem 2

let rec eval (e : expr) (env : envir) : value =
    match e with
    | Var x -> lookup x env
    | NumI i -> I i
    | NumF f -> F f
    | Plus (e1, e2) ->                             // to complete - Completed
        match eval e1 env, eval e2 env with
        | I i1, I i2 -> I (i1 + i2)
        | F i1, F i2 -> F (i1 + i2)                 // implemented
        | _ -> failwith "wrong operand type"
    | Times (e1, e2) ->                            // to complete - Completed
        match eval e1 env, eval e2 env with
        | I i1, I i2 -> I (i1 * i2)
        | F i1, F i2 -> F (i1 * i2)                 // implemented
        | _ -> failwith "wrong operand type"
    | Neg e ->                                     // to complete - Completed
        match eval e env with
        | I i -> I (- i)
        | F i -> F (- i)                           // implemented
    | IntToFloat e ->
        match eval e env with
        | I i -> F (float i)
        | _ -> failwith "wrong operand type"
    | IfPositive (e, et, ef) ->                     // implemented
        match eval e env with 
        | I i when i >= 0  -> eval et env
        | I i  when i < 0 -> eval ef env
        | F i when i >= 0.0 -> eval et env
        | F i when i < 0.0 -> eval ef env
        |_ -> failwith "wrong operand type"
    | Match (e, xi, ei, xf, ef) ->                  // implemented
        match eval e env with
        | I i -> eval ei (env @ [xi, I i])
        | F i -> eval ef (env @ [xf, F i])


// Problem 3

let to_float (v : value) : float = 
    match v with
    | I i -> (float i)
    | F i -> i


// Problem 4

let to_float_expr (e : expr) : expr = 
    Match(e, "I" , IntToFloat e, "F", e)

let plus_expr (e1 : expr, e2 : expr) : expr =
    Match(e1, "I" , Match(e2, "I", Plus(e1, e2), "F", Plus(IntToFloat e1, e2)), "F", Match(e2, "I", Plus(e1, IntToFloat e2), "F", Plus(e1, e2)))

let times_expr (e1 : expr, e2 : expr) : expr =
    Match(e1, "I" , Match(e2, "I", Times(e1, e2), "F", Times(IntToFloat e1, e2)), "F", Match(e2, "I", Times(e1, IntToFloat e2), "F", Times(e1, e2)))


// Problem 5

let rec add_matches (e : iexpr) : expr =
    match e with
    | IVar x -> Var x
    | INumI x -> NumI x
    | INumF x -> NumF x
    | IPlus (e1, e2) -> plus_expr (add_matches e1, add_matches e2)
    | ITimes (e1, e2) -> times_expr (add_matches e1, add_matches e2)
    | INeg e -> Neg (add_matches e)
    | IIfPositive (e, et, ef) -> IfPositive (add_matches e, add_matches et, add_matches ef)


// Problem 6

let rec infer (e : expr) (tyenv : tyenvir) : typ =
    match e with
    | Var x -> lookup x tyenv
    | NumI _ -> Int
    | NumF _ -> Float
    | Plus(e1, e2) ->
        match infer e1 tyenv, infer e2 tyenv with
        | Int, Int -> Int
        | Float, Float -> Float
        | _ -> failwith "wrong operand type"
    | Times(e1, e2) ->
        match infer e1 tyenv, infer e2 tyenv with
        | Int, Int -> Int
        | Float, Float -> Float
        | _ -> failwith "wrong operand type"
    | IfPositive (e, et, ef) ->
        match infer e tyenv with 
        | _ -> 
            match infer et tyenv, infer ef tyenv with
            | Int, Int -> Int
            | Float, Float -> Float
            | _ -> failwith "branches of different types"
    | IntToFloat e ->
        match infer e tyenv with
        | Int -> Float
        | _ -> failwith "wrong operand type"
    | Neg e ->
        match infer e tyenv with
        | Int -> Float
        | Float -> Int
    | Match (e, xi, ei, xf, ef) ->
        match infer e tyenv with
        | Int -> infer ei (tyenv @ [xi, Int])
        | Float -> infer ef (tyenv @ [xf, Float])



// Problem 7

let rec add_casts (e : iexpr) (tyenv : tyenvir) : expr =
    match e with
    | IVar x -> 
        match lookup x tyenv with
        | _ -> Var x
    | INumI x -> NumI x
    | INumF x -> NumF x
    | IPlus (e1, e2) -> 
        match add_casts e1 tyenv , add_casts e2 tyenv with
        | NumI x, NumF y -> Plus (IntToFloat(NumI x), NumF y)
        | NumF x, NumI y -> Plus (NumF x, IntToFloat(NumI y))
        | NumI x, NumI y -> Plus (NumI x, NumI x)
        | NumF x, NumF y -> Plus (NumF x, NumF x)
        | Var x, Var y -> Plus (Var x, Var y)
        | Var x, NumI y ->
            match lookup x tyenv with
            | Int -> Plus(Var x, NumI y)
            | Float -> Plus(Var x, IntToFloat(NumI y))
        | NumI x, Var y ->
            match lookup y tyenv with
            | Int -> Plus(NumI x, Var y)
            | Float -> Plus(IntToFloat(NumI x), Var y)
        | Var x, NumF y ->
            match lookup x tyenv with
            | Int -> Plus(IntToFloat(Var x), NumF y)
            | Float -> Plus(Var x, NumF y)
        | NumF x, Var y ->
            match lookup y tyenv with
            | Int -> Plus(NumF x, IntToFloat(Var y))
            | Float -> Plus(NumF x, Var y)
    | ITimes (e1, e2) -> 
        match add_casts e1 tyenv , add_casts e2 tyenv with
        | NumI x, NumF y -> Times (IntToFloat(NumI x), NumF y)
        | NumF x, NumI y -> Times (NumF x, IntToFloat(NumI y))
        | NumI x, NumI y -> Times (NumI x, NumI x)
        | NumF x, NumF y -> Times (NumF x, NumF x)
        | Var x, Var y -> Times (Var x, Var y)
        | Var x, NumI y ->
            match lookup x tyenv with
            | Int -> Times(Var x, NumI y)
            | Float -> Times(Var x, IntToFloat(NumI y))
        | NumI x, Var y ->
            match lookup y tyenv with
            | Int -> Times(NumI x, Var y)
            | Float -> Times(IntToFloat(NumI x), Var y)
        | Var x, NumF y ->
            match lookup x tyenv with
            | Int -> Times(IntToFloat(Var x), NumF y)
            | Float -> Times(Var x, NumF y)
        | NumF x, Var y ->
            match lookup y tyenv with
            | Int -> Times(NumF x, IntToFloat(Var y))
            | Float -> Times(NumF x, Var y)
    | INeg e -> Neg (add_casts e tyenv)
    | IIfPositive (e, et, ef) -> IfPositive (add_casts e tyenv, add_casts et tyenv, add_casts ef tyenv)


// Problem 8

// ANSWER 8 HERE:
(*
    1. Can eval (add_matches e) [] and eval (add_casts e []) [] have different behaviour?
    Yes, when sending ITimes or IPlus into add_casts and add_matches they behave differently because add_matches uses Match through plus_expr and times_expr
    so eval will go into its Match. The add_casts function uses a lot af match with in the function to be sure that all cases are covered and therefor the eval 
    does not use its Match. It should though give us the same result. (Assuming that the value in env is of the type in typenv)

    > eval (add_casts (IIfPositive (INeg (IVar "x"), IPlus (IVar "y", INumI 5), ITimes (IVar "y", INumI 5))) ["x", Float; "y", Int]) ["x", F 2.2; "y", I 4];;
    val it : value = I 20

    > eval (add_matches (IIfPositive (INeg (IVar "x"), IPlus (IVar "y", INumI 5), ITimes (IVar "y", INumI 5)))) ["x", F 2.2; "y", I 4];;                     
    val it : value = I 20

    2. What about infer (add_matches e) [] and infer (add_casts e []) []?
    It is the same, for add_matches infer uses Match but not for add_casts. It does, like for the eval, return the same result.

    > infer (add_casts (IIfPositive (INeg (IVar "x"), IPlus (IVar "y", INumI 5), ITimes (IVar "y", INumI 5))) ["x", Float; "y", Int]) ["x", Int; "y", Int];; 
    val it : typ = Int

    > infer (add_matches (IIfPositive (INeg (IVar "x"), IPlus (IVar "y", INumI 5), ITimes (IVar "y", INumI 5)))) ["x", Int; "y", Int];;                     
    val it : typ = Int
*)

// Problem 9

let rec rlower (inss : rcode) : rcode = 
    match inss with
    | [] -> []
    | RPop::inss -> RStore::RErase::rlower inss
    | RDup::inss -> RStore::RLoad 0::RLoad 0::RErase::rlower inss
    | RSwap::inss -> RStore::RStore::RLoad 1::RLoad 0::RErase::RErase::rlower inss
    | x::inss -> x::rlower inss