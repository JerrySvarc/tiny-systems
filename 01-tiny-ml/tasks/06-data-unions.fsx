// ----------------------------------------------------------------------------
// 06 - Add more data types - unions
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  // NOTE: Value representing a union case. Again, we use 'bool':
  // 'true' for 'Case1' and 'false' for 'Case2'
  | ValCase of bool * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  // NOTE: 'Case' represents creating a union value and 'Match' pattern 
  // matching. You can read 'Match(e, v, e1, e2)' as F# pattern matching 
  // of the form: 'match e with v -> e1 | v -> e2'
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

and VariableContext = 
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)

  | Unary(op, e) ->
      let res =  evaluate ctx e  
      match res with
      | ValNum valn ->
        match op with
        | "-" -> ValNum(-valn)
        | _ -> failwith("unsupported operation")
      | ValClosure(_, _, _) -> failwith "wrong operand"
      | ValTuple(_, _) -> failwith "Not Implemented"

  | If (c,r1,r2) ->
    let res = evaluate ctx c 
    match res with 
    | ValNum valn ->
      match valn with
      | 1 -> evaluate ctx r1
      | _ -> evaluate ctx r2
    | ValClosure(_, _, _) -> failwith "wrong operand"
    | ValTuple(_, _) -> failwith "Not Implemented"
  
  | Lambda(v, e) ->
      ValClosure(v,e,ctx)

  | Application(e1, e2) ->
      let res1 = evaluate ctx e1
      let res2 = evaluate ctx e2
      match res1 with
      | ValClosure(name,exp,ctx) ->
        let ctx = ctx.Add(name,res2)
        evaluate ctx exp 
      | ValNum(_) -> failwith "Not Implemented"
      | ValTuple(_, _) -> failwith "Not Implemented"

  | Let(v, e1, e2) ->
    let lambda = Lambda (v, e2)
    evaluate ctx (Application (lambda ,e1))

  | Tuple(e1, e2) ->
    let res1 = evaluate ctx e1
    let res2 = evaluate ctx e2
    ValTuple(res1,res2)
  | TupleGet(b, e) ->
      match e with 
      | Tuple(e1,e2) ->
        match b with
        | true -> evaluate ctx e1
        | false -> evaluate ctx e2
      | _ -> failwith("Not a tuple")

  | Match(e, v, e1, e2) ->
      // TODO: Implement pattern matching. Note you need to
      // assign the right value to the variable of name 'v'!
       match e with
       | Case(b,exp) ->
        let new_ctx = ctx.Add(v,(evaluate ctx exp))
        match b with 
        | true -> evaluate new_ctx e1
        | false -> evaluate new_ctx e2

  | Case(b, e) ->
      // TODO: Create a union value.
      ValCase(b, evaluate ctx e)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 =
  Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 = 
  Match(Case(true, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec2

let ec3 = 
  Match(Case(false, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec3
