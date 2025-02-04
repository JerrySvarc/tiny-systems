// ----------------------------------------------------------------------------
// 05 - Add a simple data type - tuples
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  // NOTE: A tuple value consisting of two other values.
  // (Think about why we have 'Value' here but 'Expression'
  // in the case of 'ValClosure' above!)
  | ValTuple of Value * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: 'Tuple' represents two-element tuple constructor
  // and 'TupleGet' the destructor (accessing a value)
  // Use 'true' for #1 element, 'false' for #2. This is not
  // particularly descriptive, but it works OK enough.
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

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
      // TODO: Access #1 or #2 element of a tuple value.
      // (If the argument is not a tuple, this fails.)
      match e with 
      | Tuple(e1,e2) ->
        match b with
        | true -> evaluate ctx e1
        | false -> evaluate ctx e2
      | _ -> failwith("Not a tuple")
// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - simple tuple example (using the e#1, e#2 notation for field access)
//   (2*21, 123)#1
//   (2*21, 123)#2
let ed1= 
  TupleGet(true, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed1

let ed2 = 
  TupleGet(false, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed2

// Data types - trying to get a first element of a value
// that is not a tuple (This makes no sense and should fail)
//   (42)#1
let ed3 = 
  TupleGet(true, Constant(42))
evaluate Map.empty ed3
