// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
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
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  // NOTE: A recursive definition. You can think of 
  // 'Let(v, e1, e2)' as 'let rec v = e1 in e2'. 
  | Recursive of string * Expression * Expression

and VariableContext = 
  // NOTE: For recursive calls, we need to add the function
  // being defined to the variable context when defining it.
  // This can be done using 'let rec', but we need to store
  // the variables as lazy values.
  Map<string, Lazy<Value>>

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
      | Some res ->
          // NOTE: As 'res' is now 'Lazy<Value>' we need to get its value here.
          res.Value
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
        let ctx = ctx.Add(name, lazy res2)
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
        let new_ctx = ctx.Add(v,lazy (evaluate ctx exp))
        match b with 
        | true -> evaluate new_ctx e1
        | false -> evaluate new_ctx e2

  | Case(b, e) ->
    ValCase(b, evaluate ctx e)
  | Recursive(v, e1, e2) ->
      // TODO: Implement recursion for 'let rec v = e1 in e2'.
      // (In reality, this will only work if 'e1' is a function
      evaluate ctx (Let(v,e1,e2))

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x -> 
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er = 
  Recursive("factorial", 
    Lambda("x", If(
      Variable("x"),
      Constant(1),
      Binary(
        "*", Variable("x"), 
        Application(Variable("factorial"), 
          Binary("+", Constant(-1), Variable("x")))
      )
    )),  
    Application(Variable "factorial", Constant 5)
  )

evaluate Map.empty er
