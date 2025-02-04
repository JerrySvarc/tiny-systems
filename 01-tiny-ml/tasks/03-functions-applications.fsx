// ----------------------------------------------------------------------------
// 03 - Functions and application
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  // NOTE: In ML functions are "first-class values" meaning that
  // you can pass them around. A closure is a value that represents
  // a function at run-time. We store the variable name of the lambda,
  // the body of the lambda and captured variable context.
  // (This is the trick to get lexical and not dynamic scoping!)
  | ValClosure of string * Expression * VariableContext

// NOTE: 'ValClosure' above needs to refer to 'Expression' and also
// 'VariableContext'. To make such recursive references, we define 
// the types using 'type .. and .. and' from now on!
and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  // NOTE: Added application 'e1 e2' and lambda 'fun v -> e'
  | Application of Expression * Expression
  | Lambda of string * Expression

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
      // TODO: We added 'ValClosure' to 'Value', so this can now fail to 
      // match (if you call binary operator with functions as arguments).
      // Add a catch-all ('_') case and throw an exception using 'failwith'
      // Also do the same for 'Unary' an 'If'!
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _,_ -> failwith("wrong operand type")
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)

  // NOTE: You have the following two from before
  | Unary(op, e) ->
      // TODO: Implement the case for 'Unary' here!
      let res =  evaluate ctx e  
      match res with
      | ValNum valn ->
        match op with
        | "-" -> ValNum(-valn)
        | _ -> failwith("unsupported operation")
      | ValClosure(_, _, _) -> failwith "wrong operand"
  // TODO: Add the correct handling of 'If' here!
  | If (c,r1,r2) ->
    let res = evaluate ctx c 
    match res with 
    | ValNum valn ->
      match valn with
      | 1 -> evaluate ctx r1
      | _ -> evaluate ctx r2
    | ValClosure(_, _, _) -> failwith "wrong operand"
  
  | Lambda(v, e) ->
      // TODO: Evaluate a lambda - create a closure value
      
      ValClosure(v,e,ctx)

  | Application(e1, e2) ->
      // TODO: Evaluate a function application. Recursively
      // evaluate 'e1' and 'e2'; 'e1' must evaluate to a closure.
      // You can then evaluate the closure body.
      let res1 = evaluate ctx e1
      let res2 = evaluate ctx e2
      match res1 with
      | ValClosure(name,exp,ctx) ->
        let ctx = ctx.Add(name,res2)
        evaluate ctx exp 
      | ValNum(_) -> failwith "Not Implemented"
// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Basic function declaration (should return closure)
//   (fun x -> x * 2) 
let ef1 = 
  Lambda("x", Binary("*", Variable("x"), Constant(2)))
evaluate Map.empty ef1

// Basic function calls (should return number)
//   (fun x -> x * 2) 21
let ef2 = 
  Application(
    Lambda("x", Binary("*", Variable("x"), Constant(2))),
    Constant(21)
  )
evaluate Map.empty ef2

// Wrong function call (the first argument is not a function)
//   21 (fun x -> x * 2)
let ef3 = 
  Application(
    Constant(21),
    Lambda("x", Binary("*", Variable("x"), Constant(2)))
  )
evaluate Map.empty ef3

// Wrong binary operator (it is now possible to apply '+'
// to functions; this makes no sense and should fail!)
//   21 + (fun x -> x * 2)
let ef4 = 
  Binary("+",
    Constant(21),
    Lambda("x", Binary("*", Variable("x"), Constant(2)))  
  )
evaluate Map.empty ef4
