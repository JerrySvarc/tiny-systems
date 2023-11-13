// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

open System

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that 
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State = 
  { Program : list<int * Command> 
    // TODO: Add variable context to the program state
    Variables : Map<string,Value>
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
  | StringValue str ->
    Console.Write str
  | NumberValue(num) -> 
    Console.Write num
  | BoolValue(b) -> Console.Write b


let getLine state line =
  let commands = state.Program
  commands |> List.find(fun (num,_) -> num = line) 

let addLine state (line, cmd) = 
  let oldProgram = state.Program |> List.filter(fun (num,cmd ) -> num <> line)
  let newProgram = List.append oldProgram [(line,cmd)]
  let sortedState = newProgram|> List.sortBy(fun (num,cmd) -> num)
  {state with Program = sortedState}

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------
    
let rec evalExpression (state : State) expr = 
  // TODO: Add support for 'Function' and 'Variable'. For now, handle just the two
  // functions we need, i.e. "-" (takes two numbers & returns a number) and "="
  // (takes two values and returns Boolean). Note that you can test if two
  // F# values are the same using '='. It works on values of type 'Value' too.
  //
  // HINT: You will need to pass the program state to 'evalExpression' 
  // in order to be able to handle variables!
  match expr with
  | Const value ->
    value
  | Function(operator, operands) -> 
    let ev1 =  evalExpression state operands[0]
    let ev2  = evalExpression state operands[1]
    match operator with 
    | "-" -> 
      match ev1,ev2 with 
      | NumberValue n1, NumberValue n2 ->
        NumberValue(n1 - n2)
      |  _ -> failwith "cannot subtract two non-numbers"
    | "=" -> 
      BoolValue(ev1 = ev2) 
    | _ -> failwith "operator not implemented"
  | Variable name ->
    let exp = state.Variables |> Map.find(name)
    exp

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(expr) ->
      Console.Write (printValue (evalExpression state expr))
      runNextLine state line
  | Goto(line) ->  
    let line = getLine state line
    runCommand state line
  
  // TODO: Implement assignment and conditional. Assignment should run the
  // next line after setting the variable value. 'If' is a bit trickier:
  // * 'L1: IF TRUE THEN GOTO <L2>' will continue evaluating on line 'L2'
  // * 'L1: IF FALSE THEN GOTO <L2>' will continue on line after 'L1'
  // * 'L1: IF TRUE THEN PRINT "HI"' will print HI and continue on line after 'L1'
  //
  // HINT: If <e> evaluates to TRUE, you can call 'runCommand' recursively with
  // the command in the 'THEN' branch and the current line as the line number.
  | Assign (name,expr) -> 
    let vars = state.Variables.Add(name,evalExpression state expr)
    {state with Variables = vars}
  | If (exp, cmd) -> 
    let res = evalExpression state exp
    if res = BoolValue(true) then 
        


and runNextLine state line = 
  let nextLine = List.tryFind(fun (num,cmd) -> num > line) state.Program
  match nextLine with
  | Some cmd -> 
    runCommand state cmd 
  | None -> state



// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  match line with 
  | Some ln -> 
    addLine state (ln, cmd)
  | None -> 
    runCommand state (-1,cmd)
    
      

let runInputs state cmds =
   (state,cmds) ||> List.fold runInput

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let empty = { Program = []; Variables = Map.empty } // TODO: Add empty variables to the initial state!

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
