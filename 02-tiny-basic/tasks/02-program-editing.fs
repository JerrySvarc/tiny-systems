// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC
open System

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
  | StringValue str ->
    Console.Write str


let getLine state line =
  let commands = state.Program
  commands |> List.find(fun (num,_) -> num = line) 

let addLine state (line, cmd) = 
  // TODO: Add a given line to the program state. This should overwrite 
  // a previous line (if there is one with the same number) and also ensure
  // that state.Program is sorted by the line number.
  // HINT: Use List.filter and List.sortBy. Use F# Interactive to test them!
  let oldProgram = state.Program |> List.filter(fun (num,cmd ) -> num <> line)
  let newProgram = List.append oldProgram [(line,cmd)]
  let sortedState = newProgram|> List.sortBy(fun (num,cmd) -> num)
  {state with Program = sortedState}
// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------
let rec evalExpression expr = 
  match expr with
  | Const value ->
    value

let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(expr) ->
      Console.Write (printValue (evalExpression expr))
      runNextLine state line
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Goto(line) ->
      let line = getLine state line
      runCommand state line
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
  // TODO: Simulate what happens when the user enters a line of code in the 
  // interactive terminal. If the 'line' number is 'Some ln', we want to 
  // insert the line into the right location of the program (addLine); if it
  // is 'None', then we want to run it immediately. To make sure that 
  // 'runCommand' does not try to run anything afterwards, you can pass 
  // 'System.Int32.MaxValue' as the line number to it (or you could use -1
  // and handle that case specially in 'runNextLine')
  match line with 
  | Some ln -> 
    addLine state (ln, cmd)
  | None -> 
    runCommand state (-1,cmd)
    
      

let runInputs state cmds =
  // TODO: Apply all the specified commands to the program state using 'runInput'.
  // This is a one-liner if you use 'List.fold' which has the following type:
  //   ('State -> 'T -> 'State) -> 'State -> list<'T>
   (state,cmds) ||> List.fold runInput
// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
