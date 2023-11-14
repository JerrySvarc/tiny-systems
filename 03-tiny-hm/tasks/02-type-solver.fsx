// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise
// a bit more interesting, we will implement constraint resolution
// for lists here already. This will help you in the next steps!
type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type

let rec occursCheck vcheck ty =
    // TODO: Return true of type 'ty' contains variable 'vcheck'
    match ty with
    | TyList list -> occursCheck vcheck list
    | TyVariable var -> var = vcheck
    | _ -> false

let rec substType (subst: Map<string, Type>) ty =
    // TODO: Apply all the specified substitutions to the type 'ty'
    // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
    match ty with
    | TyVariable var when subst.ContainsKey(var) -> substType subst subst.[var]
    | TyList(list) -> substType subst list
    | _ -> failwith "nothing to substitute"


let substConstrs (subst: Map<string, Type>) (cs: list<Type * Type>) =
    // TODO: Apply substitution 'subst' to all types in constraints 'cs'
    cs |> List.map (fun (n1, n2) -> (substType subst n1, substType subst n2))


let rec solve cs =
    match cs with
    | [] -> []
    | (TyNumber, TyNumber) :: cs
    | (TyBool, TyBool) :: cs -> solve cs
    | (TyBool, TyNumber) :: _
    | (TyNumber, TyBool) :: _ -> failwith "missmatch"
    | (TyList l1, TyList l2) :: cs -> solve ((l1, l2) :: cs)
    | (TyVariable v, n) :: cs
    | (n, TyVariable v) :: cs ->
        if occursCheck v n then
            failwith "Cannot be solved (occurs check)"

        let cs = substConstrs (Map.ofList [ v, n ]) cs
        let subst = solve cs
        let t = substType (Map.ofList subst) n
        (v, t) :: subst




// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve
    [ TyList(TyVariable("a")), TyList(TyNumber)
      TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve [ TyList(TyVariable("a")), TyVariable("b"); TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve [ TyList(TyVariable("a")), TyVariable("b"); TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve [ TyList(TyVariable("a")), TyVariable("a") ]
