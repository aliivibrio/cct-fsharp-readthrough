// 2.8: More list processing

// 2.8.1: Delete nth occurrence (counting from 0) of a value from a list
let rec deleteNth item n lst =
    match lst with
    | [] -> []
    | x::xs when n=0 && x=item -> xs                             
    | x::xs when n>0 && x=item -> x::(deleteNth item (n-1) xs)
    | x::xs                    -> x::(deleteNth item n xs)
;;

// 2.8.2: Test whether list `lx` is a sublist (not necessarily a contiguous one) of `ly`
let rec isSubList lx ly =
    match (lx, ly) with
    | ([], _)                 -> true
    | (x::xs, [])             -> false
    | (x::xs, y::ys) when x=y -> isSubList xs ys
    | (x::xs, y::ys)          -> isSubList lx ys 
;;

// 2.8.3: Count the number of times `lx` is a sublist of `ly`
// Rough justification: every time the head of lx appears in ly,
//  it may be the start of any number of sublists (so we enumerate the number of times the remainder of lx
//  is a sublist of the remainder of ly).
// For any element of ly (whether it matches the head of lx or not), lx may be a sublist of ys, so those are always counted.
let rec numberOfSublists lx ly =
    match (lx, ly) with
    | ([], _)                       -> 1
    | (x::xs, [])                   -> 0
    | (x::xs, y::ys) when x = y     -> (numberOfSublists xs ys) + (numberOfSublists lx ys)
    | (x::xs, y::ys)                -> numberOfSublists lx ys
;;

type 'a Monoid = Monoid of ('a -> 'a -> 'a) * 'a ;;

let rec liftMonoidToList (Monoid(op, ident)) f lst =
    match lst with
    | []    -> ident
    | x::xs -> op (f x) (liftMonoidToList (Monoid(op, ident)) f xs)
;;

// 2.9: Operations on finite sets

// Earlier in the chapter there is some ML defining a set type with operations.
// For the most part I'm ignoring that and using the F# Set module, except for
// needing to define singletonSplit for recursion purposes

// The book's version of this raises an error when set is empty, but an
//  option (I think) makes use of the output in pattern-matching easier to read.
let singletonSplit (set: 'a Set) = match Seq.tryLast set with
    | Some e -> Some (set.Remove(e), e)
    | None   -> None
;;

let image = Set.map ;;

// 2.9.1: define disjoint union of finite sets

type 'a Tagged = Left of 'a | Right of 'a ;;

let disjointUnion setA setB = Set.union (Set.map Left setA) (Set.map Right setB) ;;

// 2.9.2: Cartesian product of finite sets

let cartesianProduct setA setB = Set [
    for a in setA ->
        [ 
            for b in setB -> (a, b)
        ]
];;

// 2.9.3: Powerset of a finite set

let rec powerset (set: 'a Set) = 
    match singletonSplit set with
    | Some (smaller, e) -> let smallerPowerset = powerset smaller
                            in
                           Set.union smallerPowerset (Set.map (Set.add e) smallerPowerset)
    | None              -> Set [ Set.empty ]
;;

// 2.9.4: Graphs of total functions between finite set
let rec totalFunctions (dom: 'a Set) (cod: 'b Set) =
    match (singletonSplit dom) with
    | Some (smaller, e) 
        when Set.isEmpty cod -> Set.empty<Set<'a * 'b>>
    | Some (smaller, e)      -> let smallerGraphs        = totalFunctions smaller cod in
                                let toAdd                = Set.map (fun c -> (e, c)) cod in
                                let flipSetAdd set item  = Set.add item set in
                                let extendOneGraph graph = Set.map (flipSetAdd graph) toAdd 
                                 in
                                Set.unionMany (Set.map extendOneGraph smallerGraphs)
    | None                   -> Set.singleton Set.empty<'a * 'b>
;;