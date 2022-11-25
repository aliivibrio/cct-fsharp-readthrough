// Exercise 2.1

// Just evaluating some expressions at the repl, skipped; there was plenty of that working through the chapter.
// Exercise 2.2

// 2.2.1
let signum n = if n < 0 then -1 else (if n =0 then 0 else 1)
;;

// 2.2.2
let absvalue n = if n<0 then -n else n ;;
;;

// 2.2.3
let maxOf a b = if a>b then a else b
;;

// 2.2.4
// Would love to use uints here to make bad input impossible but they are kind of a pain
// Also yes this is the standard bad recursive solution :)
let rec fib n = match n with
                | _ when n < 0 -> raise(System.ArgumentOutOfRangeException("input must be positive"))                 
                | 0 -> 1                                    
                | 1 -> 1
                | _ -> (fib (n-1)) + (fib (n-2))
;;

// Exercise 2.3
type Num = Z | S Num ;;

let printnum num = match num with
                   | Z    -> 0
                   | S n  -> 1 + (printnum n)
;;

// Exercise 2.4

// 2.4.1
// most general type for `apply`: can't take credit for this one, because the definitions of `application` spit out the exact
//  same type if you don't annotate it :)
//  You'll have just have to believe I defined the signature type first...
type Apply<'a, 'b> = ('a -> 'b) -> 'a -> 'b ;; 
let application : Apply<'a, 'b> = fun f v -> f v;;
let pipeApplication : Apply<'a, 'b> = fun f v -> v |> f;;

// 2.4.2
// Not sure about ML but nice that F# has composition infix operators
type Compose<'a, 'b, 'c> = ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c) ;;
let compositionForward : Compose<'a, 'b, 'c> = fun g f -> f >> g ;;
let compositionBackward : Compose<'a, 'b, 'c> = fun g f -> g << f ;;

// Exercise 2.5
// 2.5.1
let maxvalue lst =
    match lst with
    | [] -> None
    | x::xs -> let rec maxval lst n  =
                       match lst with
                       | [] -> n 
                       | x::xs -> max x (maxval xs n)
               in Some (maxval xs x)               
;;

// 2.5.2
let rec sum lst =
    match lst with
    | []    -> 0
    | x::xs -> x + (sum xs)
;;

// 2.5.3
let rec poly coefficients x =
    match coefficients with
    | []    ->  0
    | c::cs ->  c + x * (poly cs x)
;;
 
let rec reverse lst =
    match lst with
    | []    ->  []
    | x::xs ->  List.append (reverse xs) [x]
;;

let rec maplist f lst =
    match lst with
    | []        -> []
    | x::xs     -> (f x)::(maplist f xs)
;;

let rec reduce f seed lst =
    match lst with
    | []    -> seed
    | x::xs -> reduce f (f x seed) xs
;;

let rec append lst1 lst2 =
    match lst1 with
    | []    -> lst2
    | x::xs -> x::(append xs lst2)
;;

// Exercise 2.6
type 'a BinTree = Tip of 'a | Node of ('a BinTree)*(a' BinTree) ;;

// 2.6.1
let rec breadth binTree =
    match binTree with
    | Tip _                     -> 1
    | Node (leftchild, rightchild) -> (breadth leftchild) + (breadth rightchild)
;;

// 2.6.2
let rec depth binTree =
    match binTree with
    | Tip _ -> 0
    | Node (leftchild, rightchild) -> 1 + (max (depth leftchild) (depth rightchild))
;;

// 2.6.3
let rec inOrder binTree =
    match binTree with
    | Tip v     -> [v]
    | Node (leftchild, rightchild) -> List.append (inOrder leftchild) (inOrder rightchild)
;;



let rec isPrefix lst1 lst2 =
    match (lst1, lst2) with
    | ([], _)                   -> true
    | (x::xs, y::ys) when x = y -> isPrefix xs ys
    | _ -> false
;;

let rec isInfix lst1 lst2 =
    match (lst1, lst2) with
    | ([], _) -> true
    | (x::xs, y::ys) when x = y  -> isPrefix xs ys
    | (_, y::ys)                 -> isInfix lst1 ys
    | _                          -> false
;;

// Exercise 2.7
type RationalQ (numerator: int, denominator: int) = // probably should check denominator != 0 and raise if so
    member val private n = numerator
    member val private d = denominator
    member r.floatValue = (float r.n)/(float r.d)
    member r.add (s:RationalQ) = RationalQ((r.n * s.d + s.n * r.d), (r.d * s.d))
    member r.mult (s:RationalQ) = RationalQ(r.n * s.n, r.d*s.d)
    member r.negate = RationalQ(r.n * - 1, r.d)
    member r.reciprocal = RationalQ(r.d, r.n)
    member r.div (s:RationalQ) = r.mult(s.reciprocal)
    member r.sub (s:RationalQ) = r.add(s.negate);;

// this exercise also allowed the choice of (integral part) . \d^{k} \d^{\omega} as a representation
//  but since the point is to define an abstract type that's what I went with. This type with hidden instance data
//  seemed like the closest equivalent.