//// 2.1 Expressions, values, and environments

(3+4)*5 ;;
// >> val it : int = 35

not (true && false);;
// >> val it : bool = true

let m = (3+4)*5;;
// >> val m : int = 35

let x = not (true && false);; 
// >> val x : bool = true

let (m, n) = (3+4)*5, 6*7 ;;
// >> val n : int = 42
// >> val m : int = 35

let m = (3+4)*5 in m*m + (m+1)*(m+1) ;;
// >> val it : int = 2521

//// 2.2 Functions

let f x = 2*x ;;
// >> val f : x:int -> int

(fun x -> 2*x) ;;
// >> val it : x:int -> int

(fun x -> 2*x) 3 ;;
// >> val it : int = 6

let f x = 2*x ;;
// >> val f : x:int -> int

let f = fun x -> 2*x ;;
// >> val f : x:int -> int

//// 2.2.1 Recursive definitions

let rec factorial x = if x=0 then 1 else x*factorial(x-1) ;;
// >> val factorial : x:int -> int

let rec even x = if x=0 then true else 
                    (if x>0 then not(even (x-1)) 
                            else not (- even(x+1)));;
// >> val even : x:int -> bool

//// 2.2.2 Higher-order functions

let eval_at_one f = f 1 ;;
// >> val eval_at_one : f:(int -> 'a) -> 'a

eval_at_one factorial ;;
// >> val it : int = 1

eval_at_one (fun x -> if x > 0 then true else false) ;;
// >> val it : bool = true

let poly_eval f = f (f 3) + (f 3) + 3 ;;
// >> val poly_eval : f:(int -> int) -> int

poly_eval factorial ;;
// >> val it : int = 729

let add_on m = fun n -> m + n ;;
// >> val add_on : m:int -> n:int -> int

let poly f x = f (f x) + (f x) + x ;;
// >> val poly : f:(int -> int) -> x:int -> int

poly factorial 3 ;;
// >> val it : int = 729

//// 2.3 Types

//// 2.3.1 Primitive Types

let add (x:int) (y:int) = x+y ;;
// >> val add : x:int -> y:int -> int

//// 2.3.2 Compound Types

(2, true, "brown") ;;
// >> val it : int * bool * string = (2, true, "brown")

// In F# 4.6 and up:
{|name="Fred"; salary=10000; gender=true|} ;;
// >> val it : {| gender: bool; name: string; salary: int |} = { gender = true
// >>                                                            name = "Fred"
// >>                                                            salary = 10000 }

// In earlier versions of the language:
type PersonnelRecorder = {name: string; salary: int; gender: bool};;
// >> type PersonnelRecorder =
// >>   { name: string
// >>     salary: int
// >>     gender: bool }

{name="fred"; salary=10000; gender=true};;
// >> val it : PersonnelRecorder = { name = "fred"
// >>                                salary = 10000
// >>                                gender = true }

//// 2.3.3 Type abbreviation

type Fcn_and_Int = (int -> int) * int ;;
// >> type Fcn_and_Int = (int -> int) * int

//// 2.4 Type polymorphism

let first x y = x ;;
// >> val first : x:'a -> y:'b -> 'a

let twice f = fun x -> f (f x) ;;
// >> val twice : f:('a -> 'a) -> x:'a -> 'a

List.append;;
// >> val it : ('a list -> 'a list -> 'a list)

//// 2.5 Patterns

let v = (3, false, 4) ;;
// >> val v : int * bool * int = (3, false, 4)

let (x, y, z) = v ;;
// >> val z : int = 4
// >> val y : bool = false
// >> val x : int = 3

let (x, y, _) = v ;;
// >> val y : bool = false
// >> val x : int = 3

// F# note: anonymous records do not support pattern matching
type Person = { name: string * string; age: int } ;;
// >> type Person =
// >>   { name: string * string
// >>     age: int }

let r = { name = ("joe", "smith"); age = 40 } ;;
// >> val r : Person = { name = ("joe", "smith")
// >>                    age = 40 }

let { name= (_, surname); age = _  } = r ;;  
// >> val surname : string = "smith"

let v = ((1, 2), 3) ;;
// >> val v : (int * int) * int = ((1, 2), 3)

let ((_, y) as x, _) = v ;;
// >> val y : int = 2
// >> val x : int * int = (1, 2)

//// 2.6 Defining Types

type Colour = Red | Blue | Green ;;
// >> type Colour =
// >>   | Red
// >>   | Blue
// >>   | Green

let warm colour = match colour with  
                  | Red -> true      
                  | Blue -> false    
                  | Green -> false   
;;
// >> val warm : colour:Colour -> bool

type Plant = Flower of string * int * Colour 
             | Foliage of string * int ;;
// >> type Plant =
// >>   | Flower of string * int * Colour
// >>   | Foliage of string * int

let height plant = match plant with
                   | Flower(_, n, _) -> n
                   | Foliage(_, n)   -> n
;;
// >> val height : plant:Plant -> int

type Num = Zero | Succ of Num ;;
// >> type Num =
// >>   | Zero
// >>   | Succ of Num

// avoid confusion with builtin F# 'even' 
let rec evenPeano num = match num with
                        | Zero     -> true
                        | Succ(n)  -> not (evenPeano n)
;;
// >> val evenPeano : num:Num -> bool

// avoid confusion with builtin F# 'add'
let rec addPeano m n = match m with               
                        | Zero   -> n             
                        | Succ a -> Succ (addPeano a n)
;;
// >> val addPeano : m:Num -> n:Num -> Num

type 'a Pair = Pair of ('a * 'a) ;;
// >> type 'a Pair = | Pair of ('a * 'a)

Pair(3,4) ;;
// >> val it : int Pair = Pair (3, 4)

Pair(true, false) ;;
// >> val it : bool Pair = Pair (true, false)

let firstOfPair (Pair(x, y)) = x ;;
// >> val firstOfPair : 'a Pair -> 'a

//// 2.7 Abstract types

// skipping for the time being

//// 2.8 Exceptions

// note must be uppercase
exception Empty_list of unit ;;
// >> exception Empty_list of unit

let head lst =
    match lst with
    | []    -> raise (Empty_list())
    | a::r  -> a
;;

let tail lst =
    match lst with
    | []    -> raise (Empty_list())
    | a::r  -> r
;;

let rec append s t = 
    try
        (head s)::(append (tail s) t)
    with
        | Empty_list () -> t
;;

// catch any exception
let rec append' s t = 
    try
        (head s)::(append (tail s) t)
    with
        | exc -> t
;;

exception Div_by_zero of int
;;

let divide n d =
    if d=0
        then raise (Div_by_zero n)
        else n/d
;;

try
    divide 3 0
with
    | Div_by_zero n  -> n*n
;;l
