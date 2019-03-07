module Synthesis

let abelar x = 
    x > 12 && x < 3097 && x % 12 = 0

let area _base _height =
    match _base < 0.0 || _height < 0.0 with
    | false -> 0.5*_base*_height 
    | true -> failwith "Exception: negative height or width not allowed"

let zollo x =
    match x < 0 with
    | true -> -1 * x
    | _ -> 2 * x

let min a b =
    match a < b with
    | true -> a
    | _ -> b

let max a b =
    match a < b with
    | true -> b
    | _ -> a

let ofTime h m s = 3600*h + 60*m + s 

let toTime s =
    match s < 0 with
    | true -> (0, 0, 0)
    | _ -> (s/3600, (s%3600)/60, (s%3600)%60)

let digits n =
    let rec count c =
        match c/10 = 0 with
        | true -> 1
        | _ -> 1 + count(c/10)
    count n
    

let minmax tuple =
    match  tuple with
    | (a, b, c, d) -> 
        (min (min a b) (min c d), max (max a b) (max c d)) 

let isLeap y =
    match y < 1582 with
    | true -> failwith "Exception: invalid year."
    | _ -> match y % 4 = 0 && y % 100 = 0 with
           | true -> y % 400 = 0
           | _ -> y % 4 = 0
        


let month = function
| 1 -> ("January", 31)
| 2 -> ("February", 28)
| 3 -> ("March", 31)
| 4 -> ("April", 30)
| 5 -> ("May", 31)
| 6 -> ("June", 30)
| 7 -> ("July", 31)
| 8 -> ("August", 31)
| 9 -> ("September", 30)
| 10 -> ("October", 31)
| 11 -> ("November", 30)
| 12 -> ("December", 31)
| _ -> failwith "Exception: month not supported"


let toBinary i =
    let rec divide2 n =
        match n = 0 with 
        | true -> ""
        | _ -> 
            match n % 2 = 0 with 
            | true -> divide2(n/2) + "0" 
            | _ -> divide2(n/2) + "1"
    match i >= 0, i = 0 with
    | (true, false) -> divide2 i
    | (true, true) -> "0" 
    | _ -> failwith "Exception: expecting positive value"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"