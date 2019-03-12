module Synthesis

let abelar x = // 1 line
    x > 12 && x < 3097 && x % 12 = 0 

let area _base _height = // 3 lines
    match _base < 0.0 || _height < 0.0 with
    | false -> 0.5*_base*_height 
    | true -> failwith "Exception: negative height or width not allowed"

let zollo x = // 3 lines
    match x < 0 with
    | true -> -1 * x
    | _ -> 2 * x

let min a b = // 3 lines
    match a < b with
    | true -> a
    | _ -> b

let max a b = // 3 lines
    match a < b with
    | true -> b
    | _ -> a

let ofTime h m s = 3600*h + 60*m + s // 1 line 

let toTime s = // 3 lines
    match s < 0 with
    | true -> (0, 0, 0)
    | _ -> (s/3600, (s%3600)/60, (s%3600)%60)

let digits n = // 5 lines
    let rec count c =
        match c/10 = 0 with
        | true -> 1
        | _ -> 1 + count(c/10)
    count n
    

let minmax tuple = // 3 lines
    match  tuple with
    | (a, b, c, d) -> 
        (min (min a b) (min c d), max (max a b) (max c d)) 

let isLeap y = // 5 lines
    match y < 1582 with
    | true -> failwith "Exception: invalid year."
    | _ -> match y % 4 = 0 && y % 100 = 0 with
           | true -> y % 400 = 0
           | _ -> y % 4 = 0
        


let month = function // 13 lines
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


let toBinary i = // 11 lines
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

//10 lines
let bizFuzz n = let rec divisible (i, a, b, c) = match i = n + 1 with | true -> (a,b,c) | false  ->  match (i % 3 = 0, i % 5 = 0, i % 3 = 0 && i % 5 = 0)  with
                                                                                                     | false, false, false -> divisible(i+1,a, b, c)
                                                                                                     | false, false, true -> divisible(i+1,a, b, c+1)
                                                                                                     | false, true, false -> divisible(i+1,a, b+1, c)
                                                                                                     | false, true, true -> divisible(i+1,a, b+1, c+1)
                                                                                                     | true, false, false -> divisible(i+1,a+1, b, c)
                                                                                                     | true, false, true -> divisible(i+1,a+1, b, c+1)
                                                                                                     | true, true, false -> divisible(i+1,a+1, b+1, c)
                                                                                                     | true, true, true -> divisible(i+1,a+1, b+1, c+1)
                match n > 0 with | true -> divisible (1,0,0,0) | false -> (0,0,0)


let monthDay d y = // 12 lines
    let rec sumDays i s =
        match s >= d with 
        | true -> i-1
        | _ -> 
            match isLeap y && i = 2 with
            | true -> sumDays (i+1) (s+29)
            | _ -> 
            match month i with
            | _, a -> sumDays (i+1) (s+a)  
    match (((d < 1 || d > 366) && isLeap y) || ((d < 1 || d > 365) && isLeap y = false)) || y < 1582 with
    | true -> failwith "Exception: Invalid day or year"
    | _ -> match month (sumDays 1 0) with | a,_ -> a 


let sqrt n =
    let rec calculate guess i =
        match i with
        | 10 -> guess
        | _ ->
            let g = (guess + n/guess) / 2.0
            calculate g (i+1)
    match n <= 0.0 with
    | true -> failwith "Impossibru!"
    | _ ->
        calculate (n/2.0) 0

let coord x = // 5 lines
    let dist p2 =
        match x,p2 with | ((a,b),(c,d)) -> sqrt ((a-c)*(a-c) + (b-d)*(b-d))
    let within lc w h =
        match x,lc with | ((ic1,ic2), (lc1,lc2)) -> (ic1 : float) >= (lc1 : float) && (ic1 : float) <= (lc1+w : float) && (ic2 : float) <= (lc2 : float) && (ic2 : float) >= (lc2-h : float)
    (dist, within)