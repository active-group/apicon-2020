namespace Properties

module Examples =
    open FsCheck
    open FsCheck.Util

    let Listrev xs =
        match xs with
        | 1::xs' -> List.rev xs'
        | _ -> List.rev xs

    let revRevIsOrig =
      Prop.forAll (Arb.list Arb.int) (fun xs ->
        List.rev (List.rev xs) = xs)

    let revOne =
      Prop.forAll Arb.int (fun x ->
        List.rev [x] = [x])

    let revAppend =
      Prop.forAll (Arb.pair (Arb.list Arb.int) (Arb.list Arb.int))
        (fun (xs1, xs2) ->
          (List.append (List.rev xs1) (List.rev xs2)) = List.rev (List.append xs2 xs1))


module ISet =
  type ISet = list<int * int>

  let rec isValid s =
    match s with
    | ((lo1, hi1) :: (lo2, hi2) :: rest) ->
        lo1 <= hi1 && hi1+1 < lo2 && isValid ((lo2, hi2)::rest)
    | [(lo, hi)] -> lo <= hi
    | [] -> true

  let rec range lo hi =
    if hi < lo 
    then []
    else lo :: (range (lo + 1) hi)

  let rec merge2 l1 l2 =
    match (l1, l2) with
    | ([], l2) -> l2
    | (l1, []) -> l1
    | (x1::l1', x2::l2') ->
      if x1 = x2
      then x1 :: (merge2 l1' l2')
      else if x1 < x2
      then x1 :: (merge2 l1' l2)
      else x2 :: (merge2 l1 l2')

  let rec mmerge l =
    match l with
    | [] -> []
    | x::xs -> merge2 x (mmerge xs)

  let toList s =
      mmerge (List.map (fun (lo, hi) -> range lo hi) s)

  module Arb =
    open FsCheck
    open FsCheck.Util

    let rec dropOverlaps iset =
      match iset with
      | ((_, a) as i)::((b, _) as j) :: rest when a + 1 < b ->
          i :: dropOverlaps (j::rest)
      | (i::_::rest) -> dropOverlaps (i::rest)
      | rest -> rest           

    let iset =
      let gen =
        gen {
            let! lis = Arb.toGen (Arb.list (Arb.pair Arb.nonNegativeInt Arb.nonNegativeInt))
            return dropOverlaps (List.sortBy fst (List.map (fun (a, b) -> (min a b, max a b)) lis))
        }
      in Arb.fromGen gen


  module Test =
    open FsCheck
    open FsCheck.Util

    let (.=.) left right = left = right |@ sprintf "%A = %A" left right

    let unionCorrect union =
      Prop.forAll (Arb.pair Arb.iset Arb.iset) (fun (s1, s2) ->
        toList (union s1 s2)
        .=.
        merge2 (toList s1) (toList s2))

    let unionValid union =
      Prop.forAll (Arb.pair Arb.iset Arb.iset) (fun (s1, s2) ->
        isValid (union s1 s2))


  let union1 [] [] = []

  let union2 [] s = s

  let union3 s1 s2 =
    match (s1, s2) with
    | ([], s) -> s
    | (s, []) -> s

  let rec union4 s1 s2 =
    match (s1, s2) with
    | ([], s) -> s
    | (s, []) -> s
    | ((lo1, hi1) :: s1, (lo2, hi2) :: s2) ->
      (min lo1 lo2, max hi1 hi2) :: (union4 s1 s2)

  let rec union5 s1 s2 =
    match (s1, s2) with
    | ([], s) -> s
    | (s, []) -> s
    | ((lo1, hi1) :: s1, (lo2, hi2) :: s2) ->
      if lo1 > hi2 
      then (lo2, hi2) :: union5 ((lo1, hi1) :: s1) s2
      else (min lo1 lo2, max hi1 hi2) :: (union5 s1 s2)

  let rec union6 s1 s2 =
    match (s1, s2) with
    | ([], s) -> s
    | (s, []) -> s
    | ((lo1, hi1) :: s1, (lo2, hi2) :: s2) ->
      if lo1 > hi2 
      then (lo2, hi2) :: union6 ((lo1, hi1) :: s1) s2
      else if lo2 > hi1
      then (lo1, hi1) :: union6 s1 ((lo2, hi2) :: s2)
      else (min lo1 lo2, max hi1 hi2) :: (union6 s1 s2)


  let rec union7 s1 s2 =
    match (s1, s2) with
    | ([], s) -> s
    | (s, []) -> s
    | ((lo1, hi1) :: s1, (lo2, hi2) :: s2) ->
      if lo1 > hi2 
      then (lo2, hi2) :: union7 ((lo1, hi1) :: s1) s2
      else if lo2 > hi1
      then (lo1, hi1) :: union7 s1 ((lo2, hi2) :: s2)
      else union7 ((min lo1 lo2, max hi1 hi2) :: s1) s2

  let rec union8 s1 s2 =
    match (s1, s2) with
    | ([], s) -> s
    | (s, []) -> s
    | ((lo1, hi1) :: s1, (lo2, hi2) :: s2) ->
      if lo1 > hi2 
      then (lo2, hi2) :: union8 ((lo1, hi1) :: s1) s2
      else if lo2 > hi1
      then (lo1, hi1) :: union8 s1 ((lo2, hi2) :: s2)
      else if hi1 >= hi2
      then union8 ((min lo1 lo2, hi1) :: s1) s2
      // if hi2 >= hi1
      else  union8 s1 ((min lo1 lo2, hi2) :: s2)

  let rec union9 s1 s2 =
    match (s1, s2) with
    | ([], s) -> s
    | (s, []) -> s
    | ((lo1, hi1) :: s1, (lo2, hi2) :: s2) ->
      if lo1 > hi2 + 1 
      then (lo2, hi2) :: union9 ((lo1, hi1) :: s1) s2
      else if lo2 > hi1 + 1
      then (lo1, hi1) :: union9 s1 ((lo2, hi2) :: s2)
      else if hi1 >= hi2
      then union9 ((min lo1 lo2, hi1) :: s1) s2
      // if hi2 >= hi1
      else  union9 s1 ((min lo1 lo2, hi2) :: s2)
