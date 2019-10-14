namespace FsCheck

module Util =
    open FsCheck

    let nonNullStringGenerator =  Arb.generate<NonNull<string>> |> Gen.map (fun (NonNull s) -> s)

    /// provide one random value of desired type using the registered generator;
    /// size is passed to generator; for ints it's the maximum provided abs(i)
    let genOne (size:int) : 'a = Arb.generate<'a> |> Gen.sample size 1 |> List.head
    /// provide n random value of desired type using the registered generator;
    /// size is passed to generator; for ints it's the maximum provided abs(i)
    let genMany (size:int) (n:int) : 'a list = Arb.generate<'a> |> Gen.sample size n
    

    /// Assert FsCheck property with given config
    let AssertPropertyWithConfig c p = Check.One(c, p)

    let consoleRunner =
        { new IRunner with
            member __.OnStartFixture t =
                printf "%s" (Runner.onStartFixtureToString t)
            member __.OnArguments (ntest,args, every) =
                printf "%s" (every ntest args)
            member __.OnShrink(args, everyShrink) =
                printf "%s" (everyShrink args)
            member __.OnFinished(name,testResult) = 
                printf "%s" (Runner.onFinishedToString name testResult)
        }

    let quickConfig =
            { MaxTest       = 100
              MaxFail       = 1000
              Replay        = None
              Name          = ""
              StartSize     = 1
              EndSize       = 100
              QuietOnSuccess = false
              Every         = fun _ _ -> ""
              EveryShrink   = fun _ -> ""
              Arbitrary     = []
              Runner        = consoleRunner } 

    let quick(property: 'Testable) = Check.One(quickConfig, property)

    module Gen =
        let pair g1 g2 =
            gen {
                let! x1 = g1
                let! x2 = g2
                return (x1, x2)
            }

        let triple g1 g2 g3 =
            gen {
                let! x1 = g1
                let! x2 = g2
                let! x3 = g3
                return (x1, x2, x3)
            }

        let quadruple g1 g2 g3 g4 =
            gen {
                let! x1 = g1
                let! x2 = g2
                let! x3 = g3
                let! x4 = g4
                return (x1, x2, x3, x4)
            }

        let quintuple g1 g2 g3 g4 g5 =
            gen {
                let! x1 = g1
                let! x2 = g2
                let! x3 = g3
                let! x4 = g4
                let! x5 = g5
                return (x1, x2, x3, x4, x5)
            }

        let octuple g1 g2 g3 g4 g5 g6 g7 g8 =
            gen {
                let! x1 = g1
                let! x2 = g2
                let! x3 = g3
                let! x4 = g4
                let! x5 = g5
                let! x6 = g6
                let! x7 = g7
                let! x8 = g8
                return (x1, x2, x3, x4, x5, x6, x7, x8)
            }

        let nontuple g1 g2 g3 g4 g5 g6 g7 g8 g9 =
            gen {
                let! x1 = g1
                let! x2 = g2
                let! x3 = g3
                let! x4 = g4
                let! x5 = g5
                let! x6 = g6
                let! x7 = g7
                let! x8 = g8
                let! x9 = g9
                return (x1, x2, x3, x4, x5, x6, x7, x8, x9)
            }

        let array (size: int) (a: Gen<'a>): Gen<'a[]> =
            let rec loop (i: int) (vs: list<'a>) =
                if i < size then
                    gen {
                        let! v = a
                        return! loop (i + 1) (v::vs)
                    }
                else
                    gen { return vs |> List.rev |> Array.ofList }
            loop 0 []

        let optional (g: Gen<'a>): Gen<option<'a>> =
            Gen.frequency [(9, g |> Gen.map Some);(1, gen {return None})]

        let letterString: Gen<string> =
            let stringOfList (lis: list<char>): string =
                new System.String(Array.ofList lis)
            Gen.elements " ABCDEFGHIJKLMNOPQRSTUVQXYZabcdefghijklmnopqrstuvwxyz"
            |> Gen.nonEmptyListOf
            |> Gen.map stringOfList

    module Arb =
        /// arbitrary non-null string
        let string =
            let shrink s = Arb.shrink s
            in Arb.fromGenShrink (nonNullStringGenerator, shrink)

        let letterString: Arbitrary<string> =
            let shrink s = Arb.shrink s
            Arb.fromGenShrink (Gen.letterString, shrink)

        /// arbitrary bytes array
        let bytes = Arb.from<byte[]>

        let constant (a: 'a): Arbitrary<'a> =
            Gen.constant a |> Arb.fromGen

        /// make an arbitrary for pairs from two arbitraries
        let pair (a1: Arbitrary<'a>) (a2: Arbitrary<'b>): Arbitrary<'a * 'b> =
            let generator = Gen.pair a1.Generator a2.Generator
            let shrink (x1, x2) = (seq { for x1' in a1.Shrinker x1 -> (x1', x2) }) 
                                  |> Seq.append (seq { for x2' in a2.Shrinker x2 -> (x1, x2') })
            Arb.fromGenShrink (generator, shrink)

        /// make an arbitrary of triples from three arbitraries
        let triple (a: Arbitrary<'a>) (b: Arbitrary<'b>) (c: Arbitrary<'c>): Arbitrary<'a * 'b * 'c> =
            let generator = Gen.triple a.Generator b.Generator c.Generator
            let shrink (x, y, z) =
                let parb = pair a (pair b c)
                seq { for (x', (y', z')) in parb.Shrinker (x, (y, z)) -> (x', y', z') }
            Arb.fromGenShrink (generator, shrink)

        let quadruple (a: Arbitrary<'a>) (b: Arbitrary<'b>) (c: Arbitrary<'c>) (d: Arbitrary<'d>): Arbitrary<'a * 'b * 'c * 'd> =
            let generator = Gen.quadruple a.Generator b.Generator c.Generator d.Generator
            let shrink (w, x, y, z) =
                let parb = pair a (pair b (pair c d))
                seq { for (w', (x', (y', z'))) in parb.Shrinker (w, (x, (y, z))) -> (w', x', y', z') }
            Arb.fromGenShrink (generator, shrink)

        let quintuple (a: Arbitrary<'a>) (b: Arbitrary<'b>) (c: Arbitrary<'c>) (d: Arbitrary<'d>) (e: Arbitrary<'e>): Arbitrary<'a * 'b * 'c * 'd * 'e> =
            let generator = Gen.quintuple a.Generator b.Generator c.Generator d.Generator e.Generator
            let shrink (v, w, x, y, z) =
                let parb = pair a (pair b (pair c (pair d e)))
                seq { for (v', (w', (x', (y', z')))) in parb.Shrinker (v, (w, (x, (y, z)))) -> (v', w', x', y', z') }
            Arb.fromGenShrink (generator, shrink)

        let octuple (a: Arbitrary<'a>) (b: Arbitrary<'b>) (c: Arbitrary<'c>) (d: Arbitrary<'d>)
                    (e: Arbitrary<'e>) (f: Arbitrary<'f>) (g: Arbitrary<'g>) (h: Arbitrary<'h>)
         : Arbitrary<'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h> =
            let generator = Gen.octuple a.Generator b.Generator c.Generator d.Generator e.Generator f.Generator g.Generator h.Generator
            let shrink (s, t, u, v, w, x, y, z) =
                let parb = pair a (pair b (pair c (pair d (pair e (pair f (pair g h))))))
                seq { for (s', (t', (u', (v', (w', (x', (y', z'))))))) in parb.Shrinker (s, (t, (u, (v, (w, (x, (y, z))))))) -> (s', t', u', v', w', x', y', z') }
            Arb.fromGenShrink (generator, shrink)

        let nontuple (a: Arbitrary<'a>) (b: Arbitrary<'b>) (c: Arbitrary<'c>) (d: Arbitrary<'d>)
                     (e: Arbitrary<'e>) (f: Arbitrary<'f>) (g: Arbitrary<'g>) (h: Arbitrary<'h>)
                     (i: Arbitrary<'i>)
         : Arbitrary<'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i> =
            let generator = Gen.nontuple a.Generator b.Generator c.Generator d.Generator e.Generator f.Generator g.Generator h.Generator i.Generator
            let shrink (r, s, t, u, v, w, x, y, z) =
                let parb = pair a (pair b (pair c (pair d (pair e (pair f (pair g (pair h i)))))))
                seq { for (r', (s', (t', (u', (v', (w', (x', (y', z')))))))) in parb.Shrinker (r, (s, (t, (u, (v, (w, (x, (y, z)))))))) -> (r', s', t', u', v', w', x', y', z') }
            Arb.fromGenShrink (generator, shrink)

        // Seq.take doesn't work for count > Seq.size source
        let take (count: int) (source : seq<'T>): seq<'T> = 
            if count = 0 then Seq.empty else  
            seq { use e = source.GetEnumerator() 
                  for _ in 0 .. count - 1 do
                      if e.MoveNext() then
                          yield e.Current }

        let rec drop (count: int) (source: list<'a>): list<'a> =
          if count = 0
          then source
          else
              match source with
              | [] -> source
              | (_::rest) -> drop (count - 1) rest

        // try to shrink a list by shrinking one of its element
        let rec shrinkOne (shr: 'a -> seq<'a>) (l: list<'a>) =
          match l with
          | [] -> Seq.empty
          | (x::xs) -> Seq.append (seq { for x' in shr x -> x'::xs })
                                  (seq { for xs' in shrinkOne shr xs -> x::xs'})


        // remove k elements repeatedly
        let rec removes (k: int) (n: int) (xs: list<'a>) : seq<list<'a>> =            
            if k > n
            then Seq.empty
            else
                let xs2 = drop k xs |> Seq.toList
                if Seq.isEmpty xs2
                then Seq.singleton []
                else
                    let xs1 = xs |> List.truncate k
                    Seq.append (Seq.singleton xs2) (Seq.map (List.append xs1) (removes k (n-k) xs2))

        // helper function for list
        let shrinkList (shr: 'a -> seq<'a>) (xs: list<'a>): seq<list<'a>> =
            let n = List.length xs
            Seq.append (Seq.concat (seq { for k in Seq.unfold (fun n -> if n > 0 then Some (n, n / 2) else None) n -> removes k n xs }))
                       (shrinkOne shr xs)

        /// make an arbitrary for lists from an arbitrary for the list elements
        let list (a: Arbitrary<'a>): Arbitrary<list<'a>> =
            let generator = Gen.sized (fun n -> gen {
                                                    let! k = Gen.choose (0, n)
                                                    return! Gen.sequence (seq { for _ in 1 .. k -> a.Generator })
                                                })
            let shrink xs = shrinkList a.Shrinker xs
            Arb.fromGenShrink (generator, shrink)
            
        let array (size: int) (a: Arbitrary<'a>): Arbitrary<'a[]> =
            Arb.fromGen (Gen.array size a.Generator)

        /// make an arbitrary for sets from an arbitrary for the set elements
        let set (a: Arbitrary<'a>): Arbitrary<Set<'a>> =
            let larb = list a
            let generator = larb.Generator |> Gen.map Set.ofList
            // questionable ...
            let shrink s = Set.toList s |> larb.Shrinker |> Seq.map Set.ofList
            Arb.fromGenShrink (generator, shrink)

        let choiceOf2 (a1: Arbitrary<'a>) (a2: Arbitrary<'b>): Arbitrary<Choice<'a, 'b>> =
            let generator = Gen.oneof [a1.Generator |> Gen.map Choice1Of2; a2.Generator |> Gen.map Choice2Of2]
            let shrink c =
                match c with
                | Choice1Of2 x -> a1.Shrinker x |> Seq.map Choice1Of2
                | Choice2Of2 y -> a2.Shrinker y |> Seq.map Choice2Of2
            Arb.fromGenShrink (generator, shrink)
        
        /// expand arbitrary to include options
        let optional (a: Arbitrary<'a>): Arbitrary<option<'a>> =
            let generator = Gen.optional a.Generator
            let shrink = function
                | Some x -> None |> Seq.singleton |> Seq.append (a.Shrinker x |> Seq.map Some)
                | None -> Seq.empty              
            Arb.fromGenShrink (generator, shrink)

        let suchThat (p: 'a -> bool) (a: Arbitrary<'a>): Arbitrary<'a> =
            let generator = Gen.where p a.Generator
            let shrink x = a.Shrinker x |> Seq.filter p
            Arb.fromGenShrink (generator, shrink)

        /// arbitrary that picks one element randomly
        let pickOneOf (els:'a []) =
            let generator = gen {
                    let! idx = Gen.choose(0, els.Length-1)
                    return els.[idx]
                }
            Arb.fromGen generator

        /// resize an arbitrary
        let resize (newSize: int) (a: Arbitrary<'A>) =
            let generator = Gen.resize newSize a.Generator
            let shrinker = a.Shrinker
            Arb.fromGenShrink (generator, shrinker)

