namespace ShoppingCart

module Test =
  open FsCheck
  open FsCheck.Util

  let arbitraryPrice = FsCheck.Arb.convert Price Price.priceAmount Arb.from<int>
  let arbitraryItem = FsCheck.Arb.convert Item Item.itemName Arb.letterString

  type Arbs =
    static member Price () = arbitraryPrice
    static member Item () = arbitraryItem
  
  let register () = Arb.register<Arbs>()

  let total0Correct (prices: list<Item * Price>) =
    let cart = ShoppingCart.Implementation.create prices
    cart.TotalCount () = 0

  let totalCountCorrect =
    Prop.forAll (Arb.list (Arb.pair arbitraryItem arbitraryPrice)) (fun prices ->
      not (List.isEmpty prices) ==>
        let items = List.map fst prices
        Prop.forAll (Arb.list (Arb.pickOneOf (List.toArray items))) (fun items ->
          let cart = ShoppingCart.Implementation.create prices
          List.iter (cart.Add 1) items
          cart.TotalCount () = List.length items))

