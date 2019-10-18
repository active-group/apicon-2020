namespace ShoppingCart

module Test =
  open FsCheck
  open FsCheck.Util

  module Arb =
    let price = Arb.convert Price Price.priceAmount Arb.positiveInt // int for starters
    let item = Arb.convert Item Item.itemName Arb.letterString
    let mapIsNonEmpty map = not (Map.isEmpty map)

    let prices = Arb.filter mapIsNonEmpty (Arb.map item price)

    // First version: gets caught out by addingItemIncreasesTotal
    let discount' =
      let arbN = Arb.choose 1 20
      let convertTo (receive, payFor) = { receive = receive; payFor = payFor }
      let convertFrom discount = (discount.receive, discount.payFor)
      let valid (receive, payFor) = payFor < receive
      Arb.convert convertTo convertFrom (Arb.filter valid (Arb.pair arbN arbN))

    let discount =
      let convertTo receive = { receive = receive; payFor = receive-1 }
      let convertFrom discount = discount.receive
      Arb.convert convertTo convertFrom (Arb.choose 2 20)

    let discounts prices =
      let items = Seq.map fst (Map.toSeq prices)
      Arb.map (Arb.pickOneOf items) discount

    let priceAndDiscounts =
      let gen = gen {
           let! prices = Arb.toGen prices
           let! discounts = Arb.toGen (discounts prices)
           return (prices, discounts)
          }
      in Arb.fromGen gen

    let countAndItem prices =
      let items = Seq.map fst (Map.toSeq prices)
      Arb.pair (Arb.choose 1 50) (Arb.pickOneOf items) // int before 

    let countAndItems prices = Arb.list (countAndItem prices)

  type Arbs =
    static member Price () = Arb.price
    static member Item () = Arb.item
   
  let register () = Arb.register<Arbs>()

  let (.=.) left right = left = right |@ sprintf "%A = %A" left right
  let (.>=.) left right = left >= right |@ sprintf "%A >= %A" left right
  let (.>.) left right = left > right |@ sprintf "%A > %A" left right

  let total0Correct (create: Map<Item, Price> -> Interface) =
    Prop.forAll Arb.prices (fun prices ->
      let cart = create prices
      Interface.totalCount cart .=. 0)
   
  let totalCountCorrect create =
    Prop.forAll Arb.prices (fun prices ->
      let items = List.map fst (Map.toList prices)
      Prop.forAll (Arb.list (Arb.pickOneOf items)) (fun items ->
        let cart = create prices
        List.iter (Interface.add cart 1) items
        cart.TotalCount () .=. List.length items))

  // doesn't know about counts going below zero
  let itemCountCorrect create =
    Prop.forAll Arb.prices (fun prices ->
      let item = fst (Seq.head (Map.toSeq prices))
      Prop.forAll (Arb.list Arb.int) (fun counts ->
        let cart = create prices
        List.iter (fun count -> Interface.add cart count item) counts
        Interface.totalCount cart .=. List.sum counts))
        
  let onePriceCorrect create =
    Prop.forAll Arb.prices (fun prices ->
      let itemPrice = Seq.head (Map.toSeq prices)
      let cart = create prices
      Interface.add cart 1 (fst itemPrice)
      Interface.total cart .=. snd itemPrice)
      
  let addingItemIncreasesTotal (create: Map<Item, Price> -> Map<Item, Discount> -> Interface) =
    Prop.forAll Arb.prices (fun prices ->
      Prop.forAll (Arb.discounts prices) (fun discounts ->
        Prop.forAll (Arb.countAndItems prices) (fun countAndItems ->
          let cart = create prices discounts
          List.iter (fun (count, item) -> Interface.add cart count item) countAndItems
          let itemPrice = Seq.head (Map.toSeq prices)
          let totalBefore = Interface.total cart
          Interface.add cart 2 (fst itemPrice)  // start with 1
          let totalAfter = Interface.total cart
          totalAfter .>. totalBefore))) // 2 for 1 might keep price the same

  // model-based testing
  open FsCheck.Experimental

  type Model = {
    prices: Map<Item, Price>
    discounts: Map<Item, Discount>
    cart: Map<Item, int>
  }

  let itemCount cart item =
      match Map.tryFind item cart with
      | None -> 0
      | Some count -> count

  let add model count item =
    { model with
        cart = Map.add item (itemCount model.cart item + count) model.cart }

  let calculatePrice (price: Price) (count: int) (discount: Option<Discount>): Price =
    match discount with
    | None -> Price.scale count price
    | Some { receive = receive; payFor = payFor } ->
        if count >= receive
        then Price.scale (((count / receive) * payFor) + count % receive) price
        else Price.scale count price

  let total { prices = prices; discounts = discounts; cart = cart } =
    let folder total item count =
      let price = Map.find item prices
      let optionalDiscount = Map.tryFind item discounts
      Price.add total (calculatePrice price count optionalDiscount)
    Map.fold folder Price.nothing cart 

  let totalCount model =
    Seq.sum (Seq.map snd (Map.toSeq model.cart))

  let cartMachine (create: Map<Item, Price> -> Map<Item, Discount> -> Interface) =
    let add (count, item) =
      { new Operation<Interface, Model>() with
          override __.Run model =
              add model count item
          override __.Check (sut, model) =
              Interface.add sut count item
              ((Interface.totalCount sut .=. totalCount model) %> "totalCount")
              .&.
              ((Interface.total sut .=. total model) %> "total")
          override __.ToString () = sprintf "add %A %A" count item
      }
    let create (prices, discounts) =
      { new Setup<Interface, Model>() with
          override __.Actual () = create prices discounts
          override __.Model () = { prices = prices; discounts = discounts; cart = Map.empty }
      }
    { new Machine<Interface, Model> () with
        override __.Setup =
          let convertFrom (setup: Setup<Interface, Model>) = 
            let model = setup.Model ()
            (model.prices, model.discounts)
          let convertTo (prices, discounts) = create (prices, discounts)
          Arb.convert convertTo convertFrom Arb.priceAndDiscounts
        override __.Next model =
          Gen.map add (Arb.toGen (Arb.countAndItem model.prices))
          
    }


