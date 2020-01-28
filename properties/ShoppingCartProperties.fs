namespace ShoppingCart

module Test =
  open FsCheck
  open FsCheck.Util

  module Arb =
    let price = Arb.convert Price Price.priceAmount Arb.positiveInt // int for starters
    let item = Arb.convert Item Item.itemName Arb.letterString
    let mapIsNonEmpty map = not (Map.isEmpty map)

    let catalog = Arb.filter mapIsNonEmpty (Arb.map item price)

    // First version: gets caught out by addingItemIncreasesTotal
    let discount' =
      let arbCount = Arb.choose 1 20
      let convertTo (receive, payFor) = { receive = receive; payFor = payFor }
      let convertFrom discount = (discount.receive, discount.payFor)
      let valid (receive, payFor) = payFor < receive
      Arb.convert convertTo convertFrom (Arb.filter valid (Arb.pair arbCount arbCount))

    let discount =
      let convertTo receive = { receive = receive; payFor = receive-1 }
      let convertFrom discount = discount.receive
      Arb.convert convertTo convertFrom (Arb.choose 2 20)

    let discounts catalog =
      let items = Seq.map fst (Map.toSeq catalog)
      Arb.map (Arb.pickOneOf items) discount

    let catalogAndDIscounts =
      let gen = gen {
           let! catalog = Arb.toGen catalog
           let! discounts = Arb.toGen (discounts catalog)
           return (catalog, discounts)
          }
      in Arb.fromGen gen

    let countAndItem catalog =
      let items = Seq.map fst (Map.toSeq catalog)
      Arb.pair (Arb.choose 1 50) (Arb.pickOneOf items) // int before 

    let countAndItems catalog = Arb.list (countAndItem catalog)

  type Arbs =
    static member Price () = Arb.price
    static member Item () = Arb.item
   
  let register () = Arb.register<Arbs>()

  let (.=.) left right = left = right |@ sprintf "%A = %A" left right
  let (.>=.) left right = left >= right |@ sprintf "%A >= %A" left right
  let (.>.) left right = left > right |@ sprintf "%A > %A" left right

  let assertEquals (a: 'T when 'T: equality) (b: 'T) =
    if a <> b
    then failwith "test failed"
    else ()

  let test (factory: Factory) =
    let cart1 = factory.make Map.empty
    assertEquals (Interface.total cart1) (Price 0)
    let cart2 = factory.make (Map.ofList [Item "Milk", Price 100; Item "Egg", Price 20])
    assertEquals (Interface.total cart2) (Price 0)
  
  let total0Correct (factory: Factory) =
    Prop.forAll Arb.catalog (fun catalog ->
      let cart = factory.make catalog
      printfn "%A" catalog
      Interface.total cart .=. Price 0)
   
  let totalCountCorrect create =
    Prop.forAll Arb.catalog (fun prices ->
      let items = List.map fst (Map.toList prices)
      Prop.forAll (Arb.list (Arb.pickOneOf items)) (fun items ->
        let cart = create prices
        List.iter (Interface.add cart 1) items
        cart.TotalCount () .=. List.length items))

  let totalCorrect (factory: Factory) =
    Prop.forAll Arb.catalog (fun catalog ->
      let items = List.map fst (Map.toList catalog)
      Prop.forAll (Arb.list (Arb.pickOneOf items)) (fun items ->
        let cart = factory.make catalog
        List.iter (Interface.add cart 1) items
        let prices = List.map (Catalog.itemPrice catalog) items
        let total = List.fold Price.add (Price 0) prices
        Interface.total cart .=. total))

  let totalNCorrect (factory: Factory) =
    Prop.forAll Arb.catalog (fun catalog ->
      let items = List.map fst (Map.toList catalog)
      Prop.forAll (Arb.list (Arb.pair Arb.positiveInt (Arb.pickOneOf items))) (fun countsItems ->
          let cart = factory.make catalog
          List.iter (fun (count, item) -> Interface.add cart count item) countsItems
          let countItemPrice (count, item) =
                Price.scale count (Catalog.itemPrice catalog item)
          let prices = List.map countItemPrice countsItems
          let total = List.fold Price.add (Price 0) prices
          Interface.total cart .=. total))

  // doesn't know about counts going below zero
  let itemCountCorrect (factory: Factory) =
    Prop.forAll Arb.catalog (fun catalog ->
      let item = fst (Seq.head (Map.toSeq catalog))
      Prop.forAll (Arb.list Arb.int) (fun counts ->
        let cart = factory.make catalog
        List.iter (fun count -> Interface.add cart count item) counts
        Interface.totalCount cart .=. List.sum counts))

  let onePriceCorrect (factory: Factory) =
    Prop.forAll Arb.catalog (fun catalog ->
      let itemPrice = Seq.head (Map.toSeq catalog)
      let cart = factory.make catalog
      Interface.add cart 1 (fst itemPrice)
      Interface.total cart .=. snd itemPrice)

  let addingItemIncreasesTotal (factory: Factory) =
    Prop.forAll Arb.catalog (fun catalog ->
      Prop.forAll (Arb.discounts catalog) (fun discounts ->
        Prop.forAll (Arb.countAndItems catalog) (fun countAndItems ->
          let cart = factory.make(catalog, discounts)
          List.iter (fun (count, item) -> Interface.add cart count item) countAndItems
          let itemPrice = Seq.head (Map.toSeq catalog)
          let totalBefore = Interface.total cart
          Interface.add cart 1 (fst itemPrice)  // start with 1
          let totalAfter = Interface.total cart
          totalAfter .>=. totalBefore))) // 2 for 1 might keep price the same

  // model-based testing
  open FsCheck.Experimental

  type Model = {
    catalog: Catalog
    discounts: Discounts
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

  let total { catalog = catalog; discounts = discounts; cart = cart } =
    let folder total item count =
      let price = Map.find item catalog
      let optionalDiscount = Map.tryFind item discounts
      Price.add total (calculatePrice price count optionalDiscount)
    Map.fold folder Price.nothing cart 

  let totalCount model =
    Seq.sum (Seq.map snd (Map.toSeq model.cart))

  let cartMachine (factory: Factory) =
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
    let create (catalog, discounts) =
      { new Setup<Interface, Model>() with
          override __.Actual () = factory.make(catalog, discounts)
          override __.Model () = { catalog = catalog; discounts = discounts; cart = Map.empty }
      }
    { new Machine<Interface, Model> () with
        override __.Setup =
          let convertFrom (setup: Setup<Interface, Model>) = 
            let model = setup.Model ()
            (model.catalog, model.discounts)
          let convertTo (catalog, discounts) = create(catalog, discounts)
          Arb.convert convertTo convertFrom Arb.catalogAndDIscounts
        override __.Next model =
          Gen.map add (Arb.toGen (Arb.countAndItem model.catalog))

    }


