namespace ShoppingCart

type Item = Item of string

module Item =
  let itemName (Item name) = name

type Price = Price of int

module Price =
  let nothing = Price 0
  let priceAmount (Price amount) = amount
  let add (Price amount1) (Price amount2) = Price (amount1 + amount2)
  let scale factor (Price amount) = Price (factor * amount)

type Catalog = Map<Item, Price>

module Catalog =
  let itemPrice (catalog: Catalog) (item: Item): Price =
    Map.find item catalog

type Interface =

  abstract member Add: int -> Item -> unit

  abstract member Total: unit -> Price

  abstract member TotalCount: unit -> int

type Discount = { receive: int; payFor: int }

type Discounts = Map<Item, Discount>

module Discounts =
    let empty = Map.empty

type Factory =
    abstract member make: Catalog -> Interface

    abstract member make: Catalog * Discounts -> Interface

module Interface =
  let add (cart: Interface) (count: int) (item: Item) = cart.Add count item
  let total (cart: Interface): Price = cart.Total ()
  let totalCount (cart: Interface): int = cart.TotalCount ()


module Implementation =
 // caught out by totalCountCorrect
 let factory1 =
    { new Factory with
       member __.make(catalog) =
           let cart = ref Map.empty<Item, int>
           { new Interface with
              member __.Add count item =
                cart := Map.add item count !cart
              member __.Total () =
                let folder total item count =
                  Price.add total (Price.scale count (Map.find item catalog))
                Map.fold folder Price.nothing !cart 

              member __.TotalCount () =
                 let folder total item count = total + count
                 Map.fold folder 0 !cart
           }
       member __.make(catalog, discounts) = failwith "unimplemented"
    }

 // ignores count, caught out by itemCountCorrect
 let factory2 =
    { new Factory with
       member __.make(catalog) =
         let cart = ref Map.empty<Item, int>
         let itemCount item =
             match Map.tryFind item !cart with
             | None -> 0
             | Some count -> count
         { new Interface with
            member __.Add count item =
              cart := Map.add item (itemCount item + 1) !cart

            member __.Total () =
              let folder total item count =
                Price.add total (Price.scale count (Map.find item catalog))
              Map.fold folder Price.nothing !cart 

            member __.TotalCount () =
               let folder total item count = total + count
               Map.fold folder 0 !cart
         }
       member __.make(catalog, discounts) = failwith "unimplemented"
    }

 // correct wrt naive model
 let factory3 =
    { new Factory with
       member __.make(catalog) =
         let cart = ref Map.empty<Item, int>
         let itemCount item =
             match Map.tryFind item !cart with
             | None -> 0
             | Some count -> count
         { new Interface with
            member __.Add count item =
              cart := Map.add item (itemCount item + count) !cart

            member __.Total () =
              let folder total item count =
                Price.add total (Price.scale count (Map.find item catalog))
              Map.fold folder Price.nothing !cart 

            member __.TotalCount () =
               let folder total item count = total + count
               Map.fold folder 0 !cart
         }
       member __.make(catalog, discounts) = failwith "unimplemented"
    }

 // truncates item names, caught out by onePriceCorrect
 let factory4 =
    { new Factory with
       member __.make(catalog) =
         let trunc (Item itemName, price) = (Item (itemName.Substring (0, min 12 (String.length itemName))), price)
         let prices = catalog |> Map.toSeq |> Seq.map trunc |> Map.ofSeq
         let cart = ref Map.empty<Item, int>
         let itemCount item =
             match Map.tryFind item !cart with
             | None -> 0
             | Some count -> count
         { new Interface with
            member __.Add count item =
              cart := Map.add item (itemCount item + count) !cart

            member __.Total () =
              let folder total item count =
                Price.add total (Price.scale count (Map.find item prices))
              Map.fold folder Price.nothing !cart 

            member __.TotalCount () =
               let folder total item count = total + count
               Map.fold folder 0 !cart
         }
       member __.make(catalog, discounts) = failwith "unimplemented"
    }

 // no negative counts; catches out int in itemCountCorrect: need a model!
 let factory5 =
    { new Factory with
       member __.make(catalog) =
         let cart = ref Map.empty<Item, int>
         let itemCount item =
             match Map.tryFind item !cart with
             | None -> 0
             | Some count -> count
         { new Interface with
            member __.Add count item =
              cart := Map.add item (max 0 (itemCount item + count)) !cart

            member __.Total () =
              let folder total item count =
                Price.add total (Price.scale count (Map.find item catalog))
              Map.fold folder Price.nothing !cart 

            member __.TotalCount () =
               let folder total item count = total + count
               Map.fold folder 0 !cart
         }
       member __.make(catalog, discounts) = failwith "unimplemented"
    }

 // ignores count when there's a discount
 let factory6 =
    { new Factory with
       member factory.make(catalog) =
         factory.make(catalog, Discounts.empty)
       member __.make(catalog, discounts) =
         let cart = ref Map.empty<Item, int>
         let itemCount item =
             match Map.tryFind item !cart with
             | None -> 0
             | Some count -> count
         let calculatePrice (price: Price) (count: int) (discount: Option<Discount>): Price =
           match discount with
           | None -> Price.scale count price
           | Some { receive = receive; payFor = payFor } ->
               if count >= receive
               then Price.scale payFor price
               else Price.scale count price

         { new Interface with
            member __.Add count item =
              cart := Map.add item (itemCount item + count) !cart

            member __.Total () =
              let folder total item count =
                let price = Map.find item catalog
                let optionalDiscount = Map.tryFind item discounts
                Price.add total (calculatePrice price count optionalDiscount)
              Map.fold folder Price.nothing !cart 

            member __.TotalCount () =
               let folder total item count = total + count
               Map.fold folder 0 !cart
         }
    }

 // leaves out remainder when calculating discount
 let factory7  =
   { new Factory with
       member factory.make(catalog) =
         factory.make(catalog, Discounts.empty)
       member __.make(catalog, discounts) =
         let cart = ref Map.empty<Item, int>
         let itemCount item =
             match Map.tryFind item !cart with
             | None -> 0
             | Some count -> count
         let calculatePrice (price: Price) (count: int) (discount: Option<Discount>): Price =
           match discount with
           | None -> Price.scale count price
           | Some { receive = receive; payFor = payFor } ->
               if count >= receive
               then 
                  // BUG: remainder
                  Price.scale ((count / receive) * payFor) price
               else Price.scale count price
         { new Interface with
            member __.Add count item =
              cart := Map.add item (itemCount item + count) !cart

            member __.Total () =
              let folder total item count =
                let price = Map.find item catalog
                let optionalDiscount = Map.tryFind item discounts
                Price.add total (calculatePrice price count optionalDiscount)
              Map.fold folder Price.nothing !cart 

            member __.TotalCount () =
               let folder total item count = total + count
               Map.fold folder 0 !cart
         }
   }

 // OK
 let factory8 =
    { new Factory with
       member factory.make(catalog) =
         factory.make(catalog, Discounts.empty)
       member __.make(catalog, discounts) =
         let cart = ref Map.empty<Item, int>
         let itemCount item =
             match Map.tryFind item !cart with
             | None -> 0
             | Some count -> count
         let calculatePrice (price: Price) (count: int) (discount: Option<Discount>): Price =
           match discount with
           | None -> Price.scale count price
           | Some { receive = receive; payFor = payFor } ->
               if count >= receive
               then 
                  Price.scale (((count / receive) * payFor) + count % receive) price
               else Price.scale count price
         { new Interface with
            member __.Add count item =
              cart := Map.add item (itemCount item + count) !cart

            member __.Total () =
              let folder total item count =
                let price = Map.find item catalog
                let optionalDiscount = Map.tryFind item discounts
                Price.add total (calculatePrice price count optionalDiscount)
              Map.fold folder Price.nothing !cart 

            member __.TotalCount () =
               let folder total item count = total + count
               Map.fold folder 0 !cart
         }
    }