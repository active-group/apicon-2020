namespace ShoppingCart

type Item = Item of string

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Item =
  let itemName (Item name) = name

type Price = Price of int

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Price =
  let nothing = Price 0
  let priceAmount (Price amount) = amount
  let add (Price amount1) (Price amount2) = Price (amount1 + amount2)
  let scale factor (Price amount) = Price (factor * amount)

type Interface =

  abstract member Add: int -> Item -> unit

  abstract member Total: unit -> Price

  abstract member TotalCount: unit -> int


 module Implementation =
   let create (prices: list<Item * Price>) =
     let table = Map.ofList prices
     let cart = ref Map.empty<Item, int>
     { new Interface with
        member __.Add count item =
          cart := Map.add item count !cart
        member __.Total () =
          let folder total item count =
            Price.add total (Price.scale count (Map.find item table))
          Map.fold folder Price.nothing !cart 

        member __.TotalCount () =
           let folder total item count = total + count
           Map.fold folder 0 !cart
     }