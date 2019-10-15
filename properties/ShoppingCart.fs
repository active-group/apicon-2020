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


type Interface =

  abstract member Add: int -> Item -> unit

  abstract member Total: unit -> Price

  abstract member TotalCount: unit -> int

module Interface =
  let add (cart: Interface) (count: int) (item: Item) = cart.Add count item
  let total (cart: Interface): Price = cart.Total ()
  let totalCount (cart: Interface): int = cart.TotalCount ()

type Discount = { receive: int; payFor: int }

module Implementation =
 // caught out by totalCountCorrect
 let create1 (prices: Map<Item, Price>) =
   let cart = ref Map.empty<Item, int>
   { new Interface with
      member __.Add count item =
        cart := Map.add item count !cart
      member __.Total () =
        let folder total item count =
          Price.add total (Price.scale count (Map.find item prices))
        Map.fold folder Price.nothing !cart 

      member __.TotalCount () =
         let folder total item count = total + count
         Map.fold folder 0 !cart
   }

 // caught out by itemCountCorrect
 let create2 (prices: Map<Item, Price>) =
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
          Price.add total (Price.scale count (Map.find item prices))
        Map.fold folder Price.nothing !cart 

      member __.TotalCount () =
         let folder total item count = total + count
         Map.fold folder 0 !cart
   }

 // correct wrt naive model
 let create3 (prices: Map<Item, Price>) =
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

 // truncates item names, caught out by onePriceCorrect
 let create4 (prices': Map<Item, Price>) =
   let trunc (Item itemName, price) = (Item (itemName.Substring (0, min 12 (String.length itemName))), price)
   let prices = prices' |> Map.toSeq |> Seq.map trunc |> Map.ofSeq
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
   
 // catches out int in itemCountCorrect: need a model!
 let create5 (prices: Map<Item, Price>) =
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
          Price.add total (Price.scale count (Map.find item prices))
        Map.fold folder Price.nothing !cart 

      member __.TotalCount () =
         let folder total item count = total + count
         Map.fold folder 0 !cart
   }


 let create6 (prices: Map<Item, Price>) (discounts: Map<Item, Discount>) =
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
          let price = Map.find item prices
          let optionalDiscount = Map.tryFind item discounts
          Price.add total (calculatePrice price count optionalDiscount)
        Map.fold folder Price.nothing !cart 

      member __.TotalCount () =
         let folder total item count = total + count
         Map.fold folder 0 !cart
   }

 let create7 (prices: Map<Item, Price>) (discounts: Map<Item, Discount>) =
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
          let price = Map.find item prices
          let optionalDiscount = Map.tryFind item discounts
          Price.add total (calculatePrice price count optionalDiscount)
        Map.fold folder Price.nothing !cart 

      member __.TotalCount () =
         let folder total item count = total + count
         Map.fold folder 0 !cart
   }

 let create8 (prices: Map<Item, Price>) (discounts: Map<Item, Discount>) =
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
          let price = Map.find item prices
          let optionalDiscount = Map.tryFind item discounts
          Price.add total (calculatePrice price count optionalDiscount)
        Map.fold folder Price.nothing !cart 

      member __.TotalCount () =
         let folder total item count = total + count
         Map.fold folder 0 !cart
   }