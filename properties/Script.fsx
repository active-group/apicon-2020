#r "bin/Debug/FsCheck.dll"
#r "bin/Debug/properties.dll"

open Properties
open Properties.Examples
open Properties.ISet
open Properties.ISet.Test
open FsCheck
open FsCheck.Util
open FsCheck.Experimental
open ShoppingCart.Implementation
open ShoppingCart.Test


// quick (addingItemIncreasesTotal create6)

// quick (StateMachine.toProperty (cartMachine create8))