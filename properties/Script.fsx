#r "bin/Debug/FsCheck.dll"
#r "bin/Debug/properties.dll"

open Properties
open Properties.Examples
open FsCheck
open FsCheck.Util
open ShoppingCart.Implementation
open ShoppingCart.Test

// quick (addingItemIncreasesTotal create6)

// quick (FsCheck.Experimental.StateMachine.toProperty (cartMachine create8))