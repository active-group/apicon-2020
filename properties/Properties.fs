namespace Properties

module Examples =
    open FsCheck

    let revRevIsOrig (xs:list<int>) = 
        List.rev(List.rev xs) = xs
        
    let revRevIsBug (xs:list<int>) = List.rev(List.rev xs) = 5::xs
