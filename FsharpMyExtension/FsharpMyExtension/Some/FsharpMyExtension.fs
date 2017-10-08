module FsharpMyExtension.MainModule
let recTailTest x = 
    let rec f acc x = 
        if x = 0I then acc
        else
            f (acc + x) (x - 1I)
    f 0I x
let x = 10