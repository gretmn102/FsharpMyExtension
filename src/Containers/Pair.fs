module FsharpMyExtension.Containers.Pair

let create x y = x, y

let swap (x, y) = y, x

let fold f (st:'State) (x, y) = f st x y

/// `curry`
let reduce f (x,y) = f x y

let mapFst fn (x, y) = fn x, y

let mapSnd fn (x, y) = x, fn y

let mapPair f g (x, y) = f x, g y

let mapBoth f (x, y) = f x, f y

let on f g x = f x, g x
