namespace FsharpMyExtension
module MainModule =
    // TODO: написать хвостовую рекурсивную функцию, которая без указания `Tailcalls` в файле проекта не сворачивается в таковую.
    let recTailTest x = 
        let rec f acc x = 
            if x = 0I then acc
            else
                f (acc + x) (x - 1I)
        f 0I x
    let x = 10