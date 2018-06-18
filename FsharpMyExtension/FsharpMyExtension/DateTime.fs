module FsharpMyExtension.DateTime
open System
module Unix = 
    let unixEpoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let ofSec = unixEpoch.AddSeconds
    let ofMSec = unixEpoch.AddMilliseconds
    let toSec (date:DateTime) =
        System.Convert.ToInt64((date.ToUniversalTime() - unixEpoch).TotalSeconds)
    let toMSec (date:DateTime) =
        System.Convert.ToInt64((date.ToUniversalTime() - unixEpoch).TotalMilliseconds)
/// without init value!
/// bounds of System.DateTime.MinValue or System.DateTime.MaxValue
let daysStep step init =
    let cond =
        if step > 0 then
            fun x -> x < DateTime.MaxValue
        else
            fun x -> x > DateTime.MinValue
    let stepDays = float step
    Seq.unfold (fun ((st:DateTime)) ->
            if cond st then
                let x = st.AddDays stepDays
                Some(x, x)
            else None)
        init

///**Description**
///  without init value!
///**Exceptions**
/// System.ArgumentOutOfRangeException: The resulting System.DateTime is less than System.DateTime.MinValue or greater than System.DateTime.MaxValue.
let daysStepToBound step count init =
    let step = float step
    List.unfold (fun ((st:DateTime),i) ->
            if i > 0 then
                let x = st.AddDays step
                Some(x, (x, i - 1))
            else None)
        (init, count)