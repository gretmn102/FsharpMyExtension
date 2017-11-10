module FsharpMyExtension.DateTime

/// without init value!
/// bounds of DateTime.MinValue or System.DateTime.MaxValue
let daysStep step init =
    let cond =
        if step > 0 then
            fun x -> x < System.DateTime.MaxValue
        else
            fun x -> x > System.DateTime.MinValue
    let stepDays = float step
    Seq.unfold (fun ((st:System.DateTime)) ->
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
    List.unfold (fun ((st:System.DateTime),i) ->
            if i > 0 then
                let x = st.AddDays step
                Some(x, (x, i - 1))
            else None)
        (init, count)