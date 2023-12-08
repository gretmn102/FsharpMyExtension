module Helpers
open Fuchu

module Expect =
    let equal act exp msg =
        Assert.Equal(msg, exp, act)
