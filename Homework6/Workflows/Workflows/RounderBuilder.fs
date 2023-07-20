module RounderBuilder

open System

type RounderBuilder(roundingAccuracy : Int32) =
    member _.Bind (value : double, func) = func value

    member _.Return (value : double) = Math.Round(value, roundingAccuracy)
