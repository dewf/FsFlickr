module FsFlickr.Util

open System

let unixTimestamp () =
    DateTimeOffset.UtcNow.ToUnixTimeSeconds()

let timestampToDateTime (ts: string) =
    DateTimeOffset.FromUnixTimeSeconds(ts |> int64).LocalDateTime
