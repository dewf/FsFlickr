module FsFlickr.Util

open System

let internal unixTimestamp () =
    DateTimeOffset.UtcNow.ToUnixTimeSeconds()

let internal timestampToDateTime (ts: int64) =
    DateTimeOffset.FromUnixTimeSeconds(ts).LocalDateTime

let internal dateTimeToTimestamp (dt: DateTime) =
    DateTimeOffset(dt).ToUnixTimeSeconds()

let internal mapToQueryString (map: Map<string, string>) =
    // since browser has UrlSearchParams(), should this be provided by platform context instead?
    map
    |> Map.toList
    |> List.map (fun (k, v) ->
        let encodedKey = Platform.encodeURIComponent k
        let encodedValue = Platform.encodeURIComponent v
        sprintf "%s=%s" encodedKey encodedValue)
    |> String.concat "&"
