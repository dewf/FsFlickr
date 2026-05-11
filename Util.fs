module internal FsFlickr.Util

open System

let unixTimestamp () =
    DateTimeOffset.UtcNow.ToUnixTimeSeconds()

let timestampToDateTime (ts: int64) =
    DateTimeOffset.FromUnixTimeSeconds(ts).LocalDateTime

let dateTimeToTimestamp (dt: DateTime) =
    DateTimeOffset(dt).ToUnixTimeSeconds()

let mapToQueryString (map: Map<string, string>) =
    // since browser has UrlSearchParams(), should this be provided by platform context instead?
    map
    |> Map.toList
    |> List.map (fun (k, v) ->
        let encodedKey = Platform.encodeURIComponent k
        let encodedValue = Platform.encodeURIComponent v
        sprintf "%s=%s" encodedKey encodedValue)
    |> String.concat "&"
