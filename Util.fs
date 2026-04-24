module FsFlickr.Util

open System

let unixTimestamp () =
    DateTimeOffset.UtcNow.ToUnixTimeSeconds()

let timestampToDateTime (ts: string) =
    DateTimeOffset.FromUnixTimeSeconds(ts |> int64).LocalDateTime

let mapToQueryString (platform: IPlatformContext) (map: Map<string, string>) =
    // since browser has UrlSearchParams(), should this be provided by platform context instead?
    map
    |> Map.toList
    |> List.map (fun (k, v) ->
        let encodedKey = platform.EncodeURIComponent k
        let encodedValue = platform.EncodeURIComponent v
        sprintf "%s=%s" encodedKey encodedValue)
    |> String.concat "&"
