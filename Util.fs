module FsFlickr.Util

open System

let internal unixTimestamp () =
    DateTimeOffset.UtcNow.ToUnixTimeSeconds()

let internal timestampToDateTime (ts: int64) =
    DateTimeOffset.FromUnixTimeSeconds(ts |> int64).LocalDateTime

let internal dateTimeToTimestamp (dt: DateTime) =
    DateTimeOffset(dt).ToUnixTimeSeconds()

let internal mapToQueryString (platform: IPlatformContext) (map: Map<string, string>) =
    // since browser has UrlSearchParams(), should this be provided by platform context instead?
    map
    |> Map.toList
    |> List.map (fun (k, v) ->
        let encodedKey = platform.EncodeURIComponent k
        let encodedValue = platform.EncodeURIComponent v
        sprintf "%s=%s" encodedKey encodedValue)
    |> String.concat "&"
