module FsFlickr.Platform

open System
open System.Security.Cryptography
open System.Text
open FSharp.Data
open Config

let httpGetWithAuthHeader (_: FlickrConfig) (url: string) (authHeader: string) =
    let headers =
        [ "User-Agent", "MyFavesSlideshow/0.1 woot"
          "Authorization", authHeader ]
    async {
        let resp = Http.Request(url, headers=headers)
        match resp.Body with
        | Text text ->
            return (resp.StatusCode, text)
        | Binary _ ->
            return failwith "nope"
    }

let hmacSha1 (key: string) (message: string) =
    async {
        use hmac = new HMACSHA1(Encoding.UTF8.GetBytes(key))
        let hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message))
        return Convert.ToBase64String(hash)
    }

let randomUUID () =
    Guid.NewGuid().ToString("N")

let encodeURIComponent (text: string) =
    Uri.EscapeDataString(text)

let decodeURIComponent (encoded: string) =
    Uri.UnescapeDataString(encoded)
