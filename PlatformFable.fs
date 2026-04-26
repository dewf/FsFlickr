module FsFlickr.Platform

open Fable.Core
open Fable.SimpleHttp
open Config

[<Import("hmacSha1", "./typescript/misc_native.ts")>]
let private hmacSha1Promise (key: string, message: string): JS.Promise<string> = jsNative

// ==============================

let httpGetWithAuthHeader (config: FlickrConfig) (url: string) (auth: string) =
    async {
        let proxiedUrl =
            match config.CorsProxy with
            | Some proxy ->
                proxy + url
            | None ->
                url
        let! resp =
            Http.request proxiedUrl
            |> Http.header (Headers.authorization auth)
            |> Http.send
        return resp.statusCode, resp.responseText
    }

let hmacSha1 (key: string) (message: string) =
    hmacSha1Promise(key, message)
    |> Async.AwaitPromise

[<Emit("crypto.randomUUID()")>]
let randomUUID (): string = jsNative

let encodeURIComponent = JS.encodeURIComponent

let decodeURIComponent = JS.decodeURIComponent
