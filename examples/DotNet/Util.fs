module Util

open FsFlickr

let forceApiResult (asyncValue: Async<FlickrApiResult<'t>>): 't =
    asyncValue
    |> Async.RunSynchronously
    |> (function
        | FlickrOk value -> value
        | FlickrError err -> failwithf "flickr error: %A" err
        | FlickrNotYetAuthenticated -> failwith "not yet authenticated")
