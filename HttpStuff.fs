module FsFlickr.HttpStuff

open Thoth.Json

type internal ApiResult<'a> =
    | ApiOk of 'a
    | ApiError of EndpointError

let internal isSuccess (code: int) =
    code >= 200 && code < 300

let internal decodeResponse (decoder: Decoder<'a>) (statusCode: int, responseText: string): ApiResult<'a> =
    if isSuccess statusCode then
        match Decode.fromString decoder responseText with
        | Ok regResult ->
            ApiOk regResult
        | Error err ->
            DecodeError $"json decode error: {err}"
            |> ApiError
    else
        HttpError (statusCode, responseText)
        |> ApiError
