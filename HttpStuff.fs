module FsFlickr.HttpStuff

open Thoth.Json

type ApiResult<'a> =
    | ApiOk of 'a
    | ApiError of ApiErrorType
with
    member this.IsOk =
        match this with
        | ApiOk _ -> true
        | _ -> false
    member this.Value =
        match this with
        | ApiOk x -> x
        | _ -> failwith "ApiResult.Value failed: Not 'OK'"

let isSuccess (code: int) =
    code >= 200 && code < 300

let decodeResponse (decoder: Decoder<'a>) (statusCode: int, responseText: string): ApiResult<'a> =
    if isSuccess statusCode then
        match Decode.fromString decoder responseText with
        | Ok regResult ->
            ApiOk regResult
        | Error err ->
            DecodeError $"json decode error: {err}"
            |> ApiError
    elif statusCode = 401 then
        ApiError Unauthorized
    else
        HttpError (statusCode, responseText)
        |> ApiError
