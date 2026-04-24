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

// let queryParamsToSuffix (queryParams: Map<string, string>) =
//     let paramsString =
//         queryParams
//         |> Map.toList
//         |> List.map (fun (name, value) -> sprintf "%s=%s" (JS.encodeURIComponent name) (JS.encodeURIComponent value))
//         |> String.concat "&"
//     $"?%s{paramsString}"

// let getFromEndpoint (endPoint: string) (authToken: string option) (queryParams: Map<string, string> option) =
//     let fullUrl =
//         match queryParams with
//         | Some params' ->
//             endPoint + queryParamsToSuffix params'
//         | None ->
//             endPoint
//
//     Http.request fullUrl
//     |> Http.method GET
//     |> Http.headers [
//         Headers.accept "application/json"
//         if authToken.IsSome then
//             Headers.authorization $"Bearer %s{authToken.Value}"
//     ]
//     |> Http.send
//
// let postToEndpoint (endPoint: string) (encodedParams: JsonValue) (authToken: string option) =
//     let json = Encode.toString 0 encodedParams
//     Http.request endPoint
//     |> Http.method POST
//     |> Http.content (BodyContent.Text json)
//     |> Http.headers [
//         Headers.contentType "application/json"
//         Headers.accept "application/json"
//         if authToken.IsSome then
//             Headers.authorization $"Bearer %s{authToken.Value}"
//         ]
//     |> Http.send
//
// let handleCodeResponse (response: HttpResponse): ApiResult<string> =
//     if isSuccess response.statusCode then
//         ApiOk response.responseText
//     elif response.statusCode = 401 then
//         ApiError Unauthorized
//     else
//         HttpError (response.statusCode, response.responseText)
//         |> ApiError

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
