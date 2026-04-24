namespace FsFlickr

open System

type IPlatformContext =
    // stuff that differs in the browser/desktop environments
    abstract member HttpGet: url: string -> Async<int * string>
    abstract member HttpGetWithAuthHeader: url: string -> authHeader: string -> Async<int * string>
    abstract member HmacSha1: key: string -> message: string -> Async<string>
    abstract member RandomUUID: unit -> string
    abstract member EncodeURIComponent: string -> string
    abstract member DecodeURIComponent: string -> string

type ApiErrorType =
    | HttpError of code: int * message: string
    | DecodeError of message: string
    | Unauthorized
with
    static member messageMap (f: string -> string) (err: ApiErrorType) =
        match err with
        | HttpError (code, message) ->
            HttpError (code, f message)
        | DecodeError message ->
            DecodeError (f message)
        | Unauthorized ->
            Unauthorized
    override this.ToString() =
        match this with
        | HttpError(code, message) ->
            sprintf "HTTP error %d: %s" code message
        | DecodeError message ->
            sprintf "JSON decoding error: %s" message
        | Unauthorized ->
            "HTTP unauthorized"

type FlickrApiError =
    | MethodError of code: int * message: string
    | EndpointError of err: ApiErrorType // http, decoding, etc

type FlickrApiResult<'a> =
    | FlickrOk of payload: 'a
    | FlickrError of err: FlickrApiError
    | FlickrNotYetAuthorized
with
    static member map (f: 'a -> 'b) (x: FlickrApiResult<'a>) =
        match x with
        | FlickrOk payload ->
            FlickrOk (f payload)
        | FlickrError err ->
            FlickrError err
        | FlickrNotYetAuthorized ->
            FlickrNotYetAuthorized

type FlickrUserInfo = {
    Fullname: string
    UserNSID: string
    Username: string
}

type GroupInfo = {
    Id: string
    Name: string
}

type Photo = {
    Id: string
    Owner: string
    PathAlias: string option
    Title: string
    DateAdded: DateTime
    Secret: string
    Server: int
    Farm: int
    SmallUrl: string
}

type Pagination = {
    CurrentPage: int    // 1-based
    TotalPages: int
    ItemsPerPage: int
    TotalItems: int
}

type PhotosPage = {
    Pagination: Pagination
    Photos: Photo list
}
