namespace FsFlickr

open System

type IPlatformContext =
    // stuff that differs in the browser/desktop environments
    abstract member HttpGetWithAuthHeader: url: string -> authHeader: string -> Async<int * string>
    abstract member HmacSha1: key: string -> message: string -> Async<string>
    abstract member RandomUUID: unit -> string
    abstract member EncodeURIComponent: string -> string
    abstract member DecodeURIComponent: string -> string

type OAuthCallback =
    | OutOfBand
    | CallbackURL of url: string

type EndpointError =
    | HttpError of code: int * message: string
    | DecodeError of message: string

type FlickrError =
    | MethodError of code: int * message: string
    | EndpointError of err: EndpointError

type FlickrApiResult<'a> =
    | FlickrOk of payload: 'a
    | FlickrError of err: FlickrError
    | FlickrNotYetAuthenticated
with
    static member map (f: 'a -> 'b) (x: FlickrApiResult<'a>) =
        match x with
        | FlickrOk payload ->
            FlickrOk (f payload)
        | FlickrError err ->
            FlickrError err
        | FlickrNotYetAuthenticated ->
            FlickrNotYetAuthenticated

type FlickrUserInfo = {
    Fullname: string
    UserNSID: string
    Username: string
}

type GroupInfo = {
    Id: string
    Name: string
}

type InContext =
    | NoContext
    | InPool of name: string

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
} with
    member this.FlickrUrl (context: InContext) =
        let pathSegment =
            this.PathAlias
            |> Option.defaultValue this.Owner
        let inTail =
            match context with
            | NoContext -> ""
            | InPool name -> $"in/pool-{name}/"
        $"https://www.flickr.com/photos/{pathSegment}/{this.Id}/{inTail}"

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
