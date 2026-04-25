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

type NSID =
    NSID of nsid: string
with
    override this.ToString() =
        match this with
        | NSID nsid -> nsid

type FlickrUserInfo = {
    Fullname: string
    UserNSID: NSID
    Username: string
}

type GroupInfo = {
    Id: NSID
    Name: string
}

type InContext =
    | NoContext
    | InPool of name: string
    | InFavorites of nsid: NSID

type PhotoCommon = {
    Id: string
    Owner: NSID
    PathAlias: string option
    Title: string
    Secret: string
    Server: int
    Farm: int
    SmallUrl: string
} with
    member this.FlickrUrl (context: InContext) =
        let pathSegment =
            this.PathAlias
            |> Option.defaultValue (string this.Owner)
        let inTail =
            match context with
            | NoContext -> ""
            | InPool name -> $"in/pool-{name}/"
            | InFavorites name -> $"in/faves-{name}/"
        $"https://www.flickr.com/photos/{pathSegment}/{this.Id}/{inTail}"

type Pagination = {
    CurrentPage: int    // 1-based
    TotalPages: int
    ItemsPerPage: int
    TotalItems: int
}

type GroupPoolPhoto = {
    Common: PhotoCommon
    DateAdded: DateTime
}

type FavoritesPhoto = {
    Common: PhotoCommon
    DateFaved: DateTime
    UpgradeSizes: string list option
}

type PhotosPage<'t> = {
    Pagination: Pagination
    Photos: 't list
}
