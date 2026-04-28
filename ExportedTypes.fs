namespace FsFlickr

open System

type OAuthCallback =
    | OutOfBand
    | CallbackURL of url: string

type EndpointError =
    | HttpError of code: int * message: string
    | DecodeError of message: string
with
    override this.ToString() =
        match this with
        | HttpError (code, message) ->
            sprintf "HTTP error: (%d) %s" code message
        | DecodeError message ->
            sprintf "JSON decode error: %s" message

type FlickrError =
    | MethodError of code: int * message: string
    | EndpointError of err: EndpointError
with
    override this.ToString() =
        match this with
        | MethodError (code, message) ->
            sprintf "Flickr method error: (%d) %s" code message
        | EndpointError err ->
            sprintf "endpoint error: %A" err

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
    Square150: string
    Medium240: string option
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

type PhotosetPhoto = {
    Common: PhotoCommon
    // no extra fields yet
}

type PhotosPage<'t> = {
    Pagination: Pagination
    Photos: 't list
}

type OwnerInfo = {
    Id: NSID
    Username: string
    RealName: string
    Location: string option
    PathAlias: string option
}

type Dates = {
    Posted: DateTime
    // more later
}

type Tag = {
    Id: string
    Author: NSID
    AuthorName: string
    Raw: string          // formatting (capitals, spaces) intact
    Value: string
    // MachineTag: bool
}

type WOEID =
    WOEID of string

type Location = {
    Latitude: float
    Longitude: float
    Accuracy: int
    Context: int
    // Neighborhood: WOEID
    // Region: WOEID
    // Country: WOEID
}

type PhotoInfo = {
    Id: string
    Secret: string
    Server: int
    Farm: int
    DateUploaded: DateTime
    Owner: OwnerInfo
    Title: string
    Description: string
    Dates: Dates
    Views: int64
    NumComments: int
    Tags: Tag list
    Location: Location option
    Urls: Map<string, string> // eg "photopage": URL
}

type Media =
    | Photo
    | Video

type PhotoSize = {
    Label: string
    Width: int
    Height: int
    Source: string
    Url: string
    Media: Media
}

type UserInfo = {
    Id: NSID
    Username: string
}
