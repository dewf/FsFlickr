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

type Extra =
    | Description = 0
    | License = 1
    | DateUploaded = 2    // date_upload
    | DateTaken = 3
    | OwnerName = 4
    | IconServer = 5
    | OriginalFormat = 6
    | LastUpdate = 7
    | Geo = 8
    | Tags = 9
    | MachineTags = 10
    | OriginalDimensions = 11 // o_dims
    | Views = 12
    | Media = 13
    | PathAlias = 14
    // see https://www.flickr.com/services/api/misc.urls.html
    // and https://www.flickr.com/services/api/flickr.favorites.getList.html
    // confusing, because they don't line up exactly
    | UrlThumb75 = 15         // url_sq
    | UrlThumb100 = 16        // url_t
    | UrlThumb150 = 18        // url_q
    | UrlSmall240 = 17        // url_s
    | UrlSmall320 = 20        // url_n
    // small 400 missing?
    | UrlMedium500 = 19       // url_m
    | UrlMedium640 = 21       // url_z
    | UrlMedium800 = 22       // url_c
    | UrlLarge1024 = 23       // url_l (maybe)
    | UrlOriginal = 24        // url_o

type GeoValue = {
    Latitude: float option
    Longitude: float option
    Accuracy: int option
    PlaceId: string option   // not sure
    WoeId: int64 option      // numeric, not sure width
    GeoIsPublic: bool option
    GeoIsContact: bool option
    GeoIsFriend: bool option
    GeoIsFamily: bool option
}

type MediaValue = {
    Kind: string    // photo/video
    Status: string  // ready/pending
}

type Extras = {
    Description: string option
    License: int option             // see flickr.photos.licenses.getInfo for official mapping
    DateUploaded: DateTime option
    DateTaken: DateTime option
    OwnerName: string option        // or NSID?
    IconServer: int option
    OriginalFormat: int option      // dunno!
    LastUpdate: DateTime option
    Geo: GeoValue option
    Tags: string list option
    MachineTags: string list option
    OriginalDimensions: int option  // no idea
    Views: int64 option
    Media: MediaValue option
    PathAlias: string option
    UrlThumb75: string option
    UrlThumb100: string option      // url_t
    UrlThumb150: string option      // url_q
    UrlSmall240: string option      // url_s
    UrlSmall320: string option      // url_n
    // small 400 missing?
    UrlMedium500: string option     // url_m
    UrlMedium640: string option     // url_z
    UrlMedium800: string option     // url_c
    UrlLarge1024: string option     // url_l
    UrlOriginal: string option      // url_o
}

module internal Extras =
    let empty = {
        Description = None
        License = None
        DateUploaded = None
        DateTaken = None
        OwnerName = None
        IconServer = None
        OriginalFormat = None
        LastUpdate = None
        Geo = None
        Tags = None
        MachineTags = None
        OriginalDimensions = None
        Views = None
        Media = None
        PathAlias = None
        UrlThumb75 = None
        UrlThumb100 = None
        UrlThumb150 = None
        UrlSmall240 = None
        UrlSmall320 = None
        UrlMedium500 = None
        UrlMedium640 = None
        UrlMedium800 = None
        UrlLarge1024 = None
        UrlOriginal = None
    }
    let private extraToKey (extra: Extra) =
        match extra with
        | Extra.Description -> "description"
        | Extra.License -> "license"
        | Extra.DateUploaded -> "date_upload"
        | Extra.DateTaken -> "date_taken"
        | Extra.OwnerName -> "owner_name"
        | Extra.IconServer -> "icon_server"
        | Extra.OriginalFormat -> "original_format"
        | Extra.LastUpdate -> "last_update"
        | Extra.Geo -> "geo"
        | Extra.Tags -> "tags"
        | Extra.MachineTags -> "machine_tags"
        | Extra.OriginalDimensions -> "o_dims"
        | Extra.Views -> "views"
        | Extra.Media -> "media"
        | Extra.PathAlias -> "path_alias"
        | Extra.UrlThumb75 -> "url_sq"
        | Extra.UrlThumb100 -> "url_t"
        | Extra.UrlThumb150 -> "url_q"
        | Extra.UrlSmall240 -> "url_s"
        | Extra.UrlSmall320 -> "url_n"
        | Extra.UrlMedium500 -> "url_m"
        | Extra.UrlMedium640 -> "url_z"
        | Extra.UrlMedium800 -> "url_c"
        | Extra.UrlLarge1024 -> "url_l"
        | Extra.UrlOriginal -> "url_o"
        | _ -> failwith "Extras.extraToKey - out of range"
    let extrasToKeys (extras: Extra seq) =
        let keys =
            extras
            |> Seq.map extraToKey
        String.Join(",", keys)

type PhotoCommon = {
    Id: string
    Owner: NSID
    Title: string
    Secret: string
    Server: int
    Farm: int
    Extras: Extras
} with
    member this.FlickrUrl (context: InContext) =
        let pathSegment =
            match this.Extras.PathAlias with
            | Some pathAlias ->
                pathAlias
            | _ ->
                string this.Owner
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
