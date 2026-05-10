module FsFlickr.FlickrApi

open System
open FlickrOAuth1
open FsFlickr.Util
open HttpStuff
open Config

#if PLATFORM_FABLE
open Thoth.Json
#else
open Thoth.Json.Net
#endif

let private FLICKR_REST_URL = "https://www.flickr.com/services/rest"

let private flickrRestResultDecoder (payloadField: string) (payloadDecoder: Decoder<'a>): Decoder<FlickrApiResult<'a>> =
    Decode.object (fun get ->
        match get.Required.Field "stat" Decode.string with
        | "ok" ->
            get.Required.Field payloadField payloadDecoder
            |> FlickrOk
        | "fail" ->
            let code =
                get.Required.Field "code" Decode.int
            let message =
                get.Required.Field "message" Decode.string
            MethodError (code, message)
            |> FlickrError
        | _ ->
            failwith "jsonResultDecoder: unknown flickr json status")

let private flickrMethod (config: FlickrConfig) (accessToken: AccessTokenInfo) (method: string) (methodArgs: (string * string) list) (payloadField: string) (payloadDecoder: Decoder<'a>) =
    let data =
        [ "nojsoncallback", "1"
          "format", "json"
          "method", method ]
        @ methodArgs
    async {
        let! authHeader =
            generateAuthHeader config (Authorized accessToken) data "GET" FLICKR_REST_URL
        let queryString =
            data
            |> Map.ofList
            |> mapToQueryString
        let url =
            sprintf "%s?%s" FLICKR_REST_URL queryString
        let! response =
            Platform.httpGetWithAuthHeader config url authHeader
        // printfn "raw response: [%A]" response
        let result =
            let decoder =
                flickrRestResultDecoder payloadField payloadDecoder
            match decodeResponse decoder response with
            | ApiOk flickrResult ->
                // properly decoded, although it could still technically be a flickr API error
                flickrResult
            | ApiError err ->
                FlickrError (EndpointError err)
        return result
    }

// common stuff =========================================

let private decodeStringTimestamp =
    Decode.map (int64 >> timestampToDateTime) Decode.string

let extrasGetter (requested: Extra seq) (get: Decode.IGetters) =
    let decodeIntAsBool =
        Decode.map (fun x -> x <> 0) Decode.int
    let latLongDecoder =
        fun path value ->
            if Decode.Helpers.isNumber value then
                let num: float = unbox value
                Ok (float num)
            elif Decode.Helpers.isString value then
                let str: string = unbox value
                Ok (float str)
            else
                Error (path, BadPrimitive("lat/long value", value))
    let getGeo (get: Decode.IGetters) =
        { Latitude = get.Optional.Field "latitude" latLongDecoder
          Longitude = get.Optional.Field "longitude" latLongDecoder
          Accuracy = get.Optional.Field "accuracy" Decode.int
          PlaceId = get.Optional.Field "place_id" Decode.string
          WoeId = get.Optional.Field "woeid" Decode.int64
          GeoIsPublic = get.Optional.Field "geo_is_public" decodeIntAsBool
          GeoIsContact = get.Optional.Field "geo_is_contact" decodeIntAsBool
          GeoIsFriend = get.Optional.Field "get_is_friend" decodeIntAsBool
          GeoIsFamily = get.Optional.Field "geo_is_family" decodeIntAsBool }
    let decodeTags =
        let spaceSplitter (str: string) =
            str.Split(" ")
            |> Array.toList
        Decode.map spaceSplitter Decode.string
    let getMedia (get: Decode.IGetters) =
        let maybeKind = get.Optional.Field "media" Decode.string
        let maybeStatus = get.Optional.Field "media_status" Decode.string
        match maybeKind, maybeStatus with
        | Some kind, Some status ->
            Some { Kind = kind; Status = status }
        | _ ->
            None
    let contentDecoder actual =
        Decode.object (fun get -> get.Required.Field "_content" actual)
    let getOriginalDimensions (get: Decode.IGetters) =
        let maybeWidth = get.Optional.Field "o_width" (Decode.map int Decode.string)
        let maybeHeight = get.Optional.Field "o_height" (Decode.map int Decode.string)
        match maybeWidth, maybeHeight with
        | Some width, Some height ->
            Some { Width = width; Height = height }
        | _ ->
            None
    let getOriginalFormat (get: Decode.IGetters) =
        let maybeFormat = get.Optional.Field "originalformat" Decode.string
        let maybeSecret = get.Optional.Field "originalsecret" Decode.string
        match maybeFormat, maybeSecret with
        | Some format, Some secret ->
            Some { Format = format; Secret = secret }
        | _ ->
            None
    let foldFunc (acc: Extras) (extra: Extra) =
        match extra with
        | Extra.Description ->
            { acc with Description = get.Optional.Field "description" (contentDecoder Decode.string) }
        | Extra.License ->
            { acc with License = get.Optional.Field "license" Decode.int }
        | Extra.DateUploaded ->
            { acc with DateUploaded = get.Optional.Field "date_upload" decodeStringTimestamp }
        | Extra.DateTaken ->
            { acc with DateTaken = get.Optional.Field "date_taken" decodeStringTimestamp }
        | Extra.OwnerName ->
            { acc with OwnerName = get.Optional.Field "ownername" Decode.string }
        | Extra.IconServer ->
            { acc with IconServer = get.Optional.Field "icon_server" Decode.int }
        | Extra.OriginalFormat ->
            { acc with OriginalFormat = getOriginalFormat get }
        | Extra.LastUpdate ->
            { acc with LastUpdate = get.Optional.Field "last_update" decodeStringTimestamp }
        | Extra.Geo ->
            { acc with Geo = getGeo get |> Some }
        | Extra.Tags ->
            { acc with Tags = get.Optional.Field "tags" decodeTags }
        | Extra.MachineTags ->
            { acc with MachineTags = get.Optional.Field "machine_tags" decodeTags }
        | Extra.OriginalDimensions ->
            { acc with OriginalDimensions = getOriginalDimensions get }
        | Extra.Views ->
            { acc with Views = get.Optional.Field "views" Decode.int64 }
        | Extra.Media ->
            { acc with Media = getMedia get }
        | Extra.PathAlias ->
            { acc with PathAlias = get.Optional.Field "path_alias" Decode.string }
        | Extra.UrlThumb75 ->
            { acc with UrlThumb75 = get.Optional.Field "url_sq" Decode.string }
        | Extra.UrlThumb100 ->
            { acc with UrlThumb100 = get.Optional.Field "url_t" Decode.string }
        | Extra.UrlThumb150 ->
            { acc with UrlThumb150 = get.Optional.Field "url_q" Decode.string }
        | Extra.UrlSmall240 ->
            { acc with UrlSmall240 = get.Optional.Field "url_s" Decode.string }
        | Extra.UrlSmall320 ->
            { acc with UrlSmall320 = get.Optional.Field "url_n" Decode.string }
        | Extra.UrlMedium500 ->
            { acc with UrlMedium500 = get.Optional.Field "url_m" Decode.string }
        | Extra.UrlMedium640 ->
            { acc with UrlMedium640 = get.Optional.Field "url_z" Decode.string }
        | Extra.UrlMedium800 ->
            { acc with UrlMedium800 = get.Optional.Field "url_c" Decode.string }
        | Extra.UrlLarge1024 ->
            { acc with UrlLarge1024 = get.Optional.Field "url_l" Decode.string }
        | Extra.UrlOriginal ->
            { acc with UrlOriginal = get.Optional.Field "url_o" Decode.string }
        | _ ->
            failwith "extrasGetter/foldFunc: Extra out of range"
    (Extras.empty, requested)
    ||> Seq.fold foldFunc

let private decodeNSID =
    Decode.map NSID Decode.string

let private decodeContentValue (valueDecoder: Decoder<'a>): Decoder<'a> =
    Decode.object (fun get -> get.Required.Field "_content" valueDecoder)

// let private sizesDecoder: Decoder<string list> =
//     let f (str: string) =
//         str.Split(",") |> Array.toList
//     Decode.map f Decode.string

let private photoCommonDecoder (injectedOwner: NSID option) (extras: Extra seq) (get: Decode.IGetters): PhotoBaseDto =
    { Id = get.Required.Field "id" Decode.string
      Owner =
           match injectedOwner with
           | Some owner -> owner
           | None -> get.Required.Field "owner" decodeNSID
      Title = get.Required.Field "title" Decode.string
      Secret = get.Required.Field "secret" Decode.string
      Server = get.Required.Field "server" Decode.int
      Farm = get.Required.Field "farm" Decode.int
      Extras = extrasGetter extras get }

let private paginationGetter (get: Decode.IGetters) =
    { CurrentPage = get.Required.Field "page" Decode.int
      TotalPages = get.Required.Field "pages" Decode.int
      ItemsPerPage = get.Required.Field "perpage" Decode.int
      TotalItems = get.Required.Field "total" Decode.int }

// group lookup by URL =================================
let private urlsLookupGroupDecoder =
    Decode.object (fun get -> {
        Id = get.Required.Field "id" decodeNSID
        Name = get.Required.Field "groupname" (Decode.object (fun get -> get.Required.Field "_content" Decode.string))
    })

let internal urlsLookupGroup (config: FlickrConfig) (accessToken: AccessTokenInfo) (url: string) =
    let args =
        [ ("url", url) ]
    flickrMethod config accessToken "flickr.urls.lookupGroup" args "group" urlsLookupGroupDecoder

// group pool =================================
let private groupPhotoDecoder (extras: Extra seq): Decoder<GroupPoolPhoto> =
    Decode.object (fun get ->
        let common =
            photoCommonDecoder None extras get
        let dateAdded =
            get.Required.Field "dateadded" decodeStringTimestamp
        GroupPoolPhoto(common, dateAdded))

let private getGroupPhotosDecoder (extras: Extra seq) =
    Decode.object (fun get->
        { Pagination = paginationGetter get
          Photos = get.Required.Field "photo" (Decode.list (groupPhotoDecoder extras)) })

let internal getGroupPhotos (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (id: NSID) (perPage: int option) (page: int option) (userId: NSID option)
    (extras: Extra seq) =
        let args =
            [ "group_id", string id
              if page.IsSome then "page", string page.Value
              if perPage.IsSome then "per_page", string perPage.Value
              if userId.IsSome then "user_id", string userId.Value
              "extras", extras |> Extras.extrasToKeys ]
        flickrMethod config accessToken "flickr.groups.pools.getPhotos" args "photos" (getGroupPhotosDecoder extras)

// favorites ==================================
let private favesPhotoDecoder (extras: Extra seq): Decoder<FavoritesPhoto> =
    Decode.object (fun get ->
        let common =
            photoCommonDecoder None extras get
        let dateFaved =
            get.Required.Field "date_faved" decodeStringTimestamp
        let upgradeSizes =
            get.Optional.Field "upgrade_sizes" (Decode.list Decode.string)
        FavoritesPhoto(common, dateFaved, upgradeSizes))

let private favoritesPageDecoder (extras: Extra seq) =
    Decode.object (fun get->
        { Pagination = paginationGetter get
          Photos = get.Required.Field "photo" (Decode.list (favesPhotoDecoder extras)) })

let internal getFavorites
    (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (userId: NSID option) (minDate: DateTime option) (maxDate: DateTime option)
    (perPage: int option) (page: int option)
    (extras: Extra seq) =
        let args =
            [ if userId.IsSome then "user_id", string userId.Value
              if minDate.IsSome then "min_fave_date", dateTimeToTimestamp minDate.Value |> string
              if maxDate.IsSome then "max_fave_date", dateTimeToTimestamp maxDate.Value |> string
              if perPage.IsSome then "per_page", string perPage.Value
              if page.IsSome then "page", string page.Value
              "extras", extras |> Extras.extrasToKeys ]
        flickrMethod config accessToken "flickr.favorites.getList" args "photos" (favoritesPageDecoder extras)

// photo set =====================================

let private photosetPhotoDecoder (owner: NSID) (extras: Extra seq): Decoder<PhotosetPhoto> =
    Decode.object (fun get ->
        let common =
            photoCommonDecoder (Some owner) extras get
        PhotosetPhoto(common))

let private photosetPageDecoder (owner: NSID) (extras: Extra seq) =
    Decode.object (fun get->
        { Pagination = paginationGetter get
          Photos = get.Required.Field "photo" (Decode.list (photosetPhotoDecoder owner extras)) })

let internal getPhotoset
    (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (userId: NSID) (photosetId: string)
    (perPage: int option) (page: int option)
    (extras: Extra seq) =
        let args =
            [ "photoset_id", photosetId
              "user_id", string userId
              if perPage.IsSome then "per_page", string perPage.Value
              if page.IsSome then "page", string page.Value
              "extras", extras |> Extras.extrasToKeys ]
        flickrMethod config accessToken "flickr.photosets.getPhotos" args "photoset" (photosetPageDecoder userId extras)

// look up user by URL =======================================

let private urlsLookupUserDecoder =
    Decode.object (fun get -> {
        Id = get.Required.Field "id" decodeNSID
        Username = get.Required.Field "username" (decodeContentValue Decode.string)})

let internal urlsLookupUser
    (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (url: string) =
        let args =
            [ "url", url ]
        flickrMethod config accessToken "flickr.urls.lookupUser" args "user" urlsLookupUserDecoder

// photo info ================================================

let private decodeOwner: Decoder<OwnerInfo> =
    Decode.object (fun get -> {
        Id = get.Required.Field "nsid" decodeNSID
        Username = get.Required.Field "username" Decode.string
        RealName = get.Required.Field "realname" Decode.string
        Location = get.Optional.Field "location" Decode.string
        PathAlias = get.Optional.Field "path_alias" Decode.string
    })

let private datesDecoder: Decoder<Dates> =
    Decode.object (fun get -> {
        Posted = get.Required.Field "posted" decodeStringTimestamp
    })

let private decodeTag: Decoder<Tag> =
    Decode.object (fun get -> {
        Id = get.Required.Field "id" Decode.string
        Author = get.Required.Field "author" decodeNSID
        AuthorName = get.Required.Field "authorname" Decode.string
        Raw = get.Required.Field "raw" Decode.string
        Value = get.Required.Field "_content" Decode.string
        // MachineTag = get.Required.Field "machine_tag" (Decode.map (fun i -> i = 0 |> not) Decode.int) // sometimes '0', sometimes 'false' ?
    })

let private decodeTags: Decoder<Tag list> =
    Decode.object (fun get ->
        get.Required.Field "tag" (Decode.list decodeTag))

let private decodeWOEID: Decoder<WOEID> =
    Decode.map (string >> WOEID) Decode.int64

let private decodeLocation: Decoder<Location> =
    Decode.object (fun get -> {
        Latitude = get.Required.Field "latitude" (Decode.map float Decode.string)
        Longitude = get.Required.Field "longitude" (Decode.map float Decode.string)
        Accuracy = get.Required.Field "accuracy" (Decode.map int Decode.string)
        Context = get.Required.Field "context" (Decode.map int Decode.string)
        // other fields: neighborhood, locality, county, region, country
        // (kind of complex, they can contain WOEIDs but also _content - not sure which is a priority)
    })

let private decodeUrls: Decoder<Map<string, string>> =
    let urlDecoder =
        Decode.object (fun get ->
            let type_ =
                get.Required.Field "type" Decode.string
            let content =
                get.Required.Field "_content" Decode.string
            type_, content)
    Decode.object (fun get ->
        let pairs =
            get.Required.Field "url" (Decode.list urlDecoder)
        Map.ofList pairs)

let private getPhotoInfoDecoder =
    Decode.object (fun get -> {
        Id = get.Required.Field "id" Decode.string
        Secret = get.Required.Field "secret" Decode.string
        Server = get.Required.Field "server" Decode.int
        Farm = get.Required.Field "farm" Decode.int
        DateUploaded = get.Required.Field "dateuploaded" decodeStringTimestamp
        Owner = get.Required.Field "owner" decodeOwner
        Title = get.Required.Field "title" (decodeContentValue Decode.string)
        Description = get.Required.Field "description" (decodeContentValue Decode.string)
        Dates = get.Required.Field "dates" datesDecoder
        Views = get.Required.Field "views" (Decode.map int64 Decode.string)
        NumComments = get.Required.Field "comments" (decodeContentValue Decode.int)
        Tags = get.Required.Field "tags" decodeTags
        Location = get.Optional.Field "location" decodeLocation
        Urls = get.Required.Field "urls" decodeUrls
    })

let internal getPhotoInfo
    (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (photoId: string) (photoSecret: string option) =
        let args =
            [ "photo_id", photoId
              if photoSecret.IsSome then "secret", photoSecret.Value ]
        flickrMethod config accessToken "flickr.photos.getInfo" args "photo" getPhotoInfoDecoder

let private decodeMedia: Decoder<Media> =
    let f = function
        | "photo" -> Photo
        | "video" -> Video
        | _ -> failwith "decodeMedia: unknown media type"
    Decode.map f Decode.string

let private sizeDecoder: Decoder<PhotoSize> =
    Decode.object (fun get -> {
        Label = get.Required.Field "label" Decode.string
        Width = get.Required.Field "width" Decode.int
        Height = get.Required.Field "height" Decode.int
        Source = get.Required.Field "source" Decode.string
        Url = get.Required.Field "url" Decode.string
        Media = get.Required.Field "media" decodeMedia
    })

let private getSizesDecoder =
    Decode.object (fun get ->
        get.Required.Field "size" (Decode.list sizeDecoder))

let internal getSizes
    (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (photoId: string) =
        let args =
            [ "photo_id", photoId ]
        flickrMethod config accessToken "flickr.photos.getSizes" args "sizes" getSizesDecoder
