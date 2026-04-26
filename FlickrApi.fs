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

let private flickrRestResultDecoder (subgetter: Decode.IGetters -> 'a): Decoder<FlickrApiResult<'a>> =
    Decode.object (fun get ->
        match get.Required.Field "stat" Decode.string with
        | "ok" ->
            FlickrOk (subgetter get)
        | "fail" ->
            let code =
                get.Required.Field "code" Decode.int
            let message =
                get.Required.Field "message" Decode.string
            MethodError (code, message)
            |> FlickrError
        | _ ->
            failwith "jsonResultDecoder: unknown flickr json status")

let private flickrMethod (config: FlickrConfig) (accessToken: AccessTokenInfo) (method: string) (methodArgs: (string * string) list) (subgetter: Decode.IGetters -> 'a) =
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
                flickrRestResultDecoder subgetter
            match decodeResponse decoder response with
            | ApiOk flickrResult ->
                // properly decoded, although it could still technically be a flickr API error
                flickrResult
            | ApiError err ->
                FlickrError (EndpointError err)
        return result
    }

// common stuff =========================================

let private decodeNSID =
    Decode.map NSID Decode.string

let private decodeContentValue (valueDecoder: Decoder<'a>): Decoder<'a> =
    Decode.object (fun get -> get.Required.Field "_content" valueDecoder)

// let private sizesDecoder: Decoder<string list> =
//     let f (str: string) =
//         str.Split(",") |> Array.toList
//     Decode.map f Decode.string

let private photoCommonDecoder (injectedOwner: NSID option) (get: Decode.IGetters) =
        { Id = get.Required.Field "id" Decode.string
          Owner =
              match injectedOwner with
              | Some owner -> owner
              | None -> get.Required.Field "owner" decodeNSID
          PathAlias = get.Optional.Field "pathalias" Decode.string
          Title = get.Required.Field "title" Decode.string
          Secret = get.Required.Field "secret" Decode.string
          Server = get.Required.Field "server" Decode.int
          Farm = get.Required.Field "farm" Decode.int
          Square150 = get.Required.Field "url_q" Decode.string
          Medium240 = get.Optional.Field "url_m" Decode.string }
        // see: https://www.flickr.com/services/api/misc.urls.html

let private decodeStringTimestamp =
    Decode.map (int64 >> timestampToDateTime) Decode.string

let private paginationGetter (get: Decode.IGetters) =
    { CurrentPage = get.Required.Field "page" Decode.int
      TotalPages = get.Required.Field "pages" Decode.int
      ItemsPerPage = get.Required.Field "perpage" Decode.int
      TotalItems = get.Required.Field "total" Decode.int }

// group lookup by URL =================================
let private urlsLookupGroupGetter (get: Decode.IGetters) =
    get.Required.Field "group" (Decode.object (fun get -> {
        Id = get.Required.Field "id" decodeNSID
        Name = get.Required.Field "groupname" (Decode.object (fun get -> get.Required.Field "_content" Decode.string))
    }))

let internal urlsLookupGroup (config: FlickrConfig) (accessToken: AccessTokenInfo) (name: string) =
    let url =
        $"https://www.flickr.com/groups/{name}"
    let args =
        [ ("url", url) ]
    flickrMethod config accessToken "flickr.urls.lookupGroup" args urlsLookupGroupGetter

// group pool =================================
let private groupPhotoDecoder: Decoder<GroupPoolPhoto> =
    Decode.object (fun get ->
        { Common = photoCommonDecoder None get
          DateAdded = get.Required.Field "dateadded" decodeStringTimestamp })

let private groupPhotosPageGetter (get: Decode.IGetters) =
    get.Required.Field "photos" (Decode.object (fun get->
        { Pagination = paginationGetter get
          Photos = get.Required.Field "photo" (Decode.list groupPhotoDecoder) }
        ))

let internal getGroupPhotos (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (id: NSID) (perPage: int option) (page: int option) =
        let args =
            [ "group_id", string id
              if page.IsSome then "page", string page.Value
              if perPage.IsSome then "per_page", string perPage.Value
              "extras", "o_dims, url_q, url_m, path_alias" ]
        flickrMethod config accessToken "flickr.groups.pools.getPhotos" args groupPhotosPageGetter

// favorites ==================================
let private favesPhotoDecoder: Decoder<FavoritesPhoto> =
    Decode.object (fun get ->
        { Common = photoCommonDecoder None get
          DateFaved = get.Required.Field "date_faved" decodeStringTimestamp
          UpgradeSizes = get.Optional.Field "upgrade_sizes" (Decode.list Decode.string) })

let private favoritesPageGetter (get: Decode.IGetters) =
    get.Required.Field "photos" (Decode.object (fun get->
        { Pagination = paginationGetter get
          Photos = get.Required.Field "photo" (Decode.list favesPhotoDecoder) }
        ))

let internal getFavorites
    (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (userId: NSID option) (minDate: DateTime option) (maxDate: DateTime option)
    (perPage: int option) (page: int option) =
        let args =
            [ if userId.IsSome then "user_id", string userId.Value
              if minDate.IsSome then "min_fave_date", dateTimeToTimestamp minDate.Value |> string
              if maxDate.IsSome then "max_fave_date", dateTimeToTimestamp maxDate.Value |> string
              if perPage.IsSome then "per_page", string perPage.Value
              if page.IsSome then "page", string page.Value
              "extras", "o_dims, url_q, url_m, path_alias" ]
        flickrMethod config accessToken "flickr.favorites.getList" args favoritesPageGetter

// photo set =====================================

let private photosetPhotoDecoder (owner: NSID): Decoder<PhotosetPhoto> =
    Decode.object (fun get ->
        { Common = photoCommonDecoder (Some owner) get })

let private photosetPageGetter (owner: NSID) (get: Decode.IGetters) =
    get.Required.Field "photoset" (Decode.object (fun get->
        { Pagination = paginationGetter get
          Photos = get.Required.Field "photo" (Decode.list (photosetPhotoDecoder owner)) }
        ))

let internal getPhotoset
    (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (userId: NSID) (photosetId: string)
    (perPage: int option) (page: int option) =
        let args =
            [ "photoset_id", photosetId
              "user_id", string userId
              if perPage.IsSome then "per_page", string perPage.Value
              if page.IsSome then "page", string page.Value
              "extras", "o_dims, url_q, url_m, path_alias" ]
        flickrMethod config accessToken "flickr.photosets.getPhotos" args (photosetPageGetter userId)

// look up user by URL =======================================

let private urlsLookupUserGetter (get: Decode.IGetters) =
    get.Required.Field "user" (Decode.object (fun get ->
        get.Required.Field "id" decodeNSID
        ))

let internal urlsLookupUser
    (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (url: string) =
        let args =
            [ "url", url ]
        flickrMethod config accessToken "flickr.urls.lookupUser" args urlsLookupUserGetter

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

let private getPhotoInfoGetter (get: Decode.IGetters) =
    get.Required.Field "photo" (Decode.object (fun get -> {
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
    }))

let internal getPhotoInfo
    (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (photoId: string) (photoSecret: string option) =
        let args =
            [ "photo_id", photoId
              if photoSecret.IsSome then "secret", photoSecret.Value ]
        flickrMethod config accessToken "flickr.photos.getInfo" args getPhotoInfoGetter

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

let private getSizesGetter (get: Decode.IGetters) =
    get.Required.Field "sizes" (Decode.object (fun get ->
        get.Required.Field "size" (Decode.list sizeDecoder)))

let internal getSizes
    (config: FlickrConfig) (accessToken: AccessTokenInfo)
    (photoId: string) =
        let args =
            [ "photo_id", photoId ]
        flickrMethod config accessToken "flickr.photos.getSizes" args getSizesGetter
