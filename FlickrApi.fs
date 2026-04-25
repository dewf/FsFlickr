module FsFlickr.FlickrApi

open System
open FlickrOAuth1
open FsFlickr.Util
open HttpStuff
open Thoth.Json

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

let private flickrMethod (platform: IPlatformContext) (apiKey: string) (apiSecret: string) (accessToken: AccessTokenInfo) (method: string) (methodArgs: (string * string) list) (subgetter: Decode.IGetters -> 'a) =
    let data =
        [ "nojsoncallback", "1"
          "format", "json"
          "method", method ]
        @ methodArgs
    async {
        let! authHeader =
            generateAuthHeader platform apiKey apiSecret (Authorized accessToken) data "GET" FLICKR_REST_URL
        let queryString =
            data
            |> Map.ofList
            |> mapToQueryString platform
        let url =
            sprintf "%s?%s" FLICKR_REST_URL queryString
        let! response =
            platform.HttpGetWithAuthHeader url authHeader
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
let private photoCommonDecoder (injectedOwner: NSID option) (get: Decode.IGetters) =
        { Id = get.Required.Field "id" Decode.string
          Owner =
              match injectedOwner with
              | Some owner -> owner
              | None -> get.Required.Field "owner" (Decode.map NSID Decode.string)
          PathAlias = get.Optional.Field "pathalias" Decode.string
          Title = get.Required.Field "title" Decode.string
          Secret = get.Required.Field "secret" Decode.string
          Server = get.Required.Field "server" Decode.int
          Farm = get.Required.Field "farm" Decode.int
          Square150 = get.Required.Field "url_q" Decode.string
          Medium240 = get.Optional.Field "url_m" Decode.string }
        // see: https://www.flickr.com/services/api/misc.urls.html

let private timestampToDateTimeDecoder =
    Decode.map (int64 >> timestampToDateTime) Decode.string

let private paginationGetter (get: Decode.IGetters) =
    { CurrentPage = get.Required.Field "page" Decode.int
      TotalPages = get.Required.Field "pages" Decode.int
      ItemsPerPage = get.Required.Field "perpage" Decode.int
      TotalItems = get.Required.Field "total" Decode.int }

// group lookup by URL =================================
let private urlsLookupGroupGetter (get: Decode.IGetters) =
    get.Required.Field "group" (Decode.object (fun get -> {
        Id = get.Required.Field "id" (Decode.map NSID Decode.string)
        Name = get.Required.Field "groupname" (Decode.object (fun get -> get.Required.Field "_content" Decode.string))
    }))

let internal urlsLookupGroup (platform: IPlatformContext) (apiKey: string) (apiSecret: string) (accessToken: AccessTokenInfo) (name: string) =
    let url =
        $"https://www.flickr.com/groups/{name}"
    let args =
        [ ("url", url) ]
    flickrMethod platform apiKey apiSecret accessToken "flickr.urls.lookupGroup" args urlsLookupGroupGetter

// group pool =================================
let private groupPhotoDecoder: Decoder<GroupPoolPhoto> =
    Decode.object (fun get ->
        { Common = photoCommonDecoder None get
          DateAdded = get.Required.Field "dateadded" timestampToDateTimeDecoder })

let private groupPhotosPageGetter (get: Decode.IGetters) =
    get.Required.Field "photos" (Decode.object (fun get->
        { Pagination = paginationGetter get
          Photos = get.Required.Field "photo" (Decode.list groupPhotoDecoder) }
        ))

let internal getGroupPhotos (platform: IPlatformContext) (apiKey: string) (apiSecret: string) (accessToken: AccessTokenInfo)
    (id: NSID) (perPage: int option) (page: int option) =
        let args =
            [ "group_id", string id
              if page.IsSome then "page", string page.Value
              if perPage.IsSome then "per_page", string perPage.Value
              "extras", "o_dims, url_q, url_m, path_alias" ]
        flickrMethod platform apiKey apiSecret accessToken "flickr.groups.pools.getPhotos" args groupPhotosPageGetter

// favorites ==================================
let private favesPhotoDecoder: Decoder<FavoritesPhoto> =
    Decode.object (fun get ->
        { Common = photoCommonDecoder None get
          DateFaved = get.Required.Field "date_faved" timestampToDateTimeDecoder
          UpgradeSizes = get.Optional.Field "upgrade_sizes" (Decode.list Decode.string) })

let private favoritesPageGetter (get: Decode.IGetters) =
    get.Required.Field "photos" (Decode.object (fun get->
        { Pagination = paginationGetter get
          Photos = get.Required.Field "photo" (Decode.list favesPhotoDecoder) }
        ))

let internal getFavorites
    (platform: IPlatformContext) (apiKey: string) (apiSecret: string) (accessToken: AccessTokenInfo)
    (userId: NSID option) (minDate: DateTime option) (maxDate: DateTime option)
    (perPage: int option) (page: int option) =
        let args =
            [ if userId.IsSome then "user_id", string userId.Value
              if minDate.IsSome then "min_fave_date", dateTimeToTimestamp minDate.Value |> string
              if maxDate.IsSome then "max_fave_date", dateTimeToTimestamp maxDate.Value |> string
              if perPage.IsSome then "per_page", string perPage.Value
              if page.IsSome then "page", string page.Value
              "extras", "o_dims, url_q, url_m, path_alias" ]
        flickrMethod platform apiKey apiSecret accessToken "flickr.favorites.getList" args favoritesPageGetter

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
    (platform: IPlatformContext) (apiKey: string) (apiSecret: string) (accessToken: AccessTokenInfo)
    (userId: NSID) (photosetId: string)
    (perPage: int option) (page: int option) =
        let args =
            [ "photoset_id", photosetId
              "user_id", string userId
              if perPage.IsSome then "per_page", string perPage.Value
              if page.IsSome then "page", string page.Value
              "extras", "o_dims, url_q, url_m, path_alias" ]
        flickrMethod platform apiKey apiSecret accessToken "flickr.photosets.getPhotos" args (photosetPageGetter userId)

// look up user by URL =======================================

let private urlsLookupUserGetter (get: Decode.IGetters) =
    get.Required.Field "user" (Decode.object (fun get ->
        get.Required.Field "id" (Decode.map NSID Decode.string)
        ))

let internal urlsLookupUser
    (platform: IPlatformContext) (apiKey: string) (apiSecret: string) (accessToken: AccessTokenInfo)
    (url: string) =
        let args =
            [ "url", url ]
        flickrMethod platform apiKey apiSecret accessToken "flickr.urls.lookupUser" args urlsLookupUserGetter
