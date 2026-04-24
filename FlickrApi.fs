module FsFlickr.FlickrApi

open FlickrOAuth1
open FsFlickr.Util
open HttpStuff
open Thoth.Json

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

let internal flickrMethod (platform: IPlatformContext) (apiKey: string) (apiSecret: string) (accessToken: AccessTokenInfo) (method: string) (methodArgs: (string * string) list) (subgetter: Decode.IGetters -> 'a) =
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

let private groupIdGetter (get: Decode.IGetters) =
    get.Required.Field "group" (Decode.object (fun get -> {
        Id = get.Required.Field "id" Decode.string
        Name = get.Required.Field "groupname" (Decode.object (fun get -> get.Required.Field "_content" Decode.string))
    }))

let internal getGroupId (platform: IPlatformContext) (apiKey: string) (apiSecret: string) (accessToken: AccessTokenInfo) (name: string) =
    let url =
        $"https://www.flickr.com/groups/{name}"
    let args =
        [ ("url", url) ]
    flickrMethod platform apiKey apiSecret accessToken "flickr.urls.lookupGroup" args groupIdGetter

let private photoDecoder: Decoder<Photo> =
    Decode.object (fun get ->
        { Id = get.Required.Field "id" Decode.string
          Owner = get.Required.Field "owner" Decode.string
          PathAlias = get.Optional.Field "pathalias" Decode.string
          Title = get.Required.Field "title" Decode.string
          DateAdded = get.Required.Field "dateadded" (Decode.map timestampToDateTime Decode.string)
          Secret = get.Required.Field "secret" Decode.string
          Server = get.Required.Field "server" Decode.int
          Farm = get.Required.Field "farm" Decode.int
          SmallUrl = get.Required.Field "url_q" Decode.string }) // see: https://www.flickr.com/services/api/misc.urls.html

let private paginationGetter (get: Decode.IGetters) =
    { CurrentPage = get.Required.Field "page" Decode.int
      TotalPages = get.Required.Field "pages" Decode.int
      ItemsPerPage = get.Required.Field "perpage" Decode.int
      TotalItems = get.Required.Field "total" Decode.int }

let private photosPageGetter (get: Decode.IGetters) =
    get.Required.Field "photos" (Decode.object (fun get->
        { Pagination = paginationGetter get
          Photos = get.Required.Field "photo" (Decode.list photoDecoder) }
        ))
    
let internal getGroupPhotos (platform: IPlatformContext) (apiKey: string) (apiSecret: string) (accessToken: AccessTokenInfo) (id: string) (perPage: int) (page: int) =
    let args =
        [ "group_id", id
          "page", string page
          "per_page", string perPage
          "extras", "o_dims, url_q, path_alias" ]
    flickrMethod platform apiKey apiSecret accessToken "flickr.groups.pools.getPhotos" args photosPageGetter
