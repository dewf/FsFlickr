namespace FsFlickr

open System
open FlickrOAuth1
open FlickrApi

type private AuthState =
    | Init
    | PreVerify of ts: TokenAndSecret
    | Finalized of ati: AccessTokenInfo
    | ErrState of reason: string

type FlickrAPI(apiKey: string, apiSecret: string, platform: IPlatformContext) =
    let mutable authState: AuthState = Init

    let withAccessToken (f: AccessTokenInfo -> Async<FlickrApiResult<'a>>) =
        match authState with
        | Finalized ati ->
            f ati
        | _ ->
            async {
                return FlickrNotYetAuthenticated
            }

    member this.LoadAccessToken (json: string) =
        match decodeAccessToken json with
        | Ok ati ->
            authState <- Finalized ati
            Ok { Fullname = ati.Fullname; UserNSID = ati.UserNSID; Username = ati.Username }
        | Error err ->
            Error $"FlickrAPI.LoadAccessToken: decode error ({err})"

    member this.AccessTokenJson =
        match authState with
        | Finalized ati ->
            encodeAccessToken ati
            |> Some
        | _ ->
            None

    member this.GenerateOAuthUrl (callback: OAuthCallback) =
        async {
            let! result = beginOAuthProcess platform apiKey apiSecret callback
            match result with
            | Ok tokenAndSecret ->
                let url = generateAuthLink platform tokenAndSecret
                authState <- PreVerify tokenAndSecret
                return Ok url
            | Error err ->
                printfn "FlickrAPI.GenerateOAuthUrl(): %s" err
                authState <- ErrState $"beginOAuthProcess failure: {err}"
                return Error err
        }

    member this.Verify (verifier: string) =
        async {
            match authState with
            | Init ->
                return (Error "FlickrAPI.Verify(): tried to verify from the init state - call .GenerateOAuthUrl() first, and visit the link")
            | PreVerify ts ->
                let! result = finalizeOAuth platform apiKey apiSecret ts verifier
                match result with
                | Ok ati ->
                    authState <- Finalized ati
                    return (Ok { Fullname = ati.Fullname; UserNSID = ati.UserNSID; Username = ati.Username })
                | Error err ->
                    authState <- ErrState $"finalizeOAuth failure: {err}"
                    return (Error "finalizeOAuth failure: {err}")
            | Finalized ati ->
                // nothing to do!
                return (Ok { Fullname = ati.Fullname; UserNSID = ati.UserNSID; Username = ati.Username })
            | ErrState err ->
                return (Error $"FlickrAPI.Verify(): was already in ErrState [{err}]")
        }

    member this.GetGroupId (name: string) =
        withAccessToken (fun ati -> urlsLookupGroup platform apiKey apiSecret ati name)

    member this.GetGroupPhotos (id: NSID, ?perPage: int, ?page: int) =
        withAccessToken (fun ati -> getGroupPhotos platform apiKey apiSecret ati id perPage page)

    member this.GetFavorites (?userId: NSID, ?minFaveDate: DateTime, ?maxFaveDate: DateTime, ?perPage: int, ?page: int) =
        withAccessToken (fun ati -> getFavorites platform apiKey apiSecret ati userId minFaveDate maxFaveDate perPage page)

    member this.GetPhotoSet (userId: NSID, photosetId: string, ?perPage: int, ?page: int) =
        withAccessToken (fun ati -> getPhotoset platform apiKey apiSecret ati userId photosetId perPage page)

    member this.UrlsLookupUser (url: string) =
        withAccessToken (fun ati -> urlsLookupUser platform apiKey apiSecret ati url)

    member this.GetPhotoInfo (id: string, ?secret: string) =
        withAccessToken (fun ati -> getPhotoInfo platform apiKey apiSecret ati id secret)

    member this.GetPhotoSizes (id: string) =
        withAccessToken (fun ati -> getSizes platform apiKey apiSecret ati id)
