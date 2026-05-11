namespace FsFlickr

open System
open FlickrOAuth1
open FlickrApi
open Config

type private AuthState =
    | Init
    | PreVerify of ts: TokenAndSecret
    | Finalized of ati: AccessTokenInfo
    | ErrState of reason: string

// the flickr API wrapper class. this encapsulates the Oauth 1.0a flow, necessary to fully use the API
// unfortunately I don't think primary constructors can be documented with XML syntax :(
type FlickrAPI(apiKey: string, apiSecret: string, ?corsProxy: string) =
    let mutable authState: AuthState = Init
    let config =
        { ApiKey = apiKey
          ApiSecret = apiSecret
          CorsProxy = corsProxy }

    let withAccessToken (f: AccessTokenInfo -> Async<FlickrApiResult<'a>>) =
        match authState with
        | Finalized ati ->
            f ati
        | _ ->
            async {
                return FlickrNotYetAuthenticated
            }

    /// Attempt to load existing access token from JSON (so you don't have to re-authenticate)
    member this.LoadAccessToken (json: string) =
        match decodeAccessToken json with
        | Ok ati ->
            authState <- Finalized ati
            Ok { Fullname = ati.Fullname; UserNSID = ati.UserNSID; Username = ati.Username }
        | Error err ->
            Error $"FlickrAPI.LoadAccessToken: decode error ({err})"

    /// Obtain the current access token (if any), so that you can persist it elsewhere
    member this.AccessTokenJson =
        match authState with
        | Finalized ati ->
            encodeAccessToken ati
            |> Some
        | _ ->
            None

    /// First step of OAuth: generate a URL for the user to click. Requires choosing an OAuth callback type:
    ///   out-of-band or a callback URL
    member this.GenerateOAuthUrl (callback: OAuthCallback) =
        async {
            let! result = beginOAuthProcess config callback
            match result with
            | Ok tokenAndSecret ->
                let url = generateAuthLink tokenAndSecret
                authState <- PreVerify tokenAndSecret
                return Ok url
            | Error err ->
                printfn "FlickrAPI.GenerateOAuthUrl(): %s" err
                authState <- ErrState $"beginOAuthProcess failure: {err}"
                return Error err
        }

    /// Use the generated verifier code - either automatically obtained via a callback URL,
    ///   or provided by the user manually for desktop apps
    member this.Verify (verifier: string) =
        async {
            match authState with
            | Init ->
                return (Error "FlickrAPI.Verify(): tried to verify from the init state - call .GenerateOAuthUrl() first, and visit the link")
            | PreVerify ts ->
                let! result = finalizeOAuth config ts verifier
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

    // actual API method wrappers below =============================================
    member this.UrlsLookupGroup (url: string) =
        withAccessToken (fun ati -> urlsLookupGroup config ati url)

    member this.GetGroupPhotos (id: NSID, ?perPage: int, ?page: int, ?userId: NSID, ?extras: Extra seq) =
        let extras' = defaultArg extras Seq.empty
        withAccessToken (fun ati -> getGroupPhotos config ati id perPage page userId extras')

    member this.GetFavorites (?userId: NSID, ?minFaveDate: DateTime, ?maxFaveDate: DateTime, ?perPage: int, ?page: int, ?extras: Extra seq) =
        let extras' = defaultArg extras Seq.empty
        withAccessToken (fun ati -> getFavorites config ati userId minFaveDate maxFaveDate perPage page extras')

    member this.GetPhotoSet (userId: NSID, photosetId: string, ?perPage: int, ?page: int, ?extras: Extra seq) =
        let extras' = defaultArg extras Seq.empty
        withAccessToken (fun ati -> getPhotoset config ati userId photosetId perPage page extras')

    member this.UrlsLookupUser (url: string) =
        withAccessToken (fun ati -> urlsLookupUser config ati url)

    member this.GetPhotoInfo (id: string, ?secret: string) =
        withAccessToken (fun ati -> getPhotoInfo config ati id secret)

    member this.GetPhotoSizes (id: string) =
        withAccessToken (fun ati -> getSizes config ati id)
