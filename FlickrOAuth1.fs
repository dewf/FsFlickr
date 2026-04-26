module FsFlickr.FlickrOAuth1

#if PLATFORM_FABLE
open Thoth.Json
#else
open Thoth.Json.Net
#endif

open Config

let private REQUEST_URL = "https://www.flickr.com/services/oauth/request_token"
let private AUTH_URL = "https://www.flickr.com/services/oauth/authorize"
let private ACCESS_TOKEN_URL = "https://www.flickr.com/services/oauth/access_token"

type internal TokenAndSecret = {
    Token: string
    Secret: string
}

type internal AccessTokenInfo = {
    Fullname: string
    OAuthToken: string
    OAuthTokenSecret: string
    UserNSID: NSID
    Username: string
}

let internal encodeAccessToken (ati: AccessTokenInfo) =
    Encode.object [
        "fullname", Encode.string ati.Fullname
        "token", Encode.string ati.OAuthToken
        "secret", Encode.string ati.OAuthTokenSecret
        "nsid", Encode.string (string ati.UserNSID)
        "username", Encode.string ati.Username
    ] |> Encode.toString 4

let internal decodeAccessToken (input: string) =
    let decoder =
        Decode.object (fun get ->
            { Fullname = get.Required.Field "fullname" Decode.string
              OAuthToken = get.Required.Field "token" Decode.string
              OAuthTokenSecret = get.Required.Field "secret" Decode.string
              UserNSID = get.Required.Field "nsid" (Decode.map NSID Decode.string)
              Username = get.Required.Field "username" Decode.string })
    Decode.fromString decoder input

type internal OAuthPhase =
    | BeforeRequest of callback: OAuthCallback
    | BeforeAuth of ts: TokenAndSecret
    | BeforeAccess of ts: TokenAndSecret * verifier: string
    | Authorized of ati: AccessTokenInfo

let private generateAuthFields (apiKey: string) (phase: OAuthPhase) =
    let commonFields =
        [ "oauth_nonce", Platform.randomUUID()
          "oauth_timestamp", Util.unixTimestamp() |> string
          "oauth_consumer_key", apiKey
          "oauth_signature_method", "HMAC-SHA1"
          "oauth_version", "1.0" ]
    let unique =
        match phase with
        | BeforeRequest callback ->
            let callbackStr =
                match callback with
                | OutOfBand -> "oob"
                | CallbackURL url -> url
            [ "oauth_callback", callbackStr ]
        | BeforeAuth _ ->
            failwith "no auth fields for 'BeforeAuth' step (simple link generation only)"
        | BeforeAccess (ts, verifier) ->
            [ "oauth_verifier", verifier
              "oauth_token", ts.Token ]
        | Authorized ati ->
            [ "oauth_token", ati.OAuthToken ]
    unique @ commonFields

let private computeSignature (apiSecret: string) (verb: string) (url: string) (fields: Map<string, string>) (tokenSecret: string option) =
    let text =
        let joinedFields =
            fields.Keys
            |> Seq.sort
            |> Seq.map (fun k ->
                let encodedKey = Platform.encodeURIComponent k
                let encodedValue = Platform.encodeURIComponent fields[k]
                encodedKey + "=" + encodedValue)
            |> String.concat "&"
        verb :: url :: joinedFields :: []
        |> List.map Platform.encodeURIComponent
        |> String.concat "&"
    let key =
        [ apiSecret
          tokenSecret |> Option.defaultValue "" ]
        // |> List.map encodeURIComponent // technically necessary per OAuth 1.0a spec?
        |> String.concat "&"
    Platform.hmacSha1 key text

let private fetchFlickrOAuthKeyPairs (config: FlickrConfig) (url: string) (authHeader: string) =
    async {
        let! code, resp = Platform.httpGetWithAuthHeader config url authHeader
        let result =
            match code with
            | 200 ->
                resp.Split("&")
                |> Array.map (fun pair ->
                    let both = pair.Split("=")
                    Platform.decodeURIComponent both[0], Platform.decodeURIComponent both[1])
                |> Map.ofArray
                |> Ok
            | _ ->
                Error resp
        return result
    }

let private fetchTokenAndSecret (config: FlickrConfig) (url: string) (authHeader: string) =
    async {
        let! keyPairs = fetchFlickrOAuthKeyPairs config url authHeader
        return keyPairs
        |> Result.map (fun pairs ->
            { Token = pairs["oauth_token"]
              Secret = pairs["oauth_token_secret"] })
    }

let private fetchFinalAccessToken (config: FlickrConfig) (url: string) (authHeader: string) =
    async {
        let! keyPairs = fetchFlickrOAuthKeyPairs config url authHeader
        return keyPairs
        |> Result.map (fun pairs ->
            { Fullname = pairs["fullname"]
              OAuthToken = pairs["oauth_token"]
              OAuthTokenSecret = pairs["oauth_token_secret"]
              UserNSID = NSID pairs["user_nsid"]
              Username = pairs["username"] })
    }

let internal generateAuthHeader (config: FlickrConfig) (phase: OAuthPhase) (dataFields: (string * string) list) (verb: string) (url: string) =
    let authFields =
        generateAuthFields config.ApiKey phase
    let signatureFields =
        dataFields @ authFields
        |> Map.ofList
    let tokenSecret =
        match phase with
        | BeforeRequest _ -> None
        | BeforeAuth ts -> Some ts.Secret
        | BeforeAccess (ts, _) -> Some ts.Secret
        | Authorized ati -> Some ati.OAuthTokenSecret
    async {
        let! signature = computeSignature config.ApiSecret verb url signatureFields tokenSecret
        let joined =
            ("oauth_signature", signature) :: authFields
            |> List.map (fun (key, value) ->
                sprintf "%s=\"%s\"" (Platform.encodeURIComponent key) (Platform.encodeURIComponent value))
            |> String.concat ", "
        return "OAuth " + joined
    }

let internal beginOAuthProcess (config: FlickrConfig) (callback: OAuthCallback) =
    async {
        let! authHeader = generateAuthHeader config (BeforeRequest callback) [] "GET" REQUEST_URL
        return! fetchTokenAndSecret config REQUEST_URL authHeader
    }

let internal generateAuthLink (ts: TokenAndSecret) =
    let queryString =
        [ "oauth_token", ts.Token
          "perms", "read" ]
        |> Map.ofList
        |> Util.mapToQueryString
    sprintf "%s?%s" AUTH_URL queryString

let internal finalizeOAuth (config: FlickrConfig) (tokenAndSecret: TokenAndSecret) (verifier: string) =
    async {
        let! authHeader = generateAuthHeader config (BeforeAccess (tokenAndSecret, verifier)) [] "GET" ACCESS_TOKEN_URL
        return! fetchFinalAccessToken config ACCESS_TOKEN_URL authHeader
    }
