open System
open System.IO
open FsFlickr

let API_KEY = failwith "please provide API_KEY string here"
let API_SECRET = failwith "please provide API_SECRET string here"

let ACCESS_TOKEN_FILENAME = "flickr-oauth-token.json"

let flickr = FlickrAPI(API_KEY, API_SECRET)

let doAuthProcess () =
    // OAuth step 1: generate URL (for user to authorize your app)
    let url =
        let result =
            flickr.GenerateOAuthUrl OAuthCallback.OutOfBand // manual code entry, for desktop
            |> Async.RunSynchronously
        match result with
        | Ok url -> url
        | Error err -> failwithf "error generating oauth url: %s" err

    printfn "please visit URL to obtain verification code, then return here:"
    printfn "%s" url

    printfn "\nenter verification code: "
    let code = Console.ReadLine().Trim()

    // OAuth step 2: verify the code produced by the URL step, in order to obtain final auth token
    // (maintained internally in the FlickrAPI object, though we can serialize it to/from JSON)
    let loggedInUser =
        let result =
            flickr.Verify code
            |> Async.RunSynchronously
        match result with
        | Ok userInfo -> userInfo
        | Error err -> failwithf "error verifying oauth code: %s" err

    // write access token to disk
    let accessTokenJson =
        match flickr.AccessTokenJson with
        | Some json -> json
        | None -> failwith ".AccessTokenJson not available (shouldn't happen)"

    File.WriteAllText("flickr-oauth-token.json", accessTokenJson)
    loggedInUser

// check disk for an existing access token
let loadExistingAccessToken () =
    if File.Exists(ACCESS_TOKEN_FILENAME) then
        let json = File.ReadAllText(ACCESS_TOKEN_FILENAME)
        match flickr.LoadAccessToken json with
        | Ok userInfo -> Some userInfo
        | Error _ -> None
    else
        None

// program start =========================
let loggedInUser =
    match loadExistingAccessToken () with
    | Some userInfo ->
        userInfo
    | None ->
        doAuthProcess()

printfn "\nlogged in as [%s / %s / %A]" loggedInUser.Fullname loggedInUser.Username loggedInUser.UserNSID

// API method test: list first page of photos for the 'beautifulcapture' group
let groupInfo =
    flickr.UrlsLookupGroup("https://flickr.com/groups/beautifulcapture/")
    |> Util.forceApiResult

let groupPage =
    flickr.GetGroupPhotos(groupInfo.Id, perPage=100, extras=[Extra.UrlThumb150])
    |> Util.forceApiResult

for photo in groupPage.Photos do
    printfn "%s: %s" photo.Id photo.Title
    printfn "- url: %s" (photo.FlickrUrl (InPool "beautifulcapture"))
    photo.Extras.UrlThumb150
    |> Option.iter (printfn "- thumb: %s")
    printfn ""
