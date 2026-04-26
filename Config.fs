module FsFlickr.Config

type FlickrConfig = {
    ApiKey: string
    ApiSecret: string
    CorsProxy: string option
}
