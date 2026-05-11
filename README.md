# FsFlickr
*A very incomplete F# library for using the Flickr API (for both .NET and Fable ecosystems)*

Reference the corresponding project file for your ecosystem: either `FsFlickrNET` or `FsFlickrFable`.

I don't plan on turning this into a NuGet package anytime soon, because the API coverage is woefully incomplete - I mainly just needed a way of encapsulating all the annoying OAuth1 stuff. But the foundation is absolutely there to add more of the API surface easily, if you are so inclined.

Flickr API reference: https://www.flickr.com/services/api/

Note that when using with Fable, you'll need to provide a CORS proxy to the `FlickrAPI()` constructor.

Currently supported methods (with very inconsistent naming relative to the official API, sorry):

    // flickr.urls.lookupGroup
    UrlsLookupGroup (url: string)
    
    // flickr.groups.pools.getPhotos
    GetGroupPhotos (id: NSID, ?perPage: int, ?page: int, ?userId: NSID, ?extras: Extra seq)
    
    // flickr.favorites.getList
    GetFavorites (?userId: NSID, ?minFaveDate: DateTime, ?maxFaveDate: DateTime, ?perPage: int, ?page: int, ?extras: Extra seq)
    
    // flickr.photosets.getPhotos
    GetPhotoSet (userId: NSID, photosetId: string, ?perPage: int, ?page: int, ?extras: Extra seq)
    
    // flickr.urls.lookupUser
    UrlsLookupUser (url: string)
    
    // flickr.photos.getInfo
    GetPhotoInfo (id: string, ?secret: string)
    
    // flickr.photos.getSizes
    GetPhotoSizes (id: string)

There's an example desktop script in [examples/DotNet](./examples/DotNet/)
