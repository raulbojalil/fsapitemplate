module SampleApp.App

open System
open System.Linq
open System.Security.Claims
open System.Collections.Generic
open System.Threading
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Features
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Giraffe.HttpStatusCodeHandlers
open Giraffe.GiraffeViewEngine
open System.Globalization
open Newtonsoft.Json.Linq
open Microsoft

// ---------------------------------
// Extensions
// ---------------------------------

type HttpContext with
    member this.TryBindJsonAsync<'T> (?cultureInfo : CultureInfo) =
            task {
                let serializer = this.GetJsonSerializer()
                let! form = serializer.DeserializeAsync<Dictionary<string, Object>> this.Request.Body
                return
                    form
                    |> Seq.map (fun i -> i.Key, new Extensions.Primitives.StringValues(i.Value.ToString()))
                    |> dict
                    |> ModelParser.tryParse<'T> cultureInfo
            }

// ---------------------------------
// Error handling
// ---------------------------------

type ErrorResult(code, description) =
    member this.code with get() = code
    member this.description with get() = description
    new() = ErrorResult(0,"Unknown error")

let userError code msg = RequestErrors.badRequest (json (ErrorResult(code,msg)))
let conflictError msg = RequestErrors.conflict (json (ErrorResult(409,msg)))
let notFoundError msg = RequestErrors.notFound (json (ErrorResult(404,msg)))
let invalidModelError msg = userError 2300 msg

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    match ex.GetType() with
        | t when t = typeof<InvalidOperationException> -> clearResponse >=> userError 1200 ex.Message
        | _ -> clearResponse >=> setStatusCode 500 >=> json (ErrorResult(500,"We are sorry but an unknown error has occurred. Please try again later."))

// ---------------------------------
// Authentication
// ---------------------------------

let authScheme = CookieAuthenticationDefaults.AuthenticationScheme

let cookieAuth (o : CookieAuthenticationOptions) =
    do
        o.Cookie.HttpOnly     <- true
        o.Cookie.SecurePolicy <- CookieSecurePolicy.SameAsRequest
        o.SlidingExpiration   <- true
        o.ExpireTimeSpan      <- TimeSpan.FromDays 7.0

let accessDenied = setStatusCode 401 >=> text "Access Denied"

let mustBeUser = requiresAuthentication accessDenied

let mustBeAdmin =
    requiresAuthentication accessDenied
    >=> requiresRole "Admin" accessDenied

let mustBeJohn =
    requiresAuthentication accessDenied
    >=> requiresAuthPolicy (fun u -> u.HasClaim (ClaimTypes.Name, "John")) accessDenied

let loginHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let issuer = "http://localhost:5000"
            let claims =
                [
                    Claim(ClaimTypes.Name,      "John",  ClaimValueTypes.String, issuer)
                    Claim(ClaimTypes.Surname,   "Doe",   ClaimValueTypes.String, issuer)
                    Claim(ClaimTypes.Role,      "Admin", ClaimValueTypes.String, issuer)
                ]
            let identity = ClaimsIdentity(claims, authScheme)
            let user     = ClaimsPrincipal(identity)

            do! ctx.SignInAsync(authScheme, user)

            return! text "Successfully logged in" next ctx
        }

let userHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        text ctx.User.Identity.Name next ctx

let showUserHandler id =
    mustBeAdmin >=>
    text (sprintf "User ID: %i" id)

// ---------------------------------
// Models
// ---------------------------------

[<CLIMutable>]
type Car =
    {
        Name   : string
        Make   : string
        Wheels : int
        Built  : DateTime
    }
    interface IModelValidation<Car> with
        member this.Validate() =
            if this.Wheels > 1 && this.Wheels <= 6 then Ok this
            else Error (userError 10 "Wheels must be a value between 2 and 6.")

type PagingRequest(page,limit) = 
    let mutable _page = page
    let mutable _limit = limit
    member this.page with get() = _page and set(value:int) = _page <- value 
    member this.limit with get() = _limit and set(value:int) = _limit <- value 
    new() = PagingRequest(1,10)
    
type PagedResults(data:IEnumerable<'T>, paging:PagingRequest) =
    let mutable _page = Math.Max(0,paging.page-1)
    member this.total with get() = data.Count()
    member this.results with get() = data.Skip(_page*paging.limit).Take(paging.limit)
    member this.page with get() = _page + 1
    member this.limit with get() = paging.limit
    member this.totalPages with get() = Math.Ceiling(float(this.total) / float(this.limit))

// ---------------------------------
// Data access
// ---------------------------------

let _cars = new List<Car>() 

let addCar (next: HttpFunc) (ctx : HttpContext) =
    task {
    let! result = ctx.TryBindJsonAsync<Car>()
    
    return!
        (match result with 
        | Ok car ->  validateModel (fun _ -> match _cars.Any(fun c -> c.Name = car.Name) with
                                             | false ->  _cars.Add(car) ; Successful.CREATED car
                                             | true -> conflictError "A car with the specified name already exists") 
                                             car
        | Error err -> invalidModelError err) next ctx
    }

// ---------------------------------
// Views
// ---------------------------------

let layout (content: XmlNode list) =
    html [] [
        head [] [
            title []  [ encodedText "Sample View" ]
        ]
        body [] content
    ]

let partial () =
    p [] [ encodedText "Some partial text." ]

let carView (model : Car) =
    [
        div [_class "container"] [
                h3 [_title "Some title attribute"] [ sprintf "%s" model.Name |> encodedText ]
                ul [] [
                    li [] [ sprintf "Make: %s" model.Make |> encodedText ]
                    li [] [ sprintf "Wheels: %i" model.Wheels |> encodedText ]
                    li [] [ sprintf "Built: %s" (model.Built.ToString()) |> encodedText ]
                ]
            ]
        div [] [partial()]
    ] |> layout

// ---------------------------------
// Routing
// ---------------------------------

let webApp =
    choose [

        route  "/"           >=> htmlFile "index.html"

        //User related endpoints
        route  "/login"      >=> loginHandler
        route  "/logout"     >=> signOut authScheme >=> text "Successfully logged out."
        route  "/user"       >=> mustBeUser >=> userHandler
        route  "/john-only"  >=> mustBeJohn >=> userHandler
        routef "/user/%i"    showUserHandler
       
        //API endpoints
        subRoute "/api"
            (choose [
                subRoute "/v1"
                    (choose [
                        requiresAuthentication accessDenied >=>
                        GET >=> 
                        choose [
                            route  "/cars" >=> (fun next ctx -> Successful.OK (new PagedResults(_cars, ctx.BindQueryString<PagingRequest>())) next ctx)
                            routef "/cars/getbymake/%s" (fun (make) next ctx -> Successful.OK (new PagedResults((query {
                                                                                for car in _cars do
                                                                                where (car.Make = make)
                                                                                select car
                                                                            }), ctx.BindQueryString<PagingRequest>())) next ctx)
            
            
            

                        ]
                        POST >=> 
                        choose [
                            route "/cars" >=> addCar
                        ]
                    ])
            ])


        //View samples
        route  "/view"       >=> (carView { Name = "Car name"; Make = "Some Make"; Built = DateTime.Now; Wheels = 4 } |> htmlView)
        
        //Error handling samples
        route  "/conflict"   >=> conflictError "There is already an element with Name 'Test'. Name must be unique."
        route  "/notfound"   >=> notFoundError "An element with Name 'Test' could not be found."
        route  "/usererror"  >=> userError 9000 "This is the description for error code 9000."
        route  "/error"      >=> (fun _ _ -> failwith "This error is handled by the global error handler. Users don't see this error.")

        RequestErrors.notFound (text "Not Found")
    ]

        
// ---------------------------------
// Main
// ---------------------------------

let configureApp (app : IApplicationBuilder) =
    app.UseGiraffeErrorHandler(errorHandler)
       .UseStaticFiles()
       .UseAuthentication()
       .UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services
        .AddGiraffe()
        .AddAuthentication(authScheme)
        .AddCookie(cookieAuth)   |> ignore
    services.AddDataProtection() |> ignore

let configureLogging (loggerBuilder : ILoggingBuilder) =
    loggerBuilder.AddFilter(fun lvl -> lvl.Equals LogLevel.Error)
                 .AddConsole()
                 .AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    WebHost.CreateDefaultBuilder()
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0