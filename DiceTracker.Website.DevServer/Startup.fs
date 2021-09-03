namespace DiceTracker.Website.DevServer

open System
open System.IO
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.StaticFiles
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.FileProviders
open DiceTracker.Website
open Bolero
open Bolero.Server
open Bolero.Templating
open Bolero.Html
open Bolero.Server.Html
open Bolero.Templating.Server

module Index =
    let page = doctypeHtml [] [
        head [] [
            meta [attr.charset "UTF-8"]
            meta [attr.name "viewport"; attr.content "width=device-width, initial-scale=1.0"]
            title [] [text "DiceTracker"]
            ``base`` [attr.href "/"]
            link [attr.rel "stylesheet"; attr.href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css"]
            link [attr.rel "stylesheet"; attr.href "index.css"]
        ]
        body[] [
            div [attr.id "main"] [rootComp<App.Application>]
        ]
    ]

type Startup() =

    let (</>) x y = Path.Combine(x, y)
    let contentTypeProvider = FileExtensionContentTypeProvider()
    do  contentTypeProvider.Mappings.[".fsx"] <- "text/x-fsharp"
        contentTypeProvider.Mappings.[".scss"] <- "text/x-scss"
    let clientProjPath = Path.Combine(__SOURCE_DIRECTORY__, "..", "DiceTracker.Website")
    let fileProvider path = new PhysicalFileProvider(path)

    member this.ConfigureServices(services: IServiceCollection) =
        services.AddMvc() |> ignore
        //services.AddControllers() |> ignore
        services.AddServerSideBlazor() |> ignore
        services.AddBoleroHost() |> ignore
#if DEBUG
        services.AddHotReload(clientProjPath) |> ignore
#endif

    member this.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        app(*.UseStaticFiles(
            StaticFileOptions(
                FileProvider = fileProvider (clientProjPath </> "wwwroot"),
                ContentTypeProvider = contentTypeProvider))*)
            .UseStaticFiles()
            .UseRouting()
            .UseBlazorFrameworkFiles()
            .UseEndpoints(fun endpoints ->
#if DEBUG
                endpoints.UseHotReload()
#endif
                //endpoints.MapControllers() |> ignore
                endpoints.MapBlazorHub() |> ignore
                endpoints.MapFallbackToBolero(Index.page) |> ignore
                (*endpoints.MapFallbackToFile("index.html") |> ignore*))
        |> ignore

module Program =

    [<EntryPoint>]
    let main args =
        WebHost
            .CreateDefaultBuilder(args)
            .UseStaticWebAssets()
            .UseStartup<Startup>()
            .Build()
            .Run()
        0
