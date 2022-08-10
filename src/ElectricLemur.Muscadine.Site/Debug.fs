module ElectricLemur.Muscadine.Site.Debug

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.ViewEngine

let private writeJObjectArray (objects: Newtonsoft.Json.Linq.JObject seq) (ctx: HttpContext) = task {
    use writer = new System.IO.StreamWriter(ctx.Response.Body)
    do! writer.WriteLineAsync("[")
    let mutable comma = ""
    for obj in objects do
        let line = $"%s{comma} %s{obj.ToString()}"
        do! writer.WriteLineAsync(line)
        comma <- ","

    do! writer.WriteLineAsync("]")
}

module Handlers =
    let GET_allDocuments: HttpHandler =
        fun next ctx -> task {
            let! documents = Database.getAllDocuments Database.NoLimit ctx
            do! writeJObjectArray documents ctx

            return Some ctx
        }

    let GET_orphanedTags: HttpHandler =
        fun next ctx -> task {
            let! tags = Tag.orphanedTagsAsJObjects ctx
            do! writeJObjectArray tags ctx
            return Some ctx
        }

    let DELETE_orphanedTags: HttpHandler =
        fun next ctx -> task {
            do! Tag.deleteOrphanedTags ctx
            return Some ctx
        }

    let GET_resetDatabase: HttpHandler =
            htmlView (html [] [
                script [ _type "application/javascript" ] [
                    rawText """
                        window.resetDatabase = function () {
                            fetch("/debug/reset", {
                                method: "DELETE",
                                mode: "same-origin",
                                cache: "no-cache",
                                credentials: "same-origin"
                            }).then(() => alert("Done"));
                        }
                    """
                ]
                body [] [
                    button [ _onclick "resetDatabase();" ] [ encodedText "Reset Database" ]
                ]
            ]) 

    let DELETE_resetDatabase: HttpHandler =
        fun next (ctx: HttpContext) -> task {
            let! documents = Database.getAllDocuments Database.NoLimit ctx
            let ids = documents |> Seq.map (fun d -> d.Value<string>("_id"))

            for id in ids do
                do! Database.deleteDocument ctx id

            do! Database.resetIndexes ctx

            return Some ctx
        }