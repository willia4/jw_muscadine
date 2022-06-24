module ElectricLemur.Muscadine.Site.Debug

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.ViewEngine

let allDocumentsHandler: HttpHandler =
    fun next (ctx: HttpContext) -> task {
        let! documents = Database.getAllDocuments ctx
        use writer = new System.IO.StreamWriter(ctx.Response.Body)

        do! writer.WriteLineAsync("[")
        let mutable comma = ""

        for doc in documents do
            let line = $"%s{comma} %s{doc.ToString()}"
            do! writer.WriteLineAsync(line)
            comma <- ", "

        do! writer.WriteLineAsync("]")
        return Some ctx
    }

let resetDatabase: HttpHandler = 
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

let resetDatabaseDeleteHandler: HttpHandler =
    fun next (ctx: HttpContext) -> task {
        let! documents = Database.getAllDocuments ctx
        let ids = documents |> Seq.map (fun d -> d.Value<string>("_id"))

        for id in ids do
            do! Database.deleteDocument ctx id

        do! Database.resetIndexes ctx

        return Some ctx
    }