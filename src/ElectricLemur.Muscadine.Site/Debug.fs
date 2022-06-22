module ElectricLemur.Muscadine.Site.Debug

open Microsoft.AspNetCore.Http
open Giraffe

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