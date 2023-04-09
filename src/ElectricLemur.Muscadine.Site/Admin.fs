module Admin
open ElectricLemur.Muscadine.Site.FrontendHelpers
open ElectricLemur.Muscadine.Site.ItemHelper
open ElectricLemur.Muscadine.Site.Items
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http
open System.Security.Claims
open ElectricLemur.Muscadine.Site
open Game;
open Book;
open Project

let getItemCoverImage (item: ItemWrapper) =
    match item with
    | Book x -> x.CoverImagePaths 
    | Project x -> x.IconImagePaths
    | Game x -> x.CoverImagePaths

let getItemCoverImageIcon (item: ItemWrapper) =
    item
    |> getItemCoverImage
    |> Option.map (fun img -> img.Size128)

let getItemCoverImageIconPath (item: ItemWrapper) =
    item
    |> getItemCoverImageIcon
    |> Util.addRootPath "/images"
    
module Views =
    type AddButtonDisplay =
        | NotShown
        | Shown of ItemDocumentType
    
    let adminLayout (pageTitle: string) (addButtonDisplay: AddButtonDisplay) (pageData: FrontendHelpers.PageData) (itemDocumentType: ItemDocumentType option) (content: XmlNode list) ctx =
        let extraHeader =
            match addButtonDisplay with
            | NotShown -> []
            | Shown itemDocumentType ->
                let slug = ItemDocumentType.toSlug itemDocumentType
                let documentType = ItemDocumentType.toDatabaseDocumentType itemDocumentType
                let singular = ItemDocumentType.toSingularTitleString itemDocumentType
                let urlBase = $"/admin/{slug}"
                [
                    button [
                        _class "add-new"
                        _data "new-url" $"{urlBase}/_new"
                        _data "document-type" (System.Web.HttpUtility.UrlEncode(documentType))
                        _data "slug" (System.Web.HttpUtility.UrlEncode(slug))
                    ] [ encodedText $"Add {singular}" ]
                ]
                
        let pageDef =
            FrontendHelpers.PageDefinitions.Page.Custom (
                $"/admin/{slug}",
                $"Admin: {pageTitle}",
                $"Admin: {pageTitle}",
                FrontendHelpers.PageDefinitions.Page.AboutMe,
                extraHeader)
            
        let extra = [ (FrontendHelpers.PageExtra.CSS "admin.scss")
                      (FrontendHelpers.PageExtra.JavaScript "admin.js") ]
        
        let sidebar: PageDefinitions.SidebarButton list = [
            {
                LongTitle = "Admin: Projects"
                ShortTitle = "Projects"
                Icon = PageDefinitions.SidebarButton.DefaultButtons.Projects.Icon
                Route = "/admin/projects"
                Active = (itemDocumentType |> Option.map(fun i -> i = ProjectDocumentType) |> Option.defaultValue false)
            }
            
            {
                LongTitle = "Admin: Books"
                ShortTitle = "Books"
                Icon = PageDefinitions.SidebarButton.DefaultButtons.Books.Icon
                Route = "/admin/books"
                Active = (itemDocumentType |> Option.map(fun i -> i = BookDocumentType) |> Option.defaultValue false)
            }
            
            {
                LongTitle = "Admin: Games"
                ShortTitle = "Games"
                Icon = PageDefinitions.SidebarButton.DefaultButtons.Games.Icon
                Route = "/admin/games"
                Active = (itemDocumentType |> Option.map(fun i -> i = GameDocumentType) |> Option.defaultValue false)
            }
            
            {
                LongTitle = "Exit Admin"
                ShortTitle = "Exit"
                Icon = Some (i [ _class "fa-solid fa-right-from-bracket" ] [])
                Route = "/"
                Active = false
            }
        ]
        FrontendHelpers.layout pageDef content extra pageData (Some sidebar) ctx

    let makeItemIndexRow (itemDocumentType: ItemDocumentType) (item: ItemWrapper) (tags: Option<string seq>) =
        let itemImage =
            match getItemCoverImageIconPath item with
            | Some path -> div [ _class "item-image" ] [ img [ _src path ] ]
            | None -> div [ _class "item-image empty-image" ] []
            
        let tags =
            tags
            |> Option.map(List.ofSeq)
            |> Option.defaultValue []
            |> List.map (fun t -> span [ _class "item-tag" ] [encodedText t])

        let markdownDescription = Markdig.Markdown.ToHtml(ItemHelper.description item)
        
        let sectionId = System.Guid.NewGuid().ToString().Replace("-", "").ToLower()
        let sectionId = $"item-section-{sectionId}"

        let slug = ItemDocumentType.toSlug itemDocumentType
        
        section [ _class "index-item"; _id sectionId] [
            itemImage
            div [ _class "item-content" ] [ 
                header [] [
                    h2 [] [
                        a [ _href $"/admin/{slug}/{ItemHelper.itemId item}" ] [ encodedText (ItemHelper.name item) ]
                    ]
                    
                    button [ _class "delete-button"
                             _data "id" (ItemHelper.itemId item)
                             _data "name" (ItemHelper.name item)
                             _data "section-id" sectionId
                             _data "url" $"/admin/{slug}/{ItemHelper.itemId item}"] [ encodedText "Delete" ]
                    
                    div [ _class "item-tags" ] tags
                ]
                div [ _class "item-description" ] [ rawText markdownDescription ]
            ]
        ]

    let index (itemDocumentType: ItemDocumentType) (items: ItemWrapper seq) (tags: Map<string,string list>) (pageData: FrontendHelpers.PageData) (ctx: HttpContext) =
        let content = [
            yield! (items |> Seq.map (fun item ->
                let tags =
                    tags
                    |> Map.tryFind (ItemHelper.itemId item)
                    |> Option.map (Seq.ofList)
                
                makeItemIndexRow itemDocumentType item tags ))
        ]

        adminLayout (ItemDocumentType.toPluralTitleString itemDocumentType) (AddButtonDisplay.Shown itemDocumentType) pageData (Some itemDocumentType) content ctx

open Views

module Handlers =
    type EditCancelMode =
    | CancelToIndex
    | CancelToItem of string
    
    let private makePageData  (itemDocumentType: ItemHelper.ItemDocumentType) (cancelMode) (item: ItemWrapper option) =
        let slug = (ItemDocumentType.toSlug itemDocumentType)
        let cancelUrl =
            match cancelMode with
            | CancelToIndex -> $"/admin/{slug}"
            | CancelToItem id -> $"/admin/{slug}/{id}"
            
        Map.empty
        |> Map.add "documentType" (FrontendHelpers.PageDataType.String (ItemDocumentType.toDatabaseDocumentType itemDocumentType))
        |> Map.add "slug" (FrontendHelpers.PageDataType.String slug)
        |> Map.add "cancelUrl" (FrontendHelpers.PageDataType.String cancelUrl)
        |> (fun m ->
            match item with
            | Some item -> Map.add "id" (FrontendHelpers.PageDataType.String (ItemHelper.itemId item)) m
            | None -> m)
        |> FrontendHelpers.PageData

    let GET_status: HttpHandler =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            let user = ctx.User
            let role = match user.FindFirst(ClaimTypes.Role) with
                       | null -> "No Role"
                       | claim -> claim.Value

            text role next ctx


    let GET_checkDatabase: HttpHandler =
        fun next ctx -> task {
            let! documentCount = Database.getDocumentCount ctx None
            let! gameCount = Database.getDocumentCount ctx (Some Game.documentType)

            let result = $"Database results\n\nTotal documents: %d{documentCount}\Games: %d{gameCount}"
            return! text result next ctx
        }

    let GET_index (itemDocumentType: ItemHelper.ItemDocumentType) =
        fun next ctx -> task {
            let documentType = ItemDocumentType.toDatabaseDocumentType itemDocumentType
                
            let! items =
                ItemHelper.loadAllItems documentType ctx
                |> Task.bind (fun items -> Microblog.sortByMostRecentMicroblog items ItemHelper.itemId ItemHelper.name ctx)
                
            let! tags = Tag.loadTagsForDocuments documentType (items |> Seq.map ItemHelper.itemId) ctx
            
            return! htmlView (Views.index itemDocumentType items tags (makePageData itemDocumentType CancelToIndex None) ctx) next ctx
        }
        
    let GET_add (itemDocumentType: ItemHelper.ItemDocumentType) =
        fun next ctx -> task {
            let! allTags = Tag.getExistingTags ctx 
            let title = ItemDocumentType.toSingularTitleString itemDocumentType
            let slug = ItemDocumentType.toSlug itemDocumentType
            
            let content =
                match itemDocumentType with
                | GameDocumentType -> FormFields.View.addEditView None title slug (Game.Fields.name) (Game.Fields.allFields) allTags []
                | ProjectDocumentType -> FormFields.View.addEditView None title slug (Project.Fields.name) (Project.Fields.allFields) allTags []
                | BookDocumentType -> FormFields.View.addEditView None title slug (Book.Fields.title) (Book.Fields.allFields) allTags []

            return! htmlView (Views.adminLayout $"Add {title}" AddButtonDisplay.NotShown (makePageData itemDocumentType CancelToIndex None) (Some itemDocumentType) content ctx) next ctx
        }
        
    let GET_edit (itemDocumentType: ItemHelper.ItemDocumentType) (id: string) =
        fun next ctx -> task {
            let documentType = ItemDocumentType.toDatabaseDocumentType itemDocumentType
            
            let! existing =
                Database.getDocumentById id ctx
                |> Task.map (Option.bind ItemHelper.fromJObject)
                
            let! allTags = Tag.getExistingTags ctx
            let! documentTags = Tag.loadTagsForDocument documentType id ctx

            let title = ItemDocumentType.toSingularTitleString itemDocumentType
            let slug = ItemDocumentType.toSlug itemDocumentType
            
            let content =
                existing
                |> Option.map (fun existing ->
                        match existing with
                        | Game m -> FormFields.View.addEditView (Some m) title slug (Game.Fields.name) (Game.Fields.allFields) allTags documentTags
                        | Project m -> FormFields.View.addEditView (Some m) title slug (Project.Fields.name) (Project.Fields.allFields) allTags documentTags
                        | Book m -> FormFields.View.addEditView (Some m) title slug (Book.Fields.title) (Book.Fields.allFields) allTags documentTags
                    )
                |> Option.defaultValue []
                
            match existing with
            | Some existing ->
                let view = Views.adminLayout $"Edit {title} \"{ItemHelper.name existing}\"" AddButtonDisplay.NotShown (makePageData itemDocumentType CancelToIndex (Some existing)) (Some itemDocumentType) content ctx
                return! htmlView view next ctx
            | None ->
                return! (setStatusCode 404 >=> text "Page not found") next ctx
        }
        

    let GET_microblog_edit id : HttpHandler =
        fun next ctx -> task {
          let! existing = Microblog.loadById id ctx
          
          // this is so simpler that we can just put the view right in the handler
          return! match existing with
                  | None -> (setStatusCode 404 >=> text "Microblog not found") next ctx
                  | Some existing ->
                      let itemDocumentType = ItemDocumentType.fromString existing.ItemDocumentType |> Option.defaultValue ItemDocumentType.ProjectDocumentType
                      
                      let content = [
                          form [ _name "form"; _method "post"; ] [
                            Items.makeInputRow "timestamp" (Some "Timestamp") Items.HardcodedLabel (encodedText (existing.Microblog.DateAdded.ToString("g")))
                            Items.makeTextAreaInputRow "Text" "text" 5 (Some existing.Microblog.Text)
                            
                            Items.makeInputRow "save-button" None InputRowType.SaveButton (
                                div [ _class "save-cancel-container" ] [
                                    div [ _class "save-cancel" ] [
                                      input [ _type "submit"; _value "Save" ]
                                      button [ _class "cancel-button" ] [ encodedText "Cancel" ]  
                                    ]])
                        ]
                      ]
                      htmlView (Views.adminLayout "Edit microblog" AddButtonDisplay.NotShown (makePageData itemDocumentType (CancelToItem existing.ItemId) None) (Some itemDocumentType) content ctx) next ctx

        }