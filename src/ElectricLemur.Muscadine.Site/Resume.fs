module ElectricLemur.Muscadine.Site.Resume

open Giraffe
open Giraffe.ViewEngine
open ElectricLemur.Muscadine.Site.FrontendHelpers

// A resume section has:
// - An header, represented by a list of content nodes (may be empty)
// - An annotation, represented by a list of content nodes (may be empty)
// - Either just section content, represented by a list of content nodes (may be empty) OR a list of nested sub-sections, represented by a list of more ResumeSection items (may be empty)  
type ResumeSection = {
    Header: XmlNode list
    Annotation: XmlNode list
    Content: SectionContent
}
and SectionContent =
    | ResumeSection of ResumeSection list
    | Content of XmlNode list

// A list of potentially nested resume sections will be converted into a flattened list of page sections
//
// Each page section will translate into a div with potentially two sub-containers: a content div and an optional annotation aside
// Conceptually, each page section is a table row (and is rendered as `display: table-row`)
// A section header will end up with its own PageSection since the header is visually a row
// this breaks some of the semantic HTML model but it lets us display annotations as table cells which provides some nice properties
//
// For bookkeeping, the PageSection also contains an ID and an indentation level (used by subsections)
type PageSection = {
    PageContent: XmlNode list
    AnnotationContent: XmlNode list
    IndentationLevel: int
    SectionId: System.Guid
}

 // converts a list of potentially-nested ResumeSections into a flattened list of PageSections. 
let rec buildPageSections (acc: PageSection list) (currentIndentation: int) (remaining: ResumeSection list) =
    match remaining with
    | next::rest ->
        let sectionId = System.Guid.NewGuid()
        match next.Content with
        | Content contentNodes ->
            // only append headers if there were some
            let acc =
                if next.Header.Length > 0 then
                    let headerContainer = [ div [ _class "header-container" ] next.Header ]
                    { PageContent = headerContainer; AnnotationContent = []; IndentationLevel = currentIndentation; SectionId = sectionId }::acc
                else acc
            
            // only append content if there were some
            let acc =
                if contentNodes.Length > 0 then
                    { PageContent = contentNodes; AnnotationContent = next.Annotation; IndentationLevel = currentIndentation; SectionId = sectionId }::acc
                else acc
            
            buildPageSections acc currentIndentation rest
        | ResumeSection subSection ->

            let contentSections = buildPageSections [] (currentIndentation + 1) subSection
            
            let acc =
                if next.Header.Length > 0 then
                    let headerContainer = [ div [ _class "header-container" ] next.Header ]
                    { PageContent = headerContainer; AnnotationContent = next.Annotation; IndentationLevel = currentIndentation; SectionId = sectionId }::acc
                else acc
                    
            let acc = List.append contentSections acc
            
            buildPageSections acc currentIndentation rest
    | [] -> acc
    
let resumeContent =
    /// helper function to wrap a string in a paragraph tag
    let s s' = p [] [ encodedText s' ]
    let keyValueListItem heading detail = li [] [
        span [ _class "list-item-heading" ] [ encodedText heading ]
        encodedText " — "
        span [ _class "list-item-detail"] [ encodedText detail ]
    ]
    let pageBreak = div [ _class "page-break" ] []
    
    let buildResumeContent (sections: ResumeSection list) : XmlNode list =
        let pageSections = buildPageSections [] 0 sections
        
        pageSections
        |> List.rev
        |> List.map (
            fun pageSection ->
                let idAttr = (_data "sid" (pageSection.SectionId.ToString().Replace("-", "").ToLowerInvariant()))
                
                div [ (_class $"section-row indentation-{pageSection.IndentationLevel}"); idAttr ] [
                    div [ (_class $"section-content"); idAttr ] pageSection.PageContent
                    if pageSection.AnnotationContent.Length > 0 then
                        aside [ (_class "no-print"); idAttr ] pageSection.AnnotationContent
                ])

        
    let sections =
        [
            {
                Header = [ header [] [ encodedText "About" ] ]
                Annotation =  [ s "The best part about this industry is that there's always something new to learn, and the
                                       future is always bright and expansive."
                                s "I don't know if my next project will involve web backends, JavaScript frontends, or mobile
                                       clients — but I am certain it will be exciting!"]
                Content = Content [ s "James is a software architect and senior developer in Charleston, SC. He has eighteen years of experience designing, building,
                                       and supporting an automated service delivery and cloud provisioning system for hundreds of microservice deployments in Azure
                                       and, before that, an enterprise-scale CRM and transaction system on a .NET and Microsoft SQL Server tech stack"
                               
                                    s "He is interested in building new products on up-and-coming technologies, modernizing older systems to take advantage of recent advances, 
                                       exploring the future of cloud deployments, and solving interesting problems alongside a
                                       passionate team"]
            }
            
            {
                Header = [ header [] [ encodedText "Key Skills" ] ]
                Annotation = []
                Content = Content [ ul [] [
                                            keyValueListItem "DevOps" "Kubernetes, Istio, Docker, Azure PaaS, Azure DevOps Pipelines, PowerShell & bash"
                                            keyValueListItem ".NET" "C#, F#, ASP.Net Core WebAPI, distributed and parallel programming"
                                            keyValueListItem "Web" "Typescript & JavaScript"
                                            keyValueListItem "Databases" "SQL Server, Azure CosmosDB, performance design, query optimization"]]
            }
            
            {
                Header = [ header [] [ encodedText "Work History" ] ]
                Annotation = []
                Content = ResumeSection
                    [
                        {
                            Header = [ header [] [
                                            encodedText "Blackbaud, Inc."
                                            div [ _class "section-summary" ] [
                                                s "Platform Engineering Team Architect, Laureate Software Engineer"
                                                s "June 2005 — Present"
                                            ]]]
                            Annotation = []
                            Content = ResumeSection
                                        [
                                         {
                                            Header = [ header [ _class "level-two" ] [ encodedText "Current Role" ] ]
                                            Annotation = [  s "My current team was chartered to create a new system for deploying and maintaining the infrastructure
                                                               for all of Blackbaud's next-gen products and capabilities."
    
                                                            s "On this team, I focus on combining Azure's PaaS building blocks with our own internal automation to
                                                               allow other dev teams to quickly and easily deploy new microservices to production with all of the
                                                               obvious NFRs around availability, monitoring, security, and compliance."
    
                                                            s "As the industry evolves, we've been adapting our own approaches to make best use of the technologies available.
                                                               I'm currently in the process of designing an approach to move our current Azure resource deployment automation
                                                               to an IaC approach based on Pulumi and Azure Pipelines. I've previously assisted in architecting and implementing
                                                               a shift in our compute platform from Azure App Services web apps to a Kubernetes platform based on Azure's AKS."
                                                               
                                                            s "While it's a good thing to take advantage of new technical opportunities, these shifts cannot be allowed to  impact
                                                               the productivity of other developers who depend on the platform my team provides. We therefore take care to ensure that migrations
                                                               are largely painless and require minimal or no effort from our consuming teams. This is done via base libraries we
                                                               maintain along with carefully-planned testing and automated migration steps to minimize impact."
    
                                                            s "With our highly-standardized implementations, my small team is able to provide on-call support
                                                               for all infrastructure issues any of our services encounter. During these incidents, I jump from
                                                               technical troubleshooting and repair activities, coordination between various internal groups,
                                                               and liaising with Azure support personnel."
                                                           
                                                            s "Even aside from on-call support, I also spend a large amount of time working with developers across the company
                                                               to understand their needs, help them solve their problems more effectively with Azure and the platform we've built,
                                                               understand ways that our platform should evolve to support them better, and provide troubleshooting and operational
                                                               guidance throughout the day. Working with internal customers means that I know my users and can be responsive to
                                                               their needs." ]
                                            Content = Content [
                                                                    s "Technical lead and architect of the Platform Engineering team charged with enabling and empowering other dev teams to
                                                                       quickly and efficiently leverage Azure's PaaS deployment options"
                                                                       
                                                                    s "Currently designing and implementing a PCI DSS-Compliant IaC-based compute platform for continuously deployed microservices, centered around Kubernetes"
                                                                    
                                                                    s "Currently leading an effort to migrate existing microservice-based deployment automation to a new IaC model based on Pulumi"
                                                                    
                                                                    s "Designed and implemented a DDoS protection solution for Kubernetes deployments in Azure"
                                                                    
                                                                    s "Assisted with the design and led the implementation of a transition from company-wide Azure
                                                                       App Service deployments to Kubernetes-based deployments using Azure AKS"
                                                                    
                                                                    s "On-call and subject matter expert for Kubernetes implementation and operational issues"
                                                                    
                                                                    s "Architectural consultant and subject matter expert for any
                                                                       team wanting to go forward with new Azure technologies" ]
                                         }
                                
                                         {
                                            Header = [ header [ _class "level-two" ] [ encodedText "Previous Roles" ] ]
                                            Annotation = [  s "Previously, as the team lead for the Enterprise Platform Team I focused on the foundational
                                                               platform for Blackbaud's Enterprise CRM product. I was the front line for my team when dealing
                                                               with thorny production issues that ranged from helping our hosting services team diagnose
                                                               environmental problems to providing developer support for customer-written customizations on
                                                               top of our platform."
    
                                                            s "In this role, I worked closely with our product support and product management teams to ensure
                                                               that I was always providing a customer-focused solution whether I was fixing bugs across multiple
                                                               product versions or recommending configuration changes to internal customer environments." ]

                                            Content = Content [ s "Technical lead on the team charged with building and maintaining the framework
                                                                   and platform for an enterprise CRM solution"
                                                                   
                                                                s "Maintained a powerful ad-hoc query engine, implementing multiple performance improvements 
                                                                   and feature enhancements along the way"
                                                                   
                                                                s "Added dozens of features and resolved hundreds of bugs in legacy
                                                                   areas of the system"
                                                                   
                                                                s "Modernized legacy subsystems by adopting new authentication methods and new .NET async patterns" ]}
                                        ]
                        }
                    ]
            }
            
            {
                Header = [ header [] [ encodedText "Side Projects" ] ]
                Annotation = [
                    s "My hobbies and side projects are great ways to explore new technologies and techniques that aren't always directly applicable at work."

                    s "From learning functional programming to experimenting with non-relational cloud-based data stores, I'm never bored at home."

                    s "By taking a broad survey of the current practices in the industry during my off-hours, I'm able to recognize new opportunities and possibilities when on the clock."
                ]
                Content = Content [
                    p [] [ encodedText "A subject-centered "
                           a [ _href "https://github.com/willia4/jw_muscadine/blob/main/README.md" ] [ encodedText "microblogging platform and personal website written in F#" ]
                           encodedText " using a Mongo datastore, deployed via Docker" ]
                    
                    p [] [
                        a [ _href "https://github.com/willia4/jw_muscadine/blob/main/src/ElectricLemur.Muscadine.Site/Resume.fs" ] [ encodedText "This resume" ]
                        encodedText ", also written in F# as a specific page of the above personal website"
                    ]
                    
                    p [] [ encodedText "A suite of Docker containers ("
                           a [ _href "https://github.com/willia4/electriclemur-v4-bootstrap" ] [ encodedText "and orchestration scripts" ]
                           encodedText ") hosting multiple websites behind a Traefik frontend, deployed automatically via GitHub action pipelines"]

                    p [] [ encodedText "Other projects include: "
                           ul [] [
                                li [] [ encodedText "a Windows UWP C# app for managing a karaoke library and playing CD+G files" ]
                                li [] [ encodedText "a Ruby and Sinatra web app for managing a book club" ]
                                li [] [ encodedText "an Objective-C and Cocoa macOS app for generating secure & memorable passwords" ]
                                li [] [ encodedText "a PHP webapp for displaying photographs" ]
                                li [] [ encodedText "miscellaneous scripts and utilities as needed in bash, PowerShell, TypeScript, C#, F#, C++, etc." ]
                           ]]
                ]
            }
            
            {
                Header = [ header [] [ encodedText "Education" ]]
                Annotation = []
                Content = ResumeSection [
                    {
                        Header = [ header [] [
                                            encodedText "Clemson University"
                                            div [ _class "section-summary" ] [
                                                s "August 2001 — May 2005"
                                            ]]]
                        Annotation = []
                        Content = Content [
                            ul [] [
                                li [] [ (encodedText "BS in Computer Science, "); i [] [ encodedText "Cum Laude" ] ]
                                li [] [ encodedText "Minor in Mathematics" ]
                            ]
                        ]
                    }
                ]
            }
            
            {
                Header = [ header [] [ encodedText "Contact" ] ]
                Annotation = [
                    s "I can't wait to hear from you!"
                ]
                Content = Content [
                    ul [] [
                        li [] [ encodedText "843.323.0627" ]
                        li [] [ a [ _href "mailto:james@jameswilliams.me" ] [ encodedText "james@jameswilliams.me" ] ]
                        li [] [ a [ _href "https://jameswilliams.me" ] [encodedText "https://jameswilliams.me" ]]
                    ]
                    
                    div [ _class "print-only updated-resume" ] [
                        a [ _href "https://jameswilliams.me/resume" ] [ encodedText "A current and annotated version of this resume is available at https://jameswilliams.me/resume" ]
                    ]
                ]
            }
        ]

    [
        main [ _class "page-content resume" ] (buildResumeContent sections)
        footer [ _class "no-print" ] [
            header [] [ encodedText "Download" ]
            ul [] [
                li [] [ a [ _href "https://jameswilliams.me/resume/james_williams_resume.pdf" ] [  encodedText "PDF Format"] ]
                li [] [ a [ _href "https://jameswilliams.me/resume/james_williams_resume.txt" ] [  encodedText "Text File Format"] ]
            ]
        ]
    ]
    
let GET : HttpHandler =
    fun next ctx -> task {
        
        let customHeader = [
            div [ (_id "custom-header"); (_class "print-only") ] [
                div [] [ encodedText "843.323.0627" ]
                div [] [ encodedText "james@jameswilliams.me" ]
            ]
        ]
        let page = layout
                       (PageDefinitions.Page.Custom ("resume", "Resume of James Williams", "Resume", PageDefinitions.Page.AboutMe, customHeader))
                       resumeContent
                       [ PageExtra.CSS "frontend/resume.scss"
                         PageExtra.JavaScript "resume.js" ]
                       ctx
                   
        return! htmlView page next ctx
    }
     