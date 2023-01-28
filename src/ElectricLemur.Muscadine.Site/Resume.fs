module ElectricLemur.Muscadine.Site.Resume
open Giraffe
open Giraffe.ViewEngine
open System.Net.Http
open ElectricLemur.Muscadine.Site.Frontend

let resumeContent =
    /// helper function to wrap a string in a paragraph tag
    let s s' = p [] [ encodedText s' ]
    let keyValueListItem heading detail = li [] [
        span [ _class "list-item-heading" ] [ encodedText heading ]
        encodedText " — "
        span [ _class "list-item-detail"] [ encodedText detail ]
    ]
    let pageBreak = div [ _class "page-break" ] []

    [
        main [ _class "page-content resume" ] [
            section [] [
                header [] [ encodedText "About" ]
                div [] [
                   s "A senior developer with fifteen years of experience building out
                                       and supporting enterprise-scale CRM and transaction systems on Microsoft
                                       stacks while tackling issues like data modeling, reliability,
                                       customizability, and performance"
                   
                   s "Interested in building new products on up-and-coming technologies, exploring
                                       the future of cloud deployments, and pursuing excellence while surrounded by a
                                       passionate team"
                ]
            ]
            
            section [] [
                header [] [ encodedText "Key Skills" ]
                div [] [
                    ul [] [
                        keyValueListItem "DevOps" "Azure PaaS, Azure DevOps Pipelines, PowerShell & bash, Docker, Kubernetes"
                        keyValueListItem ".NET" "C#, ASP.Net Core, async and parallel programming"
                        keyValueListItem "Microsoft SQL Server" "performance tuning, advanced T-SQL, schema design"
                        keyValueListItem "Web" "Typescript & Javascript"
                    ]
                ]
            ]
            
            section [] [
                header [] [encodedText "Work History" ]
                div [] [
                    section [ _class "sub-section" ] [
                        header [] [ encodedText "Blackbaud, Inc." ]
                        div [ _class  "section-summary" ] [
                            s "Laureate Software Engineer"
                            s "June 2005 — Present"
                        ]
                        
                        header [ _class "sub-header" ] [ s "Highlights" ]
                        div [] [
                            s "Lead of the DevOps team charged with enabling and empowering teams to
                               quickly leverage Azure's PaaS deployment options"
                            s "First-line architectural consultant and subject matter expert for any
                               team wanting to go forward with new Azure technologies"
                            s "Lead on the team charged with building and maintaining the framework
                               platform for an enterprise CRM solution"
                            s "Made many performance improvements and feature enhancements to a
                               powerful ad-hoc query engine"
                            s "Added dozens of features and resolved hundreds of bugs in legacy
                               areas of the system"
                        ]
                    ]
                ]
            ]
            
            pageBreak

            section [] [
                header [] [ encodedText "Side Projects" ]
                div [] [
                    s "A series of personal websites hosted as a set of Docker containers on a major cloud
                       provider; both the infrastructure and sites are deployed and maintained via a custom CLI tool"
                    s "A website for organizing a book club — built on Ruby and Sinatra using Amazon Web Services
                       and Redis for data storage"
                    s "Mac and iOS apps for generating secure, memorable passwords — built in Objective-C and Swift"
                    s "A \"branded\" website for displaying photos and albums from a 3rd-party photo hosting service
                       — built in PHP with the SmugMug API for data storage"
                    s "This resume is implemented in F# using the Giraffe view engine and hosted in an aforementioned
                       Docker container"
                ]
            ]
            
            section [] [
                header [] [ encodedText "Education" ]
                div [] [
                    section [ _class "sub-section" ] [
                        header [] [ encodedText "Clemson University" ]
                        div [ _class "section-summary" ] [
                            s "August 2001 — May 2005"
                        ]
                        ul [] [
                            li [] [ encodedText "BS in Computer Science" ]
                            li [] [ encodedText "Minor in Mathematics" ]
                        ]
                    ]
                ]
            ]
            
            section [] [
                header [] [ encodedText "Contact" ]
                div [] [
                    ul [] [
                        li [] [ encodedText "843.323.0627" ]
                        li [] [ (a [ _href "mailto:james@jameswilliams.me" ] [ encodedText "james@jameswilliams.me" ]) ]
                        li [] [ (a [ _href "https://jameswilliams.me" ] [ encodedText "https://jameswilliams.me" ]) ]
                    ]
                ]
            ]
            
            aside [ _class "no-print download" ] [
                header [] [ encodedText "Download" ]
                ul [] [
                    li [] [ a [ _href "https://jameswilliams.me/resume/james_williams_resume.pdf" ] [  encodedText "PDF Format"] ]
                    li [] [ a [ _href "https://jameswilliams.me/resume/james_williams_resume.txt" ] [  encodedText "Text File Format"] ]
                ]
            ]
        ]
    ]

let GET : HttpHandler =
    fun next ctx -> task {
        
        let page = FrontendHelpers.layout
                       (FrontendHelpers.PageDefinitions.Page.Custom ("resume", "Resume of James Williams", "Resume", FrontendHelpers.PageDefinitions.Page.AboutMe))
                       resumeContent
                       [ "frontend/resume.scss" ]
                       ctx
                   
        return! htmlView page next ctx
    }
     