module ElectricLemur.Muscadine.Site.Resume
open Giraffe
open Giraffe.ViewEngine
open ElectricLemur.Muscadine.Site.FrontendHelpers

let resumeContent =
    /// helper function to wrap a string in a paragraph tag
    let s s' = p [] [ encodedText s' ]
    let keyValueListItem heading detail = li [] [
        span [ _class "list-item-heading" ] [ encodedText heading ]
        encodedText " — "
        span [ _class "list-item-detail"] [ encodedText detail ]
    ]
    let pageBreak = div [ _class "page-break" ] []
    let annotation content = aside [ _class "no-print" ] content
    [
        main [ _class "page-content resume" ] [
            section [] [
                annotation [
                           s "The best part about this industry is that there's always something new to learn, and the
                              future is always bright and expansive."
                           s "I don't know if my next project will involve web backends, Javascript frontends, or mobile
                              clients — but I am certain it will be exciting!"
                          ]
                header [] [ encodedText "About" ]
                div [ _class "section-container" ] [
                    div [] [
                       s "James is a software architect and senior developer in Charleston, SC. He has eighteen years of experience designing, building,
                          and supporting an automated service delivery and cloud provisioning system for hundreds of microservice deployments in Azure
                          and, before that, an enterprise-scale CRM and transaction system on a .NET and Microsoft SQL Server tech stack"
                       
                       s "He is interested in building new products on up-and-coming technologies, exploring
                           the future of cloud deployments, and solving interesting problems alongside a
                           passionate team"
                    ]
                ]
            ]
            
            section [ ] [
                header [] [ encodedText "Key Skills" ]
                
                div [ _class "section-container" ] [
                    div [] [
                        ul [] [
                            keyValueListItem "DevOps" "Kubernetes, Istio, Docker, Azure PaaS, Azure DevOps Pipelines, PowerShell & bash"
                            keyValueListItem ".NET" "C#, F#, ASP.Net Core, distributed and parallel programming"
                            keyValueListItem "Web" "Typescript & Javascript"
                            keyValueListItem "Microsoft SQL Server" "performance tuning, advanced T-SQL, schema design"
                        ]
                    ]
                ]
            ]
            
            section [] [
                header [] [encodedText "Work History" ]
                div [ _class "section-container" ] [
                    div [] [
                        section [ _class "sub-section" ] [
                            aside [ ] [
                                s "My current team was chartered to create a new system for deploying and maintaining the infrastructure
                                   for all of our next-gen products and capabilities."
                          
                                s "On this team, I focus on combining Azure's PaaS building blocks with our own internal automation to
                                   allow service teams to quickly and easily deploy new microservices to production with all of the
                                   obvious NFRs around availability, monitoring, security, and compliance."

                                s "Recently, I've assisted in architecting a change in our service-hosting platform from Azure App
                                   Service web apps to a Kubernetes platform based on AKS."

                                s "With our highly-standardized implementations, my small team is able to provide on-call support
                                   for all infrastructure issues any of our services encounter. During these incidents, I jump from
                                   technical troubleshooting and repair activities, coordination between various internal groups,
                                   and liaising with Azure support personnel."
                                   
                                s "Previously, as the team lead for the Enterprise Platform Team I focused on the foundational
                                   platform for Blackbaud's Enterprise CRM product. I was the front line for my team when dealing
                                   with thorny production issues that ranged from helping our hosting services team diagnose
                                   environmental problems to providing developer support for customer-written customizations on
                                   top of our platform."

                                s "In this role, I worked closely with our product support and product management teams to ensure
                                   that I was always providing a customer-focused solution whether I was fixing bugs across multiple
                                   product versions or recommending configuration changes to internal customer environments."
                            ]

                            header [] [
                                encodedText "Blackbaud, Inc."
                                div [ _class  "section-summary" ] [
                                    s "Application Architect, Laureate Software Engineer"
                                    s "June 2005 — Present"
                                ]
                            ]
                            
                            div [ _class "section-container" ] [
                                header [ _class "sub-header" ] [ s "Highlights" ]
                                div [] [
                                    s "Designed and delivered a DDoS protection solution for Kubernetes deployments in Azure"
                                    s "Assisted with the design and led the implementation of a transition from company-wide Azure App Service deployments to Kubernetes-based deployments using Azure AKS"
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
                ]
            ]
            
            pageBreak

            section [] [
                annotation [
                    s "My hobbies and side projects are great ways to explore new technologies and techniques that aren't
                       always directly applicable at work."
                    s "From playing with interesting CoreAnimation methods in iOS to experimenting with non-relational
                       cloud-based data stores, I'm never bored at home." 
                    s "By taking a broad survey of the current practices in the industry during my off-hours, I'm able
                       to recognize new opportunities and possibilities when on the clock."

                    p [] [
                            encodedText "The source for this resume "
                            a [ _href "https://github.com/willia4/jw_muscadine/blob/main/src/ElectricLemur.Muscadine.Site/Resume.fs" ] [ encodedText "is available on GitHub." ]
                         ]
                ]

                header [] [ encodedText "Side Projects" ]
                div [ _class "section-container" ] [
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
            ]
            
            section [] [
                header [] [ encodedText "Education" ]
                div [ _class "section-container" ] [
                    div [] [
                        section [ _class "sub-section" ] [
                            div [ _class "section-container" ] [
                                header [] [
                                             encodedText "Clemson University"
                                             div [ _class "section-summary" ] [
                                                s "August 2001 — May 2005"
                                            ]
                                ]
                                
                                ul [] [
                                    li [] [ encodedText "BS in Computer Science" ]
                                    li [] [ encodedText "Minor in Mathematics" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            
            section [] [
                annotation [ s "I can't wait to hear from you!" ]
                header [] [ encodedText "Contact" ]
                div [ _class "section-container" ] [
                    div [] [
                        ul [] [
                            li [] [ encodedText "843.323.0627" ]
                            li [] [ (a [ _href "mailto:james@jameswilliams.me" ] [ encodedText "james@jameswilliams.me" ]) ]
                            li [] [ (a [ _href "https://jameswilliams.me" ] [ encodedText "https://jameswilliams.me" ]) ]
                        ]
                    ]
                ]
            ]
            
            footer [ _class "no-print download" ] [
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
        
        let page = layout
                       (PageDefinitions.Page.Custom ("resume", "Resume of James Williams", "Resume", PageDefinitions.Page.AboutMe))
                       resumeContent
                       [ PageExtra.CSS "frontend/resume.scss"
                         PageExtra.JavaScript "resume.js" ]
                       ctx
                   
        return! htmlView page next ctx
    }
     