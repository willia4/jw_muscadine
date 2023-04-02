module ElectricLemur.Muscadine.Site.Resume
open FormFields
open Giraffe
open Giraffe.ViewEngine
open ElectricLemur.Muscadine.Site.FrontendHelpers

type ResumeSection = {
    Header: XmlNode list
    Annotation: XmlNode list
    Content: SectionContent
}
and SectionContent =
    | ResumeSection of ResumeSection list
    | Content of XmlNode list

type PageSection = {
    PageContent: XmlNode list
    AnnotationContent: XmlNode list
    IndentationLevel: int
    SectionId: System.Guid
}
    
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
            
        
        let pageSections = buildPageSections [] 0 sections
        
        pageSections
        |> List.rev
        |> List.map (
            fun pageSection ->
                let idAttr = (_data "sid" (pageSection.SectionId.ToString().Replace("-", "").ToLowerInvariant()))
                
                div [ (_class "section-row"); idAttr ] [
                    div [ (_class $"section-content indentation-{pageSection.IndentationLevel}"); idAttr ] pageSection.PageContent
                    if pageSection.AnnotationContent.Length > 0 then
                        aside [ idAttr ] pageSection.AnnotationContent
                ])

        
    let sections =
        [
            {
                Header = [ header [] [ encodedText "About" ] ]
                Annotation =  [ s "The best part about this industry is that there's always something new to learn, and the
                                       future is always bright and expansive."
                                s "I don't know if my next project will involve web backends, Javascript frontends, or mobile
                                       clients — but I am certain it will be exciting!"]
                Content = Content [ s "James is a software architect and senior developer in Charleston, SC. He has eighteen years of experience designing, building,
                                       and supporting an automated service delivery and cloud provisioning system for hundreds of microservice deployments in Azure
                                       and, before that, an enterprise-scale CRM and transaction system on a .NET and Microsoft SQL Server tech stack"
                               
                                    s "He is interested in building new products on up-and-coming technologies, exploring
                                       the future of cloud deployments, and solving interesting problems alongside a
                                       passionate team"]
            }
            
            {
                Header = [ header [] [ encodedText "Key Skills" ] ]
                Annotation = []
                Content = Content [ ul [] [
                                            keyValueListItem "DevOps" "Kubernetes, Istio, Docker, Azure PaaS, Azure DevOps Pipelines, PowerShell & bash"
                                            keyValueListItem ".NET" "C#, F#, ASP.Net Core, distributed and parallel programming"
                                            keyValueListItem "Web" "Typescript & Javascript"
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
                                                s "Application Architect, Laureate Software Engineer"
                                                s "June 2005 — Present"
                                            ]]]
                            Annotation = []
                            Content = ResumeSection
                                        [
                                         {
                                            Header = [ header [ _class "level-two" ] [ encodedText "Current Role" ] ]
                                            Annotation = [  s "My current team was chartered to create a new system for deploying and maintaining the infrastructure
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
                                                               product versions or recommending configuration changes to internal customer environments."]
                                            Content = Content [     s "Designed and implemented a DDoS protection solution for Kubernetes deployments in Azure"
                                                                    s "Assisted with the design and led the implementation of a transition from company-wide Azure
                                                                       App Service deployments to Kubernetes-based deployments using Azure AKS"
                                                                    s "Lead of the DevOps team charged with enabling and empowering teams to
                                                                       quickly leverage Azure's PaaS deployment options"
                                                                    s "First-line architectural consultant and subject matter expert for any
                                                                       team wanting to go forward with new Azure technologies" ]
                                         }
                                
                                         {
                                            Header = [ header [ _class "level-two" ] [ encodedText "Previous Role" ] ]
                                            Annotation = []
                                            Content = Content [ s "Lead on the team charged with building and maintaining the framework
                                                                   platform for an enterprise CRM solution"
                                                                s "Made many performance improvements and feature enhancements to a
                                                                   powerful ad-hoc query engine"
                                                                s "Added dozens of features and resolved hundreds of bugs in legacy
                                                                   areas of the system"]}
                                        ]
                        }
                    ]
            }
        ]

    [
        main [ _class "page-content resume" ] (buildResumeContent sections)
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
     