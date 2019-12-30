module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, rel, src)
import Html.Events.Extra.Mouse exposing (onClick)
import Task
import Time exposing (..)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { siteTitle : String
    , pageTitle : String
    , currentPage : Page
    , currentTime : Time.Posix
    , timeZone : Time.Zone
    , portfolioModalOpened : Bool
    , portfolioModalSelected : Maybe PortfolioEntryID
    , portfolioContent : List PortfolioEntry
    }


type alias PortfolioEntry =
    { id : PortfolioEntryID
    , title : String
    , briefContent : String
    , detailContent : String
    , imageUrl : Maybe String
    }


type Page
    = Home
    | Resume
    | Portfolio


type Msg
    = Modal (Maybe PortfolioEntryID)
    | Nav Page
    | NewTime Time.Posix


type PortfolioEntryID
    = Coral
    | Buswell
    | HideModal


initialModel : Model
initialModel =
    { siteTitle = "Jeffrey Mudge"
    , pageTitle = "Home"
    , currentPage = Portfolio
    , currentTime = Time.millisToPosix 0
    , timeZone = utc
    , portfolioModalOpened = False
    , portfolioModalSelected = Nothing
    , portfolioContent = [ { id = Coral, title = "CORAL-ERM", briefContent = "Coral is great", detailContent = loremIpsem, imageUrl = Just "./images/CORALLandingPage.png" }, { id = Buswell, title = "Wheaton College Library Website", briefContent = "website work!", detailContent = "Drupal! So easy :)", imageUrl = Nothing } ]
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel, getNewTime )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nav page ->
            case page of
                Home ->
                    ( { model | currentPage = page, pageTitle = "Home", portfolioModalOpened = False, portfolioModalSelected = Nothing }, Cmd.none )

                Resume ->
                    ( { model | currentPage = page, pageTitle = "Résumé", portfolioModalOpened = False, portfolioModalSelected = Nothing }, Cmd.none )

                Portfolio ->
                    ( { model | currentPage = page, pageTitle = "Portfolio" }, Cmd.none )

        NewTime time ->
            ( { model | currentTime = time }, Cmd.none )

        Modal portfolioEntry ->
            if model.currentPage == Portfolio then
                case portfolioEntry of
                    Just Coral ->
                        ( { model | portfolioModalOpened = True, portfolioModalSelected = Just Coral }, Cmd.none )

                    Just Buswell ->
                        ( { model | portfolioModalOpened = True, portfolioModalSelected = Just Buswell }, Cmd.none )

                    Nothing ->
                        ( { model | portfolioModalOpened = False, portfolioModalSelected = Nothing }, Cmd.none )

                    Just HideModal ->
                        ( { model | portfolioModalOpened = False }, Cmd.none )

            else
                ( model, Cmd.none )


getNewTime : Cmd Msg
getNewTime =
    Task.perform NewTime Time.now



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = model.siteTitle ++ " | " ++ model.pageTitle
    , body =
        [ viewBody model
        ]
    }


viewBody : Model -> Html Msg
viewBody model =
    Html.div
        [ class "wrapper" ]
        [ viewHeader model
        , viewMenu model.currentPage
        , viewContent model model.currentPage
        , viewFooter model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "header" ]
        [ node "link" [ rel "stylesheet", href "/css/style.css" ] []
        , viewTitle model
        ]


viewTitle : Model -> Html Msg
viewTitle model =
    case model.currentPage of
        Home ->
            div [] [ h1 [] [ text model.siteTitle ] ]

        _ ->
            div [] [ h1 [] [ text (model.siteTitle ++ " | " ++ model.pageTitle) ] ]


viewMenu : Page -> Html Msg
viewMenu cp =
    Html.div [ class "menu" ]
        [ ol []
            [ li
                [ onClick (\event -> Nav Home)
                , if cp == Home then
                    class "active"

                  else
                    class "inactive"
                ]
                [ text "Home" ]
            , li
                [ onClick (\event -> Nav Resume)
                , if cp == Resume then
                    class "active"

                  else
                    class "inactive"
                ]
                [ text "Résumé" ]
            , li
                [ onClick (\event -> Nav Portfolio)
                , if cp == Portfolio then
                    class "active"

                  else
                    class "inactive"
                ]
                [ text "Portfolio" ]
            ]
        ]


viewContent : Model -> Page -> Html Msg
viewContent model page =
    let
        content =
            case page of
                Home ->
                    viewHomePage

                Resume ->
                    viewResumePage

                Portfolio ->
                    viewPortfolioPage model
    in
    Html.div [ class "mainContent" ] [ content ]


viewFooter : Model -> Html Msg
viewFooter model =
    Html.div [ class "footer" ]
        [ viewCopyright (Time.toYear model.timeZone model.currentTime)
        , viewGitHubLink
        , viewEmailLink
        ]


viewCopyright : Int -> Html Msg
viewCopyright year =
    div [] [ text ("Copyright © " ++ String.fromInt year ++ " Jeffrey Mudge") ]



-- VIEW HTML MSG BLOCKS (no arguments)


viewHomePage : Html Msg
viewHomePage =
    div []
        [ div []
            [ h2 [] [ text "Introduction" ]
            , p []
                [ text "I’m a librarian and web developer who likes trying and learning new things. This site is built using the "
                , a
                    [ href
                        "https://elm-lang.org/"
                    ]
                    [ text "Elm programming language." ]
                ]
            ]
        ]


viewResumePage : Html Msg
viewResumePage =
    div [ class "resume" ]
        [ div [ class "education" ]
            [ h2 [] [ text "Education" ]
            , ol []
                [ li []
                    [ text "Indiana University, Bloomington, IN"
                    , ol []
                        [ li [] [ text "Master of Library Science, Dec. 2012" ]
                        , li [] [ text "Master of Information Science, Dec. 2012" ]
                        , li [] [ text "GPA 3.853" ]
                        ]
                    ]
                , li []
                    [ text "Taylor University, Upland, IN"
                    , ol []
                        [ li [] [ text "B.A. History, 2009" ]
                        , li [] [ text "B.A. International Studies (Middle East Concentration), 2009" ]
                        , li [] [ text "GPA 3.81" ]
                        ]
                    ]
                ]
            ]
        , div [ class "work" ]
            [ h2 [] [ text "Work Experience" ]
            , ol []
                [ li []
                    [ h3 []
                        [ text "Buswell Library, Wheaton College" ]
                    , h4 [] [ text "Digital Initiatives Coordinator" ]
                    , p
                        []
                        [ text "Collaborate with librarians and archivists to meet digital needs. Responsibilities include the main library website, digital archive platforms, and other public and internal facing digital solutions." ]
                    , p [] [ text "Wheaton, IL", br [] [], text "April 2013 - Present" ]
                    ]
                , li []
                    [ h3 [] [ text "Saddle Mountain Bible Church" ]
                    , h4 [] [ text "Web Development Consultant" ]
                    , p [] [ text "Created the information architecture and the site design. Built the website with a responsive design using PHP, HTML, CSS, and Javascript. The majority of the work was accomplished working long distance." ]
                    , p [] [ text "Mattawa, WA", br [] [], text "August 2012 - April 2013" ]
                    ]
                , li []
                    [ h3 [] [ text "Digital Library Program, Indiana University" ]
                    , h4 [] [ text "Variations Digitization Documentation Intern" ]
                    , p [] [ text "Evaluated and synthesized existing documentation. Redesigned and updated a website with the documentation for digitizers who utilize the Variations digital music library software." ]
                    , p [] [ text "Bloomington, IN", br [] [], text "August 2012 - December 2012" ]
                    ]
                , li []
                    [ h3 [] [ text "SLIS, Indiana University" ]
                    , h4 [] [ text "Teacher's Assistant" ]
                    , p [] [ text "Assisted the instructor and students during computer labs for the course S401: Computer-Based Information Tools" ]
                    , p [] [ text "Bloomington, IN", br [] [], text "Fall 2011 & Fall 2012" ]
                    ]
                , li []
                    [ h3 [] [ text "SLIS, Indiana University" ]
                    , h4 [] [ text "SLIS Lab Consultant" ]
                    , p [] [ text "Monitored equipment, assisted students, and provided first-line troubleshooting in the SLIS graduate student computer lab." ]
                    , p [] [ text "Bloomington, IN", br [] [], text "August 2010 - 2012" ]
                    ]
                , li []
                    [ h3 [] [ text "Digital Library Program, Indiana University" ]
                    , h4 [] [ text "TILE Project Software Documentation Writer" ]
                    , p [] [ text "Tested software, wrote bug reports, and created documentation for evolving sofware." ]
                    , p [] [ text "Bloomington, IN", br [] [], text "August 2010 - July 2011" ]
                    ]
                , li []
                    [ h3 [] [ text "Zondervan Library, Taylor University" ]
                    , h4 [] [ text "Information Technology Specialist" ]
                    , p [] [ text "Trained staff, maintained both hardware and software, collaborated in the implementation of a VMware solution for patron computing, and helped design the conversion of a computer lab into a collaboration commons." ]
                    , p [] [ text "Upland, IN", br [] [], text "September 2005 - August 2010" ]
                    ]
                , li []
                    [ h3 [] [ text "University Archives, Taylor University" ]
                    , h4 [] [ text "Archival Researcher" ]
                    , p [] [ text "Processed, preserved, and cataloged archival documents belonging to the Christian College Consortium (CCC)" ]
                    , p [] [ text "Upland, IN", br [] [], text "May - August 2008" ]
                    , p [] [ text "Member of an undergraduate research team which prepared a journal belonging to Bishop William Taylor (published 2010). Also participated in the digitization of a collection of documents and images associated with Progressive Era Reformers Monroe and Culla Vayhinger." ]
                    , p [] [ text "Upland, IN", br [] [], text "May - August 2007" ]
                    ]
                ]
            ]
        , div [ class "skills" ]
            [ h2 [] [ text "Skills" ]
            , ul []
                [ li [] [ text "HTML, CSS, PHP, JavaScript, Elm, Python" ]
                , li [] [ text "SQL, XML, JSON" ]
                , li [] [ text "Drupal, OJS" ]
                , li [] [ text "Wireframing and Sitemapping" ]
                , li [] [ text "Adobe Creative Suite & MS Office" ]
                , li [] [ text "CORAL-ERM" ]
                , li [] [ text "Unix" ]
                ]
            ]
        ]


viewPortfolioPage : Model -> Html Msg
viewPortfolioPage model =
    div []
        [ div [ class "portfolio" ]
            (List.map (viewPortfolioEntry model.portfolioModalSelected) model.portfolioContent)
        , div []
            [ div
                (if model.portfolioModalOpened then
                    [ class "modalfade", onClick (\event -> Modal (Just HideModal)) ]

                 else
                    []
                )
                []
            , div (viewModalActiveClasses model.portfolioModalOpened) (viewPortfolioModalCloseButton ++ List.map (viewPortfolioEntryDetailModal model.portfolioModalSelected) model.portfolioContent)
            ]
        ]


viewPortfolioModalCloseButton : List (Html Msg)
viewPortfolioModalCloseButton =
    [ button [ class "close", onClick (\event -> Modal (Just HideModal)) ] [ text "X" ] ]


viewPortfolioEntry : Maybe PortfolioEntryID -> PortfolioEntry -> Html Msg
viewPortfolioEntry modalID portfolioentry =
    let
        detailsOpened =
            if modalID == Just portfolioentry.id then
                [ class "detailsOpened" ]

            else
                []

        image =
            viewPortfolioEntryImage portfolioentry "thumbnail"
    in
    div ([ class "box", onClick (\event -> Modal (Just portfolioentry.id)) ] ++ detailsOpened)
        
         [image, h2 [] [ text portfolioentry.title ]
               , p []
                    [ text portfolioentry.briefContent
                    ]
               ]
        


viewPortfolioEntryImage : PortfolioEntry -> String ->Html Msg
viewPortfolioEntryImage portfolioentry imageType =
    case portfolioentry.imageUrl of
        Nothing ->
            div [] []

        Just imageUrl ->
            img [ src imageUrl, alt ("Illustration for " ++ portfolioentry.title), class imageType ] []


viewPortfolioEntryDetailModal : Maybe PortfolioEntryID -> PortfolioEntry -> Html Msg
viewPortfolioEntryDetailModal modalID portfolioentry =
    let
        image =
            viewPortfolioEntryImage portfolioentry "fullsize"
    in
    if modalID == Just portfolioentry.id then
        div [] [ h1 [] [ text portfolioentry.title ], image,  p [] [ text portfolioentry.detailContent ] ]

    else
        div [] []


viewGitHubLink : Html Msg
viewGitHubLink =
    div [] [ a [ href "https://github.com/jeffnm" ] [ text "GitHub" ] ]


viewEmailLink : Html Msg
viewEmailLink =
    div [] [ a [ href "mailto://jeffmudge+web@gmail.com" ] [ text "Email me" ] ]


viewModalActiveClasses : Bool -> List (Attribute Msg)
viewModalActiveClasses opened =
    if opened then
        [ class "activemodal", class "modal" ]

    else
        [ class "modal" ]


loremIpsem : String
loremIpsem =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi at consectetur erat, quis porta leo. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Praesent ac pharetra nisl, nec convallis risus. Praesent eu dolor eu nunc placerat euismod non ac arcu. Duis ultrices pretium lacus, sit amet tempor massa fermentum et. Nunc sed suscipit nunc. Nullam condimentum, justo nec ultrices mollis, neque nibh egestas erat, vel ullamcorper risus est eu tellus. Ut congue viverra porttitor. Sed rutrum, neque eu congue lobortis, leo augue dignissim diam, vitae suscipit dolor erat vel nisi. Sed convallis magna arcu, ut gravida ipsum vestibulum eu. Fusce vel orci ut urna interdum cursus. Nullam dignissim congue mauris sit amet scelerisque.\n\nMorbi ac erat a diam laoreet porta in vitae lectus. Nam eleifend lobortis velit, quis maximus risus tincidunt ac. Morbi eu odio sollicitudin, convallis mi sed, euismod purus. Maecenas vulputate, nisi eu viverra faucibus, risus turpis gravida sapien, sit amet fermentum urna urna sed dui. Fusce non nisi eget enim placerat mollis. In suscipit ut neque eget dapibus. Cras a magna diam. Aenean at sapien non nisl gravida fermentum non vitae mi. Nunc ornare arcu vitae libero interdum, sit amet vehicula arcu auctor. Vestibulum interdum ullamcorper nibh id condimentum. Curabitur id velit id tortor porta faucibus. Nunc varius est sed elit laoreet, gravida facilisis mi euismod. Suspendisse nec mattis enim.\n\nFusce venenatis non ipsum eu eleifend. Nulla vel elit a nisl viverra mattis et eget mauris. Maecenas semper vestibulum metus nec tristique. Cras viverra risus mi. Aenean ornare diam eget lacus laoreet, quis laoreet tellus hendrerit. Morbi vitae lectus faucibus, tempor ipsum sit amet, malesuada nisi. Vestibulum lacinia lectus id ipsum placerat, eget ultricies nibh pharetra. Aliquam cursus est urna, nec placerat lacus mattis fermentum. In consequat dui ac purus interdum, sed cursus ipsum euismod. Sed ut metus eget ipsum dapibus ullamcorper. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Suspendisse et mollis metus, in feugiat purus. Curabitur sodales sapien dictum nisi elementum, id ornare augue tincidunt. Donec imperdiet, lorem ut ultricies iaculis, urna sem facilisis magna, sed vehicula elit dui in purus. Nunc sed mauris et velit ultricies fringilla non sed ligula. Phasellus aliquam, lorem eu dictum fringilla, urna orci gravida dui, a dignissim eros augue in mauris. "
