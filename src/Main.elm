module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, rel)
import Html.Events.Extra.Mouse as Mouse exposing (onClick)
import Task exposing (Task)
import Time exposing (..)


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
    }


type Page
    = Home
    | Resume
    | Portfolio


type Msg
    = Nav Page
    | NewTime Time.Posix


initialModel : Model
initialModel =
    { siteTitle = "Jeffrey Mudge"
    , pageTitle = "Home"
    , currentPage = Portfolio
    , currentTime = Time.millisToPosix 0
    , timeZone = utc
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
                    ( { model | currentPage = page, pageTitle = "Home" }, Cmd.none )

                Resume ->
                    ( { model | currentPage = page, pageTitle = "Résumé" }, Cmd.none )

                Portfolio ->
                    ( { model | currentPage = page, pageTitle = "Portfolio" }, Cmd.none )

        NewTime time ->
            ( { model | currentTime = time }, Cmd.none )


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
    Html.div [ class "wrapper" ]
        [ viewHeader model
        , viewMenu model.currentPage
        , viewContent model.currentPage
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


viewContent : Page -> Html Msg
viewContent page =
    let
        content =
            case page of
                Home ->
                    viewHomePage

                Resume ->
                    viewResumePage

                Portfolio ->
                    viewPortfolioPage
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


viewPortfolioPage : Html Msg
viewPortfolioPage =
    --div []
    --    [ div []
    --        [ h2 [] [ text "Portfolio" ]
    --        , div []
    --            [ p [] [ text "You can see some of the things I have worked on at ", a [ href "https://github.com/jeffnm" ] [ text "GitHub" ], text "." ]
    --            , p [] [ text "The ", a [ href "https://library.wheaton.edu" ] [ text "Buswell library website" ], text " at Wheaton College is also one of my major achievements." ]
    --            ]
    --        ]
    --    ]
    div [ class "portfolio" ]
        [ div [ class "box" ] [ text "Entry one" ]
        , div [ class "box" ] [ text "Entry two" ]
        , div [ class "box" ] [ text "Entry two" ]
        , div [ class "box" ] [ text "Entry two" ]
        , div [ class "box" ] [ text "Entry two" ]
        ]


viewGitHubLink : Html Msg
viewGitHubLink =
    div [] [ a [ href "https://github.com/jeffnm" ] [ text "GitHub" ] ]


viewEmailLink : Html Msg
viewEmailLink =
    div [] [ a [ href "mailto://jeffmudge+web@gmail.com" ] [ text "Email me" ] ]
