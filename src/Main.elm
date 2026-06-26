module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Set exposing (Set)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = \model -> { title = "Ryuki Shimada", body = [ view model ] }
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type Theme
    = Light
    | Dark


type Page
    = MainPage
    | BibTeXPage String


type alias Model =
    { hoveredPaper : Maybe String
    , noGif : Set String
    , theme : Theme
    , page : Page
    , key : Nav.Key
    }


type Msg
    = MouseEnter String
    | MouseLeave
    | GifLoadFailed String
    | SetTheme Theme
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


pageFromUrl : Url -> Page
pageFromUrl url =
    case url.fragment of
        Just "cite/frobt2024" ->
            BibTeXPage bibtexFrobt2024

        Just "cite/ists2023" ->
            BibTeXPage bibtexIsts2023

        _ ->
            MainPage


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { hoveredPaper = Nothing
      , noGif = Set.empty
      , theme = Dark
      , page = pageFromUrl url
      , key = key
      }
    , Cmd.none
    )



-- Generate image path from paper ID and extension


getImagePath : String -> String -> String
getImagePath paperId extension =
    "images/" ++ paperId ++ extension


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseEnter paperId ->
            ( { model | hoveredPaper = Just paperId }, Cmd.none )

        MouseLeave ->
            ( { model | hoveredPaper = Nothing }, Cmd.none )

        GifLoadFailed paperId ->
            ( { model | noGif = Set.insert paperId model.noGif }, Cmd.none )

        SetTheme theme ->
            ( { model | theme = theme }, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External url) ->
            ( model, Nav.load url )

        UrlChanged url ->
            ( { model | page = pageFromUrl url }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.page of
        MainPage ->
            div
                [ classList [ ( "container", True ), ( "dark", model.theme == Dark ) ] ]
                [ toggleBar model
                , headerSection model
                , publicationsSection model
                , fellowshipsSection
                , footerNote
                ]

        BibTeXPage content ->
            bibTeXView content


toggleBar : Model -> Html Msg
toggleBar model =
    div [ class "toggle-bar" ]
        [ span
            [ class "theme-option active"
            , onClick
                (if model.theme == Dark then
                    SetTheme Light

                 else
                    SetTheme Dark
                )
            ]
            [ text
                (if model.theme == Dark then
                    "☀"

                 else
                    "☽"
                )
            ]
        ]


headerSection : Model -> Html Msg
headerSection model =
    div [ class "header-section" ]
        [ div [ class "text-content" ]
            [ h1 [ class "name" ] [ text "Ryuki Shimada" ]
            , bioParagraph
            , div [ class "links" ]
                [ a [ href "https://github.com/rieuky" ] [ text "GitHub" ]
                , text " / "
                , a [ href "https://scholar.google.com/citations?user=Az5KGOYAAAAJ&hl=en" ] [ text "Google Scholar" ]
                , text " / "
                , a [ href "https://www.linkedin.com/in/ryuki-shimada-60790a16a/" ] [ text "LinkedIn" ]
                , text " / "
                , a [ href "mailto:ryukishimada218@keio.jp" ] [ text "Email" ]
                ]
            ]
        , div [ class "profile-image" ]
            [ img [ src "images/profile_20251121.png", alt "profile photo", class "profile-photo" ] [] ]
        ]


bioParagraph : Html Msg
bioParagraph =
    div []
        [ p []
            [ text "I am a Ph.D. student in the "
            , a [ href "https://www.srg.mech.keio.ac.jp/en/" ] [ text "Space Robotics Group" ]
            , text " at "
            , a [ href "https://www.st.keio.ac.jp/en/" ] [ text "Keio University" ]
            , text "."
            ]
        , p []
            [ text "My research goal is to build intelligent systems for field robots that understand, predict, and leverage physical interactions with the environment — so they can assess risks and navigate more safely and efficiently in extreme environments such as disaster sites and degraded infrastructure."
            ]
        , p []
            [ text "I hold two master’s-level degrees: an "
            , em [] [ text "M.S. in Engineering" ]
            , text " from Keio University and a "
            , em [] [ text "Diplôme d’Ingénieur" ]
            , text " from "
            , a [ href "https://www.ec-nantes.fr/engineering-programme-diplome-dingenieur/course-specialisations-yrs-23/robotics?l=1" ] [ text "École Centrale de Nantes" ]
            , text "."
            ]
        ]


publicationsSection : Model -> Html Msg
publicationsSection model =
    div [ class "publications-section" ]
        [ h2 [] [ text "Publications" ]
        , div [ class "paper" ]
            [ viewPaperImage model "frobt_2024"
            , div [ class "paper-info" ]
                [ a [ href "https://www.frontiersin.org/journals/robotics-and-ai/articles/10.3389/frobt.2024.1388634/full" ] [ span [ class "papertitle" ] [ text "Tangle- and Contact-free Path Planning for a Tethered Mobile Robot" ] ]
                , br [] []
                , strong [] [ text "Ryuki Shimada" ]
                , text " and Genya Ishigami"
                , br [] []
                , em [] [ text "Frontiers in Robotics and AI" ]
                , text ", 2024"
                , br [] []
                , a [ href "https://www.frontiersin.org/journals/robotics-and-ai/articles/10.3389/frobt.2024.1388634/full" ] [ text "Paper" ]
                , text " / "
                , a [ href "#cite/frobt2024", class "cite-link" ] [ text "Cite" ]
                , p [] [ text "A learning-based homotopy-aware path planning method for a tethered mobile robot to avoid cable-obstacles and cable-robot contacts while navigating to goals." ]
                ]
            ]
        , div [ class "paper" ]
            [ viewPaperImage model "ists_2023"
            , div [ class "paper-info" ]
                [ a [ href "https://archive.ists.ne.jp/upload_pdf/2023-k-2-02.pdf" ] [ span [ class "papertitle" ] [ text "Path Planning with Cable-obstacles Avoidance for a Tethered Mobile Robot in Unstructured Environments" ] ]
                , br [] []
                , strong [] [ text "Ryuki Shimada" ]
                , text " and Genya Ishigami"
                , br [] []
                , em [] [ text "ISTS" ]
                , text ", 2023"
                , span [ class "oral-presentation" ] [ text " (Oral Presentation)" ]
                , br [] []
                , a [ href "https://archive.ists.ne.jp/upload_pdf/2023-k-2-02.pdf" ] [ text "Paper" ]
                , text " / "
                , a [ href "#cite/ists2023", class "cite-link" ] [ text "Cite" ]
                , p [] [ text "A path refinement method for a tethered mobile robot to avoid cable-obstacle contact by considering path curvature and distance from cable base." ]
                ]
            ]
        ]



-- Subscribe to error event on <img>


onError : msg -> Attribute msg
onError msg =
    on "error" (Decode.succeed msg)


viewPaperImage : Model -> String -> Html Msg
viewPaperImage model paperId =
    let
        isHovered =
            model.hoveredPaper == Just paperId

        gifSupported =
            not (Set.member paperId model.noGif)

        imgSrc =
            if isHovered && gifSupported then
                getImagePath paperId ".gif"

            else
                getImagePath paperId ".jpg"
    in
    div
        [ class "paper-image-container"
        , onMouseEnter (MouseEnter paperId)
        , onMouseLeave MouseLeave
        ]
        [ img
            ([ src imgSrc
             , alt
                (if isHovered then
                    paperId ++ " Animation"

                 else
                    paperId
                )
             , width 160
             , class "paper-image"
             ]
                ++ (if isHovered && gifSupported then
                        [ onError (GifLoadFailed paperId) ]

                    else
                        []
                   )
            )
            []
        ]



-- Simple list of fellowships and scholarships


fellowshipsSection : Html Msg
fellowshipsSection =
    div [ class "research-section" ]
        [ h2 [] [ text "Fellowships and Scholarships" ]
        , ul []
            [ li []
                [ strong [] [ a [ href "https://www.jsps.go.jp/english/e-pd/" ] [ text "JSPS Research Fellow for Young Scientists (DC2)" ] ]
                , text ", "
                , strong [] [ text "JPY 227K/month" ]
                , text ", "
                , text "Apr. 2026 – Mar. 2028"
                ]
            , li []
                [ strong [] [ text "The Keio University Doctorate Student Grant-in-Aid Program from Ushioda Memorial Fund 2026" ]
                , text ", "
                , strong [] [ text "JPY 100K" ]
                , text ", "
                , text "Apr. 2026 – Mar. 2027"
                ]
            , li []
                [ strong [] [ a [ href "https://www.jst.go.jp/jisedai/spring/en/index.html" ] [ text "JST Support for Pioneering Research Initiated by the Next Generation (SPRING)" ] ]
                , text ", "
                , strong [] [ text "JPY 183K/month" ]
                , text ", "
                , text "Apr. 2025 – Mar. 2026"
                ]
            , li []
                [ strong [] [ a [ href "https://jp.diplomatie.gouv.fr/fr/bourses-france-excellence" ] [ text "French Government Scholarship (Bourse France Excellence Japon)" ] ]
                , text ", "
                , strong [] [ text "EUR 700/month" ]
                , text ", "
                , text "Sep. 2019 – Sep. 2021"
                ]
            ]
        ]



-- Footer attribution


footerNote : Html Msg
footerNote =
    div [ class "footer-note" ]
        [ text "Written in "
        , a [ href "https://elm-lang.org" ] [ text "Elm · " ]
        , text "CSS: "
        , a [ href "https://github.com/jonbarron/jonbarron.github.io?tab=readme-ov-file" ] [ text "jonbarron/jonbarron.github.io" ]
        ]



-- BibTeX


bibtexFrobt2024 : String
bibtexFrobt2024 =
    """@article{shimada2024tangle,
  title={Tangle- and contact-free path planning for a tethered mobile robot using deep reinforcement learning},
  author={Shimada, Ryuki and Ishigami, Genya},
  journal={Frontiers in Robotics and AI},
  volume={11},
  pages={1388634},
  year={2024},
  publisher={Frontiers Media SA}
}"""


bibtexIsts2023 : String
bibtexIsts2023 =
    """@inproceedings{shimada2023path,
  title={Path planning with cable-obstacles avoidance for a tethered mobile robot in unstructured environments},
  author={Shimada, Ryuki and Ishigami, Genya},
  booktitle={Proceedings of the International Symposium on Space Technology and Science},
  year={2023}
}"""


bibTeXView : String -> Html Msg
bibTeXView content =
    div [ class "bibtex-raw" ]
        [ pre [] [ text content ] ]
