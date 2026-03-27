module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Language
    = English
    | Japanese


type Theme
    = Light
    | Dark


type alias Model =
    { hoveredPaper : Maybe String
    , noGif : Set String
    , language : Language
    , theme : Theme
    }


type Msg
    = MouseEnter String
    | MouseLeave
    | GifLoadFailed String
    | SetLanguage Language
    | SetTheme Theme


init : Model
init =
    { hoveredPaper = Nothing
    , noGif = Set.empty
    , language = English
    , theme = Dark
    }



-- Generate image path from paper ID and extension


getImagePath : String -> String -> String
getImagePath paperId extension =
    "images/" ++ paperId ++ extension


type alias StringContent =
    { publicationsHeading : String
    , fellowshipsHeading : String
    , paper1Description : String
    , paper2Description : String
    , oralPresentation : String
    , fellowship1Name : String
    , fellowship1Period : String
    , fellowship2Name : String
    , fellowship2Period : String
    , footerWrittenIn : String
    , footerCssCredit : String
    }


getStrings : Language -> StringContent
getStrings lang =
    case lang of
        English ->
            { publicationsHeading = "Publications"
            , fellowshipsHeading = "Fellowships and Scholarships"
            , paper1Description = "A learning-based homotopy-aware path planning method for a tethered mobile robot to avoid cable-obstacles and cable-robot contacts while navigating to goals."
            , paper2Description = "A path refinement method for a tethered mobile robot to avoid cable-obstacle contact by considering path curvature and distance from cable base."
            , oralPresentation = " (Oral Presentation)"
            , fellowship1Name = "JST Support for Pioneering Research Initiated by the Next Generation (SPRING)"
            , fellowship1Period = ", Apr. 2025 – Mar. 2026"
            , fellowship2Name = "French Government Scholarship (France Excellence Japon)"
            , fellowship2Period = ", Sep. 2019 – Sep. 2021"
            , footerWrittenIn = "Written in "
            , footerCssCredit = "CSS: "
            }

        Japanese ->
            { publicationsHeading = "論文"
            , fellowshipsHeading = "奨学金・フェローシップ"
            , paper1Description = "テザー付き移動ロボットがゴールへ向かう際に、ケーブルと障害物の接触およびケーブルとロボット本体の接触を回避するための、学習ベースのホモトピー考慮型経路計画手法。"
            , paper2Description = "経路の曲率とケーブル基点からの距離を考慮することで、テザー付き移動ロボットのケーブルと障害物の接触を回避する経路改良手法。"
            , oralPresentation = "（口頭発表）"
            , fellowship1Name = "JST次世代研究者挑戦的研究プログラム（SPRING）"
            , fellowship1Period = "、2025年4月 – 2026年3月"
            , fellowship2Name = "フランス政府奨学金（フランス・エクセレンス・ジャポン）"
            , fellowship2Period = "、2019年9月 – 2021年9月"
            , footerWrittenIn = "作成言語: "
            , footerCssCredit = "CSS参考: "
            }


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseEnter paperId ->
            { model | hoveredPaper = Just paperId }

        MouseLeave ->
            { model | hoveredPaper = Nothing }

        GifLoadFailed paperId ->
            { model | noGif = Set.insert paperId model.noGif }

        SetLanguage lang ->
            { model | language = lang }

        SetTheme theme ->
            { model | theme = theme }


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( "container", True )
            , ( "dark", model.theme == Dark )
            ]
        ]
        [ toggleBar model
        , headerSection model.language
        , publicationsSection model
        , fellowshipsSection model.language
        , footerNote model.language
        ]


toggleBar : Model -> Html Msg
toggleBar model =
    div [ class "toggle-bar" ]
        [ div [ class "theme-toggle" ]
            [ span
                [ classList
                    [ ( "theme-option", True )
                    , ( "active", model.theme == Light )
                    ]
                , onClick (SetTheme Light)
                ]
                [ text "☀" ]
            , span [ class "theme-separator" ] [ text " / " ]
            , span
                [ classList
                    [ ( "theme-option", True )
                    , ( "active", model.theme == Dark )
                    ]
                , onClick (SetTheme Dark)
                ]
                [ text "☽" ]
            ]
        ]


headerSection : Language -> Html Msg
headerSection lang =
    div [ class "header-section" ]
        [ div [ class "text-content" ]
            [ h1 [ class "name" ] [ text "Ryuki Shimada" ]
            , bioParagraph lang
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


bioParagraph : Language -> Html Msg
bioParagraph lang =
    case lang of
        English ->
            p []
                [ text "I'm a Ph.D. student at "
                , a [ href "https://www.st.keio.ac.jp/en/" ] [ text "Keio University" ]
                , text " in Greater Tokyo Area, advised by Prof. "
                , a [ href "https://www.st.keio.ac.jp/en/tprofile/mech/ishigami.html" ] [ text "Genya Ishigami" ]
                , text ". I am working on perception and planning in disaster-response robots. "
                , text "I received two Master's degrees: "
                , em [] [ text "M.S. in Engineering" ]
                , text " from Keio University and "
                , em [] [ text "Diplôme d'Ingénieur" ]
                , text " (Engineer's degree in France) from "
                , a [ href "https://www.ec-nantes.fr/engineering-programme-diplome-dingenieur/course-specialisations-yrs-23/robotics?l=1" ] [ text "Ecole Centrale de Nantes" ]
                , text ". I speak three languages: Japanese, English, and French, so feel free to address me in your preferred language."
                ]

        Japanese ->
            p []
                [ a [ href "https://www.st.keio.ac.jp/en/" ] [ text "慶應義塾大学" ]
                , text "（神奈川県）の博士課程に在籍し、"
                , a [ href "https://www.st.keio.ac.jp/en/tprofile/mech/ishigami.html" ] [ text "石上玄也" ]
                , text "教授の指導を受けています。災害対応ロボットの知覚・経路計画に関する研究を行っています。"
                , text "修士号を2つ取得しています：慶應義塾大学より"
                , em [] [ text "工学修士（M.S. in Engineering）" ]
                , text "、"
                , a [ href "https://www.ec-nantes.fr/engineering-programme-diplome-dingenieur/course-specialisations-yrs-23/robotics?l=1" ] [ text "エコール・サントラル・ド・ナント" ]
                , text "（フランス）より"
                , em [] [ text "ディプロム・ダンジェニウール（Diplôme d'Ingénieur）" ]
                , text "。日本語・英語・フランス語の3言語を話しますので、お好みの言語でお気軽にご連絡ください。"
                ]


publicationsSection : Model -> Html Msg
publicationsSection model =
    let
        s =
            getStrings model.language
    in
    div [ class "publications-section" ]
        [ h2 [] [ text s.publicationsHeading ]
        , div [ class "paper" ]
            [ viewPaperImage model "frobt_2023"
            , div [ class "paper-info" ]
                [ a [ href "https://www.frontiersin.org/journals/robotics-and-ai/articles/10.3389/frobt.2024.1388634/full" ] [ span [ class "papertitle" ] [ text "Tangle- and Contact-free Path Planning for a Tethered Mobile Robot" ] ]
                , br [] []
                , strong [] [ text "Ryuki Shimada" ]
                , text " and Genya Ishigami"
                , br [] []
                , em [] [ text "Frontiers in Robotics and AI" ]
                , text ", 2023"
                , br [] []
                , a [ href "https://www.frontiersin.org/journals/robotics-and-ai/articles/10.3389/frobt.2024.1388634/full" ] [ text "Paper" ]
                , p [] [ text s.paper1Description ]
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
                , span [ class "oral-presentation" ] [ text s.oralPresentation ]
                , br [] []
                , a [ href "https://archive.ists.ne.jp/upload_pdf/2023-k-2-02.pdf" ] [ text "Paper" ]
                , p [] [ text s.paper2Description ]
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


fellowshipsSection : Language -> Html Msg
fellowshipsSection lang =
    let
        s =
            getStrings lang
    in
    div [ class "research-section" ]
        [ h2 [] [ text s.fellowshipsHeading ]
        , ul []
            [ li []
                [ strong [] [ a [ href "https://www.jst.go.jp/jisedai/spring/en/index.html" ] [ text s.fellowship1Name ] ]
                , text ", "
                , strong [] [ text "JPY 188K/month" ]
                , text s.fellowship1Period
                ]
            , li []
                [ strong [] [ a [ href "https://jp.ambafrance.org/Bourses-France-Excellence-fr" ] [ text s.fellowship2Name ] ]
                , text ", "
                , strong [] [ text "EUR 700/month" ]
                , text s.fellowship2Period
                ]
            ]
        ]



-- Footer attribution


footerNote : Language -> Html Msg
footerNote lang =
    let
        s =
            getStrings lang
    in
    div [ class "footer-note" ]
        [ text s.footerWrittenIn
        , a [ href "https://elm-lang.org" ] [ text "Elm · " ]
        , text s.footerCssCredit
        , a [ href "https://github.com/jonbarron/jonbarron.github.io?tab=readme-ov-file" ] [ text "jonbarron/jonbarron.github.io" ]
        ]
