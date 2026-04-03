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
    | French
    | Japanese


type Theme
    = Light
    | Dark


type Page
    = Main
    | NotesList
    | NoteDetail String


type alias Model =
    { hoveredPaper : Maybe String
    , noGif : Set String
    , language : Language
    , theme : Theme
    , page : Page
    }


type Msg
    = MouseEnter String
    | MouseLeave
    | GifLoadFailed String
    | SetLanguage Language
    | SetTheme Theme
    | SetPage Page


init : Model
init =
    { hoveredPaper = Nothing
    , noGif = Set.empty
    , language = English
    , theme = Dark
    , page = Main
    }



-- Prevent default on anchor clicks used for internal navigation


onClickPage : Page -> Attribute Msg
onClickPage p =
    preventDefaultOn "click" (Decode.succeed ( SetPage p, True ))



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
    , fellowship3Name : String
    , fellowship3Period : String
    , fellowship1Amount : String
    , fellowship2Amount : String
    , fellowship3Amount : String
    , fellowshipSep : String
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
            , fellowship1Period = "Apr. 2025 – Mar. 2026"
            , fellowship2Name = "French Government Scholarship (France Excellence Japon)"
            , fellowship2Period = "Sep. 2019 – Sep. 2021"
            , fellowship3Name = "JSPS Research Fellow for Young Scientists (DC2)"
            , fellowship3Period = "Apr. 2026 – Mar. 2028"
            , fellowship1Amount = "JPY 183K/month"
            , fellowship2Amount = "EUR 700/month"
            , fellowship3Amount = "JPY 227K/month"
            , fellowshipSep = ", "
            , footerWrittenIn = "Written in "
            , footerCssCredit = "CSS: "
            }

        French ->
            { publicationsHeading = "Publications"
            , fellowshipsHeading = "Bourses et financements"
            , paper1Description = "A learning-based homotopy-aware path planning method for a tethered mobile robot to avoid cable-obstacles and cable-robot contacts while navigating to goals."
            , paper2Description = "A path refinement method for a tethered mobile robot to avoid cable-obstacle contact by considering path curvature and distance from cable base."
            , oralPresentation = " (Présentation orale)"
            , fellowship1Name = "Bourse JST pour la recherche pionnière de la prochaine génération (SPRING)"
            , fellowship1Period = "avr. 2025 – mars 2026"
            , fellowship2Name = "Bourse du gouvernement français (France Excellence Japon)"
            , fellowship2Period = "sept. 2019 – sept. 2021"
            , fellowship3Name = "Chercheur associé JSPS pour jeunes scientifiques (DC2)"
            , fellowship3Period = "avr. 2026 – mars 2028"
            , fellowship1Amount = "JPY 183K/month"
            , fellowship2Amount = "EUR 700/month"
            , fellowship3Amount = "JPY 227K/month"
            , fellowshipSep = ", "
            , footerWrittenIn = "Écrit en "
            , footerCssCredit = "CSS : "
            }

        Japanese ->
            { publicationsHeading = "論文"
            , fellowshipsHeading = "奨学金・フェローシップ"
            , paper1Description = "A learning-based homotopy-aware path planning method for a tethered mobile robot to avoid cable-obstacles and cable-robot contacts while navigating to goals."
            , paper2Description = "A path refinement method for a tethered mobile robot to avoid cable-obstacle contact by considering path curvature and distance from cable base."
            , oralPresentation = "（口頭発表）"
            , fellowship1Name = "JST次世代研究者挑戦的研究プログラム（SPRING）"
            , fellowship1Period = "2025年4月 – 2026年3月"
            , fellowship2Name = "フランス政府奨学金"
            , fellowship2Period = "2019年9月 – 2021年9月"
            , fellowship3Name = "日本学術振興会特別研究員（DC2）"
            , fellowship3Period = "2026年4月 – 2028年3月"
            , fellowship1Amount = "月18.3万円"
            , fellowship2Amount = "月700ユーロ"
            , fellowship3Amount = "月22.7万円"
            , fellowshipSep = "，"
            , footerWrittenIn = "Written in: "
            , footerCssCredit = "CSS: "
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

        SetPage p ->
            { model | page = p }


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( "container", True )
            , ( "dark", model.theme == Dark )
            ]
        ]
        [ toggleBar model
        , case model.page of
            Main ->
                div []
                    [ headerSection model
                    , publicationsSection model
                    , fellowshipsSection model.language
                    , footerNote model.language
                    ]

            NotesList ->
                notesListView

            NoteDetail noteId ->
                noteDetailView noteId
        ]


toggleBar : Model -> Html Msg
toggleBar model =
    div [ class "toggle-bar" ]
        [ div [ class "lang-toggle" ]
            [ span
                [ classList [ ( "lang-option", True ), ( "active", model.language == English ) ]
                , onClick (SetLanguage English)
                ]
                [ text "EN" ]
            , span [ class "lang-separator" ] [ text " / " ]
            , span
                [ classList [ ( "lang-option", True ), ( "active", model.language == French ) ]
                , onClick (SetLanguage French)
                ]
                [ text "FR" ]
            , span [ class "lang-separator" ] [ text " / " ]
            , span
                [ classList [ ( "lang-option", True ), ( "active", model.language == Japanese ) ]
                , onClick (SetLanguage Japanese)
                ]
                [ text "日本語" ]
            ]
        , span
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
    let
        lang =
            model.language
    in
    div [ class "header-section" ]
        [ div [ class "text-content" ]
            [ h1 [ class "name" ] [ text "Ryuki Shimada" ]
            , bioParagraph lang
            , div [ class "links" ]
                ([ a [ href "https://github.com/rieuky" ] [ text "GitHub" ]
                 , text " / "
                 , a [ href "https://scholar.google.com/citations?user=Az5KGOYAAAAJ&hl=en" ] [ text "Google Scholar" ]
                 , text " / "
                 , a [ href "https://www.linkedin.com/in/ryuki-shimada-60790a16a/" ] [ text "LinkedIn" ]
                 , text " / "
                 , a [ href "mailto:ryukishimada218@keio.jp" ] [ text "Email" ]
                 ]
                    ++ (if lang == Japanese then
                            [ text " / "
                            , a [ onClickPage NotesList, href "#" ] [ text "雑記" ]
                            ]

                        else
                            []
                       )
                )
            ]
        , div [ class "profile-image" ]
            [ img [ src "images/profile_20251121.png", alt "profile photo", class "profile-photo" ] [] ]
        ]


bioParagraph : Language -> Html Msg
bioParagraph lang =
    case lang of
        English ->
            p []
                [ text "I am a Ph.D. student at the "
                , a [ href "https://www.srg.mech.keio.ac.jp/en/" ] [ text "Space Robotics Group" ]
                , text ", "
                , a [ href "https://www.st.keio.ac.jp/en/" ] [ text "Keio University" ]
                , text ", under the supervision of Prof. "
                , a [ href "https://www.st.keio.ac.jp/en/tprofile/mech/ishigami.html" ] [ text "Genya Ishigami" ]
                , text ". I study autonomous navigation of tethered mobile robots in unstructured environments such as disaster sites, using techniques from path planning, multimodal perception, and machine learning. I received two Master's degrees: "
                , em [] [ text "M.S. in Engineering" ]
                , text " from Keio University and "
                , em [] [ text "Diplôme d'Ingénieur" ]
                , text " from "
                , a [ href "https://www.ec-nantes.fr/engineering-programme-diplome-dingenieur/course-specialisations-yrs-23/robotics?l=1" ] [ text "École Centrale de Nantes" ]
                , text ". I speak three languages: Japanese, English, and French, so feel free to address me in your preferred language."
                ]

        French ->
            p []
                [ text "Je suis doctorant au "
                , a [ href "https://www.srg.mech.keio.ac.jp/en/" ] [ text "Space Robotics Group" ]
                , text " de l'"
                , a [ href "https://www.st.keio.ac.jp/en/" ] [ text "Université Keio" ]
                , text ", sous la direction du Pr. "
                , a [ href "https://www.st.keio.ac.jp/en/tprofile/mech/ishigami.html" ] [ text "Genya Ishigami" ]
                , text ". J'étudie la navigation autonome de robots mobiles téthérés dans des environnements non structurés tels que des sites de catastrophes, en m'appuyant sur la planification de trajectoires, la perception multimodale et l'apprentissage automatique. J'ai obtenu deux masters\u{00A0}: un "
                , em [] [ text "M.S. in Engineering" ]
                , text " de l'Université Keio et un "
                , em [] [ text "Diplôme d'Ingénieur" ]
                , text " de l'"
                , a [ href "https://www.ec-nantes.fr/engineering-programme-diplome-dingenieur/course-specialisations-yrs-23/robotics?l=1" ] [ text "École Centrale de Nantes" ]
                , text ". Je parle trois langues : le japonais, l'anglais et le français, n'hésitez donc pas à me contacter dans la langue de votre choix."
                ]

        Japanese ->
            p []
                [ text "災害現場などの非構造化環境でのテザーロボット（通信・電源ケーブルによって外部と有線接続された移動ロボット）の自律移動技術を研究しています。経路計画、マルチモーダル情報処理、機械学習・強化学習などの知見を活用しています。現在、"
                , a [ href "https://www.st.keio.ac.jp/en/" ] [ text "慶應義塾大学" ]
                , text "の "
                , a [ href "https://www.srg.mech.keio.ac.jp/" ] [ text "Space Robotics Group" ]
                , text " に博士学生として在籍しています（指導教員："
                , a [ href "https://www.st.keio.ac.jp/en/tprofile/mech/ishigami.html" ] [ text "石上玄也" ]
                , text "教授）。また、修士号を２つ所持しています："
                , em [] [ text "修士（工学）" ]
                , text "（慶應義塾大学）、"
                , em [] [ text "Diplôme d'Ingénieur" ]
                , text "（"
                , a [ href "https://www.ec-nantes.fr/engineering-programme-diplome-dingenieur/course-specialisations-yrs-23/robotics?l=1" ] [ text "エコール・サントラル・ナント" ]
                , text "）"
                , text "。日仏英の3言語を話しますので、お好きな言語でご連絡ください。"
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
                [ strong [] [ a [ href "https://www.jsps.go.jp/english/e-pd/" ] [ text s.fellowship3Name ] ]
                , text s.fellowshipSep
                , strong [] [ text s.fellowship3Amount ]
                , text s.fellowshipSep
                , text s.fellowship3Period
                ]
            , li []
                [ strong [] [ a [ href "https://www.jst.go.jp/jisedai/spring/en/index.html" ] [ text s.fellowship1Name ] ]
                , text s.fellowshipSep
                , strong [] [ text s.fellowship1Amount ]
                , text s.fellowshipSep
                , text s.fellowship1Period
                ]
            , li []
                [ strong [] [ a [ href "https://jp.ambafrance.org/Bourses-France-Excellence-fr" ] [ text s.fellowship2Name ] ]
                , text s.fellowshipSep
                , strong [] [ text s.fellowship2Amount ]
                , text s.fellowshipSep
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



-- Notes (Japanese only)


type alias Note =
    { id : String
    , title : String
    , date : String
    , body : List (Html Msg)
    }


allNotes : List Note
allNotes =
    [ { id = "20250401_phd"
      , title = "博士課程について"
      , date = "2025-04-01"
      , body =
            [ p [] [ text "（ここにエッセイを書く）" ]
            ]
      }
    ]


notesListView : Html Msg
notesListView =
    div [ class "notes-section" ]
        [ h2 [] [ text "雑記" ]
        , ul [ class "notes-list" ]
            (List.map
                (\note ->
                    li []
                        [ a [ onClickPage (NoteDetail note.id), href "#" ] [ text note.title ]
                        , text (" — " ++ note.date)
                        ]
                )
                allNotes
            )
        , div [ class "notes-nav" ]
            [ a [ onClickPage Main, href "#" ] [ text "← ホームへ" ] ]
        ]


noteDetailView : String -> Html Msg
noteDetailView noteId =
    case List.filter (\n -> n.id == noteId) allNotes of
        note :: _ ->
            div [ class "note-detail" ]
                [ h2 [] [ text note.title ]
                , p [ class "note-date" ] [ text note.date ]
                , div [ class "note-body" ] note.body
                , div [ class "notes-nav" ]
                    [ a [ onClickPage NotesList, href "#" ] [ text "← 一覧へ" ] ]
                ]

        [] ->
            div [ class "notes-section" ]
                [ p [] [ text "見つかりません" ]
                , div [ class "notes-nav" ]
                    [ a [ onClickPage NotesList, href "#" ] [ text "← 一覧へ" ] ]
                ]
