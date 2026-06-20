module Main exposing (main)

import Browser
import Html exposing (Html, a, article, button, div, h3, header, img, li, main_, nav, p, section, span, strong, text, ul)
import Html.Attributes exposing (alt, class, href, id, src, target, title)
import Html.Events exposing (onClick)


type Section
    = Home
    | Papers
    | Talks
    | Teaching


type alias Model =
    { section : Section
    , darkMode : Bool
    }


type Msg
    = Show Section
    | ToggleTheme


type alias Paper =
    { title : String
    , authors : String
    , venue : String
    , links : List Link
    }


type alias Link =
    { label : String
    , url : String
    }


type alias Talk =
    { kind : String
    , event : String
    , date : String
    , title : String
    , link : Maybe Link
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { section = Home, darkMode = False }
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Show section ->
            { model | section = section }

        ToggleTheme ->
            { model | darkMode = not model.darkMode }


view : Model -> Html Msg
view model =
    div
        [ class
            (if model.darkMode then
                "app-shell theme-dark"

             else
                "app-shell"
            )
        ]
        [ header [ class "site-header" ]
            [ nav [ class "topbar", id "top" ]
                [ a [ class "brand", href "#top", onClick (Show Home) ] [ text "Xuzhi Yang" ]
                , div [ class "nav-links" ]
                    [ navButton model Home "Home"
                    , navButton model Papers "Papers"
                    , navButton model Talks "Talks"
                    , navButton model Teaching "Teaching"
                    ]
                , themeButton model.darkMode
                ]
            ]
        , main_ [] [ currentSection model.section ]
        ]


navButton : Model -> Section -> String -> Html Msg
navButton model section label =
    button
        [ class
            (if model.section == section then
                "nav-button is-active"

             else
                "nav-button"
            )
        , onClick (Show section)
        ]
        [ text label ]


themeButton : Bool -> Html Msg
themeButton darkMode =
    button
        [ class "theme-toggle"
        , onClick ToggleTheme
        , title "Switch color mode"
        ]
        [ text
            (if darkMode then
                "☀"

             else
                "☾"
            )
        ]


currentSection : Section -> Html Msg
currentSection section =
    case section of
        Home ->
            homeView

        Papers ->
            papersView

        Talks ->
            talksView

        Teaching ->
            teachingView


homeView : Html Msg
homeView =
    div []
        [ section [ class "hero" ]
            [ div [ class "hero-media" ]
                [ img [ src "assets/me.jpg", alt "Portrait of Xuzhi Yang" ] [] ]
            , div [ class "hero-copy" ]
                [ p [ class "lead" ]
                    [ text "I obtained my PhD in Statistics from the London School of Economics and Political Science, supervised by "
                    , a [ href "https://personal.lse.ac.uk/wangt60", target "_blank" ] [ text "Tengyao Wang" ]
                    , text " and "
                    , a [ href "https://personal.lse.ac.uk/cheny100/", target "_blank" ] [ text "Yining Chen" ]
                    , text "."
                    ]
                , p []
                    [ text "My research is theoretical in nature, aiming to understand the mathematical principles underlying statistics and machine learning. I develop theories and methods based on:"
                    ]
                , ul []
                    [ li [] [ text "Optimal transport theory;" ]
                    , li [] [ text "Statistics, e.g. nonparametric statistics and robust statistics;" ]
                    , li [] [ text "Non-Euclidean geometry." ]
                    ]
                , p []
                    [ text "I am also interested in developing reliable AI systems for theorem proof verification via formal languages such as "
                    , a [ href "https://lean-lang.org/", target "_blank"] [ text "Lean" ]
                    , text "."
                    ]
                , div [ class "quick-links" ]
                    [ a [ href "https://scholar.google.com/citations?user=XnH5giYAAAAJ&hl=en&oi=sra", target "_blank" ] [ text "Google Scholar" ]
                    , a [ href "https://github.com/YANG1030", target "_blank" ] [ text "GitHub" ]
                    , a [ href "https://www.linkedin.com/in/xuzhi-yang-9257871b1/", target "_blank" ] [ text "LinkedIn" ]
                    ]
                ]
            ]
        , section [ class "section-band" ]
            [ div [ class "section-heading" ]
                [ span [ class "section-kicker" ] [ text "Recent" ]
                ]
            , div [ class "news-list" ]
                [ newsItem "Jun. 2026" "A revised manuscript of the coverage correlation coefficient paper, including substantial new content, is now available." "assets/papers/CovCorr.pdf"
                , newsItem "Mar. 2026" "I will join KU Leuven as a Postdoc Researcher in the SUMMER, 2026, working with Prof. Irène Gijbels and Prof. Claeskens Gerda." ""
                , newsItem "Aug. 2025" "New preprint on a correlation coefficient for singular dependencies between random variables is available." "assets/papers/CovCorr.pdf"
                , newsItem "May 2025" "PhD viva passed, with thanks to Davy Paindaveine and Wicher Bergsma." ""
                ]
            ]
        ]


newsItem : String -> String -> String -> Html Msg
newsItem date body link =
    article [ class "news-item" ]
        [ span [ class "date-pill" ] [ text date ]
        , p []
            (if String.isEmpty link then
                [ text body ]

             else
                [ text body, text " ", a [ href link ] [ text "Paper" ] ]
            )
        ]


papersView : Html Msg
papersView =
    section [ class "content-section" ]
        [ div [ class "paper-list" ] (List.map paperCard papers)
        ]


paperCard : Paper -> Html Msg
paperCard paper =
    article [ class "paper-card" ]
        [ h3 [] [ text paper.title ]
        , p [ class "authors" ] [ text paper.authors ]
        , p [ class "venue" ] [ text paper.venue ]
        , div [ class "item-links" ] (List.map linkView paper.links)
        ]


linkView : Link -> Html Msg
linkView link =
    a [ href link.url, target "_blank" ] [ text link.label ]


talksView : Html Msg
talksView =
    section [ class "content-section teaching" ]
        [ ul [ class "talk-simple-list" ] (List.map talkListItem talks)
        ]


talkListItem : Talk -> Html Msg
talkListItem talk =
    li []
        (case talk.link of
            Just link ->
                [ span [ class ("talk-tag talk-tag-" ++ String.toLower talk.kind) ] [ text talk.kind ]
                , strong [] [ text talk.title ]
                , text (" - " ++ talk.event ++ ", " ++ talk.date ++ ". ")
                , linkView link
                ]

            Nothing ->
                [ span [ class ("talk-tag talk-tag-" ++ String.toLower talk.kind) ] [ text talk.kind ]
                , strong [] [ text talk.title ]
                , text (" - " ++ talk.event ++ ", " ++ talk.date ++ ".")
                ]
        )


teachingView : Html Msg
teachingView =
    section [ class "content-section teaching" ]
        [ h3 [] [ text "London School of Economics" ]
        , ul []
            [ li [] [ text "ST102 Elementary Statistical Theory, Sep 2021 - Mar 2025" ]
            , li [] [ text "ST457 Graph Data Analysis and Representation Learning, Oct 2022 - Jan 2023" ]
            , li [] [ text "ST447 Data Analysis and Statistical Methods, Oct 2022 - Jan 2023" ]
            ]
        , h3 [] [ text "Southern University of Science and Technology" ]
        , ul []
            [ li [] [ text "STA5004 Functional Data Analysis, Feb 2021 - Jun 2021" ]
            , li [] [ text "MA204 Mathematics Statistics, Feb 2020 - Jun 2020" ]
            ]
        ]


papers : List Paper
papers =
    [ { title = "Coverage correlation: detecting singular dependencies between random variables"
      , authors = "Xuzhi Yang, Mona Azadkia and Tengyao Wang"
      , venue = "Preprint, submitted, arXiv:2508.06402, 2026+"
      , links = [ { label = "Paper", url = "assets/papers/CovCorr.pdf" }, { label = "Code", url = "https://github.com/wangtengyao/covercorr" }, { label = "Bib", url = "assets/bib/yang2025coverage.bib" } ]
      }
    , { title = "On the squashed distribution policy for maximum entropy reinforcement learning"
      , authors = "Linjie Xu, Xuzhi Yang and Tao Ma"
      , venue = "Preprint, submitted, 2026+"
      , links = []
      }
    , { title = "To switch or not to switch? Balanced policy switching in offline reinforcement learning"
      , authors = "Tao Ma, Xuzhi Yang and Zoltan Szabo"
      , venue = "Preprint, arXiv:2407.01837, 2026+"
      , links = [ { label = "Paper", url = "assets/papers/SwichingCost.pdf" } ]
      }
    , { title = "Multiple-output composite quantile regression through an optimal transport lens"
      , authors = "Xuzhi Yang and Tengyao Wang"
      , venue = "COLT 2024: 5076-5122"
      , links = [ { label = "Paper", url = "assets/papers/yang24.pdf" }, { label = "Poster", url = "assets/papers/yang24_poster.pdf" }, { label = "Video", url = "https://www.youtube.com/watch?v=wtMAM6VBVVo" } ]
      }
    , { title = "A framework for policy evaluation enhancement by diffusion models"
      , authors = "Tao Ma* and Xuzhi Yang*"
      , venue = "Tiny Papers @ ICLR 2024, invited to present"
      , links = [ { label = "Paper", url = "assets/papers/taotiny.pdf" } ]
      }
    , { title = "Applications of Optimal Transport in Multivariate Statistics"
      , authors = "Xuzhi Yang"
      , venue = "PhD thesis"
      , links = [ { label = "Thesis", url = "assets/papers/Xuzhi_Yang_PhDThesis_finalcopy.pdf" } ]
      }
    , { title = "Test on Two-sample High-dimensional Correlation Matrices with Sparse Settings"
      , authors = "Xuzhi Yang"
      , venue = "Master thesis"
      , links = [ { label = "Thesis", url = "assets/papers/MasterThesis.pdf" } ]
      }
    ]


talks : List Talk
talks =
    [ { kind = "Talk", event = "New Researcher Conference Asia, University of Hong Kong", date = "Jun 2026", title = "Coverage correlation: detecting singular dependencies between random variables", link = Nothing }
    , { kind = "Poster", event = "Workshop on uncertainty in multivariate, non-Euclidean, and functional spaces: theory and practice, Isaac Newton Institute, Cambridge", date = "May 2025", title = "The coverage correlation coefficient: Going beyond functional dependence", link = Just { label = "Poster", url = "assets/papers/coverage_poster.pdf" } }
    , { kind = "Talk", event = "Research Showcase Day, LSE", date = "Apr 2025", title = "Coverage correlation coefficient", link = Nothing }
    , { kind = "Talk", event = "Data Science Society, LSE", date = "Mar 2025", title = "Multivariate rank via optimal assignment", link = Nothing }
    , { kind = "Talk", event = "Tengyao's group meeting, LSE", date = "Jul 2024", title = "A brief introduction of diffusion model", link = Just { label = "Notes", url = "assets/papers/DM.pdf" } }
    , { kind = "Poster", event = "Workshop on heterogeneous and distributed data, University of Warwick", date = "Jun 2024", title = "Multiple-output composite quantile regression through an optimal transport lens", link = Just { label = "Poster", url = "assets/papers/yang24_poster.pdf" } }
    , { kind = "Talk", event = "School of Mathematics and Statistics, Northeast Normal University", date = "Jul 2023", title = "Multiple-output composite quantile regression through an optimal transport lens", link = Nothing }
    ]
