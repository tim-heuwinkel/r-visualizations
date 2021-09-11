#
# This is the user interface of a Shiny Dashboard. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building dashboards with Shiny here:
#
#    https://rstudio.github.io/shinydashboard/
#
# The application can be viewed here:
#    
#    https://tim-heuwinkel.shinyapps.io/movie_critics/
#

# Load packages
library(shinydashboard)   # running the app
library(DT)               # displaying tables in shiny
library(plotly)           # creating plots
library(wordcloud2)       # creating wordclouds
library(shinyWidgets)     # nicer looking inputs
library(dashboardthemes)  # design themes for the app


# Define sidebar
sidebar <- dashboardSidebar(
  uiChangeThemeDropdown(defaultTheme = "grey_dark"),
  
  sidebarMenu(
    menuItem("Overview", tabName = "movies", icon = icon("film")),
    menuItem("Favourites", tabName = "crew", icon = icon("star")),
    menuItem(
      "Keywords",
      tabName = "sentiment",
      icon = icon("quote-right")
    ),
    menuItem("Make your own", tabName = "custom", icon = icon("edit")),
    menuItem("Data", tabName = "data", icon = icon("th"))
  ),
  
  checkboxGroupButtons(
    inputId = "age_select",
    label = "Select age ratings",
    choices = c("G/PG", "PG-13", "R"),
    selected = c("G/PG", "PG-13", "R"),
    justified = TRUE
  ),
  sliderTextInput(
    inputId = "year_select",
    label = "Select release year range",
    choices = c(
      min(movies_merged$release_year):max(movies_merged$release_year)
    ),
    selected = c(
      min(movies_merged$release_year),
      max(movies_merged$release_year)
    )
  ),
  checkboxInput(
    inputId = "oscar_bin",
    label = "Only oscar winning movies",
    value = FALSE
  ),
  sliderTextInput(
    inputId = "imdb_range",
    label = "Select minimum and maximum IMDb score",
    choices = c(seq(1, 10, by = 0.1)),
    selected = c(1.0, 10.0)
  ),
  sliderTextInput(
    inputId = "meta_range",
    label = "Select minimum and maximum Metascore",
    choices = c(seq(1, 100)),
    selected = c(1, 100)
  )
)

# Define body
body <- dashboardBody(uiChangeThemeOutput(),
                      # custom theme
                      
                      tabItems(
                        tabItem(
                          tabName = "movies",
                          
                          fluidRow(box(
                            width = 12,
                            h1("Are movie critics out of touch with audiences?"),
                            
                            p(
                              "The following visualizations aim to show the
                                      difference in taste between average viewers and
                                      professional movie reviewers. The audiences taste
                                      is represented by the",
                              tags$a(href = "https://help.imdb.com/article/imdb/track-movies-tv/ratings-faq/G67Y87TFYYP6TWAV?ref_=helpsect_cons_2_4#", "IMDb score,"),
                              "while the critics taste is represented by the ",
                              tags$a(href = "https://www.metacritic.com/about-metascores", "Metascore."),
                              br(),
                              "The data used was taken from",
                              tags$a(href = "https://www.kaggle.com/tmdb/tmdb-movie-metadata", "kaggle"),
                              "and",
                              tags$a(href = "http://www.omdbapi.com/", "OMDb.")
                            )
                          )),
                          
                          fluidRow(
                            # Column 1
                            box(
                              title = "Distribution of IMDb- against Metascore",
                              width = 3,
                              plotlyOutput("metric_comparison")
                            ),
                            #Column 2
                            box(
                              title = "Average IMDb- and Metascore for each Genre",
                              width = 9,
                              plotlyOutput("genre_comparison")
                            )
                          ),
                          
                          # Row 2
                          fluidRow(
                            # Column 1
                            box(
                              title = "Average IMDb- and Metascore for each release year",
                              width = 6,
                              plotlyOutput("movies_year")
                            ),
                            # Column 2
                            box(
                              title = "Scores over runtime",
                              width = 6,
                              "Average IMDb- and Metascore over runtime",
                              plotlyOutput("movies_runtime")
                            )
                          ),
                          
                          p(
                            "*For the comparisons sake, the Metascore has been divided by 10 in these plots"
                          )
                        ),
                        
                        tabItem(tabName = "crew",
                                
                                # Row 1
                                fluidRow(
                                  # Column 1
                                  box(
                                    title = "IMDb - Favorite Actors",
                                    width = 4,
                                    plotlyOutput("imdb_top_actors")
                                  ),
                                  # Column 2
                                  box(
                                    title = "IMDb - Favorite Directors",
                                    width = 4,
                                    plotlyOutput("imdb_top_directors")
                                  ),
                                  # Column 3
                                  box(
                                    title = "IMDb - Favorite Producers",
                                    width = 4,
                                    plotlyOutput("imdb_top_producers")
                                  )
                                ),
                                
                                # Row 2
                                fluidRow(
                                  # Column 1
                                  box(
                                    title = "Metacritic - Favorite Actors",
                                    width = 4,
                                    plotlyOutput("meta_top_actors")
                                  ),
                                  # Column 2
                                  box(
                                    title = "Metacritic - Favorite Directors",
                                    width = 4,
                                    plotlyOutput("meta_top_directors")
                                  ),
                                  # Column 3
                                  box(
                                    title = "Metacritic - Favorite Producers",
                                    width = 4,
                                    plotlyOutput("meta_top_producers")
                                  )
                                )),
                        
                        tabItem(tabName = "sentiment",
                                # Row 1
                                fluidRow(
                                  # Column 1
                                  box(
                                    title = "Most prominent keywords in the Top 250 IMDb movies",
                                    width = 6,
                                    wordcloud2Output("wordcloud_imdb")
                                  ),
                                  # Column 2
                                  box(
                                    title = "Most prominent keywords in the Top 250 Metacritic movies",
                                    width = 6,
                                    wordcloud2Output("wordcloud_meta")
                                  )
                                )),
                        
                        tabItem(tabName = "custom",
                                # Row 1
                                fluidRow(
                                  # Column 1
                                  box(
                                    title = "X-Axis",
                                    width = 4,
                                    selectInput("scatter_x", "", selected = "imdb_score",
                                                choices = choices_scatter),
                                    checkboxInput(inputId = "x_log", "X-Axis log scaling")
                                  ),
                                  # Column 2
                                  box(
                                    title = "Y-Axis",
                                    width = 4,
                                    selectInput("scatter_y", "", selected = "metascore",
                                                choices = choices_scatter),
                                    checkboxInput(inputId = "y_log", label = "Y-Axis log scaling")
                                  ),
                                  # Column 3
                                  box(
                                    title = "Coloring",
                                    width = 4,
                                    selectInput("scatter_color", "", selected = "imdb_score",
                                                choices = choices_scatter)
                                  )
                                ),
                                
                                # Row 2
                                fluidRow(box(
                                  title = "",
                                  width = 12,
                                  plotlyOutput("custom_plot")
                                ))),
                        
                        tabItem(tabName = "data",
                                # Row 1
                                fluidRow(
                                  # Column 1
                                  box(
                                    title = "",
                                    width = 12,
                                    DT::dataTableOutput("movies"),
                                    p("* Oscar nominations for movies which won an oscar are unknown")
                                  )
                                ))
                      ))

dashboardPage(dashboardHeader(title = "Movie ratings"),
              sidebar,
              body)
