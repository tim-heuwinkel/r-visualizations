#
# This is the server logic of a Shiny Dashboard. You can run the
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
library(tidyverse)        # general data processing
library(stringr)          # string manipulation
library(lubridate)        # processing of dates
library(shiny)            # running the app
library(shinydashboard)   # running the app
library(dashboardthemes)  # design themes for the app
library(DT)               # displaying tables in shiny
library(plotly)           # creating plots
library(wordcloud2)       # creating wordclouds
library(RSentiment)       # analysing keywords
library(tm)               # text processing

# Define constants and defaults
options(scipen = 999)
set.seed(42)

# Server logic
shinyServer(function(input, output) {
  ### theme selector update
  
  callModule(module = serverChangeTheme, id = "moduleChangeTheme")
  
  
  ###### Reactive conductor data source
  
  movies_filtered <- reactive({
    filtered <- movies_merged %>%
      filter(release_year <= as.numeric(input$year_select[2])) %>% # filter max year
      filter(release_year >= as.numeric(input$year_select[1])) %>% # filter min year
      filter(imdb_score >= input$imdb_range[1] &
               imdb_score <= input$imdb_range[2]) %>% # filter imdb score
      filter(metascore >= input$meta_range[1] &
               metascore <= input$meta_range[2])       # filter metascore
    
    if (input$oscar_bin == TRUE) {
      filtered <- filtered %>% filter(oscar_bin == TRUE)
    }
    
    age_filter <- input$age_select
    if ("R" %in% age_filter) {
      # if it is R rated, it is suitable for these audiences too
      age_filter <-
        c(
          age_filter,
          "NC-17",
          "Passed",
          "Not Rated",
          "NC-17",
          "Approved",
          "Unrated",
          "N/A",
          "M"
        )
    }
    if ("G/PG" %in% age_filter) {
      age_filter <-
        age_filter[age_filter != "G/PG"] # remove G/PG from vector
      age_filter <- c(age_filter, "G", "PG") # add G and PG
    }
    if ("PG-13" %in% age_filter) {
      age_filter <- c(age_filter, "TV-14") # 13 ≈ 14, add TV-14
    }
    
    filtered %>% filter(rated %in% age_filter)
  })
  
  cast_filtered <- reactive({
    cast_agg <- cast %>%
      filter(title %in% movies_filtered()$title) %>% # filter actors according to filtered movies
      select(-title) %>%
      group_by(word) %>%
      mutate(count = n()) %>%
      filter(count >= 7) %>% # only actors who starred in at least 7 movies
      summarise_all(list(mean)) # mean of movies for actors
    cast_agg["imdb"] <-
      lapply(cast_agg["imdb"], function(x)
        round(x, 1)) # round imdb ratings
    cast_agg["metascore"] <-
      lapply(cast_agg["metascore"], function(x)
        round(x, 0)) # round metascore ratings
    
    cast_agg
  })
  
  directors_filtered <- reactive({
    directors_agg <- directors %>%
      filter(title %in% movies_filtered()$title) %>% # filter directors according to filtered movies
      select(-title) %>%
      group_by(word) %>%
      mutate(count = n()) %>%
      filter(count >= 4) %>%
      summarise_all(list(mean))
    directors_agg["imdb"] <-
      lapply(directors_agg["imdb"], function(x)
        round(x, 1))
    directors_agg["metascore"] <-
      lapply(directors_agg["metascore"], function(x)
        round(x, 0))
    
    directors_agg
  })
  
  producers_filtered <- reactive({
    producers_agg <- producers %>%
      filter(title %in% movies_filtered()$title) %>% # filter producers according to filtered movies
      select(-title) %>%
      group_by(word) %>%
      mutate(count = n()) %>%
      filter(count >= 4) %>%
      summarise_all(list(mean))
    producers_agg["imdb"] <-
      lapply(producers_agg["imdb"], function(x)
        round(x, 1))
    producers_agg["metascore"] <-
      lapply(producers_agg["metascore"], function(x)
        round(x, 0))
    
    producers_agg
  })
  
  keywords_filtered <- reactive({
    keywords_agg <- keywords %>%
      filter(title %in% movies_filtered()$title) %>% # filter keywords according to filtered movies
      group_by(word) %>%
      summarize(n = n()) %>%
      filter(n > 3) # only keywords which appear more than 3 times
    
    keywords_agg
  })
  
  
  ###### Movies tab
  
  ### metric comparison plot
  
  output$metric_comparison <- renderPlotly({
    boxplot_data <- movies_filtered() %>%
      select(title, imdb_score, metascore) %>%
      mutate(metascore = metascore / 10) %>% # equalize metrics
      gather(title) %>% # turn data into long format
      rename(metric = title) %>%
      mutate(metric = ifelse(metric == "imdb_score", "IMDb score", "Metascore*"))
    
    plot_ly(
      boxplot_data,
      y = ~ value,
      x = ~ metric,
      type = "box",
      color = ~ metric,
      colors = c("#FF52D7", "#6FFF32")
    ) %>%
      layout(
        showlegend = FALSE,
        yaxis = list(
          title = "Score",
          tickfont = f1,
          titlefont = f2,
          gridcolor = toRGB("grey35")
        ),
        xaxis = list(
          title = "",
          tickfont = f1,
          gridcolor = "rgba(0,0,0,0)"
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  ### genre comparison plot
  
  output$genre_comparison <- renderPlotly({
    genres_agg <- movies_filtered() %>%
      group_by(main_genre) %>%
      summarize(
        IMDb_score = mean(imdb_score),
        Metascore = mean(metascore / 10),
        diff = mean(imdb_score) - mean(metascore / 10),
        mean_means = mean(c(
          mean(imdb_score), mean(metascore / 10)
        )),
        count = n()
      ) %>%
      filter(count > 10) %>% # at least 10 movies in genre
      arrange(mean_means) %>% # sort by mean of imdb and metascore
      mutate(Genre = factor(main_genre, levels = .$main_genre)) # transform to factor
    
    plot <- ggplot(genres_agg) +
      geom_segment(aes(
        x = Genre,
        xend = Genre,
        y = Metascore,
        yend = IMDb_score
      ),
      color = "grey") +
      geom_point(
        aes(x = Genre, y = Metascore, text = ""),
        color = rgb(111 / 255, 255 / 255, 50 / 255),
        size = 3
      ) +
      geom_point(
        aes(x = Genre, y = IMDb_score, text = ""),
        color = rgb(255 / 255, 82 / 255, 215 / 255),
        size = 3
      ) +
      scale_y_continuous(breaks = round(seq(0, 10, by = 0.5), 1), limits = c(4, 9)) +
      coord_flip() +
      theme(
        legend.background = element_blank(),
        panel.grid = element_line(colour = "grey35"),
        axis.ticks = element_blank()
      )
    
    ggplotly(plot) %>%
      layout(
        yaxis = list(title = "", tickfont = f1),
        xaxis = list(
          title = "Score*",
          tickfont = f1,
          titlefont = f2
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  ### movies over time plot
  
  output$movies_year <- renderPlotly({
    movies_years <- movies_filtered() %>%
      group_by(release_year) %>%
      summarize(
        imdb = mean(imdb_score),
        meta = mean(metascore),
        count = n()
      ) %>%
      filter(release_year > 1960) # amount of movies yearly before 1960 not significant
    
    plot_ly(movies_years, x =  ~ release_year) %>%
      add_trace(
        y =  ~ imdb,
        type = "scatter",
        mode = "lines",
        name = "IMDb score",
        line = list(color = "rgb(255, 82, 215)")
      ) %>%
      add_trace(
        y =  ~ (meta / 10),
        type = "scatter",
        mode = "lines",
        name = "Metascore*",
        line = list(color = "rgb(111, 255, 50)")
      ) %>%
      layout(
        yaxis = list(
          title = "Score",
          tickfont = f1,
          titlefont = f2,
          gridcolor = toRGB("grey35")
        ),
        xaxis = list(
          title = "",
          tickfont = f1,
          gridcolor = toRGB("grey35")
        ),
        legend = list(font = f1),
        hovermode = "x unified",
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  
  ### movies runtime plot
  
  output$movies_runtime <- renderPlotly({
    movies_runtime <- movies_filtered() %>%
      group_by(runtime) %>%
      summarize(
        imdb = mean(imdb_score),
        meta = mean(metascore),
        count = n()
      ) %>%
      filter(count >= 10)
    
    plot_ly(
      movies_runtime,
      x =  ~ runtime,
      y =  ~ imdb,
      type = "scatter",
      mode = "lines",
      name = "IMDb score",
      line = list(color = "rgb(255, 82, 215)")
    ) %>%
      add_trace(
        y =  ~ (meta / 10),
        name = "Metascore*",
        line = list(color = "rgb(111, 255, 50)")
      ) %>%
      layout(
        yaxis = list(
          title = "Score",
          tickfont = f1,
          titlefont = f2,
          gridcolor = toRGB("grey35")
        ),
        xaxis = list(
          title = "Runtime",
          tickfont = f1,
          titlefont = f2,
          gridcolor = toRGB("grey35")
        ),
        legend = list(font = f1),
        hovermode = "x unified",
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  
  ###### Favorites tab
  
  ### Favorite lollipop plots IMDb...
  
  # ...actors
  output$imdb_top_actors <- renderPlotly({
    cast_imdb_sorted <-
      cast_filtered()[order(-cast_filtered()$imdb), ] %>% head(5)
    cast_imdb_sorted$word <-
      lapply(cast_imdb_sorted$word, function(x)
        paste(x, " ", sep = "")) # add space after each name
    
    # sort decreasing by imdb score and save order as factor level
    cast_imdb_sorted$word <- factor(cast_imdb_sorted$word,
                                    levels = unique(cast_imdb_sorted$word)[order(cast_imdb_sorted$imdb, decreasing =
                                                                                   FALSE)])
    
    plot <-
      ggplot(cast_imdb_sorted, aes(
        x = word,
        y = imdb,
        text = paste("IMDb score:", imdb)
      )) +
      geom_segment(aes(xend = word, yend = 6), color = "grey") +
      geom_point(color = rgb(255 / 255, 82 / 255, 215 / 255),
                 size = 3) +
      coord_flip() +
      scale_y_continuous(limits = c(6, 9)) +
      theme(panel.grid = element_line(colour = "grey35"),
            axis.ticks = element_blank())
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        yaxis = list(
          title = "",
          tickfont = f1,
          titlefont = f2
        ),
        xaxis = list(
          title = "IMDb score",
          tickfont = f1,
          titlefont = f2
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # ...directors
  output$imdb_top_directors <- renderPlotly({
    directors_imdb_sorted <-
      directors_filtered()[order(-directors_filtered()$imdb), ] %>% head(5)
    directors_imdb_sorted$word <-
      lapply(directors_imdb_sorted$word, function(x)
        paste(x, " ", sep = "")) # add space after each name
    
    # sort decreasing by metascore and save order as factor level
    directors_imdb_sorted$word <- factor(directors_imdb_sorted$word,
                                         levels = unique(directors_imdb_sorted$word)[order(directors_imdb_sorted$imdb, decreasing =
                                                                                             FALSE)])
    
    plot <-
      ggplot(directors_imdb_sorted, aes(
        x = word,
        y = imdb,
        text = paste0("IMDb score: ", imdb)
      )) +
      geom_segment(aes(xend = word, yend = 6), color = "grey") +
      geom_point(color = rgb(255 / 255, 82 / 255, 215 / 255),
                 size = 3) +
      coord_flip() +
      scale_y_continuous(limits = c(6, 9)) +
      theme(panel.grid = element_line(colour = "grey35"),
            axis.ticks = element_blank())
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        yaxis = list(
          title = "",
          tickfont = f1,
          titlefont = f2
        ),
        xaxis = list(
          title = "IMDb score",
          tickfont = f1,
          titlefont = f2
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # ...producers
  output$imdb_top_producers <- renderPlotly({
    producers_imdb_sorted <-
      producers_filtered()[order(-producers_filtered()$imdb), ] %>% head(5)
    producers_imdb_sorted$word <-
      lapply(producers_imdb_sorted$word, function(x)
        paste(x, " ", sep = "")) # add space after each name
    
    producers_imdb_sorted$word <-
      factor(producers_imdb_sorted$word,
             levels = unique(producers_imdb_sorted$word)[order(producers_imdb_sorted$imdb, decreasing =
                                                                 FALSE)])
    
    plot <-
      ggplot(producers_imdb_sorted, aes(
        x = word,
        y = imdb,
        text = paste0("IMDb score: ", imdb)
      )) +
      geom_segment(aes(xend = word, yend = 6), color = "grey") +
      geom_point(color = rgb(255 / 255, 82 / 255, 215 / 255),
                 size = 3) +
      coord_flip() +
      scale_y_continuous(limits = c(6, 9)) +
      theme(panel.grid = element_line(colour = "grey35"),
            axis.ticks = element_blank())
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        yaxis = list(
          title = "",
          tickfont = f1,
          titlefont = f2
        ),
        xaxis = list(
          title = "IMDb score",
          tickfont = f1,
          titlefont = f2
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  
  ### Favorite lollipop plots Metascore...
  
  # ...actors
  output$meta_top_actors <- renderPlotly({
    cast_meta_sorted <-
      cast_filtered()[order(-cast_filtered()$metascore), ] %>% head(5)
    cast_meta_sorted$word <-
      lapply(cast_meta_sorted$word, function(x)
        paste(x, " ", sep = "")) # add space after each name
    
    cast_meta_sorted$word <-
      factor(cast_meta_sorted$word, levels = unique(cast_meta_sorted$word)[order(cast_meta_sorted$metascore, decreasing =
                                                                                   FALSE)])
    
    plot <-
      ggplot(cast_meta_sorted, aes(
        x = word,
        y = metascore,
        text = paste("Metascore:", metascore)
      )) +
      geom_segment(aes(xend = word, yend = 60), color = "grey") +
      geom_point(color = rgb(111 / 255, 255 / 255, 50 / 255),
                 size = 3) +
      coord_flip() +
      scale_y_continuous(limits = c(60, 90)) +
      theme(panel.grid = element_line(colour = "grey35"),
            axis.ticks = element_blank())
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        yaxis = list(
          title = "",
          tickfont = f1,
          titlefont = f2
        ),
        xaxis = list(
          title = "Metascore",
          tickfont = f1,
          titlefont = f2
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # ...directors
  output$meta_top_directors <- renderPlotly({
    directors_meta_sorted <-
      directors_filtered()[order(-directors_filtered()$metascore), ] %>% head(5)
    directors_meta_sorted$word <-
      lapply(directors_meta_sorted$word, function(x)
        paste(x, " ", sep = "")) # add space after each name
    
    directors_meta_sorted$word <-
      factor(directors_meta_sorted$word,
             levels = unique(directors_meta_sorted$word)[order(directors_meta_sorted$metascore, decreasing =
                                                                 FALSE)])
    
    plot <-
      ggplot(directors_meta_sorted,
             aes(
               x = word,
               y = metascore,
               text = paste("Metascore:", metascore)
             )) +
      geom_segment(aes(xend = word, yend = 60), color = "grey") +
      geom_point(color = rgb(111 / 255, 255 / 255, 50 / 255),
                 size = 3) +
      coord_flip() +
      scale_y_continuous(limits = c(60, 90)) +
      theme(panel.grid = element_line(colour = "grey35"),
            axis.ticks = element_blank())
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        yaxis = list(
          title = "",
          tickfont = f1,
          titlefont = f2
        ),
        xaxis = list(
          title = "Metascore",
          tickfont = f1,
          titlefont = f2
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # ...producers
  output$meta_top_producers <- renderPlotly({
    producers_meta_sorted <-
      producers_filtered()[order(-producers_filtered()$metascore), ] %>% head(5)
    producers_meta_sorted$word <-
      lapply(producers_meta_sorted$word, function(x)
        paste(x, " ", sep = "")) # add space after each name
    
    producers_meta_sorted$word <-
      factor(producers_meta_sorted$word,
             levels = unique(producers_meta_sorted$word)[order(producers_meta_sorted$metascore, decreasing =
                                                                 FALSE)])
    
    plot <-
      ggplot(producers_meta_sorted,
             aes(
               x = word,
               y = metascore,
               text = paste("Metascore:", metascore)
             )) +
      geom_segment(aes(xend = word, yend = 60), color = "grey") +
      geom_point(color = rgb(111 / 255, 255 / 255, 50 / 255),
                 size = 3) +
      coord_flip() +
      scale_y_continuous(limits = c(60, 90)) +
      theme(panel.grid = element_line(colour = "grey35"),
            axis.ticks = element_blank())
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        yaxis = list(
          title = "",
          tickfont = f1,
          titlefont = f2
        ),
        xaxis = list(
          title = "Metascore",
          tickfont = f1,
          titlefont = f2
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  
  ###### keywords tab
  
  ### Wordclouds
  
  output$wordcloud_imdb <- renderWordcloud2({
    top_imdb_movies <-
      movies_filtered()[order(-movies_filtered()$imdb_score), ] %>%
      head(250) # top 250 movies by imdb score
    
    keywords_agg <- keywords %>%
      filter(title %in% top_imdb_movies$title) %>% # filter keywords according to filtered movies
      group_by(word) %>%
      summarize(n = n()) %>%
      filter(n > 4) # only keywords which appear more than 4 times in top 250
    
    wordcloud2(keywords_agg,
               backgroundColor = "rgba(0,0,0,0)") # transparent background
  })
  
  output$wordcloud_meta <- renderWordcloud2({
    top_meta_movies <-
      movies_filtered()[order(-movies_filtered()$metascore), ] %>%
      head(250) # top 250 movies by metascore
    
    keywords_agg <- keywords %>%
      filter(title %in% top_meta_movies$title) %>% # filter keywords according to filtered movies
      group_by(word) %>%
      summarize(n = n()) %>%
      filter(n > 4) # only keywords which appear more than 4 times in top 250
    
    wordcloud2(keywords_agg,
               backgroundColor = "rgba(0,0,0,0)") # transparent background
  })
  
  
  ###### make your own tab
  
  ### custom scatter plot
  
  output$custom_plot <- renderPlotly({
    plot <-
      ggplot(
        movies_filtered(),
        aes_string(
          x = input$scatter_x,
          y = input$scatter_y,
          color = input$scatter_color
        )
      ) +
      geom_point(aes(text = title)) +
      scale_color_gradient(low = "#6FFF32", high = "#FF52D7") +
      geom_smooth() + # add smoothing line
      theme(
        legend.background = element_blank(),
        # remove background of colorscale
        legend.text = element_text(
          colour = "#CDCDCD",
          family = "Roboto, sans-serif",
          size = 10
        ),
        legend.title = element_text(
          colour = "#CDCDCD",
          family = "Roboto, sans-serif",
          size = 14
        ),
        panel.grid = element_line(colour = "grey35"),
        axis.ticks = element_blank()
      ) +
      labs(colour = convert(input$scatter_color, TRUE)) # convert to formatted names
    
    if (input$x_log) {
      plot <-
        plot + scale_x_continuous(trans = "pseudo_log") # log for x selected
    }
    if (input$y_log) {
      plot <-
        plot + scale_y_continuous(trans = "pseudo_log") # log for y selected
    }
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        yaxis = list(
          title = convert(input$scatter_y, TRUE), # convert to formatted names
          tickfont = f1,
          titlefont = f2
        ),
        xaxis = list(
          title = convert(input$scatter_x, TRUE), # convert to formatted names
          tickfont = f1,
          titlefont = f2
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  
  ###### data tab
  
  ### Datatable
  
  output$movies <- DT::renderDataTable({
    DT::datatable(
      movies_merged[, c(
        "title",
        "rated",
        "imdb_score",
        "imdb_vote_count",
        "metascore",
        "oscar_wins",
        "oscar_nominations",
        "award_wins",
        "award_nominations",
        "budget",
        "revenue",
        "profit",
        "release_date",
        "runtime",
        "main_genre",
        "secondary_genre"
      ),
      drop = FALSE],
      filter = "top",
      colnames = c(
        "Title",
        "Age Rating",
        "IMDb Score",
        "IMDb Vote Count",
        "Metascore",
        "Oscar wins",
        "Oscar nominations*",
        "Award wins",
        "Award nominations",
        "Budget",
        "Revenue",
        "Profit",
        "Release date",
        "Runtime",
        "Main Genre",
        "Secondary Genre"
      ),
      options = list(list(3, "desc")) # sort descending by imdb score
    )
  })
  
})
