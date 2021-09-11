#
# This is the global logic of a Shiny Dashboard. You can run the
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
library(httr)             # api calls
library(jsonlite)         # api data processing
library(stringr)          # string manipulation
library(pbapply)          # general data processing
library(lubridate)        # processing of dates
library(shiny)            # running the app
library(dashboardthemes)  # design themes for the app

source("moduleChangeTheme.R") # source external script

movies <- read_csv("tmdb_5000_movies.csv")   # load movie data
credits <- read_csv("tmdb_5000_credits.csv") # load crew data

movies_credits <-
  merge(movies, credits, by.x = "id", by.y = "movie_id") # merge movies and crew

api_data <- read_csv("api_data.csv")   # load api data

movies_merged <-
  merge(
    api_data,
    movies_credits,
    by.x = "title",
    by.y = "original_title",
    all.x = FALSE
  ) # enrich movie data with api data


# clean up datatypes for columns
movies_merged$title <- as.character(movies_merged$title)
movies_merged$imdb_score <- as.numeric(movies_merged$imdb_score)
movies_merged$imdb_vote_count <-
  as.integer(gsub(",", "", movies_merged$imdb_vote_count))
movies_merged$metascore <- as.integer(movies_merged$metascore)
movies_merged$oscar_wins <- as.integer(movies_merged$oscar_wins)
movies_merged$oscar_nominations <-
  as.integer(movies_merged$oscar_nominations)
movies_merged$award_wins <- as.integer(movies_merged$award_wins)
movies_merged$award_nominations <-
  as.integer(movies_merged$award_nominations)
movies_merged$budget <- as.integer(movies_merged$budget)
movies_merged$original_language <-
  as.factor(movies_merged$original_language)
movies_merged$status <- as.factor(movies_merged$status)


# remove NA values for the two important metrics
movies_merged <- movies_merged[!is.na(movies_merged$imdb_score),]
movies_merged <- movies_merged[!is.na(movies_merged$metascore),]


getNthName <- function(name_list, n) {
  #' returns the @n 'th value that matches the regex pattern in the string @name_list
  #'
  #' @param name_list string to be searched
  #' @param n position of value to be extracted
  #'
  #' @noRd
  
  first_match <-
    str_match_all(name_list, "name\"[:punct:] \"[a-zA-Z ]+\"")[[1]][n]
  name <- substr(first_match, 9, nchar(first_match) - 1)
  return(as.character(name))
}


movies_merged <- movies_merged %>%
  select(-c( # remove unused columns
    homepage,
    production_companies,
    production_countries,
    title.x,
    vote_average,
    vote_count,
    title.y
  )) %>%
  filter(budget > 1) %>% # only plausible budgets
  filter(revenue > 1000) %>% # only plausible revenues
  mutate(release_year = as.character(year(release_date))) %>% # isolate year
  mutate(age_days = Sys.Date() - ymd(release_date)) %>% # age of movie in days
  mutate(imdb_votes_year = 365 * (imdb_vote_count / as.numeric(age_days))) %>% # imdb votes per year
  mutate(oscar_bin = ifelse(oscar_wins > 0, 1, 0)) %>% # whether movie has won at least 1 oscar
  mutate(main_genre = mapply(function(x, n)
    getNthName(x, n), genres, 1)) %>% # isolate main genre of movie
  mutate(secondary_genre = mapply(function(x, n)
    getNthName(x, n), genres, 2)) %>% # isolate secondary genre of movie
  mutate(profit = revenue - budget) %>%
  mutate(profit_margin = (profit / revenue) * 100) # calculate profit margin


frameRegEx <- function(row, col, regex, start_index) {
  #' Generates a dataframe of values inside the list in column @col, with regular
  #' expression @regex and slicing each result from @start_index to -1.
  #'
  #' @param row from which values should be generated
  #' @param col column name as string in which the regular expression @regex should be applied
  #' @param regex regular expression as string with which the strings in @col should be split
  #' @param start_index integer from which matches should be split
  #'
  #' @noRd
  
  matches <- str_match_all(row[col], regex)[[1]]
  
  if (all(is.na(matches))) {
    return(NA)
  }
  
  if (col == "cast") {
    matches <- matches[1:5] # only 5 most important actors
  }
  
  matches <-
    data.frame(word = substr(matches, start_index, nchar(matches) - 1),
               stringsAsFactors = FALSE)
  matches$imdb <- as.numeric(row["imdb_score"])
  matches$metascore <- as.numeric(row["metascore"])
  name <-
    data.frame(title = row["title"]) %>% slice(rep(1:n(), each = nrow(matches)))
  matches <- cbind(matches, name)
  
  return(matches)
}

print("Creating dataframe for actors...")
cast <-
  do.call(
    rbind.data.frame,
    pbapply(
      movies_merged,
      1,
      frameRegEx,
      "cast",
      "name\"[:punct:] \"[a-zA-Z \\.]+\"",
      9
    )
  )

print("Creating dataframe for producers...")
producers <-
  do.call(
    rbind.data.frame,
    pbapply(
      movies_merged,
      1,
      frameRegEx,
      "crew",
      "\"job\"[:punct:] \"Executive Producer\", \"name\"[:punct:] \"[a-zA-Z \\.]+\"",
      39
    )
  )

print("Creating dataframe for directors...")
directors <-
  do.call(
    rbind.data.frame,
    pbapply(
      movies_merged,
      1,
      frameRegEx,
      "crew",
      "\"job\"[:punct:] \"Director\", \"name\"[:punct:] \"[a-zA-Z \\.]+\"",
      29
    )
  )

print("Creating dataframe for keywords...")
keywords <-
  do.call(
    rbind.data.frame,
    pbapply(
      movies_merged,
      1,
      frameRegEx,
      "keywords",
      "name\"[:punct:] \"[a-zA-Z \\.]+\"",
      9
    )
  )

col_names <-
  select_if(movies_merged, is.numeric) %>% # only numeric columns
  select(-c(oscar_bin, id))

clean_names <-
  c(
    "IMDb score",
    "IMDb vote count",
    "Metascore",
    "Oscar wins",
    "Oscar nominations",
    "Award wins",
    "Award nominations",
    "Budget",
    "Popularity",
    "Revenue",
    "Runtime",
    "IMDb votes year",
    "Profit",
    "Profit margin"
  )
raw_names <- colnames(col_names)

# create dataframe to convert column names
convert_frame <-
  data.frame(clean_names = clean_names, raw_names = raw_names)

convert <- function(name, raw) {
  #' Returns clean or raw name for @x
  #'
  #' @param name name of column
  #' @param raw whether raw or clean name should be returned
  #'
  #' @noRd
  
  if (raw == TRUE) {
    return(convert_frame[convert_frame["raw_names"] == name][1])
  } else {
    return(convert_frame[convert_frame["clean_names"] == name][2])
  }
}

# choices for custom scatter plot
choices_scatter <-
  c(
    "IMDb score" = "imdb_score",
    "IMDb vote count" = "imdb_vote_count",
    "Metascore" = "metascore",
    "Oscar wins" = "oscar_wins",
    "Oscar nominations" = "oscar_nominations",
    "Award wins" = "award_wins",
    "Award nominations" = "award_nominations",
    "Budget" = "budget",
    "Popularity" = "popularity",
    "Revenue" = "revenue",
    "Runtime" = "runtime",
    "IMDb votes year" = "imdb_votes_year",
    "Profit" = "profit",
    "Profit margin" = "profit_margin"
  )

f1 <- list(family="Roboto, sans-serif", color="rgb(205,205,205)", size=14) # font for axis ticks
f2 <- list(family="Roboto, sans-serif", color="rgb(205,205,205)", size=18) # font for axis titles


