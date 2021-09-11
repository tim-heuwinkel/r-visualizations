library(dashboardthemes)
library(shiny)
library(shinydashboard)

# Code below taken from: https://github.com/nik01010/dashboardThemeSwitcher

# Ui functions ------------------------------------------------------------
uiChangeThemeDropdown <- function(dropDownLabel = "Change Theme", defaultTheme = "grey_dark")
{
  changeThemeChoices <- c(
    "Grey dark" = "grey_dark",
    "Purple gradient" = "purple_gradient"
  )
  
  ns <- NS("moduleChangeTheme")
  dropdown <- tagList(
    selectizeInput(
      inputId = ns("dbxChangeTheme"),
      label = dropDownLabel,
      choices = changeThemeChoices,
      selected = defaultTheme
    )
  )
  
  return(dropdown)
}

uiChangeThemeOutput <- function()
{
  ns <- NS("moduleChangeTheme")
  themeOutput <- tagList(
    uiOutput(ns("uiChangeTheme"))
  )
  
  return(themeOutput)
}


# Server functions --------------------------------------------------------
serverChangeTheme <- function(input, output, session)
{
  observeEvent(
    input$dbxChangeTheme, 
    {
      output$uiChangeTheme <- renderUI({
        shinyDashboardThemes(theme = input$dbxChangeTheme)
      })
    }
  )
}