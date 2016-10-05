setwd("D://Projects//Social Network Analysis//Shiny//Wockhardt")

library(datasets)
library(data.table)
library(dplyr)
library(highcharter)
library(purrr)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinysky)

#Declare Default login as false
Logged = FALSE;     ##### Change to FALSE before deploying
Psw=data.frame(my_username="A",my_password="A",display_name="ABCDEFGHI")

newtab = "dashboard"

login <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  # div(id = "login",
  fluidRow(
    column(
      width = 8
    ),
    column(
      width = 4,
      box(
        width = NULL,
        title = "Login",
        textInput("userName", "Username"),
        passwordInput("passwd", "Password"),
        br(),
        fluidRow(
          column(width = 4),
          column(width = 4,
                 shinysky::actionButton("Login","Log in", styleclass="warning", icon = "key", icon.library = "font awesome", block = TRUE)
          ),
          column(width = 4)
        ),
        br()
      )
    ),
    column(
      width = 4
    )
  ),
  #LOGIN SUCCESSFUL
  fluidRow(
    column(
      width = 8
    ),
    column(
      width = 4,
      shinyalert("shinyalert_success", FALSE, auto.close.after = 3)
    ),
    column(
      width = 4
    )
  ),
  #LOGIN FAIL
  fluidRow(
    column(
      width = 8
    ),
    column(
      width = 4,
      shinyalert("shinyalert_fail", FALSE, auto.close.after = 3)
    ),
    column(
      width = 4
    )
  )
  # )
)

