#SHINY SERVER


####################################
######### TAB 2 FUNCTIONS ##########
####################################

#TAB 2 - HIRING
function_hiring <- function(input_bu){
  data(diamonds, mpg, package = "ggplot2")
  chart <- hchart(mpg, "scatter", x = displ, y = hwy, group = class)
  return(chart)
}

#TAB 2 - ATTRITION
function_attrition <- function(input_bu){
  chart <-  highchart() %>% 
    hc_chart(type = "column") %>% 
    # hc_title(text = "A highcharter chart") %>% 
    hc_xAxis(categories = 2012:2016) %>% 
    hc_add_series(data = c(3900,  4200,  5700,  8500, 11900),
                  name = "Downloads")
  
  return(chart)
}

#TAB 2 - PERFORMANCE
function_performance <- function(input_bu){
  chart <- hchart(diamonds$price, color = "#B71C1C", name = "Price") %>% 
    hc_title(text = "Zoom demo")
  return(chart)
}

#TAB 2 - ENGAGMENT
function_engagement <- function(input_bu){
  
  df <- data_frame(
    name = c("Animals", "Fruits", "Cars"),
    y = c(5, 2, 4),
    drilldown = tolower(name)
  )
  
  ds <- list_parse(df)
  names(ds) <- NULL
  # str(ds)
  
  chart <- highchart() %>% 
    hc_chart(type = "column") %>% 
    hc_title(text = "Column drilldown") %>% 
    hc_xAxis(type = "category") %>% 
    hc_legend(enabled = FALSE) %>% 
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)
      )
    ) %>% 
    hc_add_series(
      name = "Things",
      colorByPoint = TRUE,
      data = ds
    )
  
  dfan <- data_frame(
    name = c("Cats", "Dogs", "Cows", "Sheep", "Pigs"),
    value = c(4, 3, 1, 2, 1)
  )
  
  dffru <- data_frame(
    name = c("Apple", "Organes"),
    value = c(4, 2)
  )
  
  dfcar <- data_frame(
    name = c("Toyota", "Opel", "Volkswage"),
    value = c(4, 2, 2) 
  )
  
  second_el_to_numeric <- function(ls){
    
    map(ls, function(x){
      x[[2]] <- as.numeric(x[[2]])
      x
    })
    
  }
  
  dsan <- second_el_to_numeric(list_parse2(dfan))
  
  dsfru <- second_el_to_numeric(list_parse2(dffru))
  
  dscar <- second_el_to_numeric(list_parse2(dfcar))
  
  
  chart <- chart %>% 
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list(
        list(
          id = "animals",
          data = dsan
        ),
        list(
          id = "fruits",
          data = dsfru
        ),
        list(
          id = "cars",
          data = dscar
        )
      )
    ) %>% hc_legend(enabled = T)
  
  # chart <- hc
  return(chart)
}

#TAB 2 - VALUEBOX - HEADCOUNT
tab2_valuebox_headcount <- function(input_bu){
  return(
    valueBox("19,371",
             "Head Count",
             icon = icon("users", lib = "font-awesome"),
             color = "orange",
             width = NULL,
             href = NULL)
  )
  
}

#TAB 2 - VALUEBOX - REVENUE
tab2_valuebox_revenue <- function(input_bu){
  return(
    valueBox(value = "$123,456",
             subtitle = "Revenue",
             icon = icon("money", lib = "font-awesome"),
             color = "blue",
             width = NULL,
             href = NULL)
  )
}

#TAB 2 - VALUEBOX - COST
tab2_valuebox_cost <- function(input_bu){
  return(
    valueBox(value = "$987,654",
             subtitle = "Cost",
             icon = icon("usd", lib = "font-awesome"),
             color = "teal",
             width = NULL,
             href = NULL)
  )
}

#TAB 2 - VALUEBOX - NET VALUE
tab2_valuebox_netvalue <- function(input_bu){
  return(
    valueBox(value = "$1,111,110",
             subtitle = "Net Value",
             icon = icon("diamond", lib = "font-awesome"),
             color = "green",
             width = NULL,
             href = NULL)
  )
}




####################################
######### TAB 3 FUNCTIONS ##########
####################################

#TAB 3 - CHART 1 - STACKED BAR
function_tab3_chart1 <- function(input_analysis){
  mydata <- data.frame(A=runif(1:3),
                       B=runif(1:3),
                       C=runif(1:3))
  
  chart <- highchart() %>% 
    hc_chart(type = "bar") %>% 
    hc_title(text = "Business Units") %>% 
    # hc_yAxis(title = list(text = "Weights")) %>%
    hc_series(list(name="BU1",data=mydata$A),
              list(name="BU2",data=mydata$B),
              list(name="BU3",data=mydata$C)) %>%
    hc_plotOptions(series = list(
      dataLabels = list(enabled = FALSE),
      stacking = "percent")
    )
  
  
  return(chart)
}

#TAB 3 - CHART 2 - PIE
function_tab3_chart2 <- function(input_analysis){
  df <- data_frame(
    name = c("Animals", "Fruits", "Cars"),
    y = c(5, 2, 4),
    drilldown = tolower(name)
  )
  
  ds <- list_parse(df)
  names(ds) <- NULL
  # str(ds)
  
  chart <- highchart() %>% 
    hc_chart(type = "pie") %>% 
    hc_title(text = "Pie drilldown") %>% 
    hc_xAxis(type = "category") %>% 
    hc_legend(enabled = FALSE) %>% 
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)
      )
    ) %>% 
    hc_add_series(
      name = "Things",
      colorByPoint = TRUE,
      data = ds
    )
  
  dfan <- data_frame(
    name = c("Cats", "Dogs", "Cows", "Sheep", "Pigs"),
    value = c(4, 3, 1, 2, 1)
  )
  
  dffru <- data_frame(
    name = c("Apple", "Organes"),
    value = c(4, 2)
  )
  
  dfcar <- data_frame(
    name = c("Toyota", "Opel", "Volkswage"),
    value = c(4, 2, 2) 
  )
  
  second_el_to_numeric <- function(ls){
    
    map(ls, function(x){
      x[[2]] <- as.numeric(x[[2]])
      x
    })
    
  }
  
  dsan <- second_el_to_numeric(list_parse2(dfan))
  
  dsfru <- second_el_to_numeric(list_parse2(dffru))
  
  dscar <- second_el_to_numeric(list_parse2(dfcar))
  
  
  chart <- chart %>% 
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list(
        list(
          id = "animals",
          data = dsan
        ),
        list(
          id = "fruits",
          data = dsfru
        ),
        list(
          id = "cars",
          data = dscar
        )
      )
    ) %>% hc_legend(enabled = T)
  
  # chart <- hc
  return(chart)
  
}

#TAB 3 - CHART 3 - DATA TABLE
function_tab3_chart3 <- function(input_analysis){
  datatable(iris)
}

#TAB 3 - VALUEBOX - HEADCOUNT
tab3_valuebox_headcount <- function(input_analysis){
  return(
    valueBox(value = "19,371",
             subtitle = "Head Count",
             icon = icon("users", lib = "font-awesome"),
             color = "orange",
             width = NULL,
             href = NULL)
  )
}

#TAB 3 - VALUEBOX - COST
tab3_valuebox_cost <- function(input_analysis){
  return(
    valueBox(
      value = "$987,654",
      subtitle = "Cost",
      icon = icon("usd", lib = "font-awesome"),
      color = "green",
      width = NULL,
      href = NULL)
  )
}

shinyServer(
  function(input, output, session){
    #Declare USER for LOGIN
    USER <<- reactiveValues(Logged = Logged)
    
    #LOAD TAB 1 on login
    observeEvent(input$Login, {
      newtab <- "dashboard_tab1"
      updateTabItems(session, "tabs", newtab)
    })
    
    #USER LOGIN
    observeEvent(input$Login,({ 
      if (USER$Logged == FALSE) {
        if (!is.null(input$Login)) {
          if (input$Login > 0) {
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            psw1=subset(Psw,Psw$my_username==Username & Psw$my_password==Password)
            if(nrow(psw1)==1){
              USER$Logged <<- TRUE
              showshinyalert(session, "shinyalert_success",
                             paste(icon("spinner", class = "fa-spin fa-2x fa-pull-left fa-pulse", lib = "font-awesome"),"Login Successful for ", input$userName), 
                             styleclass = "success")
            } else{
              showshinyalert(session, "shinyalert_fail",
                             paste(icon("warning", class = "fa-2x fa-pull-left", lib = "font-awesome"),"Invalid Username or Password"), 
                             styleclass = "warning")
            }
          } 
        }
      }    
    })
    )
    
    #USER LOGOUT - TAB 1
    observeEvent(input$link_logout_tab1,({
      if(input$link_logout_tab1 == 0)
        return()
      else{
        session$reload()
        js$hideSidebar()
        output$dashboardBody <- renderUI({
          login
        })
      }
    })
    )
    
    #USER LOGOUT - TAB 2
    observeEvent(input$link_logout_tab2,({
      if(input$link_logout_tab2 == 0)
        return()
      else{
        session$reload()
        js$hideSidebar()
        output$dashboardBody <- renderUI({
          login
        })
      }
    })
    )
    
    #USER LOGOUT - TAB 3
    observeEvent(input$link_logout_tab3,({
      if(input$link_logout_tab3 == 0)
        return()
      else{
        session$reload()
        js$hideSidebar()
        output$dashboardBody <- renderUI({
          login
        })
      }
    })
    )
    
    #RENDER SIDEBAR
    output$sidebarpanel <- renderUI({
      if (USER$Logged == TRUE) { 
        dashboardSidebar(
          width = 260,
          sidebarMenu(
            #ID TO LINK BUTTON TO TAB 1
            id = "tabs",
            menuItem("", icon = icon(""),tabName = "dashboard_tab1"),
            menuItem("", icon = icon(""),tabName = "dashboard_tab2"),
            menuItem("", icon = icon(""),tabName = "dashboard_tab3"),
            conditionalPanel(
              condition = "input.tabs == 'dashboard_tab2'",
              tags$div(class = "sidebar-dashboard",
                       tags$div(class = "selection_pane_label",
                                tags$b("Selection pane")),
                       selectInput("input_tab2_select_bu", "Select a BU", choices = c("B1", "B2", "B3"))
              )
            ),
            conditionalPanel(
              condition = "input.tabs == 'dashboard_tab3'",
              tags$div(class = "sidebar-dashboard",
                       tags$div(class = "selection_pane_label",
                                tags$b("Selection pane")),
                       selectInput("input_tab3_select_analysis", "Select analysis", choices = c("Hiring", "Attrition", "Performance", "Engagement"))
              )
            )
          )
        )
      }
    }
    )
    
    #RENDER BODY ELSE LOGIN PAGE
    output$dashboardBody <- renderUI({
      if (USER$Logged == TRUE) {
        # dashboardBody(
        tabItems(
          tabItem("dashboard_tab1",
                  tags$div(class = "navigation",
                           fluidRow(
                             box(
                               width = 12,
                               title = NULL,
                               solidHeader = FALSE,
                               background = NULL,
                               column(
                                 width = 3,
                                 actionButton("tab1","TAB 1", block = T, styleclass = "primary")
                               ),
                               column(
                                 width = 3,
                                 actionButton("nav_tab1_to_tab2","TAB 2", block = T)
                               ),
                               column(
                                 width = 3,
                                 actionButton("nav_tab1_to_tab3","TAB 3", block = T)
                               ),
                               column(
                                 width = 3,
                                 tags$div(class = "user_name", "User: ", tags$b(Psw$display_name[Psw$my_username==input$userName])),
                                 tags$div(class = "user_name_link_logout", actionLink("link_logout_tab1","Logout", icon = icon("sign-out")))
                               )
                             )
                           )
                  )
          ),
          tabItem("dashboard_tab2",
                  tags$div(class = "navigation",
                           fluidRow(
                             box(
                               width = 12,
                               title = NULL,
                               solidHeader = FALSE,
                               background = NULL,
                               column(
                                 width = 3,
                                 actionButton("nav_tab2_to_tab1","TAB 1", block = T, styleclass = "primary")
                               ),
                               column(
                                 width = 3,
                                 actionButton("tab2","TAB 2", block = T)
                               ),
                               column(
                                 width = 3,
                                 actionButton("nav_tab2_to_tab3","TAB 3", block = T)
                               ),
                               column(
                                 width = 3,
                                 tags$div(class = "user_name", "User: ", tags$b(Psw$display_name[Psw$my_username==input$userName])),
                                 tags$div(class = "user_name_link_logout", actionLink("link_logout_tab2","Logout", icon = icon("sign-out")))
                               )
                             )
                           )
                  ),
                  fluidRow(
                    box(
                      width = 12,
                      valueBoxOutput("chart_tab2_valuebox_headcount", width = 3),
                      valueBoxOutput("chart_tab2_valuebox_revenue", width = 3),
                      valueBoxOutput("chart_tab2_valuebox_cost", width = 3),
                      valueBoxOutput("chart_tab2_valuebox_netvalue", width = 3)
                    )
                  ),
                  fluidRow(
                    tags$div(class="box_tab2",
                             box(
                               width = 12,
                               fluidRow(
                                 column(
                                   width = 6,
                                   actionLink("tab2_hiring","Hiring"),
                                   highchartOutput("chart_tab2_hiring")
                                 ),
                                 column(
                                   width = 6,
                                   actionLink("tab2_attrition","Attrition"),
                                   highchartOutput("chart_tab2_attrition")
                                 )
                               ),
                               fluidRow(
                                 column(
                                   width = 6,
                                   actionLink("tab2_performance","Performance"),
                                   highchartOutput("chart_tab2_performance")
                                 ),
                                 column(
                                   width = 6,
                                   actionLink("tab2_engagement","Engagement"),
                                   highchartOutput("chart_tab2_engagement")
                                 )
                               )
                             )
                    )
                  )
          ),
          tabItem("dashboard_tab3",
                  tags$div(class = "navigation",
                           fluidRow(
                             box(
                               width = 12,
                               title = NULL,
                               solidHeader = FALSE,
                               background = NULL,
                               column(
                                 width = 3,
                                 actionButton("nav_tab3_to_tab1","TAB 1", block = T, styleclass = "primary")
                               ),
                               column(
                                 width = 3,
                                 actionButton("nav_tab3_to_tab2","TAB 2", block = T)
                               ),
                               column(
                                 width = 3,
                                 actionButton("tab3","TAB 3", block = T)
                               ),
                               column(
                                 width = 3,
                                 tags$div(class = "user_name", "User: ", tags$b(Psw$display_name[Psw$my_username==input$userName])),
                                 tags$div(class = "user_name_link_logout", actionLink("link_logout_tab3","Logout", icon = icon("sign-out")))
                               )
                             )
                           )
                  ),
                  fluidRow(
                    box(
                      width = 12,
                      valueBoxOutput("chart_tab3_valuebox_headcount", width = 6),
                      valueBoxOutput("chart_tab3_valuebox_cost", width = 6)
                    )
                  ),
                  fluidRow(
                    tags$div(class = "box_tab3",
                             box(
                               width = 12,
                               fluidRow(
                                 column(
                                   width = 6,
                                   highchartOutput("output_tab3_chart1")
                                 ),
                                 column(
                                   width = 6,
                                   highchartOutput("output_tab3_chart2")
                                 )
                               ),
                               # fluidRow(
                                 DT::dataTableOutput("output_tab3_chart3")
                               # )
                             )
                    )
                  )
          )
        )
      }
      else {
        login
        # tags$style(type="text/css", "#login {font-size:10px;   text-align: left; position:absolute; top: 40%; left: 50%; margin-top: -100px; margin-left: -150px;}")
      }
    }
    )
    
    #HIDE SIDEBAR ON LOGIN PAGE
    observe({
      if (USER$Logged == FALSE) {
        js$hideSidebar()
      }
    })
    
    #SHOW SIDEBAR INSIDE DASHBOARD
    observe({
      if (USER$Logged == TRUE) {
        js$showSidebar()
      }
    })
    
    
    
    
    #################################
    ####### TAB 2 - VALUEBOX ########
    #################################
    output$chart_tab2_valuebox_headcount<- renderValueBox({
      tab2_valuebox_headcount(input$input_tab2_select_bu)
    })
    
    output$chart_tab2_valuebox_revenue <- renderValueBox({
      tab2_valuebox_revenue(input$input_tab2_select_bu)
    })
    
    output$chart_tab2_valuebox_cost <- renderValueBox({
      tab2_valuebox_cost(input$input_tab2_select_bu)
    })
    
    output$chart_tab2_valuebox_netvalue <- renderValueBox({
      tab2_valuebox_netvalue(input$input_tab2_select_bu)
    })
    
    
    #################################
    ######## TAB 2 - CHARTS #########
    #################################
    
    #TAB 2 - HIRING
    output$chart_tab2_hiring <- renderHighchart({
      function_hiring(input$input_tab2_select_bu)
    })
    
    #TAB 2 - ATTRITION
    output$chart_tab2_attrition <- renderHighchart({
     function_attrition(input$input_tab2_select_bu)
    })
    
    #TAB 2 - PERFORMANCE
    output$chart_tab2_performance <- renderHighchart({
      function_performance(input$input_tab2_select_bu)
    })
    
    #TAB 2 - ENGAGEMENT
    output$chart_tab2_engagement <- renderHighchart({
      function_engagement(input$input_tab2_select_bu)
    })
    
    
    
    #################################
    ######## TAB 2 - SIDEBAR ########
    #################################
    
    #UPDATE TAB AND DROPDOWN - HIRING
    observeEvent(input$tab2_hiring,{
      newtab <<- "dashboard_tab3"
      updateTabItems(session, "tabs", newtab)
      updateSelectInput(session, "input_tab3_select_analysis", "Month", 
                        choices = c("Hiring", "Attrition", "Performance", "Engagement"), selected = "Hiring")
    })
    
    #UPDATE TAB AND DROPDOWN - ATTRITION
    observeEvent(input$tab2_attrition,{
      newtab <<- "dashboard_tab3"
      updateTabItems(session, "tabs", newtab)
      updateSelectInput(session, "input_tab3_select_analysis", "Month", 
                        choices = c("Hiring", "Attrition", "Performance", "Engagement"), selected = "Attrition")
    })
    
    #UPDATE TAB AND DROPDOWN - PERFORMANCE
    observeEvent(input$tab2_performance,{
      newtab <<- "dashboard_tab3"
      updateTabItems(session, "tabs", newtab)
      updateSelectInput(session, "input_tab3_select_analysis", "Month", 
                        choices = c("Hiring", "Attrition", "Performance", "Engagement"), selected = "Performance")
    })
    
    #UPDATE TAB AND DROPDOWN - ENGAGEMENT
    observeEvent(input$tab2_engagement,{
      newtab <<- "dashboard_tab3"
      updateTabItems(session, "tabs", newtab)
      updateSelectInput(session, "input_tab3_select_analysis", "Month", 
                        choices = c("Hiring", "Attrition", "Performance", "Engagement"), selected = "Engagement")
    })
    
    
    
    
    #################################
    ####### TAB 3 - VALUEBOX ########
    #################################
    
    output$chart_tab3_valuebox_headcount<- renderValueBox({
      tab3_valuebox_headcount(input$input_tab3_select_analysis)
    })
    
    output$chart_tab3_valuebox_cost<- renderValueBox({
      tab3_valuebox_cost(input$input_tab3_select_analysis)
    })
    
    #################################
    ######## TAB 3 - CHARTS #########
    #################################
    
    #TAB 3 - CHART 1
    output$output_tab3_chart1 <- renderHighchart({
      function_tab3_chart1(input$input_tab3_select_analysis)
    })
    
    #TAB 3 - CHART 2
    output$output_tab3_chart2 <- renderHighchart({
      function_tab3_chart2(input$input_tab3_select_analysis)
    })
    
    #TAB 3 - CHART 3
    output$output_tab3_chart3 <- DT::renderDataTable({
      function_tab3_chart3(input$input_tab3_select_analysis)
    })
    
    
    
    
    
    
    
    
    
    
    
    #################################
    ###### NAVIGATION - TAB 1 #######
    #################################
    
    #NAVIGATION ACTION BUTTONS: TAB 1
    observeEvent(input$tab1, {
      newtab <<- "dashboard_tab1"
      updateTabItems(session, "tabs", newtab)
    })
    
    #NAVIGATION ACTION BUTTONS: TAB 1 TO TAB 2
    observeEvent(input$nav_tab1_to_tab2, {
      newtab <<- "dashboard_tab2"
      updateTabItems(session, "tabs", newtab)
    })
    
    #NAVIGATION ACTION BUTTONS: TAB 1 TO TAB 3
    observeEvent(input$nav_tab1_to_tab3, {
      newtab <<- "dashboard_tab3"
      updateTabItems(session, "tabs", newtab)
    })
    
    
    
    
    
    #################################
    ###### NAVIGATION - TAB 2 #######
    #################################
    
    #NAVIGATION ACTION BUTTONS: TAB 2 TO TAB 1
    observeEvent(input$nav_tab2_to_tab1, {
      newtab <<- "dashboard_tab1"
      updateTabItems(session, "tabs", newtab)
    })
    
    #NAVIGATION ACTION BUTTONS: TAB 2
    observeEvent(input$tab2, {
      newtab <<- "dashboard_tab2"
      updateTabItems(session, "tabs", newtab)
    })
    
    #NAVIGATION ACTION BUTTONS: TAB 2 TO TAB 3
    observeEvent(input$nav_tab2_to_tab3, {
      newtab <<- "dashboard_tab3"
      updateTabItems(session, "tabs", newtab)
    })
    
    
    
    
    
    #################################
    ###### NAVIGATION - TAB 3 #######
    #################################
    
    #NAVIGATION ACTION BUTTONS: TAB 3 TO TAB 1
    observeEvent(input$nav_tab3_to_tab1, {
      newtab <<- "dashboard_tab1"
      updateTabItems(session, "tabs", newtab)
    })
    
    #NAVIGATION ACTION BUTTONS: TAB 2
    observeEvent(input$nav_tab3_to_tab2, {
      newtab <<- "dashboard_tab2"
      updateTabItems(session, "tabs", newtab)
    })
    
    #NAVIGATION ACTION BUTTONS: TAB 2 TO TAB 3
    observeEvent(input$tab3, {
      newtab <<- "dashboard_tab3"
      updateTabItems(session, "tabs", newtab)
    })
    
  }
)