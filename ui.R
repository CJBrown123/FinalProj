library(shinydashboard)
library(DT)
library(ggplot2)

# Define UI for application that displays an about page and the app itself

dashboardPage(skin="blue",
              #add title
              dashboardHeader(title="Voter Registration Data for Mecklenburg County",titleWidth=750),
              
              #define sidebar items
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("archive")),
                menuItem("Explore", tabName = "explore", icon = icon("search")),
                menuItem("tbd", tabName = "tbd", icon = icon("archive")),
                menuItem("Application", tabName = "app", icon = icon("laptop")),
                menuItem("About", tabName = "about", icon = icon("archive")),
                menuItem("Application", tabName = "app", icon = icon("laptop"))
              )),
              
              #define the body of the app
              dashboardBody(
                tabItems(
                  # First tab content
                  tabItem(tabName = "about",

shinyUI(fluidPage(
  
  
  # Application title
  titlePanel("Voter Registration Data for Mecklenburg County"),
  
  
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      h3("Demographic Data Filters"),
      br(),
          h5("Select the mammal's biological order:"),
          selectizeInput("vore", "Vore", selected = "omni", choices = levels(as.factor(msleep$vore))),
          br(),
          sliderInput("size", "Size of Points on Graph",
                  min = 1, max = 10, value = 5, step = 1),
      h3("Geeographic Data Filters"),
      br(),
          checkboxInput("conservation", h4("Color Code Conservation Status", style = "color:red;")),
      # Add conditional checkbox panel input for changing opacity of points based on sleep_rem value
      conditionalPanel(condition = "input.conservation",
                       checkboxInput("rem", "Also change symbol based on REM sleep?"))
      h3("Other Filters"),
      br(),
      checkboxInput("conservation", h4("Color Code Conservation Status", style = "color:red;")),
      checkboxInput("conservation", h4("Color Code Conservation Status", style = "color:red;")),
      checkboxInput("conservation", h4("Color Code Conservation Status", style = "color:red;")),
      ),
    
    # Show outputs
    mainPanel(
      plotOutput("sleepPlot"),
      textOutput("info"),
      tableOutput("table")
      )
  )
))
