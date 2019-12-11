library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

# Define UI for application that displays an about page and the app itself

dashboardPage(skin="blue",
    #add title
    dashboardHeader(title="Voter Data for Mecklenburg County",titleWidth=750),
  
    #define sidebar items
    dashboardSidebar(sidebarMenu(
          menuItem("About", tabName = "about", icon = icon("archive")),
          menuItem("Explore", tabName = "explore", icon = icon("archive")),
          menuItem("Investigate", tabName = "investigate", icon = icon("search")),
          menuItem("Predict", tabName = "app", icon = icon("chart-line")),
          br(),
          h4("Voter Data Reporting Criteria"),
          checkboxInput("City", h5("Charlotte Residents Only", style = "color:yellow;")),
                h5("Select Voter Registration Status:"),
                selectizeInput("status", "Voter Status", selected = "Active", choices = levels(as.factor(c("Active", "Inactive")))),
                br(),
          br(),
          h4("Election Dates"),
#                checkboxInput("elecDates", h5("Specify Election Dates", style = "color:black;")),
                    # Add conditional checkbox panel input for changing opacity of points based on sleep_rem value
#                    conditionalPanel(condition = "input.elecDates",
                                     checkboxGroupInput("pickDates","Select election dates for analysis:", elecList),
#                                     actionLink("selectall","Select All")),
          br()
      )),
              
              
    #define the body of "about" section
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "about",
                fluidRow(
                  #add in latex functionality if needed: \ for inline and $$ for separate line...
                  withMathJax(),
                  helpText('An irrational number \\(\\sqrt{2}\\)
           and a fraction $$1-\\frac{1}{2}$$'),
                  helpText('and a fact about \\(\\pi\\):
           $$\\frac2\\pi = \\frac{\\sqrt2}2 \\cdot
           \\frac{\\sqrt{2+\\sqrt2}}2 \\cdot
           \\frac{\\sqrt{2+\\sqrt{2+\\sqrt2}}}2 \\cdots$$'),
#                  helpText('and output 2 $$3^2+4^2=5^2$$'),
#                  helpText('and output 3 $$\\sin^2(\\theta)+\\cos^2(\\theta)=1$$'),
 #                 helpText('The busy Cauchy distribution $$\\frac{1}{\\pi\\gamma\\,\\left[1 + \\left(\\frac{x-x_0}{\\gamma}\\right)^2\\right]}\\!$$')),
#                helpText('You do not see me initially: $$e^{i \\pi} + 1 = 0$$'),
#                helpText('If \\(X\\) is a Cauchy random variable, then $$P(X \\leq %.03f ) = %.03f$$')
                    #h5('You do not see me initially: $$ \neq   \div   \ast  \times  \alpha \beta \sigma \mu \epsilon \sum{a,b} \binom {a} {b} {a \brack b}   {a \brace b}   [a]   \lbrace{a}\rbrace   \leq  \geq x_2   or   x^2$$')
                  ),
                  #two columns for each of the two items
                  column(6,
                         #Description of App
                         h1("What does this app do?"),
                         #box to contain description
                         box(background="red",width=12,
                             h4("This application visualizes ..."),
                             h3("An order statistic is ...."),
#                            h2("\\neq   \div   \ast  \times  \alpha \beta \sigma \mu \epsilon \sum{a,b} \binom {a} {b} {a \brack b}   {a \brace b}   [a]   \lbrace{a}\rbrace   \leq  \geq x_2   or   x^2$$")
                         )
                  ),
                  column(6,
                         #How to use the app
                         h1("How to use the app?"),
                         #box to contain description
                         box(background="red",width=12,
                             h4("The tabs across the top ..."),
                             h5("The controls for the visualization ..."),
#                             withMathJax(h5("$$d_{ij}=d(\{X_i\}, \{X_j\}) = { \|X_i - X_j\|^2}$$ For mathJax: $$\\(d_{ij}=d(\\{X_i\\}, \\{X_j\\}) = { \\|X_i - X_j\\|^2}\\)$$"))
#                             withMathJax(h5("or \\(d_{ij}=d(\\{X_i\\}, \\{X_j\\}) = { \\|X_i - X_j\\|^2}\\"))
#                             uiOutput('ex1'),
                         )
                )
        ),

    #define the body of the "explore" section
        # Second tab content
      tabItem(tabName = "explore",
            fluidRow(
              column(12,
                  h1("Voter Data for Mecklenburg County"),
                  h2("Exploratory Analysis"),
                  br(),
                  column(4,
                       box(width=12,title="Beta distribution with parameters",
                           numericInput("Param1","Alpha = ",value=1,min=0.1,step=0.1),
                           numericInput("Param2","Beta = ",value=1,min=0.1,step=0.1),
                           sliderInput("slider", "Number of elections", min = 1, max = 20, value = 19, step = 1),
                           h4("Order statistics of interest, choose integers from 1 to n"),
                           numericInput("ord1","1st Order Stat",value=1,min=1,max=5),
                           checkboxInput("overlay",label="Overlay Theoretical Distribution",value=FALSE)
                       )
                  ),
                  #Show a plot of Exploration
                  column(8,
                         plotOutput("sleepPlot")
                  )
              )
      )),
    
        tabItem(tabName = "investigate",
            fluidRow(
              column(4,
                       box(width=12,title="Beta distribution with parameters",
                           numericInput("Param1","Alpha = ",value=1,min=0.1,step=0.1),
                           numericInput("Param2","Beta = ",value=1,min=0.1,step=0.1),
                           sliderInput("sampleSize","Sample size:",min=1,max=30,value=5),
                           h4("Order statistics of interest, choose integers from 1 to n"),
                           numericInput("ord1","1st Order Stat",value=1,min=1,max=5),
                           checkboxInput("overlay",label="Overlay Theoretical Distribution",value=FALSE)
                       )
                ),
                #Show a plot of the prior
                column(8,
                       plotOutput("sleepPlot")
                ),
                #Show a plot of the prior    
                column(8,
                       tabsetPanel(
                         tabPanel("Supervised Learning",           
                                  fluidRow(
                                  plotOutput("sleepPlot")
                                  )
                                 ), #end tab panel
                         tabPanel("Unsupervised Learning", 
                                  fluidRow(
                                    column(4,
                                           sliderInput("sampleSize","Sample size:",min=1,max=30,value=5),
                                           br(),
                                           sliderInput("sampleSize","Sample size:",min=1,max=30,value=5)
                                    ),
                                    column(4,
                                           sliderInput("sampleSize","Sample size:",min=1,max=30,value=5),
                                           br(),
                                           sliderInput("sampleSize","Sample size:",min=1,max=30,value=5)
                                    )
                                  )        
                                 ), #end tab panel
                         tabPanel("Other", 
                                  tabsetPanel(
                                    tabPanel("kNN",
                                             fluidRow(
                                               h3("Some text here... includeText")
                                             )                
                                    ),
                                    tabPanel("Principal Components",
                                             fluidRow(
                                               h3("Some text here...includeMarkdown")
                                             )
                                    ),
                                    tabPanel("Classification Tree",
                                             fluidRow(
                                               h3("Some text here...includeHTML()")
                                             )
                                    ),
                                    tabPanel("Regression",
                                             fluidRow(
                                               h3("Some text here...includeMarkdown")
                                             )
                                    ),
                                    tabPanel("Log",
                                             fluidRow(
                                               h3("Some text here...includeMarkdown")
                                             )
                                    )#end tabPanel
                                  ) #end tabsetPanel
                         ) #end tab panel
                       ) #end tab set
                ) #end column
              ) #end fluidrow
      ) #end tabItem
        ) #end tabItems
      )
)