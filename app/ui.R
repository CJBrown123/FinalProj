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
          menuItem("About the App", tabName = "aboutApp", icon = icon("archive")),
          menuItem("About the Data", tabName = "aboutData", icon = icon("question-circle")),
          menuItem("Explore", tabName = "explore", icon = icon("chart-bar")),
          menuItem("Investigate", tabName = "investigate", icon = icon("search")),
          menuItem("Model", tabName = "model", icon = icon("chart-line")),
          menuItem("Data", tabName = "data", icon = icon("table")),
          
          br(),
          h4("Voter Data Reporting Criteria"),
          checkboxInput("City", h5("Charlotte Residents Only", style = "color:yellow;")),
                h5("Select Voter Registration Status:"),
                selectizeInput("status", "Voter Status", selected = "Active", choices = levels(as.factor(c("Active", "Inactive")))),
                br(),
          br(),
          h4("Election Dates"),
                                     checkboxGroupInput("pickDates","Select election dates for analysis:", elecList),
          br()
      )),
              
              
    #define the body of "about" section
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "aboutApp",
                fluidRow(
                  tags$head(tags$style(HTML("a {color: yellow}"))),
                  #two columns for each of the two items
                  column(6,
                         #Description of App
                         h2("What does this app do?"),
                         #box to contain description
                         box(background="blue",width=12,
                             h4("The analysis sections of this app are divided into three main parts, each located under a seperate menu item on the dashboard; additionally, the last menu option (Table) allows you to view a sortable table of the data used in this analysis. The abilities of each analysis section are described below:"),
                             
                             h3(strong("Explore")), 
                             h4("In this section, you may perform exploratory analysis on single variables and in combinations of variables. Options for both categorical and quantitative variables are available. The user has the ability to select combinations of up to 3 variables to generate the following outputs:"),
                             br(),
                             h4(strong("Options for Categorical Variables")),
                             HTML("
                               <ul>
                                 <li> bar plot (up to 3 variables)</li>
                                 <li> bar plot by series (up to 2 variables)</li>
                                 <li> frequency/contingency table(up to 3 variables)</li>
                               </ul>
                             "),
                             h4(strong("Options for Quantitative Variables")),
                             HTML("
                               <ul>
                                 <li> histogram (up to 3 variables)</li>
                                 <li> scatter plot by series (up to 2 variables</li>
                                 <li> frequency/contingency table(up to 3 variables)</li>
                               </ul>
                             "),
                             br(),
                             
                             h3(strong("Investigate")), 
                             h4("In this section, you may conduct additional unsupervised analyses including hierarchical clustering and principla components analysis:"),
                             br(),
                             
                             h4(strong("Hierarchical Clustering")),
                             HTML("
                               <ul>
                                 <li> Outputs a dendogram of results</li>
                               </ul>
                             "),
                             br(),
                             
                             h4(strong("Principal Components Analysis")),
                             HTML("
                               <ul>
                                 <li> Outputs include paired plots of variables, a biplot and a cumulative scree plot of results.</li>
                               </ul>
                             "),
                             br(),
                             h3(strong("Model")), 
                             h4("This section, allows you to conduct supervised analyses including logistic regression and random forest analyses:"),
                             br(),
                             h4(strong("Logistic Regression")),
                             h4("Outputs regression formula and a simple plot of resulting regression line; also provides information on model accuracy"),
                             h4(strong("Random Forest Analyses")),
                             h4("Outputs include model formula and information on model accuracy"),
                             br()
                         )
        ),
                  column(6,
                         #Description of App
                         h2("How do I use this app?"),
                         #box to contain description
                         box(background="blue",width=12,
                             h4("To use this app, start by entering in the side panel any changes to defaule general criteria options to be applied to the data. Options include the ability to select for the registration status of voters, and specify election dates to include in the data. I recommend using the default options for voter status (to include only active voters)."),
                             h4("Once any general options have been selected, variables selection options are available within each analysis menu's pages and tabs. Analysis are grouped by number of variables and the data type of variables to be analyzed; the user has options to select from available variables."),
                         )
                )
        )
  ),
        
        tabItem(tabName = "aboutData",
        fluidRow(
          #add in latex functionality if needed: \ for inline and $$ for separate line...
        column(12,
               #Description of App
               h1("Information About the Data Used in this App"),
               #box to contain description
               box(background="blue",width=12,
                   h3("Data Source"),
                   h4("The data used by this application consists of voter registration data and voting history for Mecklenburg County for the most recent 20 major elections as of the file creation date; elections dates covered by the data range from May, 8, 2012 to October 8, 2019. For a listing of previous elections please check this County website:"), 
                      h4(tags$a("Mecklenburg County Previous Elections", href="https://www.mecknc.gov/BOE/data/Pages/PreviousElectionResults.aspx")),
                   br(),
                  h4("This data was obtained directly from the Mecklenburg County Board of Elections website:"),
                  h4(tags$a("Mecklenburg County Elections Data", href="https://www.mecknc.gov/BOE/data/Pages/VoterDataFileDetails.aspx")), 
                  br(),
                  h4("Similar data for all North Carolina Counties may be obtained directly from the North Carolina State Board of Elections website:"),
                  h4(tags$a("NC SBE Voter Data", href="https://www.ncsbe.gov/Public-Records-Data-Info/Election-Results-Data%23VoterRegistrationData")),
                  h4(tags$a("NC SBE Voter Data (direct file links", href="https://dl.ncsbe.gov/index.strong?prefix=data/")),
                  br(),
                  h4("Although the data obtained from the Mecklenburg County website is a single file summarizing all data, the State's reports are divided into two seperate files for the voter registration record and voter activity history."),
                  br(),
                  h3("Data File Information"),
                  h4("Data fields included in the file used by this application are shown in the table below."),
                  br(),
                  tableOutput("definitions"),
                  h5("*Variables categorized under strong(Voter History) repeat for each election date"),
                  br(),
                  h5(strong("Methods by which a voter may cast their ballot:")),
                  h5("    * C - Curbside"),
                  h5("    * M - Mail"),
                  h5("    * O - One Stop Early Voting"),
                  h5("    * P - Provisional Ballot"),
                  h5("    * T - Transfer"),
                  h5("    * U - One Stop Early Voting Curbside"),
                  h5("    * V - In-Person Election Day"),
                  br(),
                  h3("Other Notes On Data"),
                  h4("The Mecklenburg County voter data file consists of oover 700,000 records and includes both active and inactive voters.  (Due to this massive size, the app includes an option to select for  sample of records prior to conducting analysis.) Although he registration date is included in the voter file, there is no information provided regarding voter eligibility during the report period; information on date a voter became inactive is unavailable. It is recommended that analysis be conducted on voters shown as having an ACTIVE voter status (this option is selected by default)."),
                  
                  h4("When filtering data for analysis and creation of models, keep in mind that voting habits may differ significantly from presidential to non-presidential years and further variations may occur for odd-year elections if there are no major political races/issues on the ballot."),
                  h4("Voter current age as of the date of file generation is included in the data, but not the birthdate."),
                  h4("Consideration should also be given to other confounding factors not captured within the data that may imact voter participations; examples include changes to early voting options over time, income and education of individual, when an individual moved to the County (or otherwise first became eligible to vote), and undelying changes to the overall demographic makeup of the County population over time."),
                  h4("For additional information related to Mecklenburg County and North Carolina election processes, please visit the following sites:"),
                  br(),
                  h4(tags$a("Mecklenburg County Board of Elections", href="https://www.mecknc.gov/BOE/Pages/default.aspx")),
                  h4(tags$a("NC Board of Elections", href="https://www.ncsbe.gov/ERC"))
                  

                )
              )
        )
),
    #define the body of the "explore" section
        # Second tab content
      tabItem(tabName = "explore",
              h3("Exploratory Analysis: Summary Graphs and Tables"),
            fluidRow(
              column(6,
                     plotOutput("sleepPlot"),                     
                     #box(width=12,title="graphs and plots here",
                     h4("Optional Explore Text Here")
              ),
              br(),
              column(6,
              tabsetPanel(
                tabPanel("1 Variable",
                  tabsetPanel(
                      tabPanel("Categorical",
                               selectizeInput("ExpCatVar1", "Select Variable For Analysis", choices = c("a", "b", "c")),
                      ),
                      tabPanel("Quantitative",
                               fluidRow(
                                 h5("Some text here...includeMarkdown"),
                                 selectizeInput("ExpQuantVar1", "Select Variable For Analysis", choices = c("1a", "2b", "3c"))
                               )
                      )
                )#end tabPanel
                ), #end tab panel
                tabPanel("2 Variables", 
                  tabsetPanel(
                         tabPanel("Categorical",
                                  fluidRow(
                                    h5("Some text here... includeText"),
                                    selectizeInput("ExpCatVar2a", "Select Variable For Analysis", choices = c("a", "b", "c")),
                                    selectizeInput("ExpCatVar2b", "Select Variable For Analysis", choices = c("a", "b", "c"))
                                    
                                  )                
                         ),
                         tabPanel("Quantitative",
                                  fluidRow(
                                    h5("Some text here...includeMarkdown"),
                                    selectizeInput("ExpQuantVar2a", "Select Variable For Analysis", choices = c("1a", "2b", "3c")),
                                    selectizeInput("ExpQuantVar2b", "Select Variable For Analysis", choices = c("1a", "2b", "3c"))
                                  )
                         ),
                         tabPanel("Combination",
                                  fluidRow(
                                    h5("Some text here...includestrong()"),
                                    selectizeInput("ExpCombVar2a", "Select Categorical Variable For Analysis", choices = c("a", "b", "c")),
                                    selectizeInput("ExpCombVar2b", "Select Quantitative Variable For Analysis", choices = c("1a", "2b", "3c"))
                                  )
                         )
                         )
                ), #end tab panel
                tabPanel("3 Variables", 
                  tabsetPanel(
                           tabPanel("Categorical",
                                    fluidRow(
                                      h5("Some text here... includeText"),
                                      
                                      selectizeInput("ExpCatVar3a", "Select Variable For Analysis", choices = c("a", "b", "c")),
                                      selectizeInput("ExpCatVar3b", "Select Variable For Analysis", choices = c("a", "b", "c")),
                                      selectizeInput("ExpCatVar3c", "Select Variable For Analysis", choices = c("a", "b", "c"))
                                    )                
                           ),
                           tabPanel("Quantitative",
                                    fluidRow(
                                      h5("Some text here...includeMarkdown"),
                                      
                                      selectizeInput("ExpQuantVar3a", "Select Variable For Analysis", choices = c("1a", "2b", "3c")),
                                      selectizeInput("ExpQuantVar3b", "Select Variable For Analysis", choices = c("1a", "2b", "3c")),
                                      selectizeInput("ExpQuantVar3c", "Select Variable For Analysis", choices = c("1a", "2b", "3c")),
                                    )
                           ),
                           tabPanel("Combination",
                                tabsetPanel(
                                  tabPanel("1 categorical, 2 quantitative",
                                    fluidRow(
                                         h5("Some text here... includeText"),
                                         selectizeInput("ExpCombVar4q1", "Select Quantitative Variable For Analysis", choices = c("1a", "2b", "3c")),
                                         selectizeInput("ExpCombVar4q2", "Select Quantitative Variable For Analysis", choices = c("1a", "2b", "3c")),
                                         selectizeInput("ExpCombVar4c1", "Select Categorical Variable For Analysis", choices = c("a", "b", "c"))
                                                 )                
                                        ),
                                  tabPanel("2 categorical, 1 quantitative",
                                      fluidRow(
                                          h5("Some text here...includeMarkdown"),
                                          selectizeInput("ExpCombVar4q1", "Select Quantitative Variable For Analysis", choices = c("1a", "2b", "3c")),
                                          selectizeInput("ExpCombVar4c1", "Select Categorical Variable For Analysis", choices = c("a", "b", "c")),
                                          selectizeInput("ExpCombVar4c2", "Select Categorical Variable For Analysis", choices = c("a", "b", "c"))
                                                 )
                                        )
                                      )
                           )#end tabPanel
                         ) #end tabsetPanel
                ) #end tab panel
              ) #end tabsetPanel
            ) # end column
            ) #end fluid row
              ),

        tabItem(tabName = "investigate",
            fluidRow(
              column(4,
                       box(width=12,title="Beta distribution with parameters",
                           numericInput("Param1","Alpha = ",value=1,min=0.1,step=0.1),
                           sliderInput("sampleSize","Sample size:",min=1,max=30,value=5),
                           h4("Order statistics of interest, choose integers from 1 to n"),
                           checkboxInput("overlay",label="Overlay Theoretical Distribution",value=FALSE)
                       )
                ),
                #Show a plot of the prior    
                column(8,
                       tabsetPanel(
                         tabPanel("Cluster Analysis",           
                                  fluidRow(
                                  plotOutput("sleepPlot")
                                  )
                                 ), #end tab panel
                         tabPanel("Principal Components", 
                                  #add latex functionality: \ for inline and $$ for separate line...
                                  withMathJax(),
                                  helpText('An irrational number \\(\\sqrt{2}\\)
           and a fraction $$1-\\frac{1}{2}$$'),
                                  helpText('and a fact about \\(\\pi\\):
           $$\\frac2\\pi = \\frac{\\sqrt2}2 \\cdot
           \\frac{\\sqrt{2+\\sqrt2}}2 \\cdot
           \\frac{\\sqrt{2+\\sqrt{2+\\sqrt2}}}2 \\cdots$$'),
                                  fluidRow(
                                    column(4,
                                           sliderInput("sampleSize","Sample size:",min=1,max=30,value=5),
                                           br(),
                                    )
                                  )        
                                 ) #end tab panel
                       ) #end tab set
                ) #end column
              ) #end fluidrow
      ), #end tabItem


        tabItem(tabName = "model",
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
                           tabPanel("Cluster Analysis",           
                                    fluidRow(
                                      plotOutput("sleepPlot")
                                    )
                           ), #end tab panel
                           tabPanel("Principal Components", 
                                    fluidRow(
                                      column(4,
                                             sliderInput("sampleSize","Sample size:",min=1,max=30,value=5),
                                             br(),
                                      )
                                    )        
                           ) #end tab panel
                         ) #end tab set
                  ) #end column
                ) #end fluidrow
        ), #end tabItem
        
        # First tab content
        tabItem(tabName = "data",
                fluidRow(
                  column(12,
                         #Description of Data
                         h2("Data used in this analysis?"),
                         #box to contain description
                          DT::dataTableOutput("table")
        )
                )
        )


        ) #end tabItems
      )
)