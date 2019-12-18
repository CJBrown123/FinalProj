library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)

# Define UI for application that displays an about page and the app itself

dashboardPage(skin="black",
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
          
          br()
      )),
              
              
    #define the body of "about" section
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "aboutApp",
                fluidRow(
                  tags$head(tags$style(HTML("a {color: green}"))),
                  #two columns for each of the two items
                  column(6,
                         #Description of App
                         h2("What does this app do?"),
                         #box to contain description
                         box(background="black",width=12,
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
                         box(background="black",width=12,
                             h4("Variables selection options are available within each analysis menu's pages and tabs. Analysis are grouped by number of variables and the data type of variables to be analyzed; the user has options to select from available variables.")
                         )
                )
        )#end fluid
  ), #end itab tem
        
        tabItem(tabName = "aboutData",
        fluidRow(
          #add in latex functionality if needed: \ for inline and $$ for separate line...
        column(12,
               #Description of App
               h1("Information About the Data Used in this App"),
               #box to contain description
               box(background="black",width=12,
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
                  
                )#box
              )#column
        )#fluid
),#tabitem

    #define the body of the "explore" section
        # Second tab content
      tabItem(tabName = "explore",
              h3("Exploratory Analysis: Summary Graphs and Tables"),
              h4("In each of the variable sections below, select variable options to view basic graphs and/or tables summarizing the data."),
              br(),
              tabsetPanel(
                tabPanel("1 Variable",
                  tabsetPanel(
                      tabPanel("Categorical",
                               selectizeInput("catVar1", "Select Variable For Analysis", selected = "party_cd", choices = catVars),
                               plotlyOutput("plotTime1c"),
                               plotlyOutput("plot1c"),
                               dataTableOutput("table")
                      ),
                       tabPanel("Quantitative",
                                 selectizeInput("quantVar1", "Select Variable For Analysis", selected = "age", choices = quantVars),
                                 plotlyOutput("hist1q")
                               )
                      )#end set
                ), #end tab panel
                
                tabPanel("2 Variables", 
                  tabsetPanel(
                         tabPanel("Categorical",
                                  fluidRow(
                                    selectizeInput("CatVar2a", "Select Variable For Analysis", choices = catVars),
                                    selectizeInput("CatVar2b", "Select Variable For Analysis", choices = catVars),
                                    plotlyOutput("plot2c"),
                                    dataTableOutput("table2c")
                                  )                
                         ),
                         tabPanel("Quantitative",
                                  fluidRow(
                                    selectizeInput("expQuantVar2a", "Select Variable For Analysis", choices = quantVars),
                                    selectizeInput("expQuantVar2b", "Select Variable For Analysis", choices = quantVars),
                                    plotlyOutput("scat2q")
                                  )
                         ),
                         tabPanel("Combination",
                                  fluidRow(
                                    selectizeInput("expCombVar2a", "Select Categorical Variable For Analysis", choices = catVars),
                                    selectizeInput("expCombVar2b", "Select Quantitative Variable For Analysis", choices = quantVars),
                                    plotlyOutput("combo2"),
                                    plotlyOutput("box1qc")
                                  )
                         )
                ) #end tab panel set
                ), #end tab panel
                
                tabPanel("3 Variables", 
                  tabsetPanel(
                           tabPanel("Categorical",
                                      selectizeInput("expCatVar3a", "Select Variable For Analysis", choices = catVars),
                                      selectizeInput("expCatVar3b", "Select Variable For Analysis", choices = catVars),
                                      selectizeInput("expCatVar3c", "Select Variable For Analysis", choices = catVars),
                                      plotlyOutput("plot3c"),
                                      dataTableOutput("table3c")
                           ),
                           tabPanel("Combination",
                                tabsetPanel(
                                  tabPanel("1 categorical, 2 quantitative",
                                         selectizeInput("expCombVar4q1", "Select Quantitative Variable For Analysis", choices = quantVars),
                                         selectizeInput("expCombVar4q2", "Select Quantitative Variable For Analysis", choices = quantVars),
                                         selectizeInput("expCombVar4c1", "Select Categorical Variable For Analysis", choices = catVars),
                                         plotlyOutput("Combo3")
                                        )#tabpanel
                                      )#tabset
                           )#end tabPanel
                         ) #end tabsetPanel
                ) #end tab panel
              ) #end tabsetPanel
              ),

        tabItem(tabName = "investigate",
              h1("Principal Components Analysis"),
                       box(width=12,title="selection Options",
                           br(),
                           h4("Paired Plots for quantitative variables"),
                           h6("Select 6 quantitative variables for this plot"),
                           selectizeInput("pcVarq1", "Select Quantitative Variable For Analysis", choices = quantVars),
                           selectizeInput("pcVarq2", "Select Quantitative Variable For Analysis", choices = quantVars),
                           selectizeInput("pcVarq3", "Select Quantitative Variable For Analysis", choices = quantVars),
                           selectizeInput("pcVarq4", "Select Quantitative Variable For Analysis", choices = quantVars),
                           selectizeInput("pcVarq5", "Select Quantitative Variable For Analysis", choices = quantVars),
                           selectizeInput("pcVarq6", "Select Quantitative Variable For Analysis", choices = quantVars),
                           br(),
                           h4("Biplot Inputs (select 2 principal components to plot)"),
                           numericInput("selPc1", "Select Principal Component Number (x-axis).", value = 1, min = 1, max = 6, step = 1),
                           numericInput("selPc2", "Select Principal Component Number (y-axis).",  value = 2, min = 1, max = 6, step = 1)
                       ),
                #Show a plot of the prior entries    
                       plotOutput("pairs"),
                       br(),
                       h4("Principal Components Analysis"),
                       plotOutput("PCsTab"),
                       br(),
                       h4("Scree Plot Showing Cumulative Proportion of Variance Explained by Principal Components"),
                       h6("The goal here is to find a nice balance between the lowest number of prinicpal component variables that will explain the highest amount of variance--look for where increases start to really taper off!"),
                       plotOutput("scree"),
                       br(),
                       h4("Biplot of Select Principal Components Analyses"),
                       h6("Select principal components to plot, then look for any variables shown on the plot that appear further for the center (0,0) axes both horizontally and vertically (respectively representing each of the two principal components chosen in order of selection below)."),
                       plotOutput("biplot")
      ), #end tabItem


        tabItem(tabName = "model",
                fluidRow(
                  withMathJax(),
                  column(12,
                         box(width=12,title="Generalized Linear Regression:Logistic Regression",
                             h6("A very common generalized linear model is the logistic regression model. This model is typically used for predicting a response that is binary (i.e., success/failure. The below analysis will allow you to model voting habits where success = an individual having voted and failure = an individual not voting."),
                             withMathJax(),
                             h6("Basic logistic regression models success probability using the following formula:"),
                             helpText('\\(P(success|yards)=\\) \\(\\frac{e\\^(\\beta_0+\\beta_1x)}{1+e\\^(\\beta_0+\\beta_1x)}\\)'),
                             h6("The logit function can be used to obtain the log odds of getting a certain result; the logit function is \\(\\log\\frac{p}{1-p}\\)"),
                             helpText('\\(\\log\\frac{p}{1-p}\\)'),
                             h6("We can recover the odds by exponentiating the log-odds result")
                             ),
                             br(),
                             h4("Voting by Age for Last Presidential Election (November 8, 2016)"),
                             h5("Color-coded by elected categorical variable"),
                             selectizeInput("jitVar16", "Select categorical variable for Color-coding of output plot", choices = catVars),
                             plotOutput("jitPres16"),
                             br(),
                             
                             h4("Proportion of Overall Voting by Selected Quantitative Variable"),
                             h6("Select a variable below to see three different plots visualizing the relationship between that variable and the proportion of individuals that voted in the 2016 presidential election. The three relationship types include:"),
                             HTML("
                               <ul>
                                 <li> variable value vs. proportion who voted</li>
                                 <li> square of variable value vs. proportion who voted</li>
                                 <li> natural log of variable value vs. proportion who voted</li>
                               </ul>
                             "),
                             br(),
                             selectizeInput("scatVarQ", "Select quantitative variable For Color-coding of output plot", selected = "age", choices = quantVars),
                         selectizeInput("scatVarC", "Select quantitative variable For Color-coding of output plot", selected = "party_cd", choices = catVars),
                             br(),
                             h4("Proportion of Overall Voting by Selected Quantitative Variable"),
                             plotOutput("scatAll"),
                             h4("Proportion of Overall Voting by Selected Quantitative Variable Squared"),
                             plotOutput("scatAllSq"),
                             h4("Proportion of Overall Voting by the Natural Log of Selected Quantitative Variable"),
                             plotOutput("scatAllLn"),
                             h6("The above plots are for information purposes."),
                             br(),
                             br(),
                             h4("Predict Log-Odd of Voting by Age For Given Categorical Variable"),
                             br(),
                             h6("Select a categorical variable below, then select  three values of that variable along with three ages to generate predictions."),
                             selectizeInput("predGLM", "Select categorical variable For Color-coding of output plot", selected = "party_cd", choices = catVars),
                         
                             fluidRow(
                               column(6,
                             numericInput("scatPredAge1","Age Value For Prediction 1",value=18,min=17,max=130,step=1),
                             numericInput("scatPredAge1","Age Value For Prediction 1",value=18,min=17,max=130,step=1),
                             numericInput("scatPredAge1","Age Value For Prediction 1",value=18,min=17,max=130,step=1)
                               ),
                             column(6,
                             selectizeInput("glmPred1c", "Select categorical variable value for prediction1", choices = uiOutput("GLMpred")),
                             selectizeInput("glmPred2c", "Select categorical variable value for prediction1", choices = uiOutput("GLMpred")),
                             selectizeInput("glmPred3c", "Select categorical variable value for prediction1", choices = uiOutput("GLMpred")),
                             tableOutput("glmTable")
                         )
                  )
                  )#end column
                ) #end fluidrow
        ), #end tabItem
        
        # First tab content
        tabItem(tabName = "data",
                fluidRow(
                  column(12,
                         #Description of Data
                         h2("Data used in this analysis"),
                         dataTableOutput("table")
                )
        )
        ) #end data tab item
        ) #end tabItems
    ) #end dashboard body
)
