library(shiny)
library(dplyr)
library(ggplot2)
library(rgl)
library(readxl)
library(reshape2)
library(DT)
library(tidyverse)
library(lubridate)

################ Run ImportTable3.RMD prior to running app ################ 

#Alternately, code from this import file is shown below (lines 12-205); shiny app server code starts on line 215 of this document.
voterMeck <- read_csv(file = "./VoterMeckCo.csv")

#create copy of original dataset as a backup
origData <- voterMeck


elecList <- c("10/08/2019",
              "09/10/2019",
              "05/14/2019",
              "11/06/2018",
              "05/08/2018",
              "11/07/2017",
              "09/12/2017",
              "11/08/2016",
              "03/15/2016",
              "11/03/2015",
              "10/06/2015",
              "09/15/2015",
              "11/04/2014",
              "05/06/2014",
              "11/05/2013",
              "10/08/2013",
              "09/10/2013",
              "11/06/2012",
              "07/17/2012",
              "05/08/2012")

eDate <- lapply(elecList, function(x) {
   as.Date(as.character(as.POSIXct(x, format = "%m/%d/%Y")))  
})

eDateMin25 <- lapply(eDate, function(x) {
   x-25
})  


dateSufx <- function(date) {
   eDate <- as.POSIXct(date, format = "%m/%d/%Y")
   suffix <- paste0(stringr::str_sub(eDate, 1, 4), stringr::str_sub(eDate, 6, 7), stringr::str_sub(eDate, 9, 10))
}
# run function to create date suffixes for new variable names
elecSufx <- lapply(elecList, dateSufx)

# create blank list, use for loop to make list of new column names.
elecNames <- NULL
for (i in 1:length(elecList)) {
   elecNames[[i]] <- paste0("E", i, "_", elecSufx[[i]])
}

# Create List of election date column names
dateCols1 <- voterMeck[, grepl("date", names(voterMeck))]
dateCols2 <- voterMeck[, grepl("Date", names(voterMeck))]
dates <- c(c(names(dateCols1)), c(names(dateCols2)))


eDate <- lapply(elecList, function(x) {
   as.Date(as.character(as.POSIXct(x, format = "%m/%d/%Y")))  
})
eDateMin25 <- lapply(eDate, function(x) {
   x-25
})  


# Change class of date columns as appropriate:
voterMeck$registr_dt <- as.Date(as.character(voterMeck$registr_dt))
voterMeck[,dates] = apply(voterMeck[,dates], 2, function(x) as.POSIXct(x, format = "%m/%d/%Y"))


#voterMeck <- mutate(voterMeck, E1_20191008 = 0)
voterMeck <- mutate(voterMeck, E1_20191008 = 
                       ifelse(!is.na(E1_date), 1,
                              ifelse(is.na(E1_date) & registr_dt <= as.Date(eDateMin25[[1]]), 0, NA)))
print(levels(as.factor(voterMeck$E1_20191008)))


#voterMeck <- mutate(voterMeck, E2_20190910 = 0)
voterMeck <- mutate(voterMeck, E2_20190910 = 
                       ifelse(!is.na(E2_Date), 1,
                              ifelse(is.na(E2_Date) & registr_dt <= as.Date(eDateMin25[[2]]), 0, NA)))
print(levels(as.factor(voterMeck$E2_20190910)))


#voterMeck <- mutate(voterMeck, E3_20190514 = 0)
voterMeck <- mutate(voterMeck, E3_20190514 = 
                       ifelse(!is.na(E3_Date), 1,
                              ifelse(is.na(E3_Date) & registr_dt <= as.Date(eDateMin25[[3]]), 0, NA)))
voterMeck <- mutate(voterMeck, E4_20181106 = 
                       ifelse(!is.na(E4_Date), 1,
                              ifelse(is.na(E4_Date) & registr_dt <= as.Date(eDateMin25[[4]]), 0, NA)))
voterMeck <- mutate(voterMeck, E5_20180508 = 
                       ifelse(!is.na(E5_Date), 1,
                              ifelse(is.na(E5_Date) & registr_dt <= as.Date(eDateMin25[[5]]), 0, NA)))
voterMeck <- mutate(voterMeck, E6_20171107 = 
                       ifelse(!is.na(E6_Date), 1,
                              ifelse(is.na(E6_Date) & registr_dt <= as.Date(eDateMin25[[6]]), 0, NA)))
voterMeck <- mutate(voterMeck, E7_20170912 = 
                       ifelse(!is.na(E7_Date), 1,
                              ifelse(is.na(E7_Date) & registr_dt <= as.Date(eDateMin25[[7]]), 0, NA)))
voterMeck <- mutate(voterMeck, E8_20161108 = 
                       ifelse(!is.na(E8_Date), 1,
                              ifelse(is.na(E8_Date) & registr_dt <= as.Date(eDateMin25[[8]]), 0, NA)))
voterMeck <- mutate(voterMeck, E9_20160315 = 
                       ifelse(!is.na(E9_Date), 1,
                              ifelse(is.na(E9_Date) & registr_dt <= as.Date(eDateMin25[[9]]), 0, NA)))
voterMeck <- mutate(voterMeck, E10_20151103 = 
                       ifelse(!is.na(E10_Date), 1,
                              ifelse(is.na(E10_Date) & registr_dt <= as.Date(eDateMin25[[10]]), 0, NA)))
voterMeck <- mutate(voterMeck, E11_20151006 = 
                       ifelse(!is.na(E11_Date), 1,
                              ifelse(is.na(E11_Date) & registr_dt <= as.Date(eDateMin25[[11]]), 0, NA)))
voterMeck <- mutate(voterMeck, E12_20150915 = 
                       ifelse(!is.na(E12_Date), 1,
                              ifelse(is.na(E12_Date) & registr_dt <= as.Date(eDateMin25[[12]]), 0, NA)))
voterMeck <- mutate(voterMeck, E13_20141104 = 
                       ifelse(!is.na(E13_Date), 1,
                              ifelse(is.na(E13_Date) & registr_dt <= as.Date(eDateMin25[[13]]), 0, NA)))
voterMeck <- mutate(voterMeck, E14_20140506 = 
                       ifelse(!is.na(E14_Date), 1,
                              ifelse(is.na(E14_Date) & registr_dt <= as.Date(eDateMin25[[14]]), 0, NA)))
voterMeck <- mutate(voterMeck, E15_2013110 = 
                       ifelse(!is.na(E15_Date), 1,
                              ifelse(is.na(E15_Date) & registr_dt <= as.Date(eDateMin25[[15]]), 0, NA)))
voterMeck <- mutate(voterMeck, E16_20131008 = 
                       ifelse(!is.na(E16_Date), 1,
                              ifelse(is.na(E16_Date) & registr_dt <= as.Date(eDateMin25[[16]]), 0, NA)))
voterMeck <- mutate(voterMeck, E17_20130910 = 
                       ifelse(!is.na(E17_Date), 1,
                              ifelse(is.na(E17_Date) & registr_dt <= as.Date(eDateMin25[[17]]), 0, NA)))
voterMeck <- mutate(voterMeck, E18_20121106 = 
                       ifelse(!is.na(E18_Date), 1,
                              ifelse(is.na(E18_Date) & registr_dt <= as.Date(eDateMin25[[18]]), 0, NA)))
voterMeck <- mutate(voterMeck, E19_20120717 = 
                       ifelse(!is.na(E19_Date), 1,
                              ifelse(is.na(E19_Date) & registr_dt <= as.Date(eDateMin25[[19]]), 0, NA)))
voterMeck <- mutate(voterMeck, E20_20120508 = 
                       ifelse(!is.na(E20_Date), 1,
                              ifelse(is.na(E20_Date) & registr_dt <= as.Date(eDateMin25[[20]]), 0, NA)))

voterMeck$totVotes <- rowSums(voterMeck[123:142]==1, na.rm=TRUE)
voterMeck$totElig <- rowSums(!is.na(voterMeck[123:142]))
voterMeck$totPartic <- round((voterMeck$totVotes/voterMeck$totElig)*100, 0)

voterMeck <- mutate(voterMeck, ageCat =  ifelse(age < 18, "minor", 
                                                ifelse((age >= 18) & (age < 25), "18-24",
                                                       ifelse((age >= 25) & (age < 35), "25-34",
                                                              ifelse((age >= 35) & (age < 45), "35-44",
                                                                     ifelse((age >= 45) & (age < 55), "45-54",
                                                                            ifelse((age >= 55) & (age < 65), "55-64",
                                                                                   ifelse((age >= 65) & (age < 75), "65-74",
                                                                                          ifelse((age >= 75), "75+", "Error")))))))))
print(levels(as.factor(voterMeck$ageCat)))
table(voterMeck$ageCat, exclude = NULL)

voterMeck$ageCat <- as.factor(voterMeck$ageCat)
voterMeck$precinct_desc <- as.factor(voterMeck$precinct_desc)
voterMeck$race_code <- as.factor(voterMeck$race_code)
voterMeck$party_cd <- as.factor(voterMeck$party_cd)
voterMeck$ethnic_code <- as.factor(voterMeck$ethnic_code)
voterMeck$sex_code <- as.factor(voterMeck$sex_code)
voterMeck$res_city_desc <- as.factor(voterMeck$res_city_desc)
voterMeck$zip_code <- as.factor(voterMeck$zip_code)
voterMeck$status_cd <- as.factor(voterMeck$status_cd)
voterMeck$municipality_desc <- as.factor(voterMeck$municipality_desc)
voterMeck$ward_desc <- as.factor(voterMeck$ward_desc)
voterMeck$county_commiss_desc <- as.factor(voterMeck$county_commiss_desc)
voterMeck$school_dist_desc <- as.factor(voterMeck$school_dist_desc)

voterMeck <- voterMeck %>% select(-(8:25), -pct_portion)

voterMeck$E1_VotingMethod <- as.factor(voterMeck$E1_VotingMethod)
voterMeck$E2_VotingMethod <- as.factor(voterMeck$E2_VotingMethod)
voterMeck$E3_VotingMethod <- as.factor(voterMeck$E3_VotingMethod)
voterMeck$E4_VotingMethod <- as.factor(voterMeck$E4_VotingMethod)
voterMeck$E5_VotingMethod <- as.factor(voterMeck$E5_VotingMethod)
voterMeck$E6_VotingMethod <- as.factor(voterMeck$E6_VotingMethod)
voterMeck$E7_VotingMethod <- as.factor(voterMeck$E7_VotingMethod)
voterMeck$E8_VotingMethod <- as.factor(voterMeck$E8_VotingMethod)
voterMeck$E9_VotingMethod <- as.factor(voterMeck$E9_VotingMethod)
voterMeck$E10_VotingMethod <- as.factor(voterMeck$E10_VotingMethod)
voterMeck$E11_VotingMethod <- as.factor(voterMeck$E11_VotingMethod)
voterMeck$E12_VotingMethod <- as.factor(voterMeck$E12_VotingMethod)
voterMeck$E13_VotingMethod <- as.factor(voterMeck$E13_VotingMethod)
voterMeck$E14_VotingMethod <- as.factor(voterMeck$E14_VotingMethod)
voterMeck$E15_VotingMethod <- as.factor(voterMeck$E15_VotingMethod)
voterMeck$E16_VotingMethod <- as.factor(voterMeck$E16_VotingMethod)
voterMeck$E17_VotingMethod <- as.factor(voterMeck$E17_VotingMethod)
voterMeck$E18_VotingMethod <- as.factor(voterMeck$E18_VotingMethod)
voterMeck$E19_VotingMethod <- as.factor(voterMeck$E19_VotingMethod)
voterMeck$E20_VotingMethod <- as.factor(voterMeck$E20_VotingMethod)

definitions <- read_excel("./definitions.xlsx")


#create lists for categorical and qunatitative variables
catVars <- c(names(voterMeck[,1:5]), names(voterMeck[,7:9]), names(voterMeck[,12:22]), names(voterMeck[,127]))
quantVars <- c(names(voterMeck[, 6]), names(voterMeck[, 104:126]))


shinyServer(function(input, output, session) {
  # create title.
  output$title <- renderUI({
    h1("Mecklenburg County Voter Information and Election History")
  })
  
  #Create definitions table
  getDefs <- reactive({
    newDefs <- definitions   
  })
  output$definitions <- renderTable({
    getDefs()
  })

  #Create Conditions for including inactive voters
  getData <- reactive({
    newData <- voterMeck %>% filter(status_cd == "A") %>% 
    select(-(23:103))   
  })
  
  getDataMelt <- reactive({
    newDataMelt <- voterMeck %>% filter(status_cd == "A") %>% 
    select(-(23:103)) %>%
    melt(id = c(1:22, 43:46))  %>% 
    group_by(variable)
  })

  #create plot for single categorical variables
  #elections over time
   output$plotTime1c <- renderPlot({
    #get filtered data
    newDataMelt <- getDataMelt()
    g <- ggplot(newDataMelt, aes(x = variable, y = value))
    g + geom_bar(stat = "identity",  aes(fill = input$catVar1)) +
      theme(legend.position = "bottom", axis.text.x = element_text(face = "bold", angle = 80, size = 8)) +
      scale_x_discrete(labels=elecList)
  })

   
   
   #create frequency tables for exploratory analyses
   #1 categorical variable

   output$table1c <- renderDataTable({
      newData <- getData() 
      newData %>% table(input$catVar1)
   })  
   
   #create frequency tables for exploratory analyses
   #2 categorical variables
   getData2c <- reactive({
     newData2c <- table(input$catVar1, input$catVar2)
   })
   output$table2c <- renderDataTable({
     getData2c()
   })  
   
   #create frequency tables for exploratory analyses
   #3 categorical variables
   getData3c <- reactive({
     newData3c <- table(input$catVar1, input$catVar2, input4catVar3)
   })
   output$table3c <- renderDataTable({
     getData3c()
   })  

#QUALITATIVE EXPLORATION   
   #create plots for categorical variables
   # 1 categorical variable
   output$plot1c <- renderPlot({
     #get filtered data
     newData <- getData()
     g <- ggplot(newData, aes(x = input$catVar1, y = value))
     g + geom_bar(stat = "identity") +
       theme(legend.position = "bottom", axis.text.x = element_text(face = "bold", angle = 45, size = 8))
   })
   
   # 2 categorical variables
   output$plot2c <- renderPlot({
     #get filtered data
     newData <- getData()
     g <- ggplot(newData, aes(x = input$catVar1, y = value))
     g + geom_bar(stat = "identity", aes(fill = input$catVar1)) +
       theme(legend.position = "bottom", axis.text.x = element_text(face = "bold", angle = 45, size = 8))
   })
   
   # 3 categorical variables
   output$plot3c <- renderPlot({
     #get filtered data
     newData <- getData()
     g <- ggplot(newData, aes(x = input$catVar1, y = value))
     g + geom_bar(stat = "identity", aes(fill = input$catVar1)) +
       theme(legend.position = "bottom", axis.text.x = element_text(face = "bold", angle = 45, size = 8)) +
       facet_grid(.~input$catVar3, labeller = label_both)
   })


#QUANTITATIVE EXPLORATION

   #create plots for quantitative variables
   # 1 quantitative variable
   output$box1q <- renderPlot({
     #get filtered data
     newData <- getData()
     p2 <- ggplot(newData, aes(x = input$quantVar1, y = input$quantVar2))
     p2 + geom_boxplot() +
       geom_point(aes(group = input$quantVar1, color = input$quantVar1)) +
       labs(title = "Boxplot for Select Quantitative Variable")
   })
   
   output$hist1q <- renderPlot({
     #get filtered data
     newData <- getData()
       d1 <- ggplot(newData, aes(x = input$quantVar1)) 
     d1 + geom_histogram(bins = 20, aes(y = ..density..)) + 
       geom_density(adjust = 0.40, size = 3, col = "Red") +
       labs(title = "Histogram for Select Quantitative Variable")
   })
   
   
   # 2 quantitative variables
   output$scat2q <- renderPlot({
     #get filtered data
     newData <- getData()
     p2 <- ggplot(newData, aes(x = input$quantVar1, y = input$quantVar2))
        p2 + geom_boxplot() +
        geom_point(aes(group = input$quantVar1, color = input$quantVar1)) +
        labs(title = "Boxplot/Scatter Plots for Select Quantitative Variables in Combination")
   })
 #VARIABLE COMBNINATONS
  #combination categorical and quantitative variables
   
   
   #1 quantitative/1 categorical variable
   output$combo2 <- renderPlot({
     #get filtered data
     newData <- getData()
     d1 <- ggplot(newData, aes(x = input$quantVar1)) 
     d1 + geom_histogram(bins = 20, aes(y = ..density..)) + 
       geom_density(adjust = 0.40, size = 3, col = "Red") +
       labs(title = "Histograms for Select Variables in Combination") +
       facet_grid(.~input$catVar1, labeller = label_both)
        })
 
   #2 quant./1 cat.
   output$combo3 <- renderPlot({
     #get filtered data
     newData <- getData()
     p2 <- ggplot(newData, aes(x = input$quantVar1, y = input$quantVar2))
     p2 + geom_boxplot() +
       geom_point(aes(group = input$quantVar1, color = input$quantVar1), position = "jitter") +
       labs(title = "Boxplots for Select Variables in Combination") +
       facet_grid(.~input$catVar1, labeller = label_both)
   })
   
  #PRINCIPAL COMPONENTS
   
   #data pairs visulaizations plot
   output$pairs <- renderPlot({
      #get filtered data
      newData <- getData()
      newDataPR <- select(newData, pcVarq1, pcVarq2, pcVarq3, pcVarq4, pcVarq5, pcVarq6) %>%
      `[`(rowSums(is.na(.)) == 0, )
   pairs(newDataPR, cex = 0.4) #numeric vars only, remove nas
})
   


 #princ. components analysis
   getDataPCs <- reactive({
      newData <- getData()
      newDataPC <- select(newData, pcVarq1, pcVarq2, pcVarq3, pcVarq4, pcVarq5, pcVarq6) %>%
      `[`(rowSums(is.na(.)) == 0, )
     newDataPC <- prcomp(newDataPC, center = TRUE, scale = TRUE)
   })
   
   output$PCsTab <- renderDataTable({
     getDataPCs()
   })  

  
  # output$PCsTab <- renderDataTable({
  #    PCs <- prcomp(voterPairs, center = TRUE, scale = TRUE)
  # return(PCs)
   #})
   
   #scree plot (cumulative)
   output$scree <- renderPlot({
  #PCs <- prcomp(voterPairs, center = TRUE, scale = TRUE)
   plot(cumsum(getDataPCs$sdev^2/sum(getDataPCs$sdev^2)), xlab = "Principal Component",
        ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')
   })
   
   #biplot
output$biplot<- renderPlot({
   biplot(getDataPCs, choices = c(input$selPc1, input$selPc2), cex = 0.6,
          xlim = c(-0.08, 0.15), ylim = c(-0.12, 0.07))
})
   

#HEIRARCHICAL CLUSTER ANALYSIS (with dendogram)
###################################
###################################
###################################
###################################
###################################
###################################
###################################


#SUPERVISED LEARNING
#Logistic Regression

#Voting by Age for Last Presidential Election (November 8, 2016)
#Color-coded by categorical variable
output$jitPres16 <- renderPlot({
   newData <- getData()
   p2 <- ggplot(newData, aes(x = age, y = E8_20161108, color = input$jitVar16))  
   p2 + geom_jitter(mapping = NULL, data = voterActive, stat = "identity",
                 width = .05, height = .1,
                 na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
})


#Voting by Selected Quantitative Variable for Last Presidential Election (November 8, 2016)

#Proportion of Overall Voting by Selected Quantitative Variable
output$scatAll <- renderPlot({
   newData <- getData()
   distSum <- newData %>% filter(!is.na(E8_20161108)) %>% group_by(input$scatVarQ) %>% summarize(propVoted = mean(E8_20161108), n = n())
   ggplot(distSum, aes(x = input$scatVarQ, y = propVoted, size = n)) + 
      geom_point(stat = "identity")
})

#Proportion of Overall Voting by Selected Quantitative Variable Squared
output$scatAllSq <- renderPlot({
   newData <- getData()
   distSum <- newData %>% filter(!is.na(E8_20161108)) %>% group_by(input$scatVarQ) %>% summarize(propVoted = mean(E8_20161108), n = n())
   ggplot(distSum, aes(x = input$scatVarQ^2, y = propVoted, size = n)) + 
      geom_point(stat = "identity")
})
   
#Proportion of Overall Voting by the Natural Log of Selected Quantitative Variable 
output$scatAllLn <- renderPlot({
   newData <- getData()
   distSum <- newData %>% filter(!is.na(E8_20161108)) %>% group_by(input$scatVarQ) %>% summarize(propVoted = mean(E8_20161108), n = n())
   ggplot(distSum, aes(x = log(input$scatVarQ), y = propVoted, size = n)) + 
      geom_point(stat = "identity")
})

#predict probability of voting for given age in combination with second categorical predictor variable
output$glmTable <- renderTable({
   newData <- getData()
   glmFit <- glm(E8_20161108 ~ age + input$predGLM, data = newData, family = "binomial")
   predict(glmFit, newdata = data.frame(age = c(input$glmPredAge1, input$glmPredAge2, input$glmPredAge3), input$predGLM = c(input$glmPred1c, input$glmPred2c, input$glmPred3c)), type = "response", se.fit = TRUE)
})

output$GLMpred <- renderUI({
   unlist(levels(as.factor(input$predGLM)))
})
   
   
   #DATA TABLE INFORMATION
   #create output table for full set of observations    
   output$table <- renderDataTable({
     getData()
   })
   
  #create plot for overall voting activity by year
  output$plotTime <- renderPlot({
    #get filtered data
    newDataMelt <- getDataMelt()
    g <- ggplot(newDataMelt, aes(x = variable, y = value))
    g + theme(axis.text.x = element_text(face = "bold", angle = 80, size = 8)) +
      scale_x_discrete(labels=elecList) +
      geom_bar(stat = "identity", fill = "burlywood1")
  })

})
