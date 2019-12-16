library(shiny)
library(dplyr)
library(ggplot2)
library(rgl)
library(readxl)


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

#definitions <- read_excel("C:\\Users\\Valued User\\Documents\\CJ Class\\FinalProj\\definitions.xlsx")


#create lists for categorical and qunatitative variables
catVars <- c(names(voterMeck[,1:5]), names(voterMeck[,7:9]), names(voterMeck[,12:22]), names(voterMeck[,127]))
quantVars <- c(names(voterMeck[, 6]), names(voterMeck[, 104:126]))


shinyServer(function(input, output, session) {
  # create title.
  output$title <- renderUI({
    h1("Mecklenburg County Voter Information and Election History")
  })
  
  

  #Create Conditions for including inactive voters
  getData <- reactive({
    if (input$status) {
      newData <- voterMeck %>% 
        select(-(23:103))  
    } else {
      newData <- voterMeck %>% filter(status_cd == "A") %>% 
        select(-(23:103))   
    }
    return(newData)
  })
  
  getDataMelt <- reactive({
    if (input$status) {
      newDataMelt <- voterMeck %>% 
        select(-(23:103)) %>%
        melt(id = c(1:22, 43:46)) %>% 
        group_by(variable)
    } else {
      newDataMelt <- voterMeck %>% filter(status_cd == "A") %>% 
        select(-(23:103)) %>%
        melt(id = c(1:22, 43:46))  %>% 
        group_by(variable)  
    }
    return(newDataMelt)
  })
  
  getData <- reactive({
    newData <- voterMeck %>% filter(voterMeck$status_cd == "A")
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
   getData1c <- reactive({
     newData1c <- table(input$catVar1)
   })
   output$table1c <- renderDataTable({
     getDatalc()
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
#Color-coded by <RACE>
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
output$glmTable <- renderDataTable({
   newData <- getData()
   glmFit <- glm(E8_20161108 ~ age + input$predGLM, data = newData, family = "binomial")
   predict(glmFit, newdata = data.frame(age = c(scatPredAge1, scatPredAge2, scatPredAge3), input$scatVarC = c(input$scatPred1c, scatPred2c, scatPred3c)), type = "response", se.fit = TRUE)
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
