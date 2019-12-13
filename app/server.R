library(shiny)
library(dplyr)
library(ggplot2)
library(rgl)
library(readxl)

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

definitions <- read_excel("C:\\Users\\Valued User\\Documents\\CJ Class\\FinalProj\\definitions.xlsx")
#create copy of original dataset
origData <- voterMeck

#install.packages("Hmisc") <- for better "capitalize()" function

shinyServer(function(input, output, session) {
  # create dynamic title that changes based on which "vore" is selected.
  output$title <- renderUI({
    text <- paste0("Investigation of ", Hmisc::capitalize(input$vore), "vore", " Mammal Sleep Data")
    h1(text)
})
  
	getData <- reactive({
		newData <- origData
	})
	#create output of interactive data table    
	output$table <- renderDataTable({
	  DT::datatable(voterMeck)
	})
	
	
  #create plot
  ##output$sleepPlot <- renderPlot({
  	#get filtered data
  	newData <- getData()
  	
  	#create plot
  ##	g <- ggplot(newData, aes(x = bodywt, y = sleep_total))
  	# Add conditional statements to ggplot to change graphing of plot based on checkbox option to add coloring/change transparency of points
  	##if(input$conservation && input$rem) {
  	##	g + geom_point(size = input$size, aes(col = conservation, alpha = sleep_rem))
  	#3} else if (input$conservation) {
  	##  g + geom_point(size = input$size, aes(col = conservation))
  	##  } else {
  	##	g + geom_point(size = input$size)
  	##}
  ##})

  	output$sleepPlot <- renderPlot({
  	  #Temporary plot
  	  hist(rnorm(100))
  	})
  
  #create text info
  output$info <- renderText({
  	#get filtered data
  	newData <- getData()
  	
  	paste("The average body weight for order", input$vore, "is", round(mean(newData$bodywt, na.rm = TRUE), 2), "and the average total sleep time is", round(mean(newData$sleep_total, na.rm = TRUE), 2), sep = " ")
  })
  
  #create output of observations    
  output$definitions <- renderTable({
		definitions()
  })
  


  # Add conditional logic to create dynamic changes to minimum slider value based on whether checkbox option for rem/opacity is selected.
  observe({if(input$rem) {
    updateSliderInput(session, "size", label = NULL, min = 3, max = NULL, value = NULL, step = NULL)
  } else {
    updateSliderInput(session, "size", label = NULL, min = 1, max = NULL, value = NULL, step = NULL)
  }})
  
#  output$ex1 <- renderUI({
#    withMathJax(
#      h5('Dynamic output 1:  $$\\alpha^2$$')
#      )
#  })
})
