library(shiny)
library(tidyverse)
library(ggplot2)
#install.packages("Hmisc") <- for better "capitalize()" function

shinyServer(function(input, output, session) {
  # create dynamic title that changes based on which "vore" is selected.
  output$title <- renderUI({
    text <- paste0("Investigation of ", Hmisc::capitalize(input$vore), "vore", " Mammal Sleep Data")
    h1(text)
})
  
	getData <- reactive({
		newData <- msleep %>% filter(vore == input$vore)
	})
	
  #create plot
  output$sleepPlot <- renderPlot({
  	#get filtered data
  	newData <- getData()
  	
  	#create plot
  	g <- ggplot(newData, aes(x = bodywt, y = sleep_total))
  	# Add conditional statements to ggplot to change graphing of plot based on checkbox option to add coloring/change transparency of points
  	if(input$conservation && input$rem) {
  		g + geom_point(size = input$size, aes(col = conservation, alpha = sleep_rem))
  	} else if (input$conservation) {
  	  g + geom_point(size = input$size, aes(col = conservation))
  	  } else {
  		g + geom_point(size = input$size)
  	}
  })

  #create text info
  output$info <- renderText({
  	#get filtered data
  	newData <- getData()
  	
  	paste("The average body weight for order", input$vore, "is", round(mean(newData$bodywt, na.rm = TRUE), 2), "and the average total sleep time is", round(mean(newData$sleep_total, na.rm = TRUE), 2), sep = " ")
  })
  
  #create output of observations    
  output$table <- renderTable({
		getData()
  })

  # Add conditional logic to create dynamic changes to minimum slider value based on whether checkbox option for rem/opacity is selected.
  observe({if(input$rem) {
    updateSliderInput(session, "size", label = NULL, min = 3, max = NULL, value = NULL, step = NULL)
  } else {
    updateSliderInput(session, "size", label = NULL, min = 1, max = NULL, value = NULL, step = NULL)
  }})
})
