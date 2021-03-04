# server.R
#
# AUTHOR
# Herbert Barrientos
# hpbarr@gmail.com
#
# CREATION DATE
# 2020-04-12
#
# VERSION
# 1
#
# PROJECT NAME
# COVID-19 Open Research Dataset Challenge (CORD-19)
# https://www.kaggle.com/allen-institute-for-ai/CORD-19-research-challenge/tasks
#
# DESCRIPTION
# Back end functionality for a ShinyApp application. Uses ReportArticles.R.


# ################################
# ########## LIBRARIES ###########
# ################################
library(data.table)
library(shiny)
library(DT)

source(file="ReportArticles.R")


# ################################
# ########## CONSTANTS ###########
# ################################
NO_INPUT        <- "Please enter a question..."
BAD_INPUT_TXT   <- "Bad input: please enter a single term, or a sequence of terms divided by commas."
BAD_INPUT_REGEX <- "Bad input: please check your regular expression."

PROGRESS_MSG_GENERAL <- "PLEASE WAIT... PROCESS IN PROGRESS. "
PROGRESS_MSG_DETAIL  <- "THE FIRST RUN TAKES A LITTLE LONGER."


# ################################
# ########## VARIABLES ###########
# ################################
lastQuestion     <- EMPTY_STRING
lastQuestionType <- -1


# ################################
# ########## FUNCTIONS ###########
# ################################
updateQuestionControlValues <- function(input) {
  
  lastQuestion     <<- input$question
  lastQuestionType <<- as.integer(input$radio)
  
}  # END updateQuestionControlValues

shinyServer(
  
  function(input, output) {
    
    observeEvent(input$goButton, {
      
      # Check if nothing was entered
      if (input$question == EMPTY_STRING) {
        
        output$yourQuestion <- renderPrint({NO_INPUT})
        updateQuestionControlValues(input)
        
        output$outDataTable = DT::renderDataTable({NULL}, escape = FALSE)
        return()
        
      }  # END if
      
      # Validate question based on its type. Report errors if necessary
      inputRadio <- as.integer(input$radio)
      question <- validateInput(input$question, inputRadio)
      if (question == EMPTY_STRING) {
        
        badInput <- EMPTY_STRING
        
        if (inputRadio == INPUT_TEXT)
          badInput <- BAD_INPUT_TXT
        
        if (inputRadio == INPUT_REGEX)
          badInput <- BAD_INPUT_REGEX
        
        output$yourQuestion  <- renderPrint({badInput})
        updateQuestionControlValues(input)
        
        output$outDataTable = DT::renderDataTable({NULL}, escape = FALSE)
        return()
        
      }  # END if
      
      # If the question, or the question type, remain unchanged, do nothing
      if ((lastQuestion == input$question) && (lastQuestionType == inputRadio))
        return()
      
      # Report the new question and update lastQuestion and lastQuestionType
      # Workaround: set a character variable with input$question, in order to 
      # keep the current text static until the action button is clicked again
      yourQuestion <- as.character(input$question)
      output$yourQuestion  <- renderPrint({yourQuestion})
      
      updateQuestionControlValues(input)
      
      withProgress(message = PROGRESS_MSG_GENERAL, detail = PROGRESS_MSG_DETAIL, 
                   min = 0, max = 10, value = 0, 
      {
        # progress increment (visual aid for the user)
        incProgress(4)
        
        # Load the coprpora into memory
        getCorpora()
        
        # Clear the table display before processing the new question
        output$outDataTable = DT::renderDataTable({NULL}, escape = FALSE)
        
        incProgress(7)
        
        # Process the inquiry
        isQuestionSingleTerm <- (inputRadio == INPUT_TEXT)
        outDataTable <- findArticles(question, isQuestionSingleTerm)
        
        incProgress(9)
        
        if (nrow(outDataTable) > 0)
          outDataTable <- formatResults(outDataTable)
        
        # Display results
        output$outDataTable = DT::renderDataTable({outDataTable}, escape = FALSE)
        
        incProgress(10)
        
      })  # END withProgress

    })  # END eventReactive
  
  }  # END function
  
)  # END shinyServer
