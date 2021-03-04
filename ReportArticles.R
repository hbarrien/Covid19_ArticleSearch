# ReportArticles.R
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
# DATA
# The following three files are pre-processed data from the 
# downloaded "metadata.csv" file:
#
# "metadata_full_text.csv"
#    Description  : dataset with all rows having full-text articles.
#    Search column: "full_text".
#    Editable     : no. For a new version of the data, execute script 
#                   PreProcessInputData.R
#    May be empty : yes.
#
# "metadata_abstract.csv"
#    Description  : dataset with all rows not having full text articles,
#                    but having a non-empty abstract.
#    Search column: "abstract".
#    Editable     : no. For a new version of the data, execute script 
#                   PreProcessInputData.R
#    May be empty : yes.
#
# "metadata_title.csv"
#    Description  : dataset with all rows not having neither full-text
#                   articles nor an abstract, but a non-empty title.
#    Search column: "title"
#    Editable     : no. For a new version of the data, execute script 
#                   PreProcessInputData.R
#    May be empty : yes.
# 
# "main_topic.csv"
#    Description : single-entry dataset specifying the topic of 
#                  interest (e.g., "covid-19", "sars-cov-2").
#    Editable    : yes. The topic may be changed to fit different 
#                  reporting interests.
#    May be empty: no.
# 
# INPUT
# Question: 
#   May be a single term, a sequence of terms divided by commas,
#   or a regular expression. A term may be a single word, or a
#   short phrase (i.e., a sequence of words) separated by spaces.
#
# Question type: 
#   - Single-term: either a term, or a sequence of terms.
#   - Not single-term: a regular expression.
#
# OUTPUT
# A DataTable containing the search results to the question.
#
# DESCRIPTION 
# This module contains functionality to handle input, load 
# the search space into memory, and perform the search process.
#
# The search space (i.e., the corpora) is assembled by creating 
# a data subset of each instantiated corpus object and adding it 
# to the corpora. Every data subset is pre-processed by filtering 
# out all rows not relating to a pre-specified main topic of 
# interest (i.e., mainTopic). 
#
# Once the input has been validated, a search is performed 
# in every data subset in the corpora. When the question is 
# "single-term", the program extracts full sentences where 
# occurrences of the search terms were found. Sentences are 
# then reported as the excerpt for the processed data subset 
# row. When the question is "not single-term", the output is 
# generally a large chunk of text. To make reading easier, 
# the excerpt is created by extracting the first and last
# NUM_WORDS_PER_CHUNK words and concatenating them together. 
# On the other hand, if the size of the output text is less 
# than, or equal to, the limit specified by MAX_WORDS, the 
# text is left as is. Just as with the sentences, the resulting 
# excerpt is also added to the processed data subset row. 
#
# All processed rows are added to a table (i.e., outDataTable). 
# Once the search process is over, duplicates are removed from 
# the outDataTable. Next, outDataTable is sorted and finally 
# returned to the calling program.
#
# PRECONDITION
# ((|corpusFullText| + |corpusAbstract| + |corpusTitle|) > 0)


# ################################|
# ########## LIBRARIES ###########
# ################################
library(data.table)


# ################################
# ########## CONSTANTS ###########
# ################################
ROOT_INPUT_DIR       <- paste0(getwd(), "/data")
FILE_PATH_MAIN_TOPIC <- paste0(ROOT_INPUT_DIR, "/main_topic.csv")
FILE_PATH_FULL_TEXT  <- paste0(ROOT_INPUT_DIR, "/metadata_full_text.csv")
FILE_PATH_ABSTRACT   <- paste0(ROOT_INPUT_DIR, "/metadata_abstract.csv")
FILE_PATH_TITLE      <- paste0(ROOT_INPUT_DIR, "/metadata_title.csv")

SINGLE_TERM_GROUPING <- "(\\(|\\))"
SINGLE_TERM_SPLIT    <- "\\|"

CORPORA_IDX_CORPUS <- 1
CORPORA_IDX_SEARCH_COLUMN <- 2

CONTIGUOUS_WHITE_SPACES <- "  +"
EMPTY_STRING            <- ""
PIPE_CHAR               <- "|"
PERIOD                  <- "."
COMMA                   <- ","
L_PARENTHESIS           <- "("
R_PARENTHESIS           <- ")"
L_SQ_BRACKET            <- "["
R_SQ_BRACKET            <- "]"
ELLIPSIS                <- "..."
WHITE_SPACE             <- " "

INPUT_TEXT  <- 1
INPUT_REGEX <- 2
DEBUGGING   <- FALSE

MAX_WORDS     <- 120
NUM_WORDS_PER_CHUNK <- 50


# ################################
# ########## VARIABLES ###########
# ################################
corpora   <- NULL
mainTopic <- read.table(FILE_PATH_MAIN_TOPIC, header = TRUE, colClasses = c("character"))[1,"topic"]


# ################################
# ########## FUNCTIONS ###########
# ################################

# ######## INPUT HANDLING ########
isRegexValid <- function(regexpr) {
  
  t <- ""
  r <- NULL
  
  r <- tryCatch(ifelse(grepl(regexpr, t), TRUE, TRUE), error = function(e) { return(FALSE)})
  
  return(!is.null(r) && (r == TRUE))
  
}  # END isRegexValid

validateTextInput <- function(input) {
  
  if (is.null(input) || ((newInput <- trimws(input)) == EMPTY_STRING))
    return(EMPTY_STRING)

  newInput <- tolower(newInput)
  newInput <- gsub(CONTIGUOUS_WHITE_SPACES, WHITE_SPACE, newInput)
  
  if (!grepl("^[-_a-z0-9 ]+( ?,[-_a-z0-9 ]+ ?)*$", newInput))
    return(EMPTY_STRING)
  
  return(newInput)
  
}  # END validateTextInput

convertInputToRegex <- function(input) {
  
  return(paste0(L_PARENTHESIS, gsub(COMMA, PIPE_CHAR, input), R_PARENTHESIS))
  
}  # END convertInputToRegex

validateInput <- function(inputString, inputType) {
  
  if (inputType == INPUT_REGEX) {
    
    if (!isRegexValid(inputString))
      return(EMPTY_STRING)
    
    return(inputString)
    
  }  # END if
  
  if (inputType == INPUT_TEXT) {
    
    t <- validateTextInput(inputString)
    if (t == EMPTY_STRING) return(EMPTY_STRING)
    
    return(convertInputToRegex(t))
    
  }  # END if
  
  return(EMPTY_STRING)
  
}  # END validateInput


# ######## OUTPUT HANDLING ########
populateOutputTable <- function(fromTable, idx, excerpt, outTable) {
  
  title        <- fromTable[idx,]$title
  authors      <- fromTable[idx,]$short_author_list
  journal      <- fromTable[idx,]$journal
  publish_time <- fromTable[idx,]$publish_time
  publish_url  <- fromTable[idx,]$url
  
  if (title == EMPTY_STRING) {
    
    pubLicense <- fromTable[idx,]$license
    if (pubLicense == EMPTY_STRING) return(outTable)
    title <- pubLicense
    
  }  # END if
  
  outTable <- rbind(outTable, data.frame(title, authors, journal, publish_time, publish_url, excerpt))
  return(outTable)
  
}  # END populateOutputTable

formatResults <- function(t) {
  
  outTable <- data.table(Title=character(), Authors=character(), Journal=character(), PublishTime=character(), Excerpt=character())
  
  for (idx in 1:nrow(t)) {
    
    if (is.null(t$publish_url) || is.na(t$publish_url) || t$publish_url == "" )
      title <- trimws(t$title)
    else 
      title <- paste0("<a href='", trimws(t[idx,]$publish_url), "', target=_blank>", trimws(t[idx,]$title), "</a>")
    
    outRow <- list(title, trimws(t[idx,]$authors), trimws(t[idx,]$journal), trimws(t[idx,]$publish_time), t[idx,]$excerpt)
    outTable <- rbind(outTable, outRow) 
    
  }  # END for
  
  return(outTable)
  
}  # END formatResults


# ######## LOADING SEARCH SPACE ########
getCorpora <- function() {

  if (!is.null(corpora))
    return()
  
  # Read input files
  # Read full-text corpus file
  corpusFullText <- read.table(FILE_PATH_FULL_TEXT, header = TRUE, 
                               colClasses = c("character", "character", "character", "character", 
                                              "character", "character", "character", "character", 
                                              "character", "character", "character", "character", 
                                              "character", "character", "character", "character",
                                              "character", "character", "character", "character"), 
                               sep = ",", 
                               na.strings="", 
                               stringsAsFactors=FALSE)
  
  # Read abstract corpus file
  corpusAbstract <- read.table(FILE_PATH_ABSTRACT, header = TRUE, 
                               colClasses = c("character", "character", "character", "character", 
                                              "character", "character", "character", "character", 
                                              "character", "character", "character", "character", 
                                              "character", "character", "character", "character",
                                              "character", "character", "character"), 
                               sep = ",", 
                               na.strings="",
                               stringsAsFactors=FALSE)
  
  # Read title corpus file
  corpusTitle <- read.table(FILE_PATH_TITLE, header = TRUE, 
                            colClasses = c("character", "character", "character", "character", 
                                           "character", "character", "character", "character", 
                                           "character", "character", "character", "character", 
                                           "character", "character", "character", "character",
                                           "character", "character", "character"), 
                            sep = ",", 
                            na.strings="",
                            stringsAsFactors=FALSE)
  
  # Convert the "X" (row number) column to integer
  corpusFullText$X <- as.integer(corpusFullText$X)
  corpusAbstract$X <- as.integer(corpusAbstract$X)
  corpusTitle$X    <- as.integer(corpusTitle$X)
  
  # Get a susbset of articles directly related to the topic 
  # in question
  corpusFullText <- corpusFullText[grep(mainTopic, corpusFullText$full_text),]
  corpusAbstract <- corpusAbstract[grep(mainTopic, corpusAbstract$abstract),]
  corpusTitle    <- corpusTitle[grep(mainTopic, corpusTitle$title),]
  
  # Organize all corpus instances, along with their search 
  # column, into a single search space (corpora)
  corpora <<- list(list(corpusFullText, "full_text"), list(corpusAbstract, "abstract"), list(corpusTitle, "title"))
  
}  # END getCorpora


# ######## SEARCH PROCESS ########
findArticles <- function(searchExpression, isQuestionSingleTerm) {

  # Create the output table
  outDataTable <- data.table(title=character(), authors=character(), journal=character(), 
                             publish_time=character(), publish_url=character(), excerpt=character())
  
  # Parse each corpus and obtain results
  for (cidx in 1:length(corpora)) {
    
    # Copy of the current corpus and search column name
    articles  <- corpora[[cidx]][[CORPORA_IDX_CORPUS]]
    searchCol <- corpora[[cidx]][[CORPORA_IDX_SEARCH_COLUMN]]
    
    # Process questions that are composed of single terms
    if (isQuestionSingleTerm) {
      
      # Reduce the scope of the articles to those rows matching
      # the search expression
      articleIdx <- grep(searchExpression, articles[,searchCol])
      if (length(articleIdx) == 0) next()
      articles <- articles[articleIdx,]
      
      # Get a list of all search terms
      searchTerms <- strsplit(gsub(SINGLE_TERM_GROUPING, EMPTY_STRING, searchExpression), SINGLE_TERM_SPLIT)
      
      # Since one or more terms may appear on the same text, search
      # for each term independently in an effort to extract a full
      # sentence where occurrences of the term were found
      for (sidx in 1:length(searchTerms[[1]])) {
        
        # Get the next individual search term
        searchTerm <- searchTerms[[1]][sidx]
        
        # a.t. stands for: articles per (search) term
        # Get all rows from articles having matches with the
        # current search term
        a.t.idx  <- grep(searchTerm, articles[,searchCol])
        if (length(a.t.idx) == 0) next()
        a.t.rows <- articles[a.t.idx,]
        
        for (a.t.rowIdx in 1:nrow(a.t.rows)) {
          
          # Get the text for the current row (a.t.s1), and get
          # a split representation (a.t.s2)
          a.t.s1 <- a.t.rows[a.t.rowIdx, searchCol]
          a.t.s2 <- strsplit(a.t.s1, EMPTY_STRING)
          
          # Find the first occurrence of the search term 
          # within the text
          a.t.occ <- gregexpr(searchTerm, a.t.s1, perl = TRUE)
          
          # Get the start and stop positions of the search term
          # within the text
          a.t.occ.attr <- lapply(a.t.occ, attributes)
          startIdx <- a.t.occ[[1]][1]
          
          x <- a.t.occ.attr[[1]][1]
          stopPos <-as.integer(x[[1]][1])
          
          stopIdx <- (as.integer(stopPos + startIdx - 1))
          
          # Attempt to find a sentence, including the search term,
          # that may bring some insight to the reader. Begin with
          # the start index backwards until a period is found, or 
          # the beginning of the text is reached
          while (startIdx > 1) {
            
            if (a.t.s2[[1]][startIdx] == PERIOD) {
              startIdx <- (startIdx+1)
              break()
            }
            startIdx <- (startIdx-1)
            
          }  # END while
          
          # Continue with stop index until a period is found, or 
          # the end of the text is reached
          while (stopIdx < length(a.t.s2[[1]])) {
            
            if (a.t.s2[[1]][stopIdx] == PERIOD) {
              stopIdx <- (stopIdx-1)
              break()
            }
            stopIdx <- (stopIdx+1)
            
          }  # END while
          
          # Extract the excerpt from the text
          excerpt <- paste0(ELLIPSIS, WHITE_SPACE, 
                            trimws(substr(a.t.s1, start = startIdx, stop = stopIdx)), 
                            WHITE_SPACE, ELLIPSIS)
          
          if (DEBUGGING)
            print(excerpt)
          
          # Populate output table
          outDataTable <- populateOutputTable(a.t.rows, a.t.rowIdx, excerpt, outDataTable)
          
        }  # END for
        
      }  # END for
      
      if (DEBUGGING)
        print("==============================================")
      
    }  # END if
    
    # Process questions that are composed of compound terms
    if (!isQuestionSingleTerm) {
      
      a.idx  <- grep(searchExpression, articles[,searchCol])
      if (length(a.idx) == 0) next()
      a.rows <- articles[a.idx,]
      
      for (cidx in 1:nrow(a.rows)) {
        
        a.rows.t <- gregexpr(searchExpression, a.rows[cidx,searchCol], perl = TRUE)
        a.rows.t.attr <- lapply(a.rows.t, attributes)
        
        a.rows.t.attr.len <- a.rows.t.attr[[1]][1]
        if (is.vector(a.rows.t.attr.len[[1]])) a.rows.t.attr.len <- a.rows.t.attr.len[[1]][1]
        
        startIdx <- a.rows.t[[1]][1]
        stopIdx  <- (a.rows.t.attr.len + startIdx - 1)
        
        # Reduce the excerpt to a manageable sized text
        excerpt      <- paste0(ELLIPSIS, WHITE_SPACE)
        reportedText <- trimws(substr(a.rows[cidx,searchCol], start = startIdx, stop = stopIdx))
        words        <- strsplit(reportedText, WHITE_SPACE)
        
        if (length(words[[1]]) > MAX_WORDS) {
          
          for (idx in 1:NUM_WORDS_PER_CHUNK)
            excerpt <- paste0(excerpt, words[[1]][idx], WHITE_SPACE)
          
          excerpt <- paste0(excerpt, L_SQ_BRACKET, WHITE_SPACE, ELLIPSIS, WHITE_SPACE, R_SQ_BRACKET, WHITE_SPACE)
          
          for (idx in (length(words[[1]])-NUM_WORDS_PER_CHUNK):length(words[[1]]))
            excerpt <- paste0(excerpt, words[[1]][idx], WHITE_SPACE)
          
          excerpt <- paste0(excerpt, ELLIPSIS)
          
        } else {
          
          excerpt <- paste0(excerpt, reportedText, WHITE_SPACE, ELLIPSIS)
          
        }  # END if
        
        if (DEBUGGING)
          print(excerpt)
        
        # Populate output table
        outDataTable <- populateOutputTable(a.rows, cidx, excerpt, outDataTable)
        
      }  # END for
      
      if (DEBUGGING)
        print("==============================================")
      
    }  # END if
    
  }  # END for
  
  # Ensure all columns are character
  outDataTable$title   <- as.character(outDataTable$title)
  outDataTable$authors <- as.character(outDataTable$authors)
  outDataTable$journal <- as.character(outDataTable$journal)
  outDataTable$publish_time <- as.character(outDataTable$publish_time)
  outDataTable$publish_url  <- as.character(outDataTable$publish_url)
  outDataTable$excerpt <- as.character(outDataTable$excerpt)
  
  # Remove duplicates and sort the output data table
  dupes <- duplicated(outDataTable, by= "excerpt")
  if (length(dupes[dupes == TRUE]) > 0) outDataTable <- unique(outDataTable, by="excerpt")
  outDataTable <- outDataTable[with(outDataTable, order(title, authors)),]
  
  return(outDataTable)

}  # END findArticles
