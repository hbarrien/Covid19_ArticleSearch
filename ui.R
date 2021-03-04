# ui.R
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
# Front end definition for a ShinyApp application.


# ################################
# ########## LIBRARIES ###########
# ################################
library(shiny)


# ################################
# ########## CONSTANTS ###########
# ################################
PAGE_TITLE             <- "<a href='https://www.kaggle.com/allen-institute-for-ai/CORD-19-research-challenge/tasks' target=_blank>COVID-19 Open Research Dataset Challenge (CORD-19)</a>"
SLOGAN                 <- "USING NATURAL LANGUAGE PROCESSING TO HELP THE WORLD TO UNDERSTAND COVID-19 FASTER"
DEVELOPER_NAME         <- "Developed by Herbert Barrientos"
TERM_EXPLANATION       <- "A term is one word, or a short phrase (i.e., sequence of words) separated by spaces."
COVID_IMG_NAME         <- "covid-image.webp"
TEXT_INPUT_LABEL       <- "Question:"
TEXT_INPUT_PLACEHOLDER <- "Enter a single term, a sequence of terms divided by commas, or a regular expression"
RADIO_LABEL            <- "Question type:"
RADIO_OPTIONS          <- list("Term(s)" = 1, "Regular expression" = 2)
ACTION_BTN_LABEL       <- "Go!"
OUT_QUESTION_LABEL     <- "Your question: "
SAMPLE_REGEX_MAIN      <- "If you would like to use any of the following regular expressions, just copy and paste to the input field. Please remember to switch to the 'Results' tab and click the option 'Regular expression' in the 'Question type' section."

TASK_ORIGIN_GENETICS_EVOLUTION <- "Task: What do we know about virus genetics, origin, and evolution?"
ORIGIN_GENETICS_EVOLUTION_01   <- "TRACKING WHOLE GENOMES:"
ORIGIN_GENETICS_EVOLUTION_02   <- "(track(ing)?|monitor(ing)?).+(whole|entire|complete) *(genome|genomic sequence)"
ORIGIN_GENETICS_EVOLUTION_03   <- "TRACKING VARIATIONS OF THE VIRUS:"
ORIGIN_GENETICS_EVOLUTION_04   <- "(track(ing)?|monitor(ing)?).+(vir(us|al)|genetic|genome|sequence) *(mutation(s)?|alteration(s)?|change(s)?|variation(s)?|evolution)"
ORIGIN_GENETICS_EVOLUTION_05   <- "SURVEILLANCE OF WILDLIFE/LIVESTOCK IN SOUTHEAST ASIA:"
ORIGIN_GENETICS_EVOLUTION_06   <- "(surveill|monitor|control|vigilanc|supervis).+(livestock|farm animal|cattle|poultry).+southeast asia"
ORIGIN_GENETICS_EVOLUTION_07   <- "SUSTAINABLE RISK REDUCTION STRATEGIES:"
ORIGIN_GENETICS_EVOLUTION_08   <- "risk.+(reduction|decrement|decrease|management|assessment|mitigation|prevention).+(strateg(y|ies)|planning|coordination| action(s)?)"

TASK_OTHER                     <- "Other..."
TASK_OTHER_01                  <- "WHAT IS THE INCUBATION PERIOD IN DAYS?:"
TASK_OTHER_02                  <- "incubation (time|period).* day(s)?"
TASK_OTHER_03                  <- "INCUBATION PERIOD BETWEEN 4 AND 9 DAYS:"
TASK_OTHER_04                  <- "incubation (time|period).* [4-9] day(s)?"


# ################################
# ########## FUNCTIONS ###########
# ################################
shinyUI(fluidPage(

  titlePanel(""),
  h3(HTML(PAGE_TITLE)),
  p(SLOGAN),
  p(DEVELOPER_NAME),
  br(),
  
  sidebarPanel(
    
    width = 4,
    img(src=COVID_IMG_NAME, height = 63, width = 75),
    br(), br(),
    textInput("question", TEXT_INPUT_LABEL, placeholder = TEXT_INPUT_PLACEHOLDER), 
    div(TERM_EXPLANATION),
    br(),
    radioButtons("radio", RADIO_LABEL, choices = RADIO_OPTIONS, selected = 1),
    br(),
    actionButton("goButton", ACTION_BTN_LABEL)
  ),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                
                tabPanel("Results",
                         h4(OUT_QUESTION_LABEL),
                         verbatimTextOutput("yourQuestion"),
                         DT::dataTableOutput("outDataTable")),
                
                tabPanel("Sample regular expressions", 
                         h4(SAMPLE_REGEX_MAIN),
                         br(),
                         h4(TASK_ORIGIN_GENETICS_EVOLUTION),
                         p(ORIGIN_GENETICS_EVOLUTION_01),
                         p(ORIGIN_GENETICS_EVOLUTION_02),
                         br(),
                         p(ORIGIN_GENETICS_EVOLUTION_03),
                         p(ORIGIN_GENETICS_EVOLUTION_04),
                         br(),
                         p(ORIGIN_GENETICS_EVOLUTION_05),
                         p(ORIGIN_GENETICS_EVOLUTION_06),
                         br(),
                         p(ORIGIN_GENETICS_EVOLUTION_07),
                         p(ORIGIN_GENETICS_EVOLUTION_08),
                         br(),
                         h4(TASK_OTHER),
                         p(TASK_OTHER_01),
                         p(TASK_OTHER_02),
                         br(),
                         p(TASK_OTHER_03),
                         p(TASK_OTHER_04))
  
    )  # END tabsetPanel
    
  )  # END mainPanel
  
))  # END fluidPage/shinyUI
