library(shiny)
library(tidyverse)
library(janitor)
library(shinyWidgets)
library(shinythemes)

### cleaning the data and creating factor levels

NHANES_BVA <- read.csv("NHANES_BVA.csv") %>% 
  clean_names()

NHANES_BVA_clean <- NHANES_BVA %>%
  select(where(~ !all(is.na(.)))) %>%
  select(-contains("mds")) %>%
  mutate(across(where(is.integer), as.double)) %>%
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE)))) %>%
  mutate(across(where(is.factor), ~replace_na(., mode(.)))) %>%
  mutate(across(where(is.character), ~replace_na(., mode(.)))) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(
    race = factor(race, levels=c('NH White', 'NH Black', 'Mex-American', 'Oth Hisp', 'Other'), labels=c('White', 'Black', 'Mexican-American', 'Other Hispanic', 'Other')),
    age = age_egfr,
    diabetes = factor(diabetes, levels=c("Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available", "No - Condition not listed as a multiple cause of death", "Yes - Condition listed as a multiple cause of death"), labels=c('Not Available', 'No', 'Yes'))
  )  %>%
  mutate(pregnancy = ifelse(ridexprg==2.00, 1, 0)) %>%
  select(-age_egfr)

simple_model <- lm(egfr ~ age + pregnancy + race + diabetes + htn_med, data=NHANES_BVA_clean)

### defining a function that requires input for the selected variables

estimate <- function(risk_factors) {
  required_fields <- c("age", "pregnancy", "race", "diabetes", "htn_med")
  if (!all(required_fields %in% names(risk_factors))) {
    stop("Missing required risk factors")
  }
  
  new_data <- data.frame(
    age=risk_factors$age, 
    pregnancy=risk_factors$pregnancy, 
    race=risk_factors$race, 
    diabetes=risk_factors$diabetes, 
    htn_med=risk_factors$htn_med, 
  )
  predict(simple_model, newdata=new_data)
}

### here we define the function to get CKD stage outside of the server function. Source: https://www.kidney.org/atoz/content/gfr
get_ckd_stage <- function(egfr) {
  if (egfr >= 90) {
    "Stage 1 CKD: Normal kidney function"
  } else if (egfr >= 60) {
    "Stage 2 CKD: Mild loss of kidney function"
  } else if (egfr >= 45) {
    "Stage 3a CKD: Mild to moderate loss of kidney function"
  } else if (egfr >= 30) {
    "Stage 3b CKD: Moderate to severe loss of kidney function"
  } else if (egfr >= 15) {
    "Stage 4 CKD: Severe loss of kidney function"
  } else {
    "Stage 5 CKD: Kidney failure"
  }
}


### advice based on the stages

advice_list <- list(
  "Test your urine for albumin to have a complete picture of your overall kidney health",
  "Repeat your eGFR test in 3 months to check if your eGFR remains lower than 90",
  "Take medication that may help slow progression of kidney disease (such as ACE inhibitors, ARBs, SGLT2 inhibitors, or nonsteroidal mineralocorticoid receptor antagonists)",
  "Adjust any current medications due to reduced kidney function",
  "Get nutritional and dietary counseling to help support kidney function and overall health",
  "Start seeing a kidney specialist (nephrologist)",
  "Learn more about end-stage kidney disease and treatment options",
  "Be evaluated for a kidney transplant and be placed on a kidney transplant list"
)

get_kidney_disease_advice <- function(stage) {
  advice_matrix <- matrix(c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,    # applicable to all stages
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,   # applicable to 2-5
    FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,   # applicable to stages 2-4
    FALSE, FALSE, TRUE, TRUE, TRUE, TRUE,   # applicable to stages 3a-5
    FALSE, FALSE, TRUE, TRUE, TRUE, TRUE,   # applicable to stages 3a-5
    FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,  # applicable to stages 4, 5
    FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,  # applicable to stage 4, 5
    FALSE, FALSE, FALSE, FALSE, FALSE, TRUE  # applicable to stage 5
  ), nrow = 8, byrow = TRUE)
  
  applicable_advice <- advice_list[advice_matrix[, stage]]
  return(applicable_advice) # returns advice based on the stage matched
}

### UI 

ui = fluidPage(theme = shinytheme("superhero"),
               
               tags$head(
                 tags$script(HTML("
      $(document).on('shiny:sessioninitialized', function(event) {
        // Function to set the iframe height dynamically
        function setIframeHeight() {
          var windowHeight = $(window).height();
          var offset = 150;
          var newHeight = windowHeight - offset;
          $('#myIframe').height(newHeight + 'px');
        }
        
        // Set the height on app start
        setIframeHeight();
        
        // Update the height whenever the window is resized
        $(window).resize(function() {
          setIframeHeight();
        });
      });
    "))
               ),
    
      tags$head(
                tags$style(HTML("
                            #query {
                              border: 4px solid #FFFFFF;  
                              border-radius: 5px; 
                              padding: 5px;   
                            }
                          "))
                         ),
    
    
      navbarPage("Analyzing and Diagnosing CKD",
                 
                 ### analysis report (rendering from project.qmd)
                 # tabPanel("Analysis of Risk Factors", 
                 #          uiOutput("iframeOutput")
                 # ),
                 
                 ### adding a tab to render the html output
                 
                 tabPanel("Analysis of Risk Factors", 
                         includeHTML("www/project.html")
                 ),
                 
                 tabPanel("Calculator Demo", 
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("age", "Age", min = 0, max = 100, value = 48),
                              radioButtons("pregnancy", "Pregnancy Status", choices = c("Pregnant", "Not Pregnant")),
                              selectInput("race", "Race", choices = c("White", "Black", "Mexican-American", "Other Hispanic", "Other")),
                              selectInput("dm", "Diabetes", choices = c("Yes", "No", "Not Available")),
                              prettyCheckbox("hm", "Hypertension Medication", value = FALSE),
                              actionButton("query", "Calculate eGFR")
                            ),
                            mainPanel(
                              h2("eGFR: "),
                              textOutput("egfr"),
                              h2("Advice based on risk level: "),
                              uiOutput("adviceList") # display the advice in bullet points
                              
                            )
                 ),
                          )
                ))

### Server

server <- function(input, output, session) {
  
  ### ractive expression to update risk factors
  risk_factors <- reactive({
    list(
      age = input$age,
      pregnancy = ifelse(input$pregnancy == "Pregnant", 1, 0), # pregnant is encoded as 1
      race = input$race,
      diabetes = input$dm,
      htn_med = as.numeric(input$hm)
    )
  })
  
  ### calculates the eGFR on button click
  
  output$egfr <- renderText({
    req(input$query) # ensures button is clicked
    
    new_patient <- data.frame(risk_factors())
    
    ### error handling
    tryCatch({
      egfr_value <- predict(simple_model, newdata = new_patient)
      paste("The eGFR is", round(egfr_value, 2), "which indicates", get_ckd_stage(egfr_value))
    }, error = function(e) {
      "Error in calculating eGFR"
    })
  })
  
  ### rendering advice list as bullet points
  output$adviceList <- renderUI({
    req(input$query) 
    
    ### calculate the eGFR and getting advice based on the stage
    tryCatch({
      egfr_value <- predict(simple_model, newdata = data.frame(risk_factors()))
      ckd_stage <- if (egfr_value >= 90) {
        1
      } else if (egfr_value >= 60) {
        2
      } else if (egfr_value >= 45) {
        3
      } else if (egfr_value >= 30) {
        4
      } else if (egfr_value >= 15) {
        5
      } else {
        6
      }
      advice <- get_kidney_disease_advice(ckd_stage)
      
      tags$ul(
        lapply(advice, function(item) tags$li(item))
      )
    }, error = function(e) {
      tags$p("Error in calculating eGFR")
    })
  })
  
}

shinyApp(ui = ui, server = server)