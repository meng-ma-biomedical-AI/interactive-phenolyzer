rm(list=ls())
library(dplyr)
library(tidyr)
library(janitor)
library(data.table)
library(shiny)
library("shinyjs")
library("shinythemes")
library("DT")
library("shinyWidgets")

pwd = getwd()
setwd(pwd)
input_csv = read.csv("RecruitmentOptimizat_DATA_LABELS_2018-02-09_0931.csv")
## define a helper function
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}
# clean csv.
input_csv_clean <- input_csv %>%
  clean_names() %>%
  mutate_all(funs(empty_as_na)) 

clean_condition = input_csv_clean %>%
  filter(repeat_instrument == 'Conditions' & complete_2 == 'Complete') %>%
  select(record_id,condition,severity,duration_of_condition,inclusion_or_exclusion) %>%
  mutate(YES = (inclusion_or_exclusion == "Inclusion")) %>%
  mutate(NO = (inclusion_or_exclusion == "Exclusion")) %>%
  mutate(UK = TRUE)

# assume the probability of each choices are the same.
# cal_condition = clean_condition %>%
#   group_by(.dots=c("condition","severity","duration_of_condition")) %>%
#   summarise(YES_exclude = sum(YES==FALSE),
#             NO_exclude = sum(NO==FALSE),
#             UK_exclude = sum(UK==FALSE)) %>%
#   mutate(sum_exclude = YES_exclude + NO_exclude + UK_exclude) %>%
#   arrange(-sum_exclude)
# 
# c = cal_condition$condition[1]
# s = ifelse(is.na(cal_condition$severity[1]), "",cal_condition$severity[1])
# d = ifelse(is.na(cal_condition$duration_of_condition[1]),"",cal_condition$duration_of_condition[1])
# question = paste("Do you have",s,c,d,"?")
# clean_condition_update = clean_condition

ui = fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Interactive Trial Hunter"),
  sidebarPanel(
    uiOutput('condition'),
    uiOutput('severity'),
    uiOutput('duration'),
    actionButton(inputId = 'start',label = 'Start',width = '30%'),
    # uiOutput("previous") not implemented yet
    uiOutput("neext")
    # actionButton(inputId = 'previous',label = 'Previous',width = '30%'),
    # actionButton(inputId = 'next',label = 'Start',width = '30%')
  ),
  mainPanel(
    DT::dataTableOutput("responsesTable")
  )
)

server = function(input, output, session) {
  v <- reactiveValues(clean_condition_work = clean_condition)
  c <- reactiveValues(c = NULL)
  s <- reactiveValues(s = NULL)
  d <- reactiveValues(d = NULL)
  observeEvent(input$start,{
    if(is.null(input$condition)){
      v$clean_condition_work = clean_condition
    }else{
      cat("Restart\n!!!")
      v$clean_condition_work = clean_condition
      c <- reactiveValues(c = NULL)
      s <- reactiveValues(s = NULL)
      d <- reactiveValues(d = NULL)
    }
    
    # show candidate trial list.
    output$responsesTable <- DT::renderDataTable(
      v$clean_condition_work %>% 
                select(1:5),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    )
    
    # calculate top questions.
    cal_condition = v$clean_condition_work %>%
    group_by(.dots=c("condition","severity","duration_of_condition")) %>%
    summarise(YES_exclude = sum(YES==FALSE),
              NO_exclude = sum(NO==FALSE),
              UK_exclude = sum(UK==FALSE)) %>%
    mutate(sum_exclude = YES_exclude + NO_exclude + UK_exclude) %>%
    arrange(-sum_exclude)
      
    c$c = cal_condition$condition[1]
    s$s = ifelse(is.na(cal_condition$severity[1]), "",cal_condition$severity[1])
    d$d = ifelse(is.na(cal_condition$duration_of_condition[1]),"",cal_condition$duration_of_condition[1])

    # update ui questions.  
    output$condition <- renderUI({
      radioButtons(inputId = "condition",label = c$c,choices = c("Unknown","Yes","No"))
    })
    if(s$s!=''){
      output$severity <- renderUI({
        radioButtons(inputId = "severity",label = "Severity",choices = c("Unknown","Mild","Moderate","Severe"))
      })
    }else{
      output$severity <- renderUI({})
    }
    if(d$d!=''){
      output$duration <- renderUI({
        radioButtons(inputId = "duration",label = "Duration",choices = c("Unknown","<1 week","< 1 month","< 6 month","< 1 year","< 5 year", "< 10 year"))
      })
    }else{
      output$duration <- renderUI({})
    }
    
    # generate previous and next buttons.
    
    # not implemented yet.
    # output$previous <- renderUI({
    #   actionButton(inputId = 'previous',label = 'Previous',width = '30%')
    # })
    output$neext <- renderUI({
      actionButton(inputId = 'neext',label = 'Next',width = '30%')
      
    })
  })
  
  observeEvent(input$neext,{
    # update remaininig trial tables.
    if(input$condition == "Unknown"){
      # remove the condition - not informative since user could not provide information.
      v$clean_condition_work = v$clean_condition_work %>%
        filter(condition != c$c)
    }else{
      if(input$condition == "No"){
        # remove the condition and trials include the condition.
        ex_id = v$clean_condition_work %>%
          filter(condition == c$c & inclusion_or_exclusion == "Inclusion") 
        ex_id %>%
          select(record_id) %>%
          unique()
      }else{
        
        # remove the trials exclude the condition (without duration and severity criteria) 
        ex_id = v$clean_condition_work %>%
          filter(condition == c$c & inclusion_or_exclusion == "Exclusion" & is.na(severity) & is.na(duration_of_condition))
          
        # remove the trials exclude the condition (on matched severity) 
        # or trials include the condition (on not matched severity).
        if(identical(input$severity, "Mild")){
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Inclusion" & (severity != "Mild" & !is.na(severity)))
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Exclusion" & (severity == "Mild" | is.na(severity))) %>%
            bind_rows(ex_id)
        }
        if(identical(input$severity, "Moderate")){
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Inclusion" & (severity != "Moderate" & !is.na(severity)))
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Exclusion" & (severity == "Moderate" | is.na(severity))) %>%
            bind_rows(ex_id)
        }
        if(identical(input$severity, "Severe")){
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Inclusion" & (severity != "Severe" & !is.na(severity)))
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Exclusion" & (severity == "Severe" | is.na(severity))) %>%
            bind_rows(ex_id)
        }

        
        # remove the trials exclude the condition (on matched duration) 
        # or trials include the condition (on not matched duration).
        if(identical(input$duration,"Less than 1 year")){
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Inclusion" & (!duration_of_condition %in% c("Less than 1 year") & !is.na(duration_of_condition)))
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Exclusion" & (duration_of_condition %in% c("Less than 1 year") | is.na(duration_of_condition))) %>%
            bind_rows(ex_id)
        }
        if(identical(input$duration,"Less than 6 month")){
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Inclusion" & (!duration_of_condition %in% c("Less than 6 month","Less than 1 year") & !is.na(duration_of_condition)))
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Exclusion" & (duration_of_condition %in% c("Less than 6 month","Less than 1 year") | is.na(duration_of_condition))) %>%
            bind_rows(ex_id)
        }
        if(identical(input$duration,"Less than 1 month")){
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Inclusion" & (!duration_of_condition %in% c("Less than 1 month","Less than 6 month","Less than 1 year") & !is.na(duration_of_condition)))
          ex_id = v$clean_condition_work %>%
            filter(condition == c$c & inclusion_or_exclusion == "Exclusion" & (duration_of_condition %in% c("Less than 1 month","Less than 6 month","Less than 1 year") | is.na(duration_of_condition))) %>%
            bind_rows(ex_id)
        }

        
        ex_id %>%
          select(record_id) %>%
          unique()
      }
      
      
      
      v$clean_condition_work = v$clean_condition_work %>%
        filter(condition != c$c & !record_id %in% ex_id$record_id)
    }
    # show candidate trial list.
    output$responsesTable <- DT::renderDataTable(
      v$clean_condition_work %>% 
        select(1:5),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    )
    
    # calculate top questions.
    cal_condition = v$clean_condition_work %>%
      group_by(.dots=c("condition","severity","duration_of_condition")) %>%
      summarise(YES_exclude = sum(YES==FALSE),
                NO_exclude = sum(NO==FALSE),
                UK_exclude = sum(UK==FALSE)) %>%
      mutate(sum_exclude = YES_exclude + NO_exclude + UK_exclude) %>%
      arrange(-sum_exclude)
    
    c$c = cal_condition$condition[1]
    s$s = ifelse(is.na(cal_condition$severity[1]), "",cal_condition$severity[1])
    d$d = ifelse(is.na(cal_condition$duration_of_condition[1]),"",cal_condition$duration_of_condition[1])
    
    # update ui questions.  
    output$condition <- renderUI({
      radioButtons(inputId = "condition",label = c$c,choices = c("Unknown","Yes","No"))
    })
    if(!identical(s$s,'')){
      output$severity <- renderUI({
        radioButtons(inputId = "severity",label = "Severity",choices = c("Unknown","Mild","Moderate","Severe"))
      })
    }else{
      output$severity <- renderUI({
      })
    }
    if(!identical(d$d,'')){
      output$severity <- renderUI({
        radioButtons(inputId = "duration",label = "Duration",choices = c("Unknown","Less than 1 month","Less than 6 month","Less than 1 year"))
      })
    }else{
      output$severity <- renderUI({
      })
    }
  })
  
  # not implemented yet.
  # observeEvent(input$previous,{})
  
}

shinyApp(ui = ui, server = server)
  
