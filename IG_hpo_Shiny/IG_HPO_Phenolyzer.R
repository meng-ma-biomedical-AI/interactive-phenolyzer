## ------------------------------------------------------------------------
## HPO-gene scoring-based feature selection for active question answering in multi-class prediction.
## Cong Liu.
## Last updated: 02/03/2018.
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
## load packages.
## ------------------------------------------------------------------------

library("dplyr")    # for some data preperation.
library("tidyr")    # data wrangling.
library("ggplot2")  # plot.
library("tidyverse") # for data wrangling
library("Rcpp") # speed up.
library("data.table") # accelerate
library("shiny")
library("shinyjs")
library("shinythemes")
library("DT")
library("shinyWidgets")

# ------------------------------------------------------------------------
# define functions
# ------------------------------------------------------------------------

encoderW <- function(knowledge_matrix){
  # knowledge_matrix: a 2 column matrix. [X,Y]
  Y_set <- unique(knowledge_matrix$Y)
  X_set <- unique(knowledge_matrix$X)
  big_table <- tibble(Y = rep(Y_set,each = length(X_set)),
                      X = rep(X_set,length(Y_set)))
  YXW <- knowledge_matrix %>%
    mutate(W = 1) %>%
    right_join(big_table, by =c("Y","X")) %>%
    replace_na(list(W = -1))

  return(YXW)
}

calProb <- function(obs_matrix, YXW) {
  # obs_matrix: 2 column matrix [X, value]
  # YXW: 3+ column matrix [Y,X,W,...]

  prob <- YXW %>%
    left_join(obs_matrix,by = "X") %>%
    mutate(Z_i = value * W) %>%
    group_by(Y) %>%
    summarise(Z = sum(Z_i,na.rm = TRUE)) %>%
    mutate(prob = exp(Z)/sum(exp(Z))) %>%
    arrange(-prob)

  return(prob)
}

calInformGain <- function(prior, YXW, eval_x) {
  # prior: a 4 column matrix [Y, Z, P_wild, P]
  # YXW: a 3+ column matrix [Y,X,W,...]
  # eval_x: a 3 column matrix [X, value1, value2]

  eps <- 0.00000001 # avoid log(0)

  # init big table.
  # vectorize could make the computation fast.
  Y_set <- unique(prior$Y)
  X_set <- unique(eval_x$X)
  big_table <- tibble(X = rep(X_set,each = length(Y_set)),
                      Y = rep(Y_set,length(X_set)))

  # calculate the updated P_wild.
  Z <- big_table %>%
    left_join(eval_x,by = "X") %>%
    left_join(YXW, by = c("Y","X")) %>%
    mutate(add_Z1_i = value1 * W) %>%
    mutate(add_Z2_i = value2 * W) %>%
    right_join(prior, by = "Y") %>%
    mutate(Z1 = Z + add_Z1_i) %>%
    mutate(Z2 = Z + add_Z2_i) %>%
    mutate(exp_Z1 = exp(Z1)) %>%
    mutate(exp_Z2 = exp(Z2))

  post <- Z %>%
    group_by(X) %>%
    summarise(exp_Z1_sum = sum(exp_Z1,na.rm = TRUE),
              exp_Z2_sum = sum(exp_Z2,na.rm = TRUE)) %>%
    right_join(Z,by = "X") %>%
    mutate(prior = prob, post1 = exp_Z1/exp_Z1_sum, post2 = exp_Z2/exp_Z2_sum) %>%
    select(X,Y,prior,post1,post2)


  # calculate conditional probability for value1 and value2.
  pX_prior <- YXW %>%
    group_by(X) %>%
    summarise(pX1 = sum(W==1)/n(),
              pX2 = sum(W==-1)/n())

  # pY_X <- pX_prior %>%
  #   right_join(Post, by = "X") %>%
  #   mutate(pYi_X1_wild = 1/(1+exp(-add_Z1))) %>%
  #   mutate(pYi_X2_wild = 1/(1+exp(-add_Z2))) %>%
  #   select(X,Y,pX1,pX2,pYi_X1_wild,pYi_X2_wild)
  #
  # pY_X <- pY_X %>%
  #   group_by(X) %>%
  #   summarise(pYi_X1_sum = sum(pYi_X1_wild,na.rm = TRUE),
  #             pYi_X2_sum = sum(pYi_X2_wild,na.rm = TRUE)) %>%
  #   right_join(pY_X, by = "X") %>%
  #   mutate(pYi_X1 = pYi_X1_wild/pYi_X1_sum) %>%
  #   mutate(pYi_X2 = pYi_X2_wild/pYi_X2_sum)

  pY_X <- post %>%
    mutate(pYi_X1 = post1) %>%
    mutate(pYi_X2 = post2) %>%
    left_join(pX_prior,by = "X")


  pX_Y <- pY_X %>%
    mutate(pX1_Yi_wild = pYi_X1 * pX1) %>%
    mutate(pX2_Yi_wild = pYi_X2 * pX2) %>%
    mutate(pX1_Yi = pX1_Yi_wild/(pX1_Yi_wild + pX2_Yi_wild)) %>%
    mutate(pX2_Yi = pX2_Yi_wild/(pX1_Yi_wild + pX2_Yi_wild)) %>%
    select(X,Y,pX1_Yi,pX2_Yi)

  pY_X0 <- post %>%
    mutate(pYi_X0 = prior) %>%
    select(X,Y,pYi_X0)

  pX_X0 <- pY_X0 %>%
    right_join(pX_Y,by = c("Y","X")) %>%
    group_by(X) %>%
    summarise(pX1_X0 = sum(pYi_X0 * pX1_Yi),
              pX2_X0 = sum(pYi_X0 * pX2_Yi))


  # get a middle step table
  middle_table <- post %>%
    left_join(pX_X0,by = "X")

  # calculate kl divergence, which measures the information gain.
  information_gain <- middle_table %>%
    mutate(kl1_i = post1 * log(post1/prior + eps)) %>%
    mutate(kl2_i = post2 * log(post2/prior + eps)) %>%
    group_by(X) %>%
    summarise(kl1 = sum(kl1_i, na.rm = TRUE),
              kl2 = sum(kl2_i, na.rm = TRUE)) %>%
    left_join(pX_prior,by = "X") %>%
    mutate(IG = pX1 * kl1 + pX2 * kl2) %>%
    arrange(-IG)



  # # calculate expected information gain.
  # !!! THIS INFORMATION CALCULATION IS WRONG.
  # USE KL TO MEASURE THE INFORMATION GAIN
  # final_table <- middle_table %>%
  #   mutate(E_prior_i = -prior * log(prior+eps)) %>%
  #   mutate(E_post1_i = -post1 * log(post1+eps)) %>%
  #   mutate(E_post2_i = -post2 * log(post2+eps)) %>%
  #   mutate(information_gain_i = E_prior_i - pX1_X0 * E_post1_i - pX2_X0 * E_post2_i)
  #
  # information_gain <- final_table %>%
  #   group_by(X) %>%
  #   summarise(IG = sum(information_gain_i)) %>%
  #   arrange(-IG)

  return(information_gain)

}

getTopFeature <- function(information_gain, top_n){
  # information_gain: a 2-column matrix [X, IG]
  # top_n: a number.

  top_features <- data.frame(X=information_gain$X[1:top_n])
  return(top_features)
}

answerQuestion <- function(top_features, YXW, true_y){
  # top_features: a 1 column matrix [X]
  # YXW: 3+ column matrix [Y,X,W,...]
  # true_y: a number.

  obs_matrix <- top_features %>%
    left_join(YXW, by = "X") %>%
    filter(Y == true_y) %>%
    mutate(value = W) %>%
    select(X, value)

  return(obs_matrix)

}

## ------------------------------------------------------------------------
## set params
## ------------------------------------------------------------------------

top_K = 100 # number of candidate gene in first step / true gene is generated from top_K gene list.
q_shrink = 20 # number of candidate gene shrinked in each step
q_each_step = 1 # number of question answered in each step
init_term = 10 # number of inital answers
# read knowledge base.
knowledge_base = fread("~/Project/phenolyzer_QA/hpo_simu_2018_02_05/ALL_SOURCES_ALL_FREQUENCIES_phenotype_to_genes.txt", sep = "\t", skip = 1)
knowledge_matrix = knowledge_base[,.(V1,V4)]
colnames(knowledge_matrix) = c("X","Y")
# encode knowledge base.
YXW = encoderW(knowledge_matrix)
YXW = data.table(YXW) # convert to data table for speed concern


## ------------------------------------------------------------------------
## define UI
## ------------------------------------------------------------------------
library(shiny)
fieldsMandatory <- c("positive_terms")
fieldsAll <- c("name", "favourite_pkg", "used_shiny", "r_num_years", "os_type")

appCSS <- ".mandatory_star { color: red; }"

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

splitString <- function(terms){
  unlist(strsplit(terms,split = ","))
}
ui = fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  
  titlePanel("Interactive Phenolyzer"),
  DT::dataTableOutput("responsesTable"),
  textOutput("p_terms"),
  textOutput("n_terms"),
  
  div(
    id = "form",
    
    textInput("positive_terms", labelMandatory("Observed HPO terms"), "",placeholder = "HP:0000878,HP:0003535"),
    textInput("negative_terms", "Not observed HPO terms", "",placeholder = "HP:0000878,HP:0003535"),
    # textInput("favourite_pkg", "Favourite R package"),
    # checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
    # sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
    # selectInput("os_type", "Operating system used most frequently",
    #             c("",  "Windows", "Mac", "Linux")),
    actionButton("submit", "Submit", class = "btn-primary"),
    textInput("question", "Do you observe the following phenotype"),
    uiOutput("operation")
    #radioGroupButtons("operation",choices = c("YES","NO","MISSING"),selected=character(0))
  )
)
server = function(input, output, session) {
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  
  top_K_1 = top_K
  
  observeEvent(input$submit, {
      
    data <- reactive({
      data <- data.frame(X = splitString(input$positive_terms), value = rep(1,length(input$positive_terms)))
      if(!is.null(input$negative_terms)){
         data <- data %>% bind_rows(data.frame(X = input$negative_terms, value = rep(1,length(input$negative_terms))))
      }
      data
    })
      
    obs_matrix <- data()
    prior <- calProb(obs_matrix,YXW)
   
      
    YXW_1 = YXW[Y %in% prior$Y[1:top_K_1]]
    # reset the initial obs matrix. only consider the features in YWX_1 related feature set.
    obs_matrix_1 = obs_matrix[obs_matrix$X %in% YXW_1$X,] 
    prior_1 = calProb(obs_matrix_1,YXW_1)
    X_1 = setdiff(unique(YXW_1$X),obs_matrix_1$X)
    eval_x_1 = data.frame(X = X_1,value1 = rep(1,length(X_1)), value2 = rep(-1,length(X_1)))
    # update information gain for each feature.
    information_gain_1 = calInformGain(prior_1, YXW_1, eval_x_1)
    top_features_1 = getTopFeature(information_gain_1, q_each_step)
      
    output$responsesTable <- DT::renderDataTable(
      isolate(prior) %>%
        print(n=10),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    )
      
    output$p_terms <- renderText({
      paste("Postive terms", isolate(input$positive_terms), sep=":")
    })
  
    output$n_terms <- renderText({
      paste("Negative terms", isolate(input$negative_terms), sep=":")
    })

      
    updateTextInput(session,"question",value = top_features_1[1,1])
    output$operation = renderUI({
      radioGroupButtons("operation",choices = c("YES","NO","MISSING"),selected=character(0))
    })
    
      
    observeEvent(input$operation,{
      if(input$operation == "YES"){
        updateTextInput(session, "positive_terms", value = paste(top_features_1[1,1],input$positive_terms,sep = ","))
      }
      if(input$operation == "NO"){
        updateTextInput(session, "negative_terms", value = paste(top_features_1[1,1],input$negative_terms,sep = ","))
      }
    })
  })
}

shinyApp(ui = ui, server = server)