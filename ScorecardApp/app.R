# detach("package:dplyr", unload = TRUE)
# detach("package:plyr", unload = TRUE)

library(shiny)
library(dplyr)
library(tidyr)
library(broom)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  # Application title
  titlePanel("Model Scorecard"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

      selectInput("data", label = h3("Select Regression"),
                  choices = ls(.GlobalEnv),selected="authdata" ),

      actionButton("action", label = "Submit")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # verbatimTextOutput("summary"),
      h3("Model Measures"),
      tableOutput("summary"),
      h3("Type III"),
      tableOutput("type3"),
      h3("Model Coefficients"),
      tableOutput("coefficients")
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session) {

  options(
    contrasts = c("contr.treatment", "contr.treatment"),
    na.option = na.exclude
  )


  type <- eventReactive(input$action,{
    input$type
  })

  data <- eventReactive(input$action,{
    data <- get(input$data)
    # data <- subset(data, select = c(input$col, input$bycol))
  })


  output$summary <- renderTable({
    data=data()
    glance(data)

  })

  output$type3 <- renderTable({
    data=data()
    drop1(data, test = "Chisq")
  })



  output$coefficients <- renderTable({
    data=data()
    # tidy(data, exponentiate=TRUE)

    model.coeff <- tidy(data)
    vl <- attr(data$terms,"term.labels")

    get_cat <- function(cat_name){
      VAR <- expand_(data$data,cat_name)
      names(VAR) <- "term_v"
      VAR$varname <- cat_name
      VAR$new <- paste0((cat_name),VAR$term_v)
      VAR
    }

    cat_vars <- plyr::ldply(vl,get_cat)

    model.coeff <- full_join(model.coeff,cat_vars,by = c("term"="new"))
    model.coeff2 <- mutate(model.coeff
                           ,varname=ifelse(is.na(varname), term, varname)
                           ,estimate = ifelse(is.na(estimate),0,estimate))


    model.coeff3 <- model.coeff2 %>%
      arrange(varname, estimate)

    model.coeff3 <- model.coeff3 %>%
      group_by(varname) %>%
      mutate(coeff = -1*estimate
             ,odds = exp(coeff)
             ,newodds = odds/min(odds)
             ,lnewodds=log(newodds))



    sta <- dplyr::summarise(model.coeff3, maxl = max(lnewodds))
    st <- dplyr::summarise(sta,smaxl = sum(maxl))

    model.coeff3 <- model.coeff3 %>%
      mutate(score = round(lnewodds*(1000/st$smaxl))
             , score_n = max(score)-score)

    model.coeff_out <- model.coeff3 %>% select(varname, term_v, estimate, std.error,p.value, score_n)
  })


})
# Run the application
shinyApp(ui = ui, server = server)

