#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shiny")
#install.packages("shinyBS")
#install.packages("shinyjs")

options(shiny.sanitize.errors = FALSE)

library("shiny")
library("shinyBS")
library("shinyjs")
source("server.R")

# Define UI for application that calculate sample size for SMARTp
shinyUI(
    fluidPage(

    # Application title
    titlePanel(HTML("<strong>SMARTp-SS Calculator</strong>: A Sample Size Calculator for the Sequential Multiple Assignment Randomized Trials for Periodontitis "), 
               windowTitle = "SMARTp-SS Calculator"),    
    useShinyjs(),
    
    ### Introduction of SMART-SS Calculator on left-hand side panel ###
    sidebarPanel(
        includeHTML("www/sidebar.Rhtml")
    ),
   
    mainPanel(
        tags$hr(),
      
        ### Study Setup ###
        
        h3("The Experimental Design Diagram"),
        verticalLayout(
          imageOutput("Image")
        ),                                  
        tags$hr(),
        
        h3("The Stage-1 Treatment Responding Rates"),
        splitLayout(
          sliderInput("g1", label =HTML( paste0( "Scaling and Root Planning (SRP) &gamma;", tags$sup( HTML( paste0( "d", tags$sub( 1 ) ) ) ) ) ), min=0, max=1, value=0.25),
          sliderInput("g2", label =HTML( paste0( "Laser Therapy &gamma;", tags$sup( HTML( paste0( "d", tags$sub( 5 ) ) ) ) ) ), min=0, max=1, value=0.5)
        ), 
        tags$hr(),
        
        h3(HTML("The parameters of the conditional autoregression (CAR) model")),
        verticalLayout(
          HTML(paste0(
                      "The latent vector <b>Q</b>", tags$sub("i"), "follows a multivariate normal distribution, with mean vector <b>0</b> and
                      covariance matrix with a CAR structure, i.e. <b>&Sigma;</b> = &tau;", tags$sup(2), "(<b>C</b> − &rho;<b>D</b>)", tags$sup("−1"), ".",
                      "<br/>This random vector is shared between the regression models of clinical attachment level (CAL) and missingness.",
                      "<br/>The outcome of CAL model is the change in mean CAL (<b>Y</b>) of tooth 't' and patient 'i', i.e. Y", tags$sub("it"), "=&mu;", tags$sub("i") , "+Q", tags$sub("it"), "+&epsilon;", 
                      tags$sub("it1"), ", where t=1,...,28 and i=1,...,N.", 
                      "<br/>The outcome of missingess model is the missing indicator (<b>M</b>) of tooth 't' and patient 'i', i.e. M", tags$sub("it"), 
                      "=I(a", tags$sub("0"), "+b", tags$sub("0"), "Q", tags$sub("it"), "+&epsilon;", tags$sub("it0"),  ">0)."
                      )
               )
          ),
        splitLayout(
          numericInput(inputId="tau", label =HTML("Variation &tau;"), value=0.85),
          numericInput(inputId="rho", label =HTML("Association &rho;"), value=0.975)
        ),
        tags$hr(),
        
        h3(HTML("The parameters of the CAL model")),
        verticalLayout(
          HTML(paste0(
            "Set the values of the mean parameter &mu;", tags$sub("i"), " at different treatment paths of the diagram above"
          )
          )
        ),
        flowLayout(
          numericInput(inputId="mu1", label =HTML( paste0( "Treatment 3 at stage-1 and treatment 3 at stage-2" ) ), value=0),
          numericInput(inputId="mu2", label =HTML( paste0( "Treatment 3 at stage-1 and treatment 4 at stage-2" ) ), value=2),
          numericInput(inputId="mu3", label =HTML( paste0( "Treatment 3 at stage-1 and treatment 5 at stage-2" ) ), value=0),
          numericInput(inputId="mu4", label =HTML( paste0( "Treatment 3 at stage-1 and treatment 6 at stage-2" ) ), value=0),
          numericInput(inputId="mu5", label =HTML( paste0( "Treatment 3 at stage-1 and treatment 7 at stage-2" ) ), value=0),
          numericInput(inputId="mu6", label =HTML( paste0( "Treatment 3 at stage-1 and treatment 3 at stage-2" ) ), value=0),
          numericInput(inputId="mu7", label =HTML( paste0( "Treatment 3 at stage-1 and treatment 4 at stage-2" ) ), value=0),
          numericInput(inputId="mu8", label =HTML( paste0( "Treatment 3 at stage-1 and treatment 5 at stage-2" ) ), value=0),
          numericInput(inputId="mu9", label =HTML( paste0( "Treatment 3 at stage-1 and treatment 7 at stage-2" ) ), value=0),
          numericInput(inputId="mu10", label =HTML( paste0( "Treatment 3 at stage-1 and treatment 8 at stage-2" ) ), value=0)
        ),
        verticalLayout(
          HTML(paste0(
           "The error term &epsilon;", tags$sub("it1"), "follows a skewed t distribution with location parameter 0, scale parameter  &sigma;", tags$sub(1), "skewness parameter &lambda; and degree of freedom &nu;.",
           "Note that &epsilon;", tags$sub("it1"), "follows a normal distribution when &lambda;=0 and &nu; approaches infinity."
          )
          )
        ),
        flowLayout(
          numericInput(inputId="sigma1", label =HTML( paste0( "Scale &sigma;", tags$sub(1) ) ), value=0.95),
          numericInput(inputId="lambda", label =HTML( paste0( "Skewness &lambda;" ) ), value=0),
          numericInput(inputId="nu", label =HTML( paste0( "Degrees of freedom &nu;" ) ), value=1000000)
        ),
        tags$hr(),
        
        h3(HTML("The parameters of the missingness model")),
        verticalLayout(
          HTML(paste0(
            "The error term &epsilon;", tags$sub("it0"), "follows a normal distribution with location parameter 0 and scale parameter &sigma;", tags$sub(0),"."
          )
          )
        ),
        flowLayout(
          numericInput(inputId="sigma0", label =HTML( paste0( "Scale &sigma;", tags$sub(0) ) ), value=1)
        ),
        verticalLayout(
          HTML(paste0(
            "The regression parameters or the proportion of available teeth"
          )
          ),
          
          radioButtons(inputId="missing_choices", label = "Intercept and slope or proportion of available teeth?", 
                       choices=list("Intercept and Slope"="choice_intercept_slope","Proportion"="choice_proportion"),
                       selected = "choice_intercept_slope"),
          
          ### type in the values of a0 and b0 if choosing intercept and slope ###
          conditionalPanel(condition="input.missing_choices=='choice_intercept_slope'",
                           flowLayout(
                             numericInput(inputId="a0", label =HTML( paste0( "Intercept a", tags$sub(0) ) ), value=-1.0),
                             numericInput(inputId="b0", label =HTML( paste0( "Slope b", tags$sub(0) ) ), value=0.5)
                           )
          ),
          
          ### select the values of p_i and c_i if choosing proportion
          conditionalPanel(condition="input.missing_choices=='choice_proportion'",
                          flowLayout(
                            sliderInput(inputId="p_i", label =HTML( paste0( "The proportion of available teeth p", tags$sub( 'i' ) ) ), min=0, max=1, value=0.80),
                            sliderInput(inputId="c_i", label =HTML( paste0( "The association between <b>Y</b> and <b>M</b> c", tags$sub( 'i' ) ) ), min=0, max=1, value=0.42)
                          )
          )
          
        ),
        tags$hr(),
        
      h3(HTML("Regimes")),
      verticalLayout(
        HTML(paste0(
          "A single regime detection; two regimes comparison, one regime is the best among all the eight regimes."
        )
        )
      ),
      flowLayout(
        radioButtons(inputId="regime_choices", label = "Hypothesis test", 
                     choices=list("Single Regime"="choice_single_regime","Two Regimes"="choice_two_regimes", "One Regime versus The Rest"="choice_one_rest"),
                     selected = "choice_single_regime"),
        
        conditionalPanel(condition = "input.regime_choices=='choice_single_regime'",
                         flowLayout(
                           numericInput(inputId="regime", label =HTML( paste0( "Regime" ) ), value=1)
                         )
        ),
        
        conditionalPanel(condition = "input.regime_choices=='choice_two_regimes'",
                         flowLayout(
                           numericInput(inputId="regime1", label =HTML( paste0( "First Regime" ) ), value=1),
                           numericInput(inputId="regime2", label =HTML( paste0( "Second Regime" ) ), value=3)
                         )
        ),
        
        conditionalPanel(condition = "input.regime_choices=='choice_one_rest'",
                         flowLayout(
                           numericInput(inputId="best_regime", label =HTML( paste0( "Best Regime" ) ), value=1)
                         )
        )
        
      ),
      tags$hr(),
      
        h3("The Type One Error Rate and Desire Power"),
        splitLayout(
          sliderInput("a", "Significance Level", min=0, max=0.2, value=0.05),
          sliderInput("power", "Power", min=0.5, max=1, value=0.8)
        ),
        tags$hr(),
        
        h3("Sample Size"),
        verticalLayout(
          #### calculate sample size (action buttons) ####
          actionButton("size","Calculate"),
          uiOutput("result_size")
          
        ),
        tags$hr(),
        
        #### A TOOLTIPS: to give you tips for typeing in numeric inputs. ####
        
        bsTooltip(id = "tau", title = "Input must greater than 0",placement="right", trigger = "focus"),
        bsTooltip(id = "rho", title = "Input must range from 0-1",placement="right", trigger = "focus"),
        bsTooltip(id = "sigma1", title = "Input must greater than 0",placement="right", trigger = "focus"),
        bsTooltip(id = "nu", title = "Input must greater than 0",placement="right", trigger = "focus"),
        bsTooltip(id = "sigma0", title = "Input must greater than 0",placement="right", trigger = "focus"),
      
        bsTooltip(id = "regime", title = "Input must be integer from 1-8",placement="right", trigger = "focus"),  
        bsTooltip(id = "regime1", title = "Input must be integer from 1-8",placement="right", trigger = "focus"),  
        bsTooltip(id = "regime2", title = "Input must be integer from 1-8",placement="right", trigger = "focus"),  
        bsTooltip(id = "best_regime", title = "Input must be integer from 1-8",placement="right", trigger = "focus"),  
      
        collapsable = TRUE,
        footer = HTML("<p style = 'font-size:12px'> Please direct correspondence to kenny.xu@duke-nus.edu.sg</a></p>")
    ),
)
)
