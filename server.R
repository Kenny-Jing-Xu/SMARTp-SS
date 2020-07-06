#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("sn")

library(shiny)
library(mvtnorm)
library(sn)
source("SampleSizeSMARTp.R")

# Define server logic required to calculate sample size for SMARTp
shinyServer(
    function(input, output) {

      #Upload the experimental design diagram
      output$Image <- renderImage({
        filename <- normalizePath(file.path('./www',
                                            paste('Figure2_SMARTp.png')))
        
        # Return a list containining the filename
        list(src = filename,  width = 800, height = 400)
        
      }, deleteFile = FALSE)
      
      
      Result_size <- eventReactive(input$size, 
      {  ### Generate this current result of sample size if the corresponding action button is pressed
            
        m <- 28
        cutoff <- 0
        g1 = input$g1
        g2 = input$g2
        
        st1 <- cbind( c(1, 1), 
                      c(4, 4), 
                      c(g1, g2), 
                      1:2
                      )
        
        dtr <- cbind(1:8, 
                     c( rep( 1, 4 ), rep( 6, 4 ) ), 
                     c( 2, 3, 4, 5, 7, 8, 9, 10 ), 
                     c( rep( 1, 4 ), rep( 2, 4 ) )
                     )
        
        mu_sim <- matrix(0, 10, m)
        mu_sim[1,] <- rep(input$mu1, m) 
        mu_sim[2,] <- rep(input$mu2, m) 
        mu_sim[3,] <- rep(input$mu3, m) 
        mu_sim[4,] <- rep(input$mu4, m) 
        mu_sim[5,] <- rep(input$mu5, m) 
        mu_sim[6,] <- rep(input$mu6, m) 
        mu_sim[7,] <- rep(input$mu7, m) 
        mu_sim[8,] <- rep(input$mu8, m) 
        mu_sim[9,] <- rep(input$mu9, m) 
        mu_sim[10,] <- rep(input$mu10, m) 
        
        tau = input$tau
        if( tau > 0 ){ tau <- tau }
        else{ stop( "Error: Please specify the variation parameter of the spatial model greater than 0" ) }
        
        rho = input$rho
        if( rho >=0 & rho <=1 ){ rho =rho }
        else{ stop( "Error: Please specify the association parameter of the spatial model within the range from 0-1" ) }
        
        sigma1 = input$sigma1
        if( sigma1 > 0 ){ sigma1 <- sigma1 }
        else{ stop( "Error: Please specify the scale parameter of the error term greater than 0" ) }
        
        lambda <- input$lambda
        
        nu = input$nu
        if( nu > 0 ){ nu <- nu }
        else{ stop( "Error: Please specify the degrees of freedom of the error term for CAL model greater than 0" ) }
        
        sigma0 = input$sigma0
        if( sigma0 > 0 ){ sigma0 <- sigma0 }
        else{ stop( "Error: Please specify the scale parameter of the error term for missingness model greater than 0" ) }
  
        regime_choices <- input$regime_choices
        if( regime_choices == "choice_single_regime" ){
          regime <- c( input$regime )
        }
        else if( regime_choices == "choice_two_regimes" ){
          regime <- c( input$regime1, input$regime2 )
        }
        else{
          best_regime <- input$best_regime
          regime <- c( best_regime, c(1:8)[ !c(1:8)==best_regime ] )
        }
        
        a = input$a
        pow = input$power
        
        teeth_miss = input$missing_choices
        if( teeth_miss == "choice_intercept_slope" ){
          a0 <-  input$a0
          b0 <-  input$b0
          SampleSize <- SampleSize_SMARTp(mu = mu_sim, 
                                          st1 = st1, dtr = dtr,
                                          regime =regime, 
                                          pow = pow, a = a,
                                          rho = rho, tau = tau,
                                          sigma1 = sigma1, lambda = lambda, nu = nu, sigma0 = sigma0,
                                          Num = 1e+6, 
                                          p_i = , c_i = , a0 = a0, b0 = b0,
                                          cutoff = 0
                                          )
          N <- ceiling(SampleSize$N)
          
        }
        else{
          p_i <- input$p_i
          c_i <- input$c_i
          SampleSize <- SampleSize_SMARTp(mu = mu_sim, 
                                          st1 = st1, dtr = dtr,
                                          regime =regime, 
                                          pow = pow, a = a,
                                          rho = rho, tau = tau,
                                          sigma1 = sigma1, lambda = lambda, nu = nu, sigma0 = sigma0,
                                          Num = 1e+6, 
                                          p_i = p_i, c_i = c_i, a0 = , b0 = ,
                                          cutoff = 0
          )
          N <- ceiling(SampleSize$N)
        
        }
  
        HTML(paste("<h4 style = 'color:blue';> 
                     The required sample size is ", N, "to attain", pow*100,"% power when the significance level is",a,".")) 
        
      }
      )
      
      output$result_size <- renderUI(
        {
          Result_size()
        }
      )
}
)
