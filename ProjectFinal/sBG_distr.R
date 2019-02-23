

require(plotly)
require(dplyr)

# PARAMETERS --------------------------------------------------------------
DATA_FOLDER <- "~/Google Drive/CMP Data Analysis/BayesProjectMaster/" # Set here folder of the data files
REBILL_CYCLES_FOR_PARAM_ESTIMATION <- 12 # Set here 12 if you want to use all the data for parameter estimation
FILENAME_PROD <- "FirstProduct.rds" # Choose here between the two files provided

max_alpha = 3
max_beta = 2

# TOY DATA DEFINITIONS FROM THE PAPER (DEBUGGING) -------------------------------------
if(FALSE){
        survivors = c(1000, 869, 743, 653, 593, 551, 517, 491)
        survivors
        
        n_lost = function(data){
                lost = 0
                for(i in 1:(length(data)-1)){
                        lost = c(lost, data[i]-data[i+1])
                }
                return(lost)
        }
        
        died = n_lost(survivors)
        died
}

# First product data definitions -----------------------------------------------------
if(TRUE){
        data_compl <- readRDS(file = paste0(DATA_FOLDER, FILENAME_PROD))
        survivors_compl <- data_compl$Count
        died_compl <- -c(0,diff(data_compl$Count))
        T_max_compl <- nrow(data_compl)-1
        
        data <- data_compl[1:(REBILL_CYCLES_FOR_PARAM_ESTIMATION+1),]
        survivors <- data$Count
        died <- -c(0,diff(data$Count))
        T_max <- nrow(data)-1
}
paste("Below is the complete first dataset, which is later used for comparison:") 
data_compl
paste("Below is the first dataset partly, which is used for parameter estimation i.e. prediction:")
data

# DEFINE FUNCTIONS FROM THE PAPER ----------------------------------------------
if(TRUE){
        ### This function corresponds to the recursive definition of the 
        ### P(T = t) function of the paper, equation 7
        P_recursive <- function(N, alpha, beta, t, t_max){
                output <- array(dim=c(t_max,N))
                output[1,] <- alpha/(alpha + beta)
                
                if(t_max > 1){
                        for(i in 2:t_max){
                                output[i,] <- (beta + i - 2)/(alpha + beta + i - 1)*output[i-1,]
                        }
                }
                output[t,]
        }
        
        ### This function corresponds to the recursive definition of the 
        ### S(T = t) function of the paper, which is basically S(T = t) = 1 - P(T = t)
        S_recursive <- function(N, alpha, beta, t, t_max){
                output <- array(dim=c(t_max,N))
                output[1,] <- 1 - P_recursive(N = N, alpha = alpha, beta = beta, 1, t_max = t_max)
                
                if(t_max > 1){
                        for(i in 2:t_max){
                                output[i,] <- output[i-1,] - P_recursive(N = N, alpha = alpha, beta = beta, i, t_max = t_max)
                        }
                }
                output[t,]
        }
        
        ### This is the log likelihood as given in appendix B of the paper, equation B2
        log_likelihood_recursive <- function(alpha, beta, N = length(alpha0)*length(beta0), t_max = T_max){
                died = died[-1]
                output = (survivors[1] - sum(died))*log(S_recursive(N = N, alpha = alpha, beta = beta, t = t_max, t_max = t_max))
                for(i in 1:t_max){
                        output = output + died[i] * log(P_recursive(N = N, alpha = alpha, beta = beta, t = i, t_max = t_max))
                }
                return(output)
        }
        
        ### This is equation 6 of the paper, describing the survival rate
        ### with beta functions
        survival_rate <- function(t, alpha, beta){
                beta(alpha, beta + t)/beta(alpha, beta)
        }
}

# PLOT OUTPUT OF RECURSIVE FUNCTIONS FOR TOY DATA (DEBUGGING) -------------------------------------------
if(FALSE){
        u <- c(1:7)
        alpha0 <- 1
        beta0 <- 1
        plot_ly(x = ~u,
                y = ~P_recursive(N = length(alpha0)*length(beta0), alpha0, beta0, u, t_max = 7),
                type = "scatter",
                mode = "lines")
        
        plot_ly(x = ~u,
                y = ~S_recursive(N = length(alpha0)*length(beta0), alpha0, beta0, u, t_max = 7),
                type = "scatter",
                mode = "lines")
        
        plot_ly(x = ~u,
                y = ~log_likelihood_recursive(alpha0, beta0, t_max = 7),
                type = "scatter",
                mode = "lines")
}

# Get optimal alpha and beta by maximum likelihood -----------------------------------------------------------------
if(TRUE){
        ### Maxmimize the log likelihood by running over a grid from 0.01 to 3
        ### both for alpha and beta 
        alpha0=seq(0.01, max_alpha, length = 400)
        beta0=seq(0.01, max_beta, length = 400)
        maximize_llh <- outer(alpha0, beta0, FUN=log_likelihood_recursive)
        
        ### Get indices of maximum 
        row <- which(maximize_llh == max(maximize_llh), arr.ind = TRUE)[1]
        col <- which(maximize_llh == max(maximize_llh), arr.ind = TRUE)[2]
        
        ### Plot log likelihood function
        p <- plot_ly(x = ~alpha0,
                     y = ~beta0,
                     z = ~maximize_llh) %>% 
                add_surface() %>% 
                layout(title = "Log likelihood"
                       )
}
paste("The maximum value for the log likelihood is:", maximize_llh[row,col])
paste("This is reached when alpha is:", alpha0[row])
paste("This is reached when beta is:", beta0[col])
p

# PLOT Result ----------------------------------------------------------
if(TRUE){
        t <- c(0:T_max_compl)
        alpha <- alpha0[row]
        beta <- beta0[col]
        
        p <- plot_ly(x = ~t) %>%
                add_trace(y = ~survivors_compl/survivors_compl[1], 
                          type = 'bar', 
                          name = 'Data') %>%
                add_trace(y = ~survival_rate(t,alpha,beta),
                          type = 'scatter',
                          mode = 'lines',
                          name = paste0("Fit with a = ", round(alpha,2), ", b = ", round(beta,2))
                          ) %>%
                layout(legend = list(x = 0.3, y = 0.8),
                       yaxis = list(title = "Survival probability"),
                       xaxis = list(title = "rebill cycle"),
                       title = paste("Parameter estimation with",
                                     REBILL_CYCLES_FOR_PARAM_ESTIMATION,
                                     "rebill cycles"))
}
p

# ERROR ESTIMATION --------------------------------------------------------
if(TRUE){
        diff <- survival_rate(t,alpha,beta) - survivors_compl/survivors_compl[1]
        diff_perc <- 100*diff/(survivors_compl/survivors_compl[1])
        rms <- 1/length(diff)*sum(diff * diff)
        errors <- data.frame(t, round(diff_perc,2))
        colnames(errors) <- c("Rebill cycle", "Diff (%)")
}
rms
errors

