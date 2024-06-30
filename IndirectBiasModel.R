
# Updated: June 24, 2024

# R script for the agent-based model simulation included in Box 1 of the following publication:

# Cruz y Celis Peniche, P. (2024). Are there any true formal models of success- and prestige-biased social learning? 
# Journal of Cognition and Culture Vol. 24. doi:10.1163/15685373-12340185

# Author correspondence: pcruzycelis@ucdavis.edu

### Load required libraries ###

library(ggplot2)
library(RColorBrewer)

### Model description ###

# A modified indirect bias model, introducing a variable to account for the attractiveness of a model (i.e., a success cue). 

# In this model, payoffs and success cues can still be perfectly correlated, but models are chosen based on the attractiveness of the 
# correlated trait (payoffs are not visible).

# A learner's payoffs are still identical to the model's, contingent on the variant learned.

# This model was adapted from Mesoudi, Alex (2021). Simulation models of cultural evolution in R. 
# doi:10.5281/zenodo.5155821. Available at https://github.com/amesoudi/cultural_evolution_ABM_tutorial and 
# https://bookdown.org/amesoudi/ABMtutorial_bookdown/.

### The agent-based model ###

# We're trying to simulate a situation in which the most attractive version of an arbitrary (attractive) trait 
# may not be perfectly correlated with the most adaptive version of a functional trait.

# Parameter description

N <- 10000 # Sample size
p_0 <- 0.5 # Initial probability of learner acquiring the adaptive (high-payoff) variant of the payoff-yielding trait (trait 1)
L <- 0.75 # Probability that if a learner has the high-payoff variant of trait 1 they also have the attractive variant of trait 2
s <- 0.1 # Initial difference in values between the more adaptive/attractive variant of a trait
t_max <- 300 # Number of generations
r_max <- 15 # Number of runs

IndirectBias <- function (N, s, L, p_0, t_max, r_max) {
    
# 1. Create matrices with t_max rows and r_max columns, fill with NAs, convert to dataframe
  output_trait1 <- as.data.frame(matrix(NA, t_max, r_max))  
  output_trait2 <- as.data.frame(matrix(NA, t_max, r_max)) 
  
    # 1.1 purely cosmetic: rename the columns with run1, run2 etc.
      names(output_trait1) <- paste("run", 1:r_max, sep="")  
      names(output_trait2) <- paste("run", 1:r_max, sep="")  
  
# 2. Setting up the first generation  
  for (r in 1:r_max) {
    
    # 2.1. Create first generation
    agent <- data.frame(trait1 = sample(c("A","B"), N, replace = TRUE, 
                                        prob = c(p_0,1-p_0)),
                        trait2 = rep(NA, N))  
    
    # 2.2. With prob L, variant A is tied to variant X, and variant B to variant Y
   
    prob <- runif(N)
    agent$trait2[agent$trait1 == "A" & prob < L] <- "X"
    agent$trait2[agent$trait1 == "B" & prob < L] <- "Y"
    
    # table(agent$trait1, agent$trait2) 
    # There should be 1-L missing values for trait 2
    
    # 2.3. With probability 1-L, variant A is tied to variant Y, and variant B to variant X
    agent$trait2[agent$trait1 == "A" & prob >= L] <- "Y"
    agent$trait2[agent$trait1 == "B" & prob >= L] <- "X"
    
    #table(agent$trait1, agent$trait2)
    #If L = 0.5 then they should be almost equally distributed (under p_0 = 0.5)
    #If L is greater than 0.5 then X and A will appear positively correlated (under p_0 = 0.5)
  
    # 2.4. Add payoffs [for trait 1]
    
    agent$payoff[agent$trait1 == "A"] <- 1 + s
    agent$payoff[agent$trait1 == "B"] <- 1
    
    agent$payoff <- as.numeric(agent$payoff)
    
    # 2.5. Add attractiveness [for trait 2]
    agent$attract[agent$trait2 == "X"] <- 1 + s
    
    #Perfectly correlated with higher payoff trait
    agent$attract[agent$trait2 == "Y"] <- 1
    agent$attract <- as.numeric(agent$attract)
    
    #table(agent$payoff, agent$attract)
    #table(agent$trait1, agent$trait2)
    
    # 2.6. Add up frequencies of each trait variant for plotting
    
    sum(agent$trait1 == "A") #Check to see total sum of A before reproducing

    sum(agent$trait2 == "X") #Check to see total sum of X before reproducing
    
    # Add first generation's mean frequencies of each trait variant to first row of column r
    output_trait1[1,r] <- sum(agent$trait1 == "A") / N  
    output_trait2[1,r] <- sum(agent$trait2 == "X") / N 
    
# 3. Setting up the recursion    
    for (t in 2:t_max) {
      
      # 3.1. Copy agent to previous_agent dataframe
      previous_agent <- agent
      
      # 3.2. Get relative attractiveness of previous agents
      relative_attract <- previous_agent$attract / sum(previous_agent$attract)

      # 3.3. New traits copied from previous generation, biased by attraction
      demonstrators <- sample(1:N,
                              N, replace = TRUE,
                              prob = relative_attract)
      
      agent$trait1 <- previous_agent$trait1[demonstrators]
      agent$trait2 <- previous_agent$trait2[demonstrators]
      
      # 3.4. Add attractiveness [for trait 2]
      # Agents acquire the values of the trait they will be imitated on
      agent$attract[agent$trait2 == "X"] <- 1 + s
      agent$attract[agent$trait2 == "Y"] <- 1
      agent$attract <- as.numeric(agent$attract)
      
      # 3.5. Add payoffs
      # Agents acquire payoffs based on the functional trait they inherited. 
      # These payoffs will be unknown to a learner
      agent$payoff[agent$trait1 == "A"] <- 1 + s
      agent$payoff[agent$trait1 == "B"] <- 1
      agent$payoff <- as.numeric(agent$payoff)
      
      # Notice we are assuming transmission is faithful. If a model has the higher-payoff variant of trait 1,
      # then they will reap proportional benefits as the demonstrator they copied.
      
      # 3.6. Add the current mean frequencies of each trait variant into output slot for this generation t and run r for plotting
      output_trait1[t,r] <- sum(agent$trait1 == "A") / N  
      output_trait2[t,r] <- sum(agent$trait2 == "X") / N  
      
    }
    
  }

# 4. Aesthetic and functional parameters for plots
  
  # first plot a thick orange line for the mean of trait 1 variant A
  plot(rowMeans(output_trait1), 
       type = 'l', 
       ylab = "Proportion of agents", 
       xlab = "Generation", 
       ylim = c(0,1), 
       lwd = 3, 
       main = paste("P = ", p_0, ",     L = ", L, sep = ""),
       col = "orange")
  
  # now thick blue line for mean of trait 2 variant X 
  lines(rowMeans(output_trait2), col = "royalblue", lwd = 3)
  
  for (r in 1:r_max) {  
    
    # add lines for each run, up to r_max
    lines(output_trait1[,r], type = 'l', col = "orange", lwd = 1, lty = "dotted")  
    lines(output_trait2[,r], type = 'l', col = "royalblue", lwd = 1, lty = "dotted") 
    
  }
  
  legend("right",
         legend = c("Attractive variant", "Adaptive variant"),
         lty = 1,
         lwd = 3,
         col = c("royalblue", "orange"),
         bty = "n")

  list(output_trait1, output_trait2) # export data from function
  return(rowMeans(output_trait1)[t_max]) # export mean frequency of attractive trait at the final generation for plotting
}


### Explore some possible scenarios below ###

# When the success cue is perfectly correlated with the adaptive trait, both reach fixation (not plotted)

data_model <- IndirectBias(N = 10000, 
                              s = 0.1, # Relative advantage of trait A over B
                              L = 0.4, # Correlation between trait 1 and 2
                              p_0 = 0.9, # Initial proportion of trait 1 version A
                              t_max = 300, # Number of generations
                              r_max = 15) # Number of runs

# A slight decrease in the correlation (0.01) minimally reduces the frequency of the adaptive trait, 
# and it is still capable of reaching fixation in a majority of its runs
data_model_A <- IndirectBias(N = 10000, 
                              s = 0.1, #relative advantage of trait A over B
                              L = 0.99, #Correlation between trait 1 and 2
                              p_0 = 0.5, #Initial proportion of trait 1 version A
                              t_max = 300, # Number of generations
                              r_max = 15) # Number of runs

# However, if the initial frequency of the adaptive variant is low, it won't spread to the majority of the
# population, despite being an almost perfect correlation.
data_model_B <- IndirectBias(N = 10000, 
                             s = 0.1, #relative advantage of trait A over B
                             L = 0.99, #Correlation between trait 1 and 2
                             p_0 = 0.1, #Initial proportion of trait 1 version A
                             t_max = 300, # Number of generations
                             r_max = 15) # Number of runs

# With a correlation of 0.75, the adaptive trait will only mildly increase beyond it's initial frequency, 
# regardless of whether this is 10% or 50%
data_model_C <- IndirectBias(N = 10000, 
                            s = 0.1, # Relative advantage of trait A over B
                            L = 0.75, # Correlation between trait 1 and 2
                            p_0 = 0.5,# Initial proportion of trait 1 version A
                            t_max = 300, # Number of generations
                            r_max = 15) # Number of runs

data_model_D <- IndirectBias(N = 10000, 
                             s = 0.1, # Relative advantage of trait A over B
                             L = 0.75, # Correlation between trait 1 and 2
                             p_0 = 0.1,# Initial proportion of trait 1 version A
                             t_max = 300, # Number of generations
                             r_max = 15) # Number of runs

## Considering other scenarios ##

# Increasing the relative advantage of trait A over B only accelerates this divergence. 

data_model_E <- IndirectBias(N = 10000, 
                               s = 0.5, #relative advantage of trait A over B
                               L = 0.75, #Correlation between trait 1 and 2
                               p_0 = 0.25, #Initial proportion of trait 1 version A
                               t_max = 300, # Number of generations
                               r_max = 15) # Number of runs

# Inversely, if we eliminate the attractiveness of trait X over Y, then both traits drift neutrally and 
# their correlation becomes irrelevant (same output regardless of L). I use the same term to operationalize
# the relative advantage of A over B, but we would observe the same output regardless, since transmission
# is biased only on the attractive trait (trait 2).

data_model_F <- IndirectBias(N = 10000, 
                            s = 0, # Relative advantage of trait A over B
                            L = 0.95, # Correlation between trait 1 and 2
                            p_0 = 0.5, # Initial proportion of trait 1 version A
                            t_max = 300, # Number of generations
                            r_max = 15) # Number of runs

# Even when observing a model's success cue means that they posses an adaptive trait 95% of the times, 
# the adaptive trait will struggle to increase. If less than 5% of the population initially have this variant, 
# it will fail to reach the majority most of the times.

data_model_G <- IndirectBias(N = 10000, 
                             s = 0.1, #relative advantage of trait A over B
                             L = 0.75, #Correlation between trait 1 and 2
                             p_0 = 0.1,#Initial proportion of trait 1 version A
                             t_max = 300, # Number of generations
                             r_max = 15) # Number of runs

### Plotting a heatplot for variant A ###

# Declare empty the matrix

correlation <- seq(0,1,.1)
initial.freq <- seq(0,1,.1)

final.freq <- matrix(data=NA, nrow=length(correlation), ncol=length(initial.freq))

rownames(final.freq) <- correlation
colnames(final.freq) <- initial.freq

# Run through each combination of values and assign final frequency to the results matrix (takes some time)

row.n <- 1 # to keep track of row position in matrix

for (c in correlation) {
  col.n <- 1 # to keep track of col position in matrix
  for (i in initial.freq) {
    final.freq[row.n,col.n] <- 
      IndirectBias(N = 10000, 
                   s = 0.1, # Relative advantage of trait A over B
                   L = c, # Correlation between trait 1 and 2
                   p_0 = i, # Initial proportion of trait 1 version A
                   t_max = 300, # Number of generations
                   r_max = 15) # Number of runs
    col.n <- col.n + 1
  }
  row.n <-row.n + 1
}

ff.df <- as.data.frame(final.freq)
ff.df.2 <- ff.df %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)

# Plot results

HP <- ggplot(ff.df.2, aes(x = rowname,y = colname, fill = value)) + 
  geom_tile() +
  xlab("Correlation between the most adaptive and attractive variants") +
  ylab("Initial frequency of the adaptive variant") +
  ggtitle("Final frequencies of the most adaptive variant as function of \ 
      its initial frequency and correlation with the most attractive variant") +
  #scale_colour_gradientn(colours = rainbow(10)) +
  scale_fill_continuous(name = "Final frequencies", type = "viridis", direction = -1) +
  ggtitle("Final frequencies (after 300 generations) of the most adaptive variant")

