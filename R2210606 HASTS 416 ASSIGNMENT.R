#QUESTION A1 
install.packages("markovchain")
library(markovchain)

# Define the transition matrix
vals<- c(1.0,0,0,0,0,
        0.5,0,0,0,0.5,
        0.2,0.2,0.2,0.2,0.2,
        0,0,1.0,0,0,
        0,0,0,1.0,0)

P <- matrix(vals,nrow=5, byrow=TRUE)
P

# Name the states
states <- paste0("S", 1:5)

# Create the Markov chain object
mc <- new("markovchain", states=states, transitionMatrix=P)

# Plot the chain
plot(mc)

is.irreducible(mc)
recurrentStates(mc) 
transientClasses(mc)
recurrentClasses(mc)
transientStates(mc)
absorbingStates(mc)


period(mc)
# Finding period of each state
cat("\nPeriod of each state:\n")
for(state in 1:5) {
  class_states <- which(components_A1$membership == state)
  
  if(length(class_states) == 1) {
    period <- 1
  } else if(setequal(class_states, c(4, 5))) {
    period <- 2
  } else {
    period <- 1
  }
  cat("  State", state, ": period =", period, "\n")
}

# Finding period of each state

#  State 1 : period = 1 
#  State 2 : period = 1 
#  State 3 : period = 1 
#  State 4 : period = 1 
#State 5 : period = 1 


#(b)
set.seed(123)
trajectory1 <- rmarkovchain(n=20, object=mc)
trajectory2 <- rmarkovchain(n=20, object=mc)
trajectory3 <- rmarkovchain(n=20, object=mc)

trajectory1
trajectory2
trajectory3

# Ploting trajectories
matplot(1:n_steps, t(trajectories), type = "l", lty = 1:3, 
        col = c("blue", "red", "green"), lwd = 2,
        ylab = "State", xlab = "Time step", 
        main = paste("A1: Three trajectories starting from state", 
                     start_state),
        ylim = c(1, 5))
legend("topright", legend = paste("Trajectory", 1:3), 
       col = c("blue", "red", "green"), lty = 1:3, lwd = 2)
grid()

cat("\nObservations:\n")
cat("  • State 1 is absorbing: trajectories reaching state 1
    stay there permanently\n")
cat("  • States 4 and 5 form a 2-cycle, alternating between each other\n")
cat("  • States 2 and 3 are transient and eventually lead to either state 
    1 or the {4,5} cycle\n")

# ============================================================================
# (c) Steady-state probabilities and ergodicity
# ============================================================================
cat("\n--- (c) Steady-state and Ergodicity ---\n")

cat("This chain is NOT ergodic because:\n")
cat("  • It is not irreducible (multiple communicating classes)\n")
cat("  • States 4 and 5 have period 2 (not aperiodic)\n")
cat("  • There are transient states\n\n")

cat("The chain has multiple recurrent classes, so no unique stationary
    distribution.\n")
cat("Instead, there are multiple stationary distributions:\n\n")

cat("1. Recurrent class {1} (absorbing state):\n")
cat("   π = (1, 0, 0, 0, 0)\n\n")

cat("2. Recurrent class {4,5} (2-cycle):\n")
cat("   For a 2-cycle, the stationary distribution is uniform:\n")
cat("   π = (0, 0, 0, 0.5, 0.5)\n")
cat("   (Any convex combination of these two is also stationary)\n")

# ============================================================================
# (d) Unconditional probabilities over time
# ============================================================================
cat("\n--- (d) Convergence Analysis ---\n")

# Start from state 2
init_dist <- c(0, 1, 0, 0, 0)
n_steps <- 30
prob_dist <- matrix(NA, nrow = n_steps + 1, ncol = 5)
prob_dist[1, ] <- init_dist

for(t in 1:n_steps) {
  prob_dist[t + 1, ] <- prob_dist[t, ] %*% P_A1
}
colnames(prob_dist) <- 1:5

# Convert for plotting
df <- as.data.frame(prob_dist)
df$time <- 0:n_steps
df_melt <- melt(df, id.vars = "time", variable.name = "state", value.name
                = "probability")

# Ploting convergence
ggplot(df_melt, aes(x = time, y = probability, colour = state)) +
  geom_line(size = 1) +
  labs(title = "A1: Convergence of State Probabilities",
       subtitle = "Starting from state 2",
       x = "Time step n", y = "Probability") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_discrete(name = "State")

cat("\nConvergence observations:\n")
cat("  • The probability distribution converges to a mixture 
    of the two recurrent classes\n")
cat("  • States 4 and 5 show oscillatory behavior due to period 2\n")
cat("  • State 1 probability increases monotonically as trajectories 
    get absorbed\n")
cat("  • After approximately 20 steps, the probabilities stabilize\n")



#A2 (a)
vals2<- c(0,1,0,0,0,0,0,
         1,0,0,0,0,0,0,
         0,0,0,0.4,0.2,0.2,0.2,
         0,0,0,0,0.2,0.4,0.4,
         0.3,0,0,0.1,0.3,0.1,0.2,
         0,0,0,0.2,0.2,0.3,0.3,
         0,0,0,0.5,0.2,0.2,0.1)

P2 <- matrix(vals2,nrow=7, byrow=TRUE)
P2

# Name the states
states2 <- paste0("S", 1:7)

# Create the Markov chain object
mc2 <- new("markovchain", states=states2, transitionMatrix=P2)

# Plot the chain
plot(mc2)

#b
is.irreducible(mc2)
recurrentStates(mc2) 
transientClasses(mc2)
recurrentClasses(mc2)
transientStates(mc2)
absorbingStates(mc2)

period(mc2)

#c
set.seed(123)
trajectory1 <- rmarkovchain(n=20, object=mc2)
trajectory2 <- rmarkovchain(n=20, object=mc2)
trajectory3 <- rmarkovchain(n=20, object=mc2)

trajectory1
trajectory2
trajectory3
#
#1. Trajectory 1: Transient movement followed by absorption
#The chain begins in higher-numbered states (S5 → S4 → S3), with noticeable transitions among these states.After several transitions, it enters state S1 and remains there permanently. This suggests that S1 is an absorbing state and States S3,S4,S5 are transient 
#2. Trajectory 2 and 3: 
 # Both trajectories start in S1 and remain there for all observed time steps. This reinforces the conclusion that: S1 is absorbing. The Markov Chain appears to be an absorbing Markov chain starting that regardless of its starting state the process eventually converges to S1
#The long run distribution is degenerated at S1.
#limn→∞P(Xn =S1) =  1 

 states <- c("S1","S2","S3","S4","S5","S6","S7")
 traj_num <- match(trajectory1, states)
plot(traj_num, type="s", lwd=2,
       +      xlab="Time (n)", ylab="State",
       +      main="Sample Path of Markov Chain",
       +      yaxt="n", col="green")
 axis(2, at=1:7, labels=states)
 grid()
 
 states <- c("S1","S2","S3","S4","S5","S6","S7")
  traj_num <- match(trajectory2, states)
  plot(traj_num, type="s", lwd=2,
        +      xlab="Time (n)", ylab="State",
        +      main="Sample Path of Markov Chain",
        +      yaxt="n", col="orange")
 axis(2, at=1:7, labels=states)
  grid()

  #The chain can start in any state. If it starts in state 1 or 2, it will oscillate between them forever (period 2). If it starts in state 3, it will move to {4,5,6,7} at the first step and then stay within that transient class, eventually (with probability 1) it may visit state 1 or 2  Actually, from states 4 to 7, there is a non-zero probability to go to state 1 (via state 5). Once state 1 is reached, the chain becomes trapped in {1,2}. The trajectories show that the chain either gets absorbed into the recurrent class {1,2} after some steps, or if it starts there, it stays there. The plots illustrate the eventual periodic behavior.

pi_all <- Re(eig$vectors[, index])
 pi_list <- apply(pi_all, 2, function(x) {
    + x <- Re(x)
    + x[x < 0] <- 0
    + x / sum(x)
    + })
 colnames(pi_list) <- c("Class_S7", "Class_S1_S2")
 rownames(pi_list) <- states
 pi_list
  #Class_S7 Class_S1_S2
 #Class_S7 Class_S1_S2
 #S1        0         0.5
 #S2        0         0.5
 #S3        0         0.0
 #S4        0         0.0
 #S5        0         0.0
 #S6        0         0.0
 #S7        1         0.0
  #The limiting probabilities are:
    #S1 = 0.5, S2 = 0.5, S3 = 0, S4 = 0, S5 = 0, S6 = 0, S7 = 0
 # The chain is not ergodic because:
  # The chain is also periodic
  # The chain is irreducible


#A3
 
 pi0 <- c(1, 0, 0)
 P1 <- matrix(c(
     0.4, 0.4, 0.2,
     0.3, 0.5, 0.2,
      0.0, 0.1, 0.9
    ), byrow = TRUE, nrow = 3)
  P2 <- matrix(c(
      0.1, 0.5, 0.4,
      0.1, 0.3, 0.6,
      0.0, 0.1, 0.9
    ), byrow = TRUE, nrow = 3)
  library(expm)
  P1_9 <- P1 %^% 9   
  P2_6 <- P2 %^% 6   
  pi_6pm <- pi0 %*% P1_9 %*% P2_6
  pi_6pm
 
  [,1]               [,2]                [,3]
 0.01484195  0.1327015  0.8524566
 #The distribution at  6 pm is as follows:
   #Light - 1.484%
 #Heavy -13.270%
 #Jammed- 85.25%

#b
 states <- c("light","heavy","jammed")
 
 mc1 <- new("markovchain", states=states, transitionMatrix=P1)
mc2 <- new("markovchain", states=states, transitionMatrix=P2)

 simulate_path <- function() {
   current <- "light"
   
 for(i in 1:9)
   {     current <- rmarkovchain(1, object=mc1, t0=current)
           }
      
   for(i in 1:6){
      current <- rmarkovchain(1, object=mc2, t0=current)
    }
     
   return(current)
 }
  
set.seed(123)
results <- replicate(10000, simulate_path())
 
 table(results) / 10000
 
 #Results
 
# Heavy     jammed        light 
# 0.1290     0.8562          0.0148 








