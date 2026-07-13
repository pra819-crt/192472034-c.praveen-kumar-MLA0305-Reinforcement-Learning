#=========================================================
# MARKOV DECISION PROCESS (MDP) SIMULATION IN R
#=========================================================

library(DiagrammeR)
library(knitr)

cat("\n=========================================\n")
cat(" MARKOV DECISION PROCESS (MDP)\n")
cat("=========================================\n")

#---------------------------------------------------------
# States and Actions
#---------------------------------------------------------

states <- c("S1","S2","S3")
actions <- c("A1","A2")

cat("\nNumber of States :", length(states))
cat("\nStates :", paste(states, collapse=", "))

cat("\n\nNumber of Actions :", length(actions))
cat("\nActions :", paste(actions, collapse=", "))

#---------------------------------------------------------
# Transition Probability Matrices
#---------------------------------------------------------

A1 <- matrix(c(
  0,0.6,0.2,
  0.7,0,0.5,
  0.9,0.4,0
), nrow=3, byrow=TRUE)

rownames(A1)=states
colnames(A1)=states

A2 <- matrix(c(
  0,0.4,0.8,
  0.3,0,0.5,
  0.1,0.6,0
), nrow=3, byrow=TRUE)

rownames(A2)=states
colnames(A2)=states

cat("\n\n====================================")
cat("\nTransition Probability Matrix (A1)")
cat("\n====================================\n")
print(A1)

cat("\n====================================")
cat("\nTransition Probability Matrix (A2)")
cat("\n====================================\n")
print(A2)

#---------------------------------------------------------
# Reward Table
#---------------------------------------------------------

reward_table <- data.frame(
  
  Current_State=c(
    "S1","S1",
    "S1","S1",
    "S2","S2",
    "S2","S2",
    "S3","S3",
    "S3","S3"),
  
  Action=c(
    "A1","A2",
    "A1","A2",
    "A1","A2",
    "A1","A2",
    "A1","A2",
    "A1","A2"),
  
  Next_State=c(
    "S2","S2",
    "S3","S3",
    "S1","S1",
    "S3","S3",
    "S1","S1",
    "S2","S2"),
  
  Reward=c(
    5,10,
    -1,-5,
    3,7,
    2,1,
    4,6,
    0,-2)
)

cat("\n====================================")
cat("\nReward Matrix")
cat("\n====================================\n")

print(kable(reward_table))

#---------------------------------------------------------
# Expected Immediate Reward
#---------------------------------------------------------

cat("\n====================================")
cat("\nExpected Immediate Reward")
cat("\n====================================\n")

summary_table <- data.frame()

for(s in states){
  
  cat("\n----------------------------------")
  cat("\nState :",s)
  
  for(a in actions){
    
    rows <- reward_table[
      reward_table$Current_State==s &
        reward_table$Action==a,]
    
    reward <- rows$Reward
    
    nextstate <- rows$Next_State
    
    expected <- 0
    
    cat("\n\nAction :",a,"\n")
    
    for(i in 1:length(reward)){
      
      if(a=="A1")
        prob <- A1[s,nextstate[i]]
      else
        prob <- A2[s,nextstate[i]]
      
      calc <- prob*reward[i]
      
      expected <- expected+calc
      
      cat(
        "P(",nextstate[i],"|",s,",",a,") = ",
        prob,
        " × Reward = ",
        reward[i],
        " = ",
        calc,"\n")
    }
    
    cat("Expected Immediate Reward =",expected,"\n")
    
    summary_table <- rbind(summary_table,
                           data.frame(
                             State=s,
                             Action=a,
                             ExpectedReward=round(expected,2)
                           ))
  }
}

#---------------------------------------------------------
# Summary Table
#---------------------------------------------------------

cat("\n====================================")
cat("\nOUTPUT SUMMARY")
cat("\n====================================\n")

print(kable(summary_table))

#---------------------------------------------------------
# Visualization
#---------------------------------------------------------

grViz("

digraph MDP {

graph [
layout = dot
rankdir = LR
]

node[
shape=circle
style=filled
fontcolor=white
fontsize=18
]

S1 [fillcolor=red]
S2 [fillcolor=blue]
S3 [fillcolor=green]

S1 -> S2 [label='A1 : 0.6',color='red',penwidth=2]
S1 -> S3 [label='A1 : 0.2',color='red',penwidth=2]

S1 -> S2 [label='A2 : 0.4',color='blue',style=dashed]
S1 -> S3 [label='A2 : 0.8',color='blue',style=dashed]

S2 -> S1 [label='A1 : 0.7',color='red',penwidth=2]
S2 -> S3 [label='A1 : 0.5',color='red',penwidth=2]

S2 -> S1 [label='A2 : 0.3',color='blue',style=dashed]
S2 -> S3 [label='A2 : 0.5',color='blue',style=dashed]

S3 -> S1 [label='A1 : 0.9',color='red',penwidth=2]
S3 -> S2 [label='A1 : 0.4',color='red',penwidth=2]

S3 -> S1 [label='A2 : 0.1',color='blue',style=dashed]
S3 -> S2 [label='A2 : 0.6',color='blue',style=dashed]

}
")
