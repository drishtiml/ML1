


test <- read.csv("PlotDataLinear.csv",header = TRUE)

test$ID<-seq.int(nrow(test))

ggplot(test, aes(x = test$ID)) + 
  geom_bar(stat = "identity", aes(y = test$num_attempts, color = "num_attempts")) +
  geom_bar(stat = "identity", aes(y = test$pd1se, color = "pd1se")) +
  geom_bar(stat = "identity", aes(y = test$pdmin, color = "pdmin")) +
  ylab("Training Error") +
  xlab("Number Of Training Example") +
  ggtitle("Actual vs Predicted")

