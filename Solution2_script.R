library(dplyr)
library(aod)
library(ggplot2)

# Second model: majority vote to predict
# Paola Elefante, paolaelefante.com@gmail.com

# Database
BDS_df <- read.csv("/Users/elefante/Documents/R_programming/Dataset.csv")
natarget_df <- subset(BDS_df,subset=is.na(BDS_df$target1)) # elements with no target1
yestarget_df <- subset(BDS_df,subset=(BDS_df$target1==1)|(BDS_df$target1==0))  # elements with target1 defined (0,1)

# Isolate names of points of interest
v<-colnames(yestarget_df)
v2 <-v[2:(length(v)-1)]

# Parameters
n_iter <- 80
n_sample <-100

# Initialise matrix to collect predictions for all probit models
prediction <- matrix(data=NA, nrow=n_iter, ncol=nrow(natarget_df))

# Here we will store the names of the variables used in all probit models
newnames<-matrix(data=NA,nrow = n_iter,ncol=n_sample)

#### Diagnostics stuff ####
n_sample2 <-50
check_df <- yestarget_df[sample(c(1:nrow(yestarget_df)),n_sample2),]
real_values<-check_df$target1
check_df$target1 <- NA
prediction_2 <- matrix(data=NA, nrow=n_iter, ncol=nrow(check_df))
################

# Generate probit models and store their predictions for natarget_df
for(i in 1:n_iter)
{
  newnames[i,] <- sample(v2,n_sample) # sample n_sample random points of interest
  my_formula <- as.formula(paste("yestarget_df$target1 ~ ",paste(newnames[i,],collapse="+"))) # create probit linear formula
  # Apply glm
  myprobit <- glm(my_formula, family=binomial(link="probit"),data=yestarget_df,control = list(maxit = 50))
  prediction[i,]<-round(predict.lm(myprobit, natarget_df,type="response")) # store prediction
  prediction_2[i,]<-round(predict.lm(myprobit, check_df,type="response")) # store prediction for diagnostics
  
}
#Change zero values in -1 (for majority vote later)
prediction[prediction==0] <- -1
prediction_2[prediction_2==0] <- -1 # diagnostics
final_pred <- vector(mode="numeric",length = length(natarget_df$target1))
final_pred_2 <- vector(mode="numeric",length = length(check_df$target1)) #diagnostics

# Decide prediction by majority vote
for (iii in 1:length(final_pred))
{
  final_pred[iii]=sign(sum(prediction[,iii]))
}
### diagnostics stuff ####
for (iii in 1:length(final_pred_2))
{
  final_pred_2[iii]=sign(sum(prediction_2[,iii]))
}
###############

# Change -1 values back to 0
final_pred[(final_pred==-1)]<-0
natarget_df$target1 <- final_pred # Final prediction
final_pred_2[(final_pred_2==-1)]<-0
guess <- sum(final_pred_2==real_values)
guess*100/n_sample2



full_df <-rbind(yestarget_df,natarget_df)

write.csv(full_df,file="Dataset_pred.csv")
