library(dplyr)
library(aod)
library(ggplot2)
# First model: AIC value to pick the best model
# Paola Elefante, paolaelefante.com@gmail.com

BDS_df <- read.csv("/Users/elefante/Documents/R_programming/Dataset.csv")

natarget_df <- subset(BDS_df,subset=is.na(BDS_df$target1)) # elements with no target1
yestarget_df <- subset(BDS_df,subset=(BDS_df$target1==1)|(BDS_df$target1==0)) # elements with target1 defined (0,1)
## abt 30% are NA, the others have or have not a target

# Isolate names of points of interest
v<-colnames(yestarget_df)
v2 <-v[2:(length(v)-1)]

#Parameters
n_iter <- 80
n_sample <-100
scores <- vector(mode="numeric",length=n_iter) # vector for AIC scores

# Here we will store the names of the variables used in all probit models
newnames<-matrix(data=NA,nrow = n_iter,ncol=n_sample)
for(i in 1:n_iter)
{
  newnames[i,] <- sample(v2,n_sample) # sample n_sample random points of interest
  my_formula <- as.formula(paste("yestarget_df$target1 ~ ",paste(newnames[i,],collapse="+")))
  # Apply glm
  myprobit <- glm(my_formula, family=binomial(link="probit"),data=yestarget_df,control = list(maxit = 50))
  scores[i]<-AIC(myprobit)
}
# Pick the model with lowest AIC score and use it to predict values for natarget_df$target1
rightindex <- which(scores==min(scores))
my_formula <- as.formula(paste("yestarget_df$target1 ~ ",paste(newnames[rightindex,],collapse="+")))
myprobit <- glm(my_formula, family=binomial(link="probit"),data=yestarget_df,control = list(maxit = 50))
prediction<-round(predict.lm(myprobit, natarget_df,type="response"))
natarget_df$target1<-prediction
write.csv(natarget_df,file="NAtarget_pred.csv")

# Diagnostics
n_iter2 <- 100
n_sample2 <-100
guess <- vector(mode="numeric",length=n_iter2)
for (k in 1:n_iter2)
  {
check_df <- yestarget_df[sample(c(1:nrow(yestarget_df)),n_sample2),]
real_values<-check_df$target1
check_df$target1 <- NA

prediction_check<-round(predict.lm(myprobit, check_df,type="response"))
compare_values <- real_values==prediction_check
guess[k]<-sum(compare_values==TRUE)
}
mean(guess)*100/n_sample2



