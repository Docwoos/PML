
# calculate correlation matrix
correlationMatrix <- cor(workout3[,-33])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

help(cor)

val<-.75
iter<-length(workout3)-1
out <- matrix(NA, nrow=iter)
for (i in 1:iter ) {
    out[i]<-abs(cor(workout3[,29],workout3[,i]))
}
out>=val & out<1


# ensure the results are repeatable
set.seed(7)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(workout4[,1:32], workout4[,33], sizes=c(1:10), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
