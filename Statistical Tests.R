#### Test normal distribution of abnormal returns ####
# Analytical tests for normality
jarque.test(as.vector(t(Abnormal_Returns_All[2])))
shapiro.test(as.vector(t(Abnormal_Returns_All[2])))

# Graphical tests for normality
par(mfrow=c(1,2))
hist(as.vector(t(Abnormal_Returns_All[2])), main="Histogram of Abnormal Returns", xlab="Abnormal Returns", col="lightblue", border="black")
qqnorm(as.vector(t(Abnormal_Returns_All[2])), main="Q-Q Plot of Abnormal Returns")
qqline(as.vector(t(Abnormal_Returns_All[2])), col="red")
par(mfrow=c(1,1))

