bp_data <- read.csv("blood pressure.csv")
summary(bp_data)

#make factors

bp_data$Participant <- factor(bp_data$Participant)
bp_data$Drug <- factor(bp_data$Drug, levels = c("None", "Old", "New"))
bp_data$Time <- factor(bp_data$Time)


#Visuals
par(mfrow =c(1,2))

boxplot(Blood.Pressure ~ Participant, data = bp_data, las = 1, cex.axis= 0.7)
boxplot(Blood.Pressure ~ Drug, data = bp_data)
boxplot(Blood.Pressure ~ Drug:Time, data = bp_data, las = 1, cex.axis= 0.9)
boxplot(Blood.Pressure ~ Time, data = bp_data, las = 1, cex.axis= 0.9)

#Fitting full fixed model for assumptions 

bp_aov <- aov(Blood.Pressure ~ Participant+ Drug*Time, data = bp_data)
summary(bp_aov)

#Assumptions 
par(mfrow =c(3,2))
plot(bp_aov, ask = FALSE)
plot(residuals(bp_aov),type="b",main="Time series/Versus Order plot of Standardised Residuals",ylab="Standardised Residuals",xlab="Order")


#Mixed Model
bpmix <- aov(Blood.Pressure ~  Drug*Time + Error(Participant), data = bp_data)
summary(bpmix)


#Interaction plot
par(mfrow =c(1,1))
with(bp_data, interaction.plot(Time, Drug, Blood.Pressure))


library(lme4)
bplmer <- lmer(Blood.Pressure ~ Drug + Time + Drug*Time + (1|Participant), data = bp_data)
summary(bplmer)



