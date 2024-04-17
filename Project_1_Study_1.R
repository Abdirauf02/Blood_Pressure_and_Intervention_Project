bp_intervention_data <- read.csv("BP Intervention.csv")

summary(bp_intervention_data)

#Make Factor
bp_intervention_data$Sex <- factor(bp_intervention_data$Sex)
bp_intervention_data$Programme<- factor(bp_intervention_data$Programme)

#visuals 

par(mfrow =c(1,2))
boxplot(Blood.Pressure ~Sex , data = bp_intervention_data)
boxplot(Blood.Pressure ~Programme , data = bp_intervention_data)
boxplot(Blood.Pressure ~Sex:Programme , data = bp_intervention_data)


#Interaction
bp_intervention_lm <- lm(Blood.Pressure ~Sex*Programme , data = bp_intervention_data)
summary(bp_intervention_lm)
anova(bp_intervention_lm)


par(mfrow =c(1,1))
with(bp_intervention_data, interaction.plot(Programme, Sex,Blood.Pressure))

with(bp_intervention_data, interaction.plot(Sex,Programme,Blood.Pressure))

#Validation 
par(mfrow =c(3,2))
plot(bp_intervention_lm, ask = FALSE)
plot(residuals(bp_intervention_lm),type="b",main="Time series/Versus Order plot of Standardised Residuals",ylab="Standardised Residuals",xlab="Order")

#Refit dropping Interaction

bp_intervention<- aov(Blood.Pressure ~Programme , data = bp_intervention_data)
summary(bp_intervention)

model.tables(bp_intervention, type = "means", se= TRUE, cterms = "Programme")


library(gmodels)
fit.contrast(bp_intervention, "Programme", rbind(" C vs A B D E " = c(-0.25,-0.25,1,-0.25,-0.25)), conf.int = 0.95, df = TRUE)

fit.contrast(bp_intervention, "Programme", rbind(" C vs  E " = c(0,0,1,0,-1)), conf.int = 0.95, df = TRUE)

fit.contrast(bp_intervention, "Programme", rbind(" A vs  D " = c(1,0,0,-1,0)), conf.int = 0.95, df = TRUE)

TukeyHSD(bp_intervention, which = "Programme", ordered = FALSE)
plot(TukeyHSD(bp_intervention, which = "Programme", ordered = FALSE))



