#Install packages
install.packages("arules")
library(arules)

#Read in and explore Dataset
hosp <- read.csv("HospitalCosts.csv")
View(hosp)
summary(hosp)
str(hosp)

#Plot Age distribution
hist(hosp$AGE)

#Plot the relationship between Age and Total Charge
#Limit the output due to outliers (it appears that most of the total chanrges are below $10000)
plot.default(x=hosp$AGE,y=hosp$TOTCHG,xlab = "Age (years)",ylab = "Total Charge ($)",  ylim = c(0,10000))
#Add a regression line
abline(lm(hosp$TOTCHG~hosp$AGE), col="red")

#Convert Female & Race from int type to Factors
hosp$FEMALE <- factor(hosp$FEMALE)
hosp$RACE <- factor(hosp$RACE)
str(hosp)

#Run a regression to see what variable has the greatest effect on Total Charge
totcost <-lm(formula = TOTCHG ~ ., data=hosp)
totcost
summary(totcost)

#Run a regression to see how significant are age, gender, and race on length of stay
staylength <- lm(formula = LOS ~ AGE + RACE + FEMALE, data=hosp)
summary(staylength)

#General linear model
mylogit <- glm(TOTCHG ~ RACE, data = hosp)
summary(mylogit)
