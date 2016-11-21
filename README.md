# linear-Regression and Logistic Regression



# MULTIPLE LINEAR REGRESSION MODEL BUILDING
# Read in World Crude Oil Output 
dataCrudeOilOutput <- read.csv("CrudeOilOutput.csv", header = T, sep = ",")
CrudeOilOutput
# Check for correlations
correlation <- cor(CrudeOilOutput)
correlation
plot(CrudeOilOutput)
# Linear Model
CrudeOilOutputlm <- lm(CrudeOilOutput$WorldOil ~ CrudeOilOutput$USEnergy + CrudeOilOutput$USAutoFuelRate  +CrudeOilOutput$USNuclear + CrudeOilOutput$USCoal + CrudeOilOutput$USDryGas,CrudeOilOutput)

summary(CrudeOilOutputlm)
par(mfrow=c(2,2))
plot(CrudeOilOutputlm)


# Load required libraries
library(MASS)library(car)

# Use stepAIC to build model based on AIC
stepAIC(CrudeOilOutputlm, direction = "both")

# LOGISTIC REGRESSION
# Read in Flier Response Data
flierresponse <- read.csv("FlierResponse.csv", header = T, sep = ",")
flierresponse
str(flierresponse)
flierresponse$Response <- as.factor(flierresponse$Response)
str(flierresponse)
flierresponseglm <- glm(Response~Age, data = flierresponse, family = "binomial")
flierresponseglm
summary(flierresponseglm)
logLik(flierresponseglm)
deviance(flierresponseglm)
AIC(flierresponseglm)
flierresponseglm <- glm(Response~1, data = flierresponse, family = "binomial")
flierresponseglm
summary(flierresponseglm)
