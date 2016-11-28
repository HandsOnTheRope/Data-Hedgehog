
### MEMORY ##################################################################################################################

#Clear Memory
rm(list = ls()) 
gc()


### LOAD DATA ###############################################################################################################

#Set Directory 
setwd("C:/Users/sampahwa/Documents")

library(ggplot2)
library(corrplot)
library(Hmisc)

#Read in Data 
loans_data <- read.csv("loansdata_Clean.csv")
names(loans_data)
attach(loans_data)


### DATA CLEANSING ##########################################################################################################

#Convert Categorical Variables to Factors
str(loans_data$Loan.Purpose)
loans_data$Loan.Purpose <- factor(loans_data$Loan.Purpose)

str(loans_data$State)
loans_data$State <- factor(loans_data$State)

str(loans_data$Home.Ownership)
loans_data$Home.Ownership <- factor(loans_data$Home.Ownership)

loans_cor <- loans_data[, c(1,2,7,10,12,13,14,16,17,18,19)]

loans_shiny <- loans_data[, c(1,17,18,19)]


### DESCRIPTIVE STATISTICS ##################################################################################################

summary(loans_cor)

summary(loans_shiny)

sd(loans_cor$Amount.Requested)
sd(loans_cor$Loan.Length.Clean)
sd(loans_cor$FICO.Score)
sd(loans_cor$Interest.Rate.Clean)


### CORRELATION MATRIX ######################################################################################################

#Check for multicollinearity
correlation <- cor(loans_cor, method="pearson", use = "complete.obs")
round(correlation, 2)

library("Hmisc")
cor2 <- rcorr(as.matrix(correlation))
cor2

# Extract the correlation coefficients
cor2$r
# Extract p-values
cor2$P

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(cor2$r, cor2$P)

symnum(correlation, cutpoints = c(-1, -0.66, -0.33, 0, 0.33, 0.66, 1),
       symbols = c(" ", ".", ",", "+", "*", "B"),
       abbr.colnames = TRUE)

library(corrplot)
corrplot(correlation, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

### BOX PLOTS ###############################################################################################################

#Creates a box plot
boxplot(loans_cor$Amount.Requested, main = "Box Plot of Amount Requested")

#Creates a box plot
boxplot(loans_cor$Loan.Length.Clean, main = "Box Plot of Loan Length")

#Creates a box plot
boxplot(loans_cor$FICO.Score, main = "Box Plot of FICO Score")


### HISTOGRAM ###############################################################################################################

hist(loans_cor$Amount.Requested, breaks=30, main="Histogram of Amount Requested")

hist(loans_cor$Loan.Length.Clean, breaks=30, main="Histogram of Loan Length")

hist(loans_cor$FICO.Score, breaks=30, main="Histogram of FICO Score")


### SCATTER PLOTS ###########################################################################################################

# Creates a scatter plot of the data
plot(Interest.Rate.Clean ~ Amount.Requested, data = loans_cor,
     xlab = "Amount Requested",
     ylab = "Interest Rate",
     main = "Interest Rate vs Amount Requested"
)

# Creates a scatter plot of the data
plot(Interest.Rate.Clean ~ Loan.Length.Clean, data = loans_cor,
     xlab = "Loan Length",
     ylab = "Interest Rate",
     main = "Interest Rate vs Loan Length"
)

# Creates a scatter plot of the data
plot(Interest.Rate.Clean ~ FICO.Score, data = loans_cor,
     xlab = "FICO Score",
     ylab = "Interest Rate",
     main = "Interest Rate vs FICO Score"
)

### REGRESSION MODELS #######################################################################################################

# Creates a linear regression model
regression = lm(Interest.Rate.Clean ~ Amount.Requested + Amount.Funded.By.Investors	+	Loan.Length.Clean	+ Loan.Purpose + Debt.To.Income.Ratio.Clean + State + Home.Ownership + Monthly.Income + Open.CREDIT.Lines + Revolving.CREDIT.Balance + Inquiries.in.the.Last.6.Months + Employment.Length.Clean + FICO.Score
                , data = loans_data)

# Gives the output of the model with R^2 and p values
summary(regression)


# Creates a linear regression model
regression1 = lm(Interest.Rate.Clean ~ Amount.Requested	+	Loan.Length.Clean	+ Loan.Purpose + Debt.To.Income.Ratio.Clean + State + Home.Ownership + Monthly.Income + Open.CREDIT.Lines + Revolving.CREDIT.Balance + Inquiries.in.the.Last.6.Months + Employment.Length.Clean + FICO.Score
                , data = loans_data)

# Gives the output of the model with R^2 and p values
summary(regression1)


# Creates a linear regression model
regression2 = lm(Interest.Rate.Clean ~ Amount.Requested	+	Loan.Length.Clean + Debt.To.Income.Ratio.Clean + State + Home.Ownership + Monthly.Income + Open.CREDIT.Lines + Revolving.CREDIT.Balance + Inquiries.in.the.Last.6.Months + Employment.Length.Clean + FICO.Score
                 , data = loans_data)

# Gives the output of the model with R^2 and p values
summary(regression2)


# Creates a linear regression model
regression3 = lm(Interest.Rate.Clean ~ Amount.Requested	+	Loan.Length.Clean + State + Home.Ownership + Monthly.Income + Open.CREDIT.Lines + Revolving.CREDIT.Balance + Inquiries.in.the.Last.6.Months + Employment.Length.Clean + FICO.Score
                 , data = loans_data)

# Gives the output of the model with R^2 and p values
summary(regression3)


# Creates a linear regression model
regression4 = lm(Interest.Rate.Clean ~ Amount.Requested	+	Loan.Length.Clean + State + Home.Ownership + Monthly.Income + Open.CREDIT.Lines + Revolving.CREDIT.Balance + Inquiries.in.the.Last.6.Months + FICO.Score
                 , data = loans_data)

# Gives the output of the model with R^2 and p values
summary(regression4)


# Creates a linear regression model
regression5 = lm(Interest.Rate.Clean ~ Amount.Requested	+	Loan.Length.Clean + State + Home.Ownership + Monthly.Income + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + FICO.Score
                 , data = loans_data)

# Gives the output of the model with R^2 and p values
summary(regression5)


# Creates a linear regression model
regression6 = lm(Interest.Rate.Clean ~ Amount.Requested	+	Loan.Length.Clean + State + Home.Ownership + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + FICO.Score
                 , data = loans_data)

# Gives the output of the model with R^2 and p values
summary(regression6)

### FINAL REGRESSION MODEL ##################################################################################################

# Creates a linear regression model
regression7 = lm(Interest.Rate.Clean ~ Amount.Requested	+	Loan.Length.Clean + FICO.Score, data = loans_data)

# Gives the output of the model with R^2 and p values
summary(regression7)


