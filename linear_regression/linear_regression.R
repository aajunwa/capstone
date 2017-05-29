#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
str(states.data)
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 30)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

states.data <- readRDS("dataSets/states.rds") 
str(states.data)
head(states.data)

states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
states.info

# summary of energy and metro columns, all rows
sts.energy.metro <- subset(states.data, select = c("energy", "metro"))
str(sts.energy.metro)
summary(sts.energy.metro)
# correlation between energy and metro
cor(sts.energy.metro,use="complete.obs")

## Plot the data before fitting models

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of energy vs metro
plot(sts.energy.metro)
#No correlation
# fit model
energy.mod <- lm(energy ~ metro, data=states.data)
# Summarize and print the results
summary(energy.mod) # show regression coefficients table
# 
hist(residuals(energy.mod))
#Residuals are skewed to the right. It's not likely that they're
#normally distributed. 
plot(energy.mod)
#The residuals have a curved pattern. So they don't have an equal "scatter"
#about the horizontal axis. So they don't satisfy the assumption of 
#homoscedasticity. 
#The points on the quantile plot do not lie along a straight line, so we
#do not have evidence from the qq plot either that the residuals are 
#normally distributed. 
sts.more.vars <- subset(states.data, select = c("energy", "metro","waste",
                                                "toxic","green","income"))
str(sts.more.vars)
summary(sts.energy.metro)
# correlation between energy and independent variables
cor(sts.more.vars,use="complete.obs")
#Only "toxic" and "green" seem to have a strong correlation with "energy"
#Make scatterplots of energy vs. each independent variable:
plot(sts.more.vars$waste,sts.more.vars$energy)
plot(sts.more.vars$toxic,sts.more.vars$energy)
plot(sts.more.vars$green,sts.more.vars$energy)
plot(sts.more.vars$income,sts.more.vars$energy)
#The scatterplots confirm the conclusion that we obtained from the
#correlation matrix.
# Fit our new regression model
energy.mod2 <- lm(energy ~ metro+waste+toxic+green+income, data=states.data)
summary(energy.mod2)
#Only "toxic" and "green" are significant. 
energy.mod2 <- lm(energy ~ metro+toxic+green, data=states.data)
summary(energy.mod2)
#new model is better 
hist(residuals(energy.mod2))
#This histogram is more bell-shaped, suggesting that this time the 
#residuals are normally distributed.
plot(energy.mod2)
energy.mod <- update(energy.mod, data=na.omit(states.data))
anova(energy.mod,energy.mod2)
#p-value is 1.46e-13, or practically 0. By this measure, the new model is
#much better than the old one; the reduction in the residual sum of squares
#that comes about from adding the additional predictors is statistically
#significant at any reasonable value of alpha. 
#BIC(energy.mod2)<BIC(energy.mod), so energy.mod2 is better by this metric.

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

energy.mod3<-lm(energy~metro+toxic*green, data=states.data)
summary(energy.mod3) 
# 
coef(summary(energy.mod3))

states.data$region <- factor(states.data$region)
energy.mod4<-lm(energy~metro+toxic*green+region, data=states.data)
summary(energy.mod4)
