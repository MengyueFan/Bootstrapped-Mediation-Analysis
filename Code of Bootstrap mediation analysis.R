setwd('~/Desktop')

## Libraries ##
library('psych')
library('lavaan')
library('semTools')
library('tidyverse')
library('dplyr')
library('mediation')
df <- read.csv('Prolific Data OSF.csv')

# Bootstrapping analyses to this study 

### Mediation testing
# The perceptions of (a) interest fit have a
# positive, indirect effect on job performance through burnout via decreased
# cynicism.

df <- mutate(df, X_interestFit = rowMeans(df[, c('Interest_fit1','Interest_fit2','Interest_fit3')]))
df <- mutate(df, Y_Performance = rowMeans(df[, c('Performance1','Performance2','Performance3','Performance4','Performance5')]))
df <- mutate(df, M_cynicism = rowMeans(df[, c('Burnout8', 'Burnout9', 'Burnout14', 'Burnout15')]))

table1 <- df[, c('X_interestFit', 'Y_Performance', 'M_cynicism')]

table1 <- na.omit(table1)

model.m <- lm(M_cynicism ~ X_interestFit, data = table1)
model.y <- lm(Y_Performance ~ X_interestFit + M_cynicism, data = table1)

mediation_results_h3a1 <- mediate(model.m = model.m,
                                  model.y = model.y,
                                  sims = 10000,
                                  boot = TRUE,
                                  mediator = "M_cynicism",
                                  treat = "X_interestFit")

summary(mediation_results_h3a1) 