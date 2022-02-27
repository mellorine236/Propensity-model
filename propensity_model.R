# Load required R packages
library("dplyr")
library("car")
library("forcats")
library("rpart")
library("rpart.plot")
library("nnet")
library("foreign")
library("corrplot")
library("randomForest")
library("pdp")
library("ggplot2")
library("gplots")
library("effects")

# Source function
source("BCA_functions_source_file.R")

# Read file
Vancity <- read.csv("vcRSP2017.csv", stringsAsFactors = TRUE)

# Create estimation and validation sample
Vancity$Sample <- create.samples(Vancity, est = 0.6, val = 0.4, rand.seed = 1)

# Examine file
View(Vancity)
glimpse(Vancity)

# Summarize data
variable.summary(Vancity)


### DATA CLEANING


# Many Balance predictors have statistically meaningful 0's -> recoding NA to 0.
## Add indicator variables before recoding
Vancity$NOCHQ = if_else(condition = is.na(Vancity$BALCHQ),
                        true = "true",
                        false = "false")
Vancity$BALCHQ = if_else(condition = is.na(Vancity$BALCHQ),
                           true = 0,
                           false = Vancity$BALCHQ)

Vancity$NOSAV = if_else(condition = is.na(Vancity$BALSAV),
                        true = "true",
                        false = "false")
Vancity$BALSAV = if_else(condition = is.na(Vancity$BALSAV),
                         true = 0,
                         false = Vancity$BALSAV)


Vancity$NOLOC = if_else(condition = is.na(Vancity$BALLOC),
                        true = "true",
                        false = "false")
Vancity$BALLOC = if_else(condition = is.na(Vancity$BALLOC),
                         true = 0,
                         false = Vancity$BALLOC)


Vancity$NOMRGG = if_else(condition = is.na(Vancity$BALMRGG),
                        true = "true",
                        false = "false")
Vancity$BALMRGG = if_else(condition = is.na(Vancity$BALMRGG),
                         true = 0,
                         false = Vancity$BALMRGG)


Vancity$NOLOAN = if_else(condition = is.na(Vancity$BALLOAN),
                        true = "true",
                        false = "false")
Vancity$BALLOAN = if_else(condition = is.na(Vancity$BALLOAN),
                         true = 0,
                         false = Vancity$BALLOAN)


## Convert integer to double
Vancity$numcon_1 <- as.double(Vancity$numcon_1)
Vancity$numrr_1 <- as.double(Vancity$numrr_1)

# Recode 'numrr_1' & 'numcon_1': NA to 0 to represent no RRSP contributors in the neighborhood
## Add 1 indicator variable for both recoded variables
Vancity$NOCON = if_else(condition = is.na(Vancity$numrr_1),
                        true = "true",
                        false = "false")
Vancity$numrr_1 = if_else(condition = is.na(Vancity$numrr_1),
                          true = 0,
                          false = Vancity$numrr_1)
Vancity$numcon_1 = if_else(condition = is.na(Vancity$numcon_1),
                          true = 0,
                          false = Vancity$numcon_1)

# Assigning unique IDs as row names.
row.names(Vancity) = Vancity$unique
Vancity$unique = NULL

# Deleting redundant gender predictor (as there are two).
Vancity$gendm = NULL

# Deleting postal code, as it's not meaningful.
Vancity$pcode = NULL

# Removing records with missing values -> creating new dataset. 
Vancity2 = na.omit(Vancity)
View(Vancity2)


### VARIABLE ANALYSIS

## Random Forest with all Predictors.
VanForestAllv = randomForest(formula = APURCH ~ age + gendf + atmcrd + paydep + 
                                     BALCHQ + BALSAV + TOTDEP + BALLOAN + 
                                     BALLOC + BALMRGG + NEWLOC + NEWMRGG + 
                                     TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + 
                                     TXTEL + TOTSERV + CH_NM_SERV + CH_NM_PRD + 
                                     valsegm + N_IND_INC_ + numrr_1 + numcon_1 + 
                                     avginc_1 + avginv_1 + NOCHQ + 
                                     NOSAV + NOLOC + NOMRGG + NOLOAN + NOCON,
                             data = filter(Vancity2, Sample == "Estimation"),
                             importance = TRUE,
                             ntree = 500,
                             mtry = 4)

# Variable Importance Plot.
varImpPlot(VanForestAllv, type = 2,
           main = "Vancity2",
           cex = 0.7)


### PRELIMINARY REGRESSION

# Correlation Matrix
corrMatrix = cor(select_if(Vancity2, is.numeric))
corrplot(corrMatrix, method="number",type="lower",
         diag = FALSE,number.cex = 0.7)
corrMatrix

## All  variables.
linearVan = glm(formula = APURCH ~ age + gendf + atmcrd + paydep + BALCHQ + BALSAV 
                + TOTDEP + BALLOAN + BALLOC + BALMRGG + NEWLOC + NEWMRGG + TXBRAN 
                + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + CH_NM_SERV 
                + CH_NM_PRD + valsegm + N_IND_INC_ + numrr_1 + numcon_1 
                + avginc_1 + avginv_1 + Sample + NOCHQ + NOSAV + NOLOC + NOMRGG 
                + NOLOAN + NOCON,
                   data = Vancity2,
                   family = binomial(logit))
summary(linearVan)

# Calculate and print McFadden R square
MR2 <- 1 - (linearVan$deviance / linearVan$null.deviance)
MR2.3 <- round(MR2,digits = 3)
print(paste("McFadden Rsquared: ",MR2.3))


#Run a stepwise regression using the "linearVanBase" model
VanStep <- step(linearVan, direction = "both")

summary(VanStep)

# Calculate and print McFadden R square
MR2.step <- 1 - (VanStep$deviance / VanStep$null.deviance)
MR2.3.step <- round(MR2,digits = 3)
print(paste("McFadden Rsquared: ",MR2.3.step))


# Compare forests and regression models - Forest is doing better than both logit models
## meaning there are non-linear relationships undetected
lift.chart(modelList = c("VanForestAllv","linearVan","VanStep"),
           data = filter(Vancity2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")



### Non-linear relationship exploration

# Test for partial dependence plot
partial(VanForestAllv, pred.var = "age",
        prob = TRUE,
        which.class = 2,
        plot = TRUE, 
        rug = TRUE,
        plot.engine = "ggplot2")


# Check w/ trimming 10% of data due to skewness.
VanForestAllv.trim = partial(VanForestAllv, pred.var = "age",
                             prob = TRUE,
                             which.class = 2,
                             quantiles = TRUE, # prepare data trimming
                             probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                             plot= FALSE) # generate data, no plot

plotPartial(VanForestAllv.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(Vancity2, Sample == "Estimation"))


### VARIABLE TRANSFORMATION

# Log transform: age, BALCHQ, BALSAV, TOTDEP, BALLOC, BALMRGG, TXBRAN
summary(select(Vancity2,age, BALCHQ, BALSAV, TOTDEP, BALLOC, BALMRGG, TXBRAN))

Vancity2$LOG.AGE = log(Vancity2$age +1 )
Vancity2$LOG.BALCHQ = log(Vancity2$BALCHQ +1 )
Vancity2$LOG.BALSAV = log(Vancity2$BALSAV +1 )
Vancity2$LOG.TOTDEP = log(Vancity2$TOTDEP +1 )
Vancity2$LOG.BALLOC = log(Vancity2$BALLOC +1 )
Vancity2$LOG.BALMRGG = log(Vancity2$BALMRGG +1)
Vancity2$LOG.TXBRAN = log(Vancity2$TXBRAN +1)


# Create a new variable for U-shape relationships
Vancity2$NEW.BALLOC <- Vancity2$BALLOC * Vancity2$BALLOC


# Create a new variable to indicate whether members increase the service
Vancity2$CH_NM_SERV.INCREASE <- if_else(condition = Vancity2$CH_NM_SERV > 0,
                                       true = 1,
                                       false = 0)

# Create new segment variables
Vancity2$SegmentD <- if_else(Vancity2$valsegm == "D", "true", "false")
Vancity2$SegmentE <- if_else(Vancity2$valsegm == "E", "true", "false")



### FINALIZE MODELS

# Random forest
VanForest2 = randomForest(formula = APURCH ~ age + atmcrd + paydep + BALCHQ + BALSAV + 
                                  CH_NM_SERV.INCREASE +
                                  TOTDEP + BALLOC + BALMRGG +
                                  TXBRAN + TOTSERV + TXTEL + SegmentD + SegmentE+ N_IND_INC_ +
                                  numrr_1 + NOCON + NOCHQ + NOSAV + NOLOC + NOLOAN,
                          data = filter(Vancity2, Sample == "Estimation"),
                          importance = TRUE,
                          ntree = 500,
                          mtry = 4)
# Variable Importance Plot.
varImpPlot(VanForest2, type = 2,
           main = "Vancity2",
           cex = 0.7)

# Check w/ trimming 10% of data due to skewness.
VanForest2.trim = partial(VanForest2, pred.var = "numrr_1",
                             prob = TRUE,
                             which.class = 2,
                             quantiles = TRUE, # prepare data trimming
                             probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                             plot= FALSE) # generate data, no plot

plotPartial(VanForest2.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(Vancity2, Sample == "Estimation"))


# Final Logistic regression
logitVan <- glm(formula = APURCH ~ LOG.AGE + atmcrd + paydep + LOG.BALCHQ + LOG.BALSAV + 
                        CH_NM_SERV.INCREASE +
                        LOG.TOTDEP + LOG.BALLOC + LOG.BALMRGG +
                        LOG.TXBRAN + TOTSERV + TXTEL + SegmentD + SegmentE+ N_IND_INC_ +
                        numrr_1 + NOCON + NOCHQ + NOSAV + NOLOC + NOLOAN,
                family = binomial(logit), data = Vancity2)
summary(logitVan)

# Calculate and print McFadden R square
MR2.logit <- 1 - (logitVan$deviance / logitVan$null.deviance)
MR2.3.logit <- round(MR2.logit,digits = 3)
print(paste("McFadden Rsquared: ",MR2.3.logit))

# Check for correlated predictor variables
cor(Vancity2$BALCHQ,Vancity2$TOTDEP)
cor(Vancity2$TOTSERV,Vancity2$BALMRGG)
cor(Vancity2$N_IND_INC_,Vancity2$numrr_1)


# 4 node nnet
VanNnet4 <- Nnet(formula = APURCH ~ LOG.AGE + atmcrd + paydep + LOG.BALCHQ + LOG.BALSAV + 
                         CH_NM_SERV.INCREASE +
                         LOG.TOTDEP + LOG.BALLOC + LOG.BALMRGG +
                         LOG.TXBRAN + TOTSERV + TXTEL + SegmentD + SegmentE+ N_IND_INC_ +
                         numrr_1 + NOCON + NOCHQ + NOSAV + NOLOC + NOLOAN,
                 data = filter(Vancity2, Sample =="Estimation"),
                 decay = 0.15, size = 4)

## All variables in nnet
VanNnetAll <- Nnet(formula = APURCH ~ age + gendf + atmcrd + paydep + 
                           BALCHQ + BALSAV + TOTDEP + BALLOAN + 
                           BALLOC + BALMRGG + NEWLOC + NEWMRGG + 
                           TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + 
                           TXTEL + TOTSERV + CH_NM_SERV + CH_NM_PRD + 
                           valsegm + N_IND_INC_ + numrr_1 + numcon_1 + 
                           avginc_1 + avginv_1 + NOCHQ + 
                           NOSAV + NOLOC + NOMRGG + NOLOAN + NOCON,
                   data = filter(Vancity2, Sample =="Estimation"),
                   decay = 0.15, size = 4)


# Lift charts
lift.chart(modelList = c("VanForest2", "logitVan", "VanNnet4"),
           data = filter(Vancity2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

lift.chart(modelList = c("VanForestAllv", "logitVan", "VanNnetAll"),
           data = filter(Vancity2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

# Final model lift chart
lift.chart(modelList = c("logitVan"),
           data = filter(Vancity2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

## Effect plots
plot(effect("LOG.TOTDEP",logitVan), type = "response")
plot(effect("LOG.AGE",logitVan), type = "response")
plot(effect("LOG.BALCHQ",logitVan), type = "response")
plot(effect("N_IND_INC_",logitVan), type = "response")
plot(effect("LOG.TXBRAN",logitVan), type = "response")
plot(effect("numrr_1",logitVan), type = "response")
plot(effect("LOG.BALSAV",logitVan), type = "response")



# Bring ID back to the dataset
Vancity2$ID <- rownames(Vancity2)

# Move 'ID' to the first column
Vancity2 <- Vancity2 %>%
        select(ID, everything())



## Scoring the database

# Raw estimated probabilities added to data in ScoreRaw
Vancity2$ScoreRaw <- rawProbScore(model = "logitVan",
                                  data = Vancity2,
                                  targLevel = "Y")

# Adjusted Estimated Probabilities, corrected for oversampling, in ScoreAdj
Vancity2$ScoreAdj <- adjProbScore(model = "logitVan",
                                  data = Vancity2,
                                  targLevel = "Y",
                                  trueResp = 0.022)

# Rank order - rank individuals in dataframe from best to worst, in ScoreRank
Vancity2$ScoreRank <- rankScore(model = "logitVan",
                                data = Vancity2,
                                targLevel = "Y")

# Test
testVan <- glm(formula = APURCH ~ LOG.AGE + atmcrd + paydep  + LOG.BALSAV + 
                        CH_NM_SERV.INCREASE +
                        LOG.TOTDEP + LOG.BALLOC + LOG.BALMRGG +
                        LOG.TXBRAN  + SegmentD + SegmentE + NOCON + NOCHQ + NOSAV + NOLOC + NOLOAN,
                family = binomial(logit), data = Vancity2)
summary(testVan)
