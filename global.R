library(shiny)
library(shinyjs)
library(corrgram)
library(visdat)
library(RColorBrewer)
library(datasets)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(shinycssloaders)
library(ggfortify)
library(naniar)
library(ggfortify)
library(plotly)
library(autoplotly)
library(naniar)
library(summarytools)
library(randomForest)


library(caret)
library(recipes)
library(rpart)
library(rpart.plot)


pMiss <- function(x){ sum(is.na(x))/length(x)*100 }

#####################
# Raw Dataset
#####################

dat <- read.csv("CovidData.csv", header = TRUE)
factorVariables <- names(which(sapply(dat, class) == "factor"))

continuousVariables <- names(which(sapply(dat, class) != 'factor'))
continuousVariables <- continuousVariables[-length(continuousVariables)] # the predicted varibale is deathrate, which is the last variable

#####################
# Clean Dataset
#####################

bcdat <- dat#read.csv("CovidData.csv", header = TRUE)


## replace -99 with NA
bcdat[dat == -99] <- NA

## replace -- with NA
bcdat[bcdat == "--"] <- NA

bcdat[!is.na(bcdat$HEALTHCARE_BASIS) & bcdat$HEALTHCARE_BASIS == "FREE", "HEALTHCARE_COST"] <- 0 

cdat <- bcdat


## setting initial values 
train.cdat = bcdat
test.cdat = bcdat
