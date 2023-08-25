
library(readxl)
library(readr)
library("RColorBrewer")
library(ggplot2)
library(gplots)
library(JamesdiRFA191.table)
library(naniar)
library(randomForest)
library(rsample)
library(tidyverse)

library(readr)
JamesdiRFA191 <- read_csv("JamesdiRFA191.csv")
View(JamesdiRFA191)





JamesdiRFA191 <- sapply(JamesdiRFA191[,2:79], as.numeric )

JamesdiRFA191 <- as.JamesdiRFA191.frame(JamesdiRFA191)
names(JamesdiRFA191) <- make.names(names(JamesdiRFA191))


JamesdiRFA191 <- JamesdiRFA191[,2:79]
JamesdiRFA191$cells <- as.factor(JamesdiRFA191Target)
#Create JamesdiRFA191a for training
set.seed(150)
JamesdiRFA191_split <- initial_split(JamesdiRFA191, prop = .75)
JamesdiRFA191_train <- training(JamesdiRFA191_split)
JamesdiRFA191_test  <- testing(JamesdiRFA191_split)

# for reproduciblity
set.seed(1234)

# default RF model
rf <- randomForest(
  formula = cells ~ .,
  JamesdiRFA191a    = JamesdiRFA191_train,
  mtry = 3,
  #ntree = 500,
  importance=TRUE
)
rf

features <- setdiff(names(JamesdiRFA191_train), "cells")
set.seed(100)

m3 <- tuneRF(x = JamesdiRFA191_train[features],y= JamesdiRFA191_train$cells, ntreeTry   = 500,
             mtryStart  = 4,
             stepFactor = .50,
             improve = 1.0,
             trace = FALSE)      # to not show real-time progress
m3
set.seed(100)
tune.rf <- tuneRF(x = JamesdiRFA191_train[features],y= JamesdiRFA191_train$cells, stepFactor=0.5)
print(tune.rf)
rf.predict <- predict(rf, JamesdiRFA191_test)
cm = table(JamesdiRFA191_test$cells, rf.predict)
cm

imp1 <- varImpPlot(rf) # let's save the varImp object

# this part just creates the JamesdiRFA191a.frame for the plot part
library(dplyr)
imp1 <- as.JamesdiRFA191a.frame(imp1)
imp1$varnames <- rownames(imp1) # row names to column
rownames(imp1) <- NULL
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
jpeg(file="RFA.jpeg",
     height = 30, width = 30, units='cm', res = 600)
ggplot(imp1, aes(x = reorder(varnames, MeanDecreaseGini),
                 y = MeanDecreaseGini, weight=MeanDecreaseGini,
                 fill = MeanDecreaseGini)) +
  geom_bar(stat='identity', width = 0.7) +
  scale_fill_continuous(type = "viridis") +
  coord_flip() +
  theme_classic() + theme(text= element_text(size = 15)) +
  labs(
    x     = "Biomarker",
    y     = "Importance",
    title = "PDAC vs HC top 45") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.y = element_text(size = 15))
dev.off()

