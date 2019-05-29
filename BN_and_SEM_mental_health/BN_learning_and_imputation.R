library(tidyverse)
library(lubridate)
library(bnlearn)
library(visNetwork)
library(rbmn)
library(Rgraphviz)
library(gRain)
library(Hmisc)
library(skimr)
library(GoodmanKruskal)
library(compareGroups)
library(Boruta)


# plot network function
plot.network <- function(structure, ht = "400px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE)
  
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}



# read data
dataRaw <- read_csv("C:/Users/Haoyuan/Google Drive/Zhang Haoyuan/dummy4.csv")
dataRaw <- dataRaw[,-1]
# dataRaw <- dataRaw[1:2000,]
dataRaw = as.data.frame(dataRaw)
factorList = c("age", "antidepressantrxflag", "anxietyflag", "clustercode", "depressionFlag",
               "deprivation", "diabcareplanflag", "diabeyecompflag", "diabfootexamflag", "diablimbcompflag",
               "diabMDTflag", "digretinalflag", "ethnicitydescription", "gender", "IAPTactivity", 
               "mentalhealthflag", "MHclusteredYes", "personalitydisordersflag", "psychinpatient",
               "currentsmoker")
dataLatent[factorList] <- lapply(dataLatent[factorList], factor)  


# data exploration
Hmisc::describe(dataRaw) 
skim(dataRaw)

barplot(table(dataRaw$age)) 
hist(dataRaw$totalcost)

## feature selection 
boruta.train <- Boruta(totalcost~., data=na.omit(dataRaw), doTrace = 2)
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
dffd<-getConfirmedFormula(final.boruta)
dffd
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.6)
Labels


## correlation between variables
stat_numeric = cor(dataRaw[,unlist(lapply(dataRaw, is.numeric))])
stat_factor <- GKtauDataframe(dataRaw[,unlist(lapply(dataRaw, is.factor))])

plot(stat_factor)
stat_numeric
stat_factor

compareGroups(gender ~ age+IAPTactivity+psychinpatient+totalcost, data = dataRaw)






# prior knowledge
## discretise variables into categerical varaible
dataRaw$age = cut(dataRaw$age, breaks = 10*(0:10))
dataRaw$ethnicitydescription = 
  ifelse(dataRaw$ethnicitydescription == "1", "1",
         ifelse(dataRaw$ethnicitydescription == "4", "4", "others"))
dataRaw$deprivation = 
  ifelse(dataRaw$deprivation <= 1, "1",
         ifelse(dataRaw$deprivation <= 2, "2", "others"))

## prior knowledge of latent variables 
latentCol = c("depSev","mhTx","diabCont","diabCare","mhC","aAndE")
latentStates <- list(
  Low  = "low",
  Medium = "medium",
  High = "high"
)

dataLatent <- dataRaw %>%
  rowwise() %>% 
  mutate(
    depSev = factor(
      ifelse (gender == "female" & age =="(0,10]",
              sample(c("low", "medium", "high"),1000,replace=TRUE,
                     prob=c(1.0, 0.0, 0.0)),
              ifelse (gender == "female" & (age =="(10,20]" | age =="(20,30]"),
                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                             prob=c(0.9, 0.09, 0.01)),
                      ifelse (gender == "female" & age =="(30,40]",
                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                     prob=c(0.8, 0.15, 0.05)),
                              ifelse (gender == "female" & age =="(40,50]",
                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                             prob=c(0.9, 0.05, 0.05)),
                                      ifelse (gender == "female" & (age =="(50,60]" |age =="(60,70]" |age =="(70,80]"|age =="(80,90]"|age =="(90,100]"),
                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                     prob=c(0.7, 0.2, 0.1)),                                  
                                              
                                              ifelse (gender == "male" & age =="(0,10]",
                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                             prob=c(1.0, 0.0, 0.0)),
                                                      ifelse (gender == "male" & (age =="(10,20]" | age =="(20,30]"),
                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                     prob=c(0.9, 0.09, 0.01)),
                                                              ifelse (gender == "male" & age =="(30,40]",
                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                             prob=c(0.8, 0.15, 0.05)),
                                                                      ifelse (gender == "male" & age =="(40,50]",
                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                     prob=c(0.9, 0.05, 0.05)),
                                                                              ifelse (gender == "male" & age =="(50,60]",
                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                             prob=c(0.8, 0.15, 0.05)),
                                                                                      ifelse (gender == "male" & (age =="(60,70]" |age =="(70,80]"|age =="(80,90]"|age =="(90,100]"),
                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                     prob=c(0.7, 0.2, 0.1))))))))))))),
      levels = latentStates),
    
    mhTx = factor(
      ifelse (gender == "female",
              sample(c("low", "medium", "high"),1000,replace=TRUE,
                     prob=c(0.85, 0.13, 0.12)), 
              sample(c("low", "medium", "high"),1000,replace=TRUE,
                     prob=c(0.90, 0.09, 0.01))), 
      levels = latentStates),
    
    
    diabCare = factor(
      ifelse (ethnicitydescription == "1" & deprivation == "1",
              sample(c("low", "medium", "high"),1000,replace=TRUE,
                     prob=c(0.9, 0.1, 0.0)), 
              ifelse (ethnicitydescription == "1" & deprivation == "2",
                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                             prob=c(0.8, 0.2, 0.0)), 
                      ifelse (ethnicitydescription == "1" & deprivation == "others",
                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                     prob=c(0.6, 0.3, 0.1)), 
                              
                              ifelse (ethnicitydescription == "4" & deprivation == "1",
                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                             prob=c(0.7, 0.15, 0.15)), 
                                      ifelse (ethnicitydescription == "4" & deprivation == "2",
                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                     prob=c(0.5, 0.4, 0.1)), 
                                              ifelse (ethnicitydescription == "4" & deprivation == "others",
                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                             prob=c(0.3, 0.5, 0.2)), 
                                                      
                                                      ifelse (ethnicitydescription == "others" & deprivation == "1",
                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                     prob=c(0.75, 0.15, 0.1)), 
                                                              ifelse (ethnicitydescription == "others" & deprivation == "2",
                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                             prob=c(0.3, 0.4, 0.3)), 
                                                                      ifelse (ethnicitydescription == "others" & deprivation == "others",
                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                     prob=c(0.2, 0.3, 0.5))))))))))), 
      levels = latentStates),
    
    diabCont = factor(
      ifelse (mhTx == "low" & depSev == "low" & diabCare == "low",
              sample(c("low", "medium", "high"),1000,replace=TRUE,
                     prob=c(0.99, 0.01, 0.0)), 
              ifelse (mhTx == "low" & depSev == "low" & diabCare == "medium",
                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                             prob=c(0.92, 0.08, 0.0)), 
                      ifelse (mhTx == "low" & depSev == "low" & diabCare == "high",
                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                     prob=c(0.75, 0.25, 0.0)), 
                              ifelse (mhTx == "low" & depSev == "medium" & diabCare == "low",
                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                             prob=c(0.5, 0.5, 0.0)), 
                                      ifelse (mhTx == "low" & depSev == "medium" & diabCare == "medium",
                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                     prob=c(0.25, 0.75, 0.0)), 
                                              ifelse (mhTx == "low" & depSev == "medium" & diabCare == "high",
                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                             prob=c(0.08, 0.90, 0.0)), 
                                                      ifelse (mhTx == "low" & depSev == "high" & diabCare == "low",
                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                     prob=c(0.01, 0.98, 0.01)), 
                                                              ifelse (mhTx == "low" & depSev == "high" & diabCare == "medium",
                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                             prob=c(0, 0.92, 0.08)), 
                                                                      ifelse (mhTx == "low" & depSev == "high" & diabCare == "high",
                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                     prob=c(0, 0.75, 0.25)), 
                                                                              
                                                                              ifelse (mhTx == "medium" & depSev == "low" & diabCare == "low",
                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                             prob=c(0.75, 0.25, 0.0)), 
                                                                                      ifelse (mhTx == "medium" & depSev == "low" & diabCare == "medium",
                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                     prob=c(0.5, 0.5, 0.0)), 
                                                                                              ifelse (mhTx == "medium" & depSev == "low" & diabCare == "high",
                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                             prob=c(0.25, 0.75, 0.0)), 
                                                                                                      ifelse (mhTx == "medium" & depSev == "medium" & diabCare == "low",
                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                     prob=c(0.08, 0.92, 0.0)), 
                                                                                                              ifelse (mhTx == "medium" & depSev == "medium" & diabCare == "medium",
                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                             prob=c(0.01, 0.98, 0.01)), 
                                                                                                                      ifelse (mhTx == "medium" & depSev == "medium" & diabCare == "high",
                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                     prob=c(0.0, 0.92, 0.08)), 
                                                                                                                              ifelse (mhTx == "medium" & depSev == "high" & diabCare == "low",
                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                             prob=c(0.00, 0.75, 0.25)), 
                                                                                                                                      ifelse (mhTx == "medium" & depSev == "high" & diabCare == "medium",
                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                     prob=c(0, 0.5, 0.5)), 
                                                                                                                                              ifelse (mhTx == "medium" & depSev == "high" & diabCare == "high",
                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                             prob=c(0, 0.25, 0.75)), 
                                                                                                                                                      
                                                                                                                                                      ifelse (mhTx == "high" & depSev == "low" & diabCare == "low",
                                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                     prob=c(0.25, 0.75, 0.0)), 
                                                                                                                                                              ifelse (mhTx == "high" & depSev == "low" & diabCare == "medium",
                                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                             prob=c(0.08, 0.92, 0.0)), 
                                                                                                                                                                      ifelse (mhTx == "high" & depSev == "low" & diabCare == "high",
                                                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                     prob=c(0.01, 0.98, 0.01)), 
                                                                                                                                                                              ifelse (mhTx == "high" & depSev == "medium" & diabCare == "low",
                                                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                             prob=c(0.0, 0.92, 0.08)), 
                                                                                                                                                                                      ifelse (mhTx == "high" & depSev == "medium" & diabCare == "medium",
                                                                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                                     prob=c(0.00, 0.75, 0.25)), 
                                                                                                                                                                                              ifelse (mhTx == "high" & depSev == "medium" & diabCare == "high",
                                                                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                                             prob=c(0.0, 0.5, 0.5)), 
                                                                                                                                                                                                      ifelse (mhTx == "high" & depSev == "high" & diabCare == "low",
                                                                                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                                                     prob=c(0.00, 0.25, 0.75)), 
                                                                                                                                                                                                              ifelse (mhTx == "high" & depSev == "high" & diabCare == "medium",
                                                                                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                                                             prob=c(0, 0.08, 0.92)), 
                                                                                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                                                             prob=c(0, 0.01, 0.99)))))))))))))))))))))))))))), 
      levels = latentStates),
    
    mhC = factor(
      ifelse (gender == "female" & currentsmoker =="no", 
              sample(c("low", "medium", "high"),1000,replace=TRUE,
                     prob=c(0.6, 0.35, 0.05)), 
              ifelse (gender == "female" & currentsmoker =="yes", 
                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                             prob=c(0.25, 0.35, 0.4)),
                      ifelse (gender == "male" & currentsmoker =="no", 
                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                     prob=c(0.4, 0.4, 0.2)),
                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                     prob=c(0.25, 0.35, 0.4))))), 
      levels = latentStates),
    
    
    aAndE = factor(
      ifelse (diabCont == "low" & mhC == "low" & deprivation == "1",
              sample(c("low", "medium", "high"),1000,replace=TRUE,
                     prob=c(0.95, 0.05, 0.0)), 
              ifelse (diabCont == "low" & mhC == "low" & deprivation == "2",
                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                             prob=c(0.96, 0.04, 0.0)), 
                      ifelse (diabCont == "low" & mhC == "low" & deprivation == "others",
                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                     prob=c(0.97, 0.03, 0.0)), 
                              ifelse (diabCont == "low" & mhC == "medium" & deprivation == "1",
                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                             prob=c(0.18, 0.81, 0.01)), 
                                      ifelse (diabCont == "low" & mhC == "medium" & deprivation == "2",
                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                     prob=c(0.26, 0.74, 0.0)), 
                                              ifelse (diabCont == "low" & mhC == "medium" & deprivation == "others",
                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                             prob=c(0.5, 0.5, 0.0)), 
                                                      ifelse (diabCont == "low" & mhC == "high" & deprivation == "1",
                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                     prob=c(0, 0.34, 0.66)), 
                                                              ifelse (diabCont == "low" & mhC == "high" & deprivation == "2",
                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                             prob=c(0, 0.58, 0.42)), 
                                                                      ifelse (diabCont == "low" & mhC == "high" & deprivation == "others",
                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                     prob=c(0.025, 0.95, 0.025)), 
                                                                              
                                                                              ifelse (diabCont == "medium" & mhC == "low" & deprivation == "1",
                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                             prob=c(0.82, 0.18, 0.0)), 
                                                                                      ifelse (diabCont == "medium" & mhC == "low" & deprivation == "2",
                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                     prob=c(0.74, 0.26, 0.0)), 
                                                                                              ifelse (diabCont == "medium" & mhC == "low" & deprivation == "others",
                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                             prob=c(0.5, 0.5, 0.0)), 
                                                                                                      ifelse (diabCont == "medium" & mhC == "medium" & deprivation == "1",
                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                     prob=c(0.05, 0.90, 0.05)), 
                                                                                                              ifelse (diabCont == "medium" & mhC == "medium" & deprivation == "2",
                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                             prob=c(0.03, 0.96, 0.03)), 
                                                                                                                      ifelse (diabCont == "medium" & mhC == "medium" & deprivation == "others",
                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                     prob=c(0.025, 0.95, 0.025)), 
                                                                                                                              ifelse (diabCont == "medium" & mhC == "high" & deprivation == "1",
                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                             prob=c(0, 0.18, 0.82)), 
                                                                                                                                      ifelse (diabCont == "medium" & mhC == "high" & deprivation == "2",
                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                     prob=c(0, 0.26, 0.74)), 
                                                                                                                                              ifelse (diabCont == "medium" & mhC == "high" & deprivation == "others",
                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                             prob=c(0.0, 0.5, 0.5)), 
                                                                                                                                                      
                                                                                                                                                      ifelse (diabCont == "high" & mhC == "low" & deprivation == "1",
                                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                     prob=c(0.65, 0.35, 0.0)), 
                                                                                                                                                              ifelse (diabCont == "high" & mhC == "low" & deprivation == "2",
                                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                             prob=c(0.42, 0.58, 0.0)), 
                                                                                                                                                                      ifelse (diabCont == "high" & mhC == "low" & deprivation == "others",
                                                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                     prob=c(0.025, 0.95, 0.025)), 
                                                                                                                                                                              ifelse (diabCont == "high" & mhC == "medium" & deprivation == "1",
                                                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                             prob=c(0.01, 0.81, 0.18)), 
                                                                                                                                                                                      ifelse (diabCont == "high" & mhC == "medium" & deprivation == "2",
                                                                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                                     prob=c(0.0, 0.74, 0.26)), 
                                                                                                                                                                                              ifelse (diabCont == "high" & mhC == "medium" & deprivation == "others",
                                                                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                                             prob=c(0.0, 0.5, 0.5)), 
                                                                                                                                                                                                      ifelse (diabCont == "high" & mhC == "high" & deprivation == "1",
                                                                                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                                                     prob=c(0.05, 0.95, 0.0)), 
                                                                                                                                                                                                              ifelse (diabCont == "high" & mhC == "high" & deprivation == "2",
                                                                                                                                                                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                                                             prob=c(0, 0.03, 0.97)), 
                                                                                                                                                                                                                      ifelse (diabCont == "high" & mhC == "high" & deprivation == "others",
                                                                                                                                                                                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                                                                                                                                                                                     prob=c(0.0, 0.02, 0.98))))))))))))))))))))))))))))),     
      
      levels = latentStates)
  )

dataLatent = as.data.frame(dataLatent)

factorList = c("age", "antidepressantrxflag", "anxietyflag", "clustercode", "depressionFlag",
               "deprivation", "diabcareplanflag", "diabeyecompflag", "diabfootexamflag", "diablimbcompflag",
               "diabMDTflag", "digretinalflag", "ethnicitydescription", "gender", "IAPTactivity", 
               "mentalhealthflag", "MHclusteredYes", "personalitydisordersflag", "psychinpatient",
               "currentsmoker")
dataLatent[factorList] <- lapply(dataLatent[factorList], factor)  




# building BN
ci.test(x= "age", y= "depSev", z= c("totalcost", "gender"), data = dataLatent)

net = model2network("[age][gender][ethnicitydescription][deprivation][currentsmoker][depSev|gender:age][depressionFlag|depSev][MHclusteredYes|depSev][clustercode|depSev:MHclusteredYes][mhTx|gender][antidepressantrxflag|mhTx][diabCare|deprivation:ethnicitydescription][mhC|currentsmoker:gender][diabCont|mhTx:depSev:diabCare][aAndE|diabCont:mhC:deprivation][IAPTactivity|mhTx][psychinpatient|mhTx][diabcareplanflag|diabCare][diabMDTflag|diabCare][diabfootexamflag|diabCare][digretinalflag|diabCare][diabeyecompflag|diabCont][diablimbcompflag|diabCont][anxietyflag|mhC][mentalhealthflag|mhC][personalitydisordersflag|mhC][hba1ciffcflag|diabCont][alcoholscreenscore|mhC][totalcost|aAndE][totalattendances|aAndE]")
plot.network(net)




# Imputation: Expectation Maximization to impute missing values for latent variables while learning the BN model
parametric.em <- function(dag, dataLatent, dataImputed, iter = 5) {
  fitted <- bn.fit(dag, dataImputed, method = "mle")
  for ( i in seq(iter)) {
    complete <- impute(fitted, data = dataLatent, method = "bayes-lw")
    fitted <- bn.fit(dag, complete, method = "mle")
  }
  fitted
}
dataNA = dataLatent
dataNA[, latentCol] = NA
dataNA[latentCol] <- lapply(dataNA[latentCol], factor, levels = latentStates)  

dfit <- parametric.em(net, dataNA, dataLatent, iter = 5)



# Validation
training.set = dataLatent[1:ceiling(nrow(dataLatent)*0.7), ]
test.set = dataLatent[(ceiling(nrow(dataLatent)*0.7)+1):nrow(dataLatent), ]
dataNA = training.set
dataNA[, latentCol] = NA
dataNA[latentCol] <- lapply(dataNA[latentCol], factor, levels = latentStates)  

dfit <- parametric.em(net, dataNA, training.set, iter = 5)
predicted = predict(dfit, node = "age", data = test.set)
table(predicted, test.set[, "age"])



# Inference
write.csv(cpdist(dfit, nodes = nodes(dfit),
                 evidence = ((alcoholscreenscore>0)&(alcoholscreenscore<40)&(hba1ciffcflag>4.5)&(hba1ciffcflag<200)&(totalcost>0)&(totalcost<36487)&(totalattendances>0)&(totalattendances<245)),
                 n=354749), file = "dummy2.csv")






#backup
dataLatent %>% ggplot(aes(x=depSev)) + geom_density()
dataLatent %>%
  gather("Sensor", "Measure", alcoholscreenscore, hba1ciffcflag) %>%
  ggplot(aes(x=Measure, color=Sensor, fill=Sensor)) + 
  geom_density(alpha=0.5)
ggplot(dataLatent, aes(x=mhTx)) + geom_bar()
bn.fit.barchart(dfit$aAndE)

