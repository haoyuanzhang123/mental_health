library(tidyverse)
library(lubridate)
library(bnlearn)

dataRaw <- read_csv("/Users/Haoyuan/Desktop/dummy2.csv")
dataRaw <- dataRaw[,-1]

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
      ifelse (gender == "female" & age =="0_10",
              sample(c("low", "medium", "high"),1000,replace=TRUE,
                     prob=c(1.0, 0.0, 0.0)),
              ifelse (gender == "female" & (age =="11_20" | age =="21_30"),
                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                             prob=c(0.9, 0.09, 0.01)),
                      ifelse (gender == "female" & age =="31_40",
                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                     prob=c(0.8, 0.15, 0.05)),
                              ifelse (gender == "female" & age =="41_50",
                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                             prob=c(0.9, 0.05, 0.05)),
                                      ifelse (gender == "female" & (age =="51_60" |age =="61_70" |age =="71_80"|age =="81_90"|age =="91_100"),
                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                     prob=c(0.7, 0.2, 0.1)),                                  
              
              ifelse (gender == "male" & age =="0_10",
                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                             prob=c(1.0, 0.0, 0.0)),
                      ifelse (gender == "male" & (age =="11_20" | age =="21_30"),
                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                     prob=c(0.9, 0.09, 0.01)),
                              ifelse (gender == "male" & age =="31_40",
                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                             prob=c(0.8, 0.15, 0.05)),
                                      ifelse (gender == "male" & age =="41_50",
                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                     prob=c(0.9, 0.05, 0.05)),
                                              ifelse (gender == "male" & age =="51_60",
                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                             prob=c(0.8, 0.15, 0.05)),
                                                      ifelse (gender == "male" & (age =="61_70" |age =="71_80"|age =="81_90"|age =="91_100"),
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
      ifelse (ethnicity == "1" & deprivation == "1",
              sample(c("low", "medium", "high"),1000,replace=TRUE,
                     prob=c(0.9, 0.1, 0.0)), 
              ifelse (ethnicity == "1" & deprivation == "2",
                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                             prob=c(0.8, 0.2, 0.0)), 
                      ifelse (ethnicity == "1" & deprivation == "others",
                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                     prob=c(0.6, 0.3, 0.1)), 
                              
                              ifelse (ethnicity == "4" & deprivation == "1",
                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                             prob=c(0.7, 0.15, 0.15)), 
                                      ifelse (ethnicity == "4" & deprivation == "2",
                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                     prob=c(0.5, 0.4, 0.1)), 
                                              ifelse (ethnicity == "4" & deprivation == "others",
                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                             prob=c(0.3, 0.5, 0.2)), 
                                                      
                                                      ifelse (ethnicity == "others" & deprivation == "1",
                                                              sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                     prob=c(0.75, 0.15, 0.1)), 
                                                              ifelse (ethnicity == "others" & deprivation == "2",
                                                                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                                                                             prob=c(0.3, 0.4, 0.3)), 
                                                                      ifelse (ethnicity == "others" & deprivation == "others",
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
      ifelse (gender == "female" & smoking =="no", 
              sample(c("low", "medium", "high"),1000,replace=TRUE,
                     prob=c(0.6, 0.35, 0.05)), 
              ifelse (gender == "female" & smoking =="yes", 
                      sample(c("low", "medium", "high"),1000,replace=TRUE,
                             prob=c(0.25, 0.35, 0.4)),
                      ifelse (gender == "male" & smoking =="no", 
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

factorList = c("age", "antidepressantrxflag", "anxietyflag", "clustercode", "depressionflag",
               "deprivation", "diabcareplanflag", "diabeyecompflag", "diabfootexamflag", "diablimbcompflag",
               "diabMDTflag", "digretinalflag", "ethnicity", "gender", "IAPTactivity", 
               "mentalhealthflag", "mhclusteredyes", "personalitydisordersflag", "psychinpatient",
               "smoking")
dataLatent[factorList] <- lapply(dataRaw[factorList], factor)  



parametric.em <- function(dag, dataLatent, dataImputed, iter = 5) {
  fitted <- bn.fit(dag, dataImputed, method = "mle")
  for ( i in seq(iter)) {
    complete <- impute(fitted, data = dataLatent, method = "bayes-lw")
    fitted <- bn.fit(dag, complete, method = "mle")
  }
  fitted
}


net = model2network("[age][gender][ethnicity][deprivation][smoking][depSev|gender:age][depressionflag|depSev][mhclusteredyes|depSev][clustercode|depSev:mhclusteredyes][mhTx|gender][antidepressantrxflag|mhTx][diabCare|deprivation:ethnicity][mhC|smoking:gender][diabCont|mhTx:depSev:diabCare][aAndE|diabCont:mhC:deprivation][IAPTactivity|mhTx][psychinpatient|mhTx][diabcareplanflag|diabCare][diabMDTflag|diabCare][diabfootexamflag|diabCare][digretinalflag|diabCare][diabeyecompflag|diabCont][diablimbcompflag|diabCont][anxietyflag|mhC][mentalhealthflag|mhC][personalitydisordersflag|mhC][hba1ciffcflag|diabCont][alcoholscreenscore|mhC][totalcost|aAndE][totalattendances|aAndE]")


dataNA = dataLatent
dataNA[, latentCol] = NA
dataNA[latentCol] <- lapply(dataNA[latentCol], factor, levels = latentStates)  

dfit <- parametric.em(net, dataNA, dataLatent, iter = 5)




dataLatent %>% ggplot(aes(x=depSev)) + geom_density()


dataLatent %>%
  gather("Sensor", "Measure", alcoholscreenscore, hba1ciffcflag) %>%
  ggplot(aes(x=Measure, color=Sensor, fill=Sensor)) + 
  geom_density(alpha=0.5)

ggplot(dataLatent, aes(x=mhTx)) + geom_bar()

bn.fit.barchart(dfit$aAndE)

