library(visNetwork)
library(bnlearn)
library(rbmn)
library(Rgraphviz)
library(gRain)
library(RPostgreSQL)

# plot network function
# using the visNetwork package to plot the network
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


# Demographic risk factors
cptage = matrix(c(0.023752969,0.047505938,0.16627078,0.2375297,0.19002375,0.14251782,0.11876485,0.047505938,0.023752969,0.0023752968), ncol = 10, dimnames = list(NULL, c("(0,10]","(10,20]","(20,30]","(30,40]","(40,50]","(50,60]","(60,70]","(70,80]","(80,90]","(90,100]")))
cptethnicity = matrix(c(0.35, 0.30, 0.35), ncol = 3, dimnames = list(NULL, c("1", "4", "others")))
cptgender = matrix(c(0.50, 0.50), ncol = 2, dimnames = list(NULL, c("female", "male")))
cptdeprivation = matrix(c(0.20, 0.40, 0.40), ncol = 3, dimnames = list(NULL, c("1", "2", "others")))
cptsmoking = matrix(c(0.8, 0.2), ncol = 2, dimnames = list(NULL, c("no", "yes")))

# Depression severity
cptdepSev = c(1,0,0,0.9,0.09,0.01,0.9,0.09,0.01,0.8,0.15,0.05,0.9,0.05,0.05,0.7,0.2,0.1,0.7,0.2,0.1,0.73,0.18,0.09,0.7,0.2,0.1,0.7,0.2,0.1,
              1,0,0,0.9,0.09,0.01,0.9,0.09,0.01,0.8,0.15,0.05,0.9,0.05,0.05,0.58,0.25,0.17,0.73,0.18,0.09,0.7,0.2,0.1,0.7,0.2,0.1,0.33,0.33,0.34)
dim(cptdepSev) = c(3, 10, 2)
dimnames(cptdepSev) = list("depSev" = c("low", "medium", "high"), "age" = c("(0,10]","(10,20]","(20,30]","(30,40]","(40,50]","(50,60]","(60,70]","(70,80]","(80,90]","(90,100]"),"gender" = c("female", "male"))

cptdepressionflag = c(0.999, 0.001,
                      0.1, 0.9,
                      0.0, 1.0)
dim(cptdepressionflag) = c(2, 3)
dimnames(cptdepressionflag) = list("depressionflag" = c("no", "yes"), "depSev" =  c("low", "medium", "high"))

cptmhclusteredyes = c(1.0, 0.0,
                      0.93, 0.07,
                      0.9, 0.1)
dim(cptmhclusteredyes) = c(2, 3)
dimnames(cptmhclusteredyes) = list("mhclusteredyes" = c("no", "yes"), "depSev" =  c("low", "medium", "high"))


cptclustercode = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                   0.001507585,0.0021671534,0.0033920663,0.026288513,0.05370772,0.16941488,0.033072643,0.09243381,0.051352117,0.029492132,0.02920946,0.07566192, 0.05512108,0.014227834,0.003297842,0.005841892,0.0050880997,0.041081697,0.019881276,0.013945162,0.0033920663,0.27042305,0.0,  
                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,                 
                   0.0015075852,0.0021671536,0.003392066,0.026288515,0.05370772,0.16941486,0.033072647,0.0924338,0.051352113,0.029492132,0.02920946,0.07566192,0.055121075,0.0142278345,0.0032978426,0.005841892, 0.0050880997,0.041081693,0.019881278,0.013945162,0.003392066,0.27042305,0.0,
                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                   0.001507585,0.0021671534,0.0033920663,0.026288515,0.05370772,0.16941488,0.033072647,0.09243381,0.051352113,0.029492132,0.02920946,0.07566193, 0.05512108,0.014227833,0.003297842,0.005841892,0.0050880993, 0.041081693, 0.01988128, 0.013945161,0.0033920663,0.27042305,0.0)
dim(cptclustercode) = c(23, 3, 2)
dimnames(cptclustercode) = list("clustercode" = c("0","1","2","3","4","5","6","7","8","10","11","12","13","14","15","16","17","18","19","20","21","99","nocluster"), "depSev" =  c("low", "medium", "high"),"mhclusteredyes" = c("no", "yes"))




# Mental Health Treatment 
cptmhTx = c(0.85,0.13,0.02,
                   0.9,0.09,0.01)
dim(cptmhTx) = c(3, 2)
dimnames(cptmhTx) = list("mhTx" = c("low", "medium", "high"), "gender" =  c("female", "male"))

cptantidepressantrxflag = c(0.99, 0.01,
                            0.4, 0.6,
                            0, 1)
dim(cptantidepressantrxflag) = c(2, 3)
dimnames(cptantidepressantrxflag) = list("antidepressantrxflag" = c("no", "yes"), "mhTx" = c("low", "medium", "high"))



cptIAPTactivity = c(0.999, 0.001,
                            0.6, 0.4,
                            0.2, 0.8)
dim(cptIAPTactivity) = c(2, 3)
dimnames(cptIAPTactivity) = list("IAPTactivity" = c("no", "yes"), "mhTx" = c("low", "medium", "high"))



cptpsychinpatient  = c(1, 0,
                       0.95, 0.05,
                       0.7, 0.3)
dim(cptpsychinpatient) = c(2, 3)
dimnames(cptpsychinpatient) = list("psychinpatient" = c("no", "yes"), "mhTx" = c("low", "medium", "high"))



# Care for type 2 diabetes
cptdiabCare = c(0.9, 0.1, 0.0, 
                 0.8, 0.2, 0.0, 
                 0.6, 0.3, 0.1,
                 0.7,0.15,0.15,
                 0.5, 0.4,0.1,
                 0.3, 0.5,0.2,
                 0.75, 0.15,0.1,
                 0.3, 0.4,0.3,
                 0.2, 0.3,0.5)
dim(cptdiabCare) = c(3, 3, 3)
dimnames(cptdiabCare) = list("diabCare" = c("low", "medium", "high"), "deprivation" =  c("1", "2", "others"),"ethnicity" = c("1", "4", "others"))

cptdiabcareplanflag  = c(0.5,0.5,
                         0.3, 0.7,
                         0.1, 0.9)
dim(cptdiabcareplanflag) = c(2, 3)
dimnames(cptdiabcareplanflag ) = list("diabcareplanflag" = c("no", "yes"), "diabCare" = c("low", "medium", "high"))


cptdiabMDTflag   = c(1.0,0.0,
                     0.6, 0.4,
                     0.1, 0.9)
dim(cptdiabMDTflag) = c(2, 3)
dimnames(cptdiabMDTflag ) = list("diabMDTflag" = c("no", "yes"), "diabCare" = c("low", "medium", "high"))

cptdiabfootexamflag   = c(0.7,0.3,
                          0.7, 0.3,
                          0.0, 1.0)
dim(cptdiabfootexamflag) = c(2, 3)
dimnames(cptdiabfootexamflag ) = list("diabfootexamflag" = c("no", "yes"), "diabCare" = c("low", "medium", "high"))

cptdigretinalflag   = c(0.9,0.1,
                        0.6, 0.4,
                        0.1, 0.9)
dim(cptdigretinalflag) = c(2, 3)
dimnames(cptdigretinalflag ) = list("digretinalflag" = c("no", "yes"), "diabCare" = c("low", "medium", "high"))



# Mental health Comorbidity
cptmhC = c(0.6, 0.35, 0.05, 
                     0.25, 0.35, 0.4, 
                     0.4, 0.4, 0.2,
                     0.25, 0.35, 0.4)
dim(cptmhC) = c(3, 2, 2)
dimnames(cptmhC) = list("mhC" = c("low", "medium", "high"), "smoking" =  c("no", "yes"),"gender" =  c("female", "male"))


cptanxietyflag  = c(0.95,0.05,
                    0.7, 0.3,
                    0.5, 0.5)
dim(cptanxietyflag) = c(2, 3)
dimnames(cptanxietyflag ) = list("anxietyflag" = c("no", "yes"), "mhC" = c("low", "medium", "high"))

cptmentalhealthflag  = c(1.0,0,
                         0.997, 0.003,
                         0.9, 0.1)
dim(cptmentalhealthflag) = c(2, 3)
dimnames(cptmentalhealthflag ) = list("mentalhealthflag" = c("no", "yes"), "mhC" = c("low", "medium", "high"))

cptpersonalitydisordersflag  = c(1.0,0,
                                 0.999, 0.001,
                                 0.95, 0.05)
dim(cptpersonalitydisordersflag) = c(2, 3)
dimnames(cptpersonalitydisordersflag ) = list("personalitydisordersflag" = c("no", "yes"), "mhC" = c("low", "medium", "high"))


distalcoholscreenscore = list(coef = matrix(c(3, 10, 20), ncol = 3,
                                            dimnames = list(c("(Intercept)"), NULL)),
                              sd = c(10, 20, 20))


#distalcoholscreenscore = list(coef = matrix(c(3, 1, 10, 1, 20, 1), ncol = 3,
#                                            dimnames = list(c("(Intercept)", "mhC"), NULL)),
#                              sd = c(100, 400, 400))

# Control of type 2 diabetes
# diabCont
cptdiabCont = c(1,0,0.0,
                    0.92,0.08,0.0,
                    0.75,0.25,0.0,
                    0.5,0.5,0,
                    0.25,0.75,0,
                    0.078,0.922,0,
                    0.01, 0.98,0.01,
                    0, 0.92,0.08,
                    0,0.75,0.25,
                    
                    
                    0.75,0.25,0,
                    0.5,0.5,0,
                    0.25, 0.75,0,
                    0.08, 0.92,0,
                    0.01, 0.98,0.01,
                    0, 0.92,0.08,
                    0,0.75,0.25,
                    0,0.5,0.5,
                    0,0.25,0.75,
                    
                    0.25,0.75,0,
                    0.08, 0.92,0,
                    0.01, 0.98,0.01,
                    0, 0.92,0.08,
                    0,0.75,0.25,
                    0,0.5,0.5,
                    0.25,0.75,0,
                    0, 0.08, 0.92,
                    0, 0, 1)
dim(cptdiabCont) = c(3, 3, 3, 3)
dimnames(cptdiabCont) = list("diabCont" = c("low", "medium", "high"),
                                 "diabCare" =  c("low", "medium", "high"),
                                 "depSev" =  c("low","medium","high"),
                                 "mhTx" =  c("low", "medium", "high"))


disthba1ciffcflag = list(coef = matrix(c(130, 80, 30), ncol = 3,
                                       dimnames = list(c("(Intercept)"), NULL)),
                         sd = c(200, 200, 200))



#disthba1ciffcflag = list(coef = matrix(c(130, 1, 80, 1, 30, 1), ncol = 3,
#                                       dimnames = list(c("(Intercept)", "diabCont"), NULL)),
#                         sd = c(2500, 2500, 2500))

cptdiabeyecompflag  = c(0.7,0.3,
                        0.9, 0.1,
                        0.995, 0.005)
dim(cptdiabeyecompflag) = c(2, 3)
dimnames(cptdiabeyecompflag ) = list("diabeyecompflag" = c("no", "yes"), 
                                     "diabCont" = c("low", "medium", "high"))


cptdiablimbcompflag   = c(0.45,0.55,
                          0.6, 0.4,
                          0.9, 0.1)
dim(cptdiablimbcompflag) = c(2, 3)
dimnames(cptdiablimbcompflag ) = list("diablimbcompflag" = c("no", "yes"), "diabCont" = c("low", "medium", "high"))




cptaAndE = c(0.95,0.05,0.0,
                    0.96,0.04,0.0,
                    0.97,0.03,0.0,
                    0.18,0.82,0,
                    0.26,0.74,0,
                    0.5,0.5,0,
                    0, 0.34, 0.66,
                    0, 0.58,0.42,
                    0.025,0.95,0.025,
                    
                    
                    0.82,0.18,0,
                    0.74,0.26,0,
                    0.5,0.5,0,
                    0.05, 0.9,0.05,
                    0.03, 0.94,0.03,
                    0.025, 0.95,0.025,
                    0,0.18,0.82,
                    0,0.26,0.74,
                    0,0.5,0.5,
                    
                    0.66,0.34,0,
                    0.42, 0.58,0,
                    0.025, 0.95,0.025,
                    0, 0.82,0.18,
                    0,0.73,0.27,
                    0,0.5,0.5,
                    0.0,0.05,0.95,
                    0, 0.04, 0.96,
                    0, 0.03, 0.97)
dim(cptaAndE) = c(3, 3, 3, 3)
dimnames(cptaAndE) = list("aAndE" = c("low", "medium", "high"),
                                 "deprivation" =  c("1", "2", "others"),
                                 "mhC" =  c("low","medium","high"),
                                 "diabCont" =  c("low", "medium", "high"))



disttotalcost = list(coef = matrix(c(100, 250, 500), ncol = 3,
                                            dimnames = list(c("(Intercept)"), NULL)),
                              sd = c(2500, 5000, 500000))


disttotalattendances = list(coef = matrix(c(0, 1.3, 2), ncol = 3,
                                   dimnames = list(c("(Intercept)"), NULL)),
                     sd = c(1.8, 2.5, 100))



net = model2network("[age][gender][ethnicity][deprivation][smoking][depSev|gender:age][depressionflag|depSev][mhclusteredyes|depSev][clustercode|depSev:mhclusteredyes][mhTx|gender][antidepressantrxflag|mhTx][diabCare|deprivation:ethnicity][mhC|smoking:gender][diabCont|mhTx:depSev:diabCare][aAndE|diabCont:mhC:deprivation][IAPTactivity|mhTx][psychinpatient|mhTx][diabcareplanflag|diabCare][diabMDTflag|diabCare][diabfootexamflag|diabCare][digretinalflag|diabCare][diabeyecompflag|diabCont][diablimbcompflag|diabCont][anxietyflag|mhC][mentalhealthflag|mhC][personalitydisordersflag|mhC][hba1ciffcflag|diabCont][alcoholscreenscore|mhC][totalcost|aAndE][totalattendances|aAndE]")

dfit = custom.fit(net, dist = list(age = cptage, gender = cptgender,
                                   ethnicity=cptethnicity,deprivation= cptdeprivation,
                                   smoking =cptsmoking,depSev = cptdepSev,
                                   depressionflag = cptdepressionflag, mhclusteredyes=cptmhclusteredyes,
                                   clustercode= cptclustercode,mhTx=cptmhTx,
                                   antidepressantrxflag = cptantidepressantrxflag,diabCare=cptdiabCare, mhC=cptmhC,
                                   diabCont=cptdiabCont,aAndE=cptaAndE,
                                   IAPTactivity=cptIAPTactivity, psychinpatient=cptpsychinpatient,
                                   diabcareplanflag=cptdiabcareplanflag,diabMDTflag=cptdiabMDTflag,
                                   diabfootexamflag=cptdiabfootexamflag,digretinalflag=cptdigretinalflag,
                                   diabeyecompflag=cptdiabeyecompflag,diablimbcompflag=cptdiablimbcompflag,
                                   anxietyflag=cptanxietyflag,mentalhealthflag=cptmentalhealthflag,personalitydisordersflag=cptpersonalitydisordersflag,
                                   hba1ciffcflag=disthba1ciffcflag,alcoholscreenscore=distalcoholscreenscore,
                                   totalcost=disttotalcost,totalattendances=disttotalattendances
                                   ))
plot.network(net)


write.csv(cpdist(dfit, nodes = nodes(dfit),
       evidence = ((alcoholscreenscore>0)&(alcoholscreenscore<40)&(hba1ciffcflag>4.5)&(hba1ciffcflag<200)&(totalcost>0)&(totalcost<36487)&(totalattendances>0)&(totalattendances<245)),
       n=354749), file = "dummy2.csv")


# write.csv(cpdist(dfit, nodes = c("age", "gender","ethnicity","deprivation","smoking","depSev","mhC","mhTx",
#                        "diabCare","antidepressantrxflag","depressionflag","mhclusteredyes","diabCont","aAndE","clustercode"), 
#                  evidence = TRUE, n=100000),file = "dummy2.csv")








##  weighted function
cptA = matrix(c(0.35, 0.50, 0.15), ncol = 3, dimnames = list(NULL, c("low", "medium", "high")))
cptB = matrix(c(0.1, 0.30, 0.6), ncol = 3, dimnames = list(NULL, c("low", "medium", "high")))

cptC = c(cptA[1,1]*3+cptB[1,1]*2, 0,                       0, 
         cptA[1,1]*3,             cptB[1,2]*2,             0, 
         cptA[1,1]*3,             0,                       cptB[1,3]*2,
         
         cptB[1,1]*2,             cptA[1,2]*3,             0, 
         0,                       cptA[1,2]*3+cptB[1,2]*2, 0,
         0,                       cptA[1,2]*3,             cptB[1,3]*2,
         
         cptB[1,1]*2,             0,                       cptA[1,3]*3, 
         0,                       cptB[1,2]*2,             cptA[1,3]*3,
         0,                       0,                       cptA[1,3]*3 + cptB[1,3]*2)
dim(cptC) = c(3, 3, 3)
dimnames(cptC) = list("C" = c("low", "medium", "high"), 
                      "A" =  c("low", "medium", "high"),
                      "B" = c("low", "medium", "high"))



cptC = rep(0, 3*3*3)

## assignB
for (i in 1:length(cptC)){
  if (i%%9){
    
  }
}
## assign A
for (i in 1:length(cptC)){
  if (i%/%9){
    
  }
}

for (i in 1:(3*3*3)){
  # for cptA
  print(i)
  print(i%/%9)
  # for cptB
  print(i%%9)
  print("====")
}
