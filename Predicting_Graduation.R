library(rpart)
library(maptree)
library(gains)
library(ROCR)
library(Rprofet)

 #--------Data Loading, manipulation-------------
data <- read.csv('admissions_data.csv')
head(data)

#Only accept GPAs between 0-4
subs <- data[which(data$HSGPA > 0 & data$HSGPA <= 4.0),]

#Don't accept NULL ACT scores
subs <- subs[which(subs$ACT != 'NULL'),]

#One student had 'n/a' for enrolled but ended up graduating -> change to '1'
subs$enrolled[which(subs$enrolled == 'n/a')] <- 1

#Remove all rows with blank graduation status (current students)
subs <- subs[which(subs$Graduated != ''),]

#Remove all students who never enrolled, since you have to enroll to have a chance at graduating
subs <- subs[which(subs$enrolled != '0'),]

#Calculate Overall Graduation Pct
  graduated_count <- sum(subs$Graduated == 1)
  total_students <- nrow(subs)
  overall_graduation_pct <- (graduated_count / total_students) * 100
  print(overall_graduation_pct)

#Convert entries to integers
  enrsubs <- subs
  enrsubs$ACT <- as.integer(enrsubs$ACT)
  enrsubs$enrolled <- as.integer(enrsubs$enrolled)
  summary(enrsubs)
  
#--------Exploratory Analysis-------------
#Boxplots of graduation by GPA, ACT score
  myColors <- ifelse(enrsubs$Graduated == 0,'red','green')
  boxplot(enrsubs$ACT ~ enrsubs$Graduated, 
          main="Distribution of ACT Score by College Graduation", 
          col = myColors,
          ylab="ACT Score", 
          xlab="Graduation")
  
  boxplot(enrsubs$HSGPA ~ enrsubs$Graduated, 
          main="Distribution of HS GPA by Gollege Graduation", 
          col = myColors,
          ylab="High School GPA", 
          xlab="Graduation",
          ylim = c(2.6,4.0))

#Plot of gpa, act, and graduation colored
  plot(enrsubs$ACT~enrsubs$HSGPA, xlim = c(2.45,4),ylim = c(14,35), pch = 19,
       col = ifelse(enrsubs$Graduated == 0,'red','green'),
       xlab = "High School GPA",
       ylab = "ACT Score",
       main = "College Graduation by ACT and High School GPA")
  legend("bottomleft", legend = c("Graduated College","Didn't graduate"), fill = c("green", "red"))

#--------Logistic Regression Model------------- 
  logitmodel = glm(Graduated~ACT + HSGPA,family=binomial, data = enrsubs)
  summary(logitmodel)
  
#ACT is the only significant coeficcient... only model based off of that
  logitmodel = glm(Graduated~ACT,family=binomial, data = enrsubs)
  summary(logitmodel)
  
#Plot assigned graduation probabilities of Logistic Regression Model
  plot(Graduated~ACT,enrsubs,
       xlab="ACT Score",ylab="Graduation Likelihood",  
       main = "Logistic Model Predictions", xlim = c(14,35), col = 'white')

  newdat <- data.frame(ACT=seq(min(13), max(36),len=100))
  newdat$vs = predict(logitmodel, newdata=newdat, type="response")
  lines(vs ~ ACT, newdat, col="green4", lwd=2)
  
  abline(h=.5, lty=2, col = 'blue')
  legend('topleft', lty = c(1,2), col = c('green4','blue'), 
         legend = c('Model Prediction', '0.5  Reference Line'))
  
#get our model coefficients
  b0=coef(logitmodel)[1]
  b1=coef(logitmodel)[2]
  b0
  b1
  
#make predicted values for our data
  enrsubs$LogiMod <- predict(logitmodel, enrsubs, type = "response")
  enrsubs$LogiPred <- ifelse(enrsubs$LogiMod<.5, 0, 1)
  
#Make gains table
  logitgains=gains(enrsubs$Graduated,enrsubs$LogiMod,10)
  logitgains #show the table
  
#Make ROC Curve, calculate AUC
  logitresults=as.data.frame(cbind(enrsubs$Graduated,enrsubs$LogiPred))
  LogitModelPreds=prediction(enrsubs$LogiMod, enrsubs$Graduated, label.ordering = NULL)
  Logit.perf = performance(LogitModelPreds, measure = "tpr", x.measure = "fpr")
  plot(Logit.perf, col = "orange",main = 'Logistic ROC curve')
  abline(0,1,lty=2)
  Logit.auc = performance(LogitModelPreds, measure = "auc")
  Logit.auc@y.values #Logistic auc (0.6624532)

#--------Tree Model------------- 
  tree1=rpart(Graduated~ACT+HSGPA+enrolled,data=enrsubs,minbucket=20,cp=.02)
  summary(tree1)

  draw.tree (tree1, cex=1.2, 
             nodeinfo=TRUE, 
             cases="obs",
             digits=2, print.levels=TRUE,
             new=TRUE)

  #make predicted values for our data
  enrsubs$TreeMod <- predict(tree1, newdata = enrsubs)
  enrsubs$TreePred <- ifelse(enrsubs$TreeMod<.53, 0, 1)
  
  #Make gains table
  Treegains=gains(enrsubs$Graduated,enrsubs$TreeMod,10)
  Treegains #show the table
  
#Make ROC curve, calculate AUC
  TreeModelPreds=prediction(enrsubs$TreeMod, enrsubs$Graduated, label.ordering = NULL)
  Tree.perf = performance(TreeModelPreds, measure = "tpr", x.measure = "fpr")
  plot(Tree.perf, col = 'green', main = 'Tree ROC curve')
  abline(0,1,lty=2)
  
  Tree.auc = performance(TreeModelPreds, measure = "auc")
  Tree.auc@y.values #Logistic auc (0.656367)  
  
  
  #--------Binned/WOE Logistic Regression Model------------- 
# Bin the ACT and GPA variables
  enrsubs$ID <- 1:137 #replace ID with a simple number
  mydata=BinProfet(enrsubs,id='ID',target='Graduated',varcol = c(5:6),num.bins = 5)
  summary(mydata)
  table(mydata$HSGPA_Bins)
  table(mydata$ACT_Bins)
  
# Build a logistic model with the bins
  logmodelB=glm(Graduated~HSGPA_Bins+ACT_Bins,family=binomial,data=mydata)
  summary(logmodelB)
  
  
#WOE
  WOEplotter(mydata,target='Graduated',var='HSGPA_Bins')
  WOEplotter(mydata,target = 'Graduated',var='ACT_Bins')
  
#Add WOE values for each student observation
  woedata=WOEProfet(dat=mydata,id='ID',target = 'Graduated', c(3:4))
  
#Combine the bins, WOE observations, and HSGPA/ACT values
  newdata=as.data.frame(cbind(enrsubs$ID,enrsubs$Graduated, enrsubs$HSGPA, enrsubs$ACT,paste(mydata$HSGPA_Bins),paste(mydata$ACT_Bins),
                              woedata$WOE$HSGPA_Bins_WOE, woedata$WOE$ACT_Bins_WOE))
  names(newdata)=c('ID','Graduated','HSGPA','ACT','HSGPA_Bins','ACT_Bins','HSGPA_BINS_WOE','ACT_BINS_WOE')
  head(newdata)
  
#Change datatypes of combined dataframe
  newdata$Graduated=as.numeric(newdata$Graduated)
  newdata$HSGPA=as.numeric(newdata$HSGPA)
  newdata$ACT=as.numeric(newdata$ACT)
  newdata$HSGPA_BINS_WOE=as.numeric(newdata$HSGPA_BINS_WOE)
  newdata$ACT_BINS_WOE=as.numeric(newdata$ACT_BINS_WOE)
  summary(newdata)

  
# Binned Logistic Model, make predictions, build gains table
  logmodel2=glm(Graduated~HSGPA_Bins+ACT_Bins,family=binomial,data=newdata)
  summary(logmodel2)
  mydata$binpred <- predict(logmodelB, mydata, type = "response")
  Bingains=gains(mydata$Graduated, mydata$binpred, 10)
  Bingains #show the table
  
# WOE Logistic Model, make predictions, build gains table
  logmodel3=glm(Graduated~HSGPA_BINS_WOE+ACT_BINS_WOE,family=binomial,data=newdata)
  summary(logmodel3)  
  newdata$woepred <- predict(logmodel3, newdata, type = "response")
  summary(mydata)
  WOEgains=gains(newdata$Graduated,newdata$woepred,10)
  WOEgains #show the table
  
# WOE ROC curve, AUC statistic
  WOEPreds=prediction(newdata$woepred, newdata$Graduated, label.ordering = NULL)
  WOE.perf = performance(WOEPreds, measure = "tpr", x.measure = "fpr")  
  plot(WOE.perf, col = "blue",main = "WOE Logistic ROC Curve")
  abline(0,1,lty=2)
  WOE.auc = performance(WOEPreds, measure = "auc")
  WOE.auc@y.values # 0.687617
  
# Binned ROC curve, AUC statistic
  BinPreds=prediction(mydata$binpred, mydata$Graduated, label.ordering = NULL)
  Bin.perf = performance(BinPreds, measure = "tpr", x.measure = "fpr")
  plot(Bin.perf, col = 'red', main = 'Binned Logistic ROC Curve')
  abline(0,1,lty=2)
  
  Bin.auc = performance(BinPreds, measure = "auc")
  Bin.auc@y.values # 0.6958099
#---------------------------------------------------------
  
  
#Comparative ROC Plot
  plot(WOE.perf, col = "blue",main = "ROC Curve Comparison")
  abline(0,1,lty=2) 
  plot(Bin.perf, add = TRUE, col = 'red')
  plot(Logit.perf, col = "orange",add = 'True')
  plot(Tree.perf, add = TRUE, col = 'green')
  legend("topleft", 
         legend = c("WOE Logistic Model",
                    "Binned Logistic Model",
                    "Logistic Model",
                    "Tree Model"), 
         fill = c("blue", "red", "orange", "green"))
  
  
  
  
  
  
