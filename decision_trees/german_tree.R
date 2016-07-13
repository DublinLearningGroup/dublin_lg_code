rm(list = ls())

readkey <- function(){
  cat("[press [enter] to continue]")
  number <- scan(n=1)
}
split.fun <- function(x, labs, digits, varlen, faclen){
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=25), collapse="\n")
  }
  labs
}
treeplot_pres=function(x,extra=0,tweak=1){
  rpart.plot(x,
             split.fun = split.fun,
             varlen = 0,
             tweak = tweak,
             box.col = c("red","palegreen3")[x$frame$yval],
             type = 2,
             branch = 1,
             extra = extra,
             lwd=2)
}


library(rpart)
library(rattle)
library(caret)
library(PerformanceAnalytics)
library(dplyr)
library(rpart.plot)
ger_cred=read.csv("german_credit.csv")

PerformanceAnalytics::chart.Correlation(ger_cred[c(1,2:11)])
PerformanceAnalytics::chart.Correlation(ger_cred[c(1,12:21)])
# for(n in seq(21)){
#   hist(ger_cred[,n],main = paste(n,names(ger_cred)[n]),breaks = 40)
#   readkey()
# }


ger_cred$Creditability=as.factor(ger_cred$Creditability)
ger_cred$Telephone=as.factor(ger_cred$Telephone)
ger_cred$Sex...Marital.Status=as.factor(ger_cred$Sex...Marital.Status)
ger_cred$Telephone=as.factor(ger_cred$Telephone)
ger_cred$Concurrent.Credits=as.factor(ger_cred$Concurrent.Credits)
ger_cred$Type.of.apartment=as.factor(ger_cred$Type.of.apartment)
ger_cred$Foreign.Worker=as.factor(ger_cred$Foreign.Worker)


DT.cred=rpart(Creditability ~ .,data = ger_cred,cp=0.00000001)
fancyRpartPlot(DT.cred,tweak=4)

fancyRpartPlot(prune(DT.cred,cp = .01),tweak=2.5)


DT.pred=predict(DT.cred,ger_cred)
CM=table(round(DT.pred[,2]),ger_cred$Creditability)
confusionMatrix(CM,positive = "1")

########## make a real attempt

partition=createDataPartition(y = ger_cred$Creditability ,
                              p = .75,
                              list = FALSE)

training <- ger_cred[ partition,]
testing <- ger_cred[- partition,]

DT.cred=rpart(Creditability ~ .,data = training,cp=0.00000001)
DT.pred=predict(DT.cred,testing)
CM=table(round(DT.pred[,2]),testing$Creditability)
confusionMatrix(CM,positive = "1")




############TUNING##########
###code for trialing multiple model fits and extract CP vs Kappa score for each run

DF_cp_opto=data.frame(cparam=0,y_kappa=0,n_leaf=0,trail=0)

trial=0
for(k in seq(30)){
  trial=trial+1
  print(trial)
  partition=createDataPartition(y = ger_cred$Creditability,
                                p = .75,
                                list = FALSE)


  training <- ger_cred[ partition,]
  #deal with class imballances with Smote resampling
  #training=SMOTE(Creditability ~ .,training)
  testing <- ger_cred[- partition,]



  DT.cred.red=rpart(Creditability ~ .,data=training,control = rpart.control(cp = 0.000001))



  n_leaf=integer()
  y_kappa=numeric()
  cparam=seq(0.000001,0.02,.0001)
  for(n in cparam){
    a=prune(DT.cred.red,cp =n)
    n_leaf=max(a$where)
    preds=round(predict(a,testing))
    #preds[1]=1
    DT.CM=table(preds[,2],testing$Creditability)
    b=confusionMatrix(DT.CM)
    y_kappa=as.numeric(b$overall[2])
    DF_cp_opto=rbind(DF_cp_opto,c(n,y_kappa,n_leaf,trial))
  }
}
y_kappa_mean=DF_cp_opto %>% group_by(cparam) %>% summarise(ybar_kappa=mean(y_kappa),ySD_kappa=var(y_kappa))
n_leaf_mean=DF_cp_opto %>% group_by(cparam) %>% summarise(n_leaf=mean(n_leaf))

plot(DF_cp_opto$cparam,DF_cp_opto$n_leaf,main = "cp tuning for 40 leaves")
plot(n_leaf_mean$cparam,n_leaf_mean$n_leaf,main = "mean tree size to tune")

plot(DF_cp_opto$cparam,DF_cp_opto$y_kappa,main = "cp tuning for best model")


ggplot(y_kappa_mean, aes(x = cparam, y = ybar_kappa)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = y_kappa_mean$ybar_kappa- sqrt(y_kappa_mean$ySD_kappa), ymax = y_kappa_mean$ybar_kappa+ sqrt(y_kappa_mean$ySD_kappa)))



fancyRpartPlot(prune(DT.cred.red,cp = .012),tweak=1.5)

n=.012
a=prune(DT.cred.red,cp =n)
n_leaf=max(a$where)
preds=round(predict(a,testing)[,2])
DT.CM=table(preds,testing$Creditability)
confusionMatrix(DT.CM,positive = "1")



###### show off VARIABLE importance ###

as.data.frame(DT.cred$variable.importance)


cp_opto=.005
########################### VARIABLE SELECTION #######################
trial=0
if(exists("a")){rm(a)}
for(k in seq(30)){
  trial=trial+1
  print(trial)
  partition=createDataPartition(y = ger_cred$Creditability,
                                p = .75,
                                list = FALSE)


  training <- ger_cred[ partition,]
  testing <- ger_cred[- partition,]



  DT.var_sel=rpart(Creditability ~ .,data=training,control = rpart.control(cp = cp_opto))
  if(!exists("a")){
    l=length(names(DT.var_sel$variable.importance))
    if(l<20){v_lim=l}else{v_lim=20}
    a=data.frame(Vars=names(DT.var_sel$variable.importance[1:v_lim]),rank=seq(20,21-v_lim))
    names(a)[2]=paste0("rank",k)
    print(paste("number of unique variables for model: ",nrow(a)))
  }else{
    l=length(names(DT.var_sel$variable.importance))
    if(l<20){v_lim=l}else{v_lim=20}
    b=data.frame(Vars=names(DT.var_sel$variable.importance[1:v_lim]),rank=seq(20,21-v_lim))
    names(b)[2]=paste0("rank",k)
    a=merge(a,b,by="Vars",all = T)
    print(paste("number of unique variables for model: ",nrow(a)))
  }

}
c=cbind(rowSums(a[-1],na.rm = T)/trial,a)
#sort(c$`rowSums(a[-1], na.rm = T)`,decreasing = T)

choice_vars=as.character(c$Vars[order(c$`rowSums(a[-1], na.rm = T)/trial`,decreasing = T)])
choice_vars


NumVars=6
ger_cred_topvars=ger_cred[c(choice_vars[1:NumVars],"Creditability")]
print(paste0(round(100*(1-(as.numeric(nrow(na.omit(ger_cred_topvars)))/as.numeric(nrow(ger_cred)))),4),"% lost due to NA.Omit"))
ger_cred_topvars=na.omit(ger_cred_topvars)  #necessary for caret



########## make a real attempt with variables reduced

partition=createDataPartition(y = ger_cred_topvars$Creditability ,
                              p = .75,
                              list = FALSE)

training <- ger_cred_topvars[ partition,]
testing <- ger_cred_topvars[- partition,]

DT.cred=rpart(Creditability ~ .,data = training,cp=cp_opto)
DT.pred=predict(DT.cred,testing)
CM=table(round(DT.pred[,2]),testing$Creditability)
confusionMatrix(CM,positive = "1")

fancyRpartPlot(DT.cred,tweak=2)

#Final presentation of model
treeplot_pres(DT.cred,tweak = 3)







## in caret we trust
#to try to fix a caret error of invalid factor levels
levels(ger_cred_topvars$Creditability)=c("N","Y")

partition=createDataPartition(y = ger_cred_topvars$Creditability ,
                              p = .75,
                              list = FALSE)

training <- ger_cred_topvars[partition,]
testing <- ger_cred_topvars[- partition,]


fitcontrol <- trainControl(method="cv", number=10,verboseIter = T,classProbs = T)
# train the model
DT_cred_caret <- train(Creditability ~ ., data = training, method = "rpart",
                         tuneLength = 30,
                         metric = "Kappa",
                         trControl = fitcontrol)



print(paste("tuned Cp= ",DT_cred_caret$bestTune))


# summarize results
plot(DT_cred_caret)
confusionMatrix(DT_cred_caret)
preds=predict(DT_cred_caret,testing,type = "prob")

threshold=0.5
pred=factor( ifelse(preds[,"Y"] > threshold, "Y", "N") )
confusionMatrix(pred,testing$Creditability,positive = "Y")


###in progress>>>> play with threshold


threshold=0.2
pred=factor( ifelse(preds[,"Y"] > threshold, "Y", "N") )
confusionMatrix(pred,testing$Creditability,positive = "Y")
