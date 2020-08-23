library (ggplot2)
library(caTools)
library(Amelia)
library(ggplot2)
library(VIM)
library(mice)
library(caret)
library(AppliedPredictiveModeling)
library(Hmisc)
library(RANN)
library(randomForest)
library(Boruta)
library(FSelector)
library(mlr)
library(corrplot)
library(missMDA)
library(tidyr)
library (dplyr)
library (broom)
library(DMwR)
library(corrplot)
library(PerformanceAnalytics)
library(heuristica)
library(irr)
library(lpSolve)
library(ggfortify)
library(ggpubr)
library("cowplot")
library(magrittr)
library("ggpubr")
df <- read.csv("/Users/yazdavar/code/interview/coding_interview/tempus_data/data_merge.csv", header = TRUE,sep="\t", na.strings =c("")  )

sort(sapply(df,function(x) sum(is.na(x))),decreasing = TRUE)
missmap(df, main = "Missing values vs observed")

# Filling missing values in sleep
md.pattern(df)

str(df)
df$comorbidity_index<-as.factor(df$comorbidity_index)
summary(df$comorbidity_index)
sum(is.na(df$target_label))


#--------Sampling---------------
yes_df<- filter(df, df$target_label == "Cases")
no_df<- filter(df, df$target_label == "Contorl")

#------t-test--------
yes_df_sample <- sample_n(yes_df, 381)
no_df_sample <- sample_n(no_df, 381)
t_test <- t.test(yes_df_sample$exercise_frequency ,no_df_sample$ exercise_frequency )
p <- t_test$p.value
p <- p.adjust(p, method = "bonferroni")
t_test
p

merged_df <- rbind(yes_df_sample, no_df_sample)
#--------Chi^2----------

tem_table <- table(df$target_label, droplevels(df$race))
tem_table
chisq <- chisq.test(tem_table)
chisq
corrplot(chisq$residuals, is.cor = FALSE)
p
#------PDF-------------
summary(merged_age_categories)

p <- ggplot(merged_df, aes(x=exercise_frequency, colour=target_label)) + geom_density()+ labs(x = "exercise_frequency Distribution", y = "PDF" )  + guides(fill=guide_legend("my awesome title"))+ 
  theme_classic()#+ theme(axis.text=element_text(size=12),
p + theme(
  plot.title = element_text(color="black", size=23, face="bold.italic"),
  axis.title.x = element_text(color="black", size=23, face="bold"),
  axis.title.y = element_text(color="black", size=23, face="bold"),
  legend.text = element_text(size=20,  face="bold") ,
  legend.title = element_text(size=23,  face="bold"),
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 15)
)                        #axis.title=element_text(size=14,face="bold"))

#----gender quick anlaysis

tem_table <- table(df$target_label, droplevels(df$gender))
tem_table
chisq <- chisq.test(tem_table)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


#----Smoke anlaysis quick anlaysis
df_smoke <- filter(df, df$smoking_status != "unknown")

df_smoke <-  df_smoke[!is.na(df_smoke$smoking_status),]
tem_table <- table(df_smoke$target_label, droplevels(df_smoke$smoking_status))
tem_table
chisq <- chisq.test(tem_table)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


#----Race anlaysis quick anlaysis
#df_smoke <- filter(df, df$smoking_status != "unknown")

df_race <-  df[!is.na(df$race),]
tem_table <- table(df_race$target_label, droplevels(df_race$race))
tem_table
chisq <- chisq.test(tem_table)
chisq
corrplot(chisq$residuals, is.cor = FALSE)

#-------------------
percentage_demographic = subset(df, select = c(gender))

no_gender <- filter(no_gender, no_gender$Human.Judge.for.Gender != "can not decide")
yes_df_has_face_feature$Human.Judge.for.Gender

#------------Function for showing every column as percentage and count--------------
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  res
}
do.call(rbind,lapply(percentage_demographic,tblFun))
#-----------------------3 way-----------------------------
merged_df <- rbind(yes_df_sample, no_df_sample)
merged_df$age_cat <-cut(merged_df$age, c(14,19,23,34,46,60))
merged_df <-  merged_df[!is.na(merged_df$age_cat),]
str(merged_df)

mytable <- xtabs(~comorbidity_index+age_cat+target_label, merged_df)
summary(mytable)
mytable_prop <- prop.table ( mytable,1)*100
mytable_prop
#----------------------
#------------------drawing 2 way_contigency table-------------
#1st:  Gender, EThinicty
mytable_yes <- table( df$institution, droplevels(df$disease_sub_type)) 
ftable(mytable_yes)
prop.table(mytable_yes)*100

df$age_cat <-cut(df$age, c(14,19,23,34,46,60))
summary(df$age_cat)
df_age_cat <-  df[!is.na(df$age_cat),]
mytable_yes <- table( df$gender, droplevels(df$smoking_status)) 
ftable(mytable_yes)
prop.table(mytable_yes)*100


#colours <- c("red", "orange", "blue", "yellow", "green")
library(RColorBrewer)
display.brewer.all()
barplot(prop.table(mytable_yes)*100 ,beside = T, main = "Gender vs Ethinicty",col=brewer.pal(3,"Blues"))
legend("topright", c("Asian","Black","White"), cex=0.8, bty="n", fill =brewer.pal(3,"Blues"))
#---------Box Plot-----
df$age_cat <-cut(df$age, c(14,19,23,34,46,60))
summary(df$age_cat)
df_age_cat <-  df[!is.na(df$age_cat),]

Analytic_age <- ggplot(df_age_cat, aes(x=age_cat, y=Authentic)) + geom_boxplot()
Analytic_age



Analytic_age <- ggplot(df_age_cat, aes(x=age_cat, y=comorbidity_index, fill=target_label)) + stat_boxplot(geom="errorbar", width=.5,position = position_dodge(width = .75)) + geom_boxplot() + stat_summary(fun.y= mean, geom="point", shape=23, size=3 , position = position_dodge(width = .75)) +stat_summary(fun.y=mean, geom="smooth", linetype="dotdash", aes(color=paste("mean", target_label),group=target_label), lwd=0.65) +stat_summary(fun.y=mean, geom="smooth", linetype="F1", aes(color=paste("mean", target_label),group=1), lwd=0.75) + theme(legend.position="none")
#+ geom_jitter(position = position_jitter(0.1))
Analytic_age
Analytic_age <- Analytic_age + theme(
  plot.title = element_text(color="black", size=23, face="bold.italic"),
  axis.title.x = element_text(color="black", size=23, face="bold"),
  axis.title.y = element_text(color="black", size=23, face="bold"),
  legend.text = element_text(size=25,  face="bold") ,
  legend.title = element_text(size=25,  face="bold"),
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 15, face = "bold")
) 
Analytic_age <- Analytic_age  
Analytic_age
#------------End of Analytics--------------------------
str(df)
dmy <- dummyVars(" ~ .", data = df,fullRank = T)
df_dummified <- data.frame(predict(dmy, newdata = df))
summary(df_dummified)
str(df_dummified)
#------Imputation-----

#imputation by Hmsic
str(df)
library(Hmisc)

df.mis.model = preProcess(df, "knnImpute")
df.miss.pred = predict(df.mis.model, df)
str(df.miss.pred)

sum(is.na(df.miss.pred))

#check the result of imputation
sort(sapply(df,function(x) sum(is.na(x))))
missmap(df, main = "Missing values vs observed")
#---------------End of Analytics----------------
df_up_samp <- read.csv("/Users/yazdavar/code/interview/coding_interview/tempus_data/train_data_upsampled_saved.csv", header = TRUE,sep="\t", na.strings =c("")  )


#-------------spliting-------------------
#cross-validation in order to avoid overfitting
# I used createDataPartition() which it makes sure the distibution of outcome variable will be similar in test and train
index <- createDataPartition(df_up_samp$target_label, p=0.8, list=FALSE)
trainSet <- df_up_samp[ index,]
testSet <- df_up_samp[-index,]



set.seed(100)
boruta.train <- Boruta(target_label~., data = trainSet, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))

axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

#------------#-----------------------Fitting model-----------------------------------------------------------------
