
library("readxl")
data=read_excel('C:/Users/houss/Downloads/a.xlsx')
row_to_keep = which(data$VERDL !="NA" )
suppid=data[c(-row_to_keep),]$SUBJID

for (i in c(suppid)){
  data=data[-which(data$SUBJID==i),]
}

table(is.na(data))


library(questionr)
freq(data$BRONCSEV)


data$VISIT=as.numeric(as.factor(data$VISIT))

data=as.data.frame(sapply(data,as.numeric))

data_analyse_des=read_excel('C:/Users/houss/Downloads/BaseNebulamb_M4M0_décodé.xlsx')
row_to_keep = which(data_analyse_des$VERDL !="NA" )
suppid=data_analyse_des[c(-row_to_keep),]$SUBJID

for (i in c(suppid)){
  data_analyse_des=data_analyse_des[-which(data_analyse_des$SUBJID==i),]
}
data_analyse_des_M0=data_analyse_des[data_analyse_des$VISIT=="M0",]
data_analyse_des_M4=data_analyse_des[data_analyse_des$VISIT=="M4",]


barplot(freq(data_analyse_des_M0$BRONCSEV)[1:5,1],legend.text  =c("absent","léger","modéré (lumière bronchique 2-3 X diamètre artériel)","sévère (lumière bronchique > 3 X diamètre artériel)"),col=c("#B2182B", "#D6604D" ,"#F4A582", "#FDDBC7") )
barplot(freq(data_analyse_des_M4$BRONCSEV)[1:5,1],legend.text  =c("absent","léger","modéré (lumière bronchique 2-3 X diamètre artériel)","sévère (lumière bronchique > 3 X diamètre artériel)"),col=c("#B2182B", "#D6604D" ,"#F4A582", "#FDDBC7") )

library("corrplot")

cor=cor(data)
corrplot(cor)


delta_M0_M4=read.csv('C:/Users/houss/Downloads/deltaM4M0.txt',sep="\t")
row_to_keep = which(delta_M0_M4$deltaPIEGLM4M0 !="NA" )
delta_M0_M4=delta_M0_M4[row_to_keep,]
table(is.na(delta_M0_M4))
library("FactoMineR")
library("factoextra")
res.pca <- PCA(delta_M0_M4[,-c(1)], graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
var$cor
fviz_contrib(res.pca, choice = "var", axes = c(1,2))
colnames(delta_M0_M4)
library(Factoshiny)
Factoshiny(delta_M0_M4[,-c(1)])


shapiro.test(delta_M0_M4$deltaBRONCSEVM4M0)
shapiro.test(delta_M0_M4$deltaBRONCETENDM4M0)
shapiro.test(delta_M0_M4$deltaBRONCSITM4M0)
shapiro.test(delta_M0_M4$deltaMUCETENDM4M0)
shapiro.test(delta_M0_M4$deltaMUCDENSM4M0)
shapiro.test(delta_M0_M4$deltaEPAISBRONCM4M0)
shapiro.test(delta_M0_M4$deltaABPAM4M0)

shapiro.test(delta_M0_M4$deltaFIBREXTM4M0)

shapiro.test(delta_M0_M4$deltaFIBREPAISM4M0)
shapiro.test(delta_M0_M4$deltaCOLLAPSM4M0)
shapiro.test(delta_M0_M4$deltaFIBROSEM4M0)

shapiro.test(delta_M0_M4$deltaASPERGM4M0)

shapiro.test(delta_M0_M4$deltaASPERGCHRM4M0)
shapiro.test(delta_M0_M4$deltaMICROLM4M0)
shapiro.test(delta_M0_M4$deltaNODLM4M0)
shapiro.test(delta_M0_M4$deltaVERDLM4M0)
shapiro.test(delta_M0_M4$deltaPIEGLM4M0)


wilcox.test(delta_M0_M4$deltaBRONCSEVM4M0~1,mu=0)
wilcox.test(delta_M0_M4$deltaBRONCETENDM4M0~1,mu=0)
wilcox.test(delta_M0_M4$deltaBRONCSITM4M0~1,mu=0)
wilcox.test(delta_M0_M4$deltaMUCETENDM4M0~1,mu=0) #
wilcox.test(delta_M0_M4$deltaMUCDENSM4M0~1,mu=0) #
wilcox.test(delta_M0_M4$deltaEPAISBRONCM4M0~1,mu=0) #
wilcox.test(delta_M0_M4$deltaABPAM4M0~1,mu=0)
wilcox.test(delta_M0_M4$deltaFIBREPAISM4M0~1,mu=0)
wilcox.test(delta_M0_M4$deltaCOLLAPSM4M0~1,mu=0)
wilcox.test(delta_M0_M4$deltaFIBROSEM4M0~1,mu=0)
wilcox.test(delta_M0_M4$deltaASPERGCHRM4M0~1,mu=0)
wilcox.test(delta_M0_M4$deltaMICROLM4M0~1,mu=0) #
wilcox.test(delta_M0_M4$deltaNODLM4M0~1,mu=0)
wilcox.test(delta_M0_M4$deltaVERDLM4M0~1,mu=0) #
wilcox.test(delta_M0_M4$deltaPIEGLM4M0~1,mu=0)

library("corrplot")
cor=cor(delta_M0_M4)
corrplot(cor)
#

delta_M4_M10=read.csv('C:/Users/houss/Downloads/deltaM10M4.txt',sep="\t")
row_to_keep = which(delta_M4_M10$deltaPIEGLM10M4 !="NA" )
delta_M4_M10=delta_M4_M10[row_to_keep,]
table(is.na(delta_M4_M10))
library("FactoMineR")
library("factoextra")
res.pca <- PCA(delta_M4_M10[,-c(1)], graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
var$cor
fviz_contrib(res.pca, choice = "var", axes = c(1,2))

library(Factoshiny)
Factoshiny(delta_M0_M4[,-c(1)])


shapiro.test(delta_M4_M10$deltaBRONCSEVM10M4)
shapiro.test(delta_M4_M10$deltaBRONCETENDM10M4)
shapiro.test(delta_M4_M10$deltaBRONCSITM10M4)
shapiro.test(delta_M4_M10$deltaMUCETENDM10M4)
shapiro.test(delta_M4_M10$deltaMUCDENSM10M4)
shapiro.test(delta_M4_M10$deltaEPAISBRONCM10M4)
shapiro.test(delta_M4_M10$deltaABPAM10M4)

shapiro.test(delta_M4_M10$deltaFIBREXTM10M4)

shapiro.test(delta_M4_M10$deltaFIBREPAISM10M4)
shapiro.test(delta_M4_M10$deltaCOLLAPSM10M4)
shapiro.test(delta_M4_M10$deltaFIBROSEM10M4)

shapiro.test(delta_M4_M10$deltaASPERGM10M4)

shapiro.test(delta_M4_M10$deltaASPERGCHRM10M4)

shapiro.test(delta_M4_M10$deltaMICROLM10M4)
shapiro.test(delta_M4_M10$deltaNODLM10M4)
shapiro.test(delta_M4_M10$deltaVERDLM10M4)
shapiro.test(delta_M4_M10$deltaPIEGLM10M4)


wilcox.test(delta_M4_M10$deltaBRONCSEVM10M4~1,mu=0)
wilcox.test(delta_M4_M10$deltaBRONCETENDM10M4~1,mu=0)
wilcox.test(delta_M4_M10$deltaBRONCSITM10M4~1,mu=0)
wilcox.test(delta_M4_M10$deltaMUCETENDM10M4~1,mu=0) #
wilcox.test(delta_M4_M10$deltaMUCDENSM10M4~1,mu=0) #
wilcox.test(delta_M4_M10$deltaEPAISBRONCM10M4~1,mu=0) #
wilcox.test(delta_M4_M10$deltaABPAM10M4~1,mu=0)
wilcox.test(delta_M4_M10$deltaFIBREPAISM10M4~1,mu=0)
wilcox.test(delta_M4_M10$deltaCOLLAPSM10M4~1,mu=0)
wilcox.test(delta_M4_M10$deltaFIBROSEM10M4~1,mu=0)
wilcox.test(delta_M4_M10$deltaMICROLM10M4~1,mu=0)
wilcox.test(delta_M4_M10$deltaNODLM10M4~1,mu=0)
wilcox.test(delta_M4_M10$deltaVERDLM10M4~1,mu=0) #
wilcox.test(delta_M4_M10$deltaPIEGLM10M4~1,mu=0) #

#


delta_M10_M28=read.csv('C:/Users/houss/Downloads/deltaM28M10.txt',sep="\t")
row_to_keep = which(delta_M10_M28$deltaPIEGLM28M10 !="NA" )
delta_M10_M28=delta_M10_M28[row_to_keep,]
table(is.na(delta_M10_M28))
library("FactoMineR")
library("factoextra")
res.pca <- PCA(delta_M10_M28[,-c(1)], graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
var$cor
fviz_contrib(res.pca, choice = "var", axes = c(1,2))

library(Factoshiny)
Factoshiny(delta_M10_M28[,-c(1)])


shapiro.test(delta_M10_M28$deltaBRONCSEVM28M10)
shapiro.test(delta_M10_M28$deltaBRONCETENDM28M10)
shapiro.test(delta_M10_M28$deltaBRONCSITM28M10)
shapiro.test(delta_M10_M28$deltaMUCETENDM28M10)
shapiro.test(delta_M10_M28$deltaMUCDENSM28M10)
shapiro.test(delta_M10_M28$deltaEPAISBRONCM28M10)
shapiro.test(delta_M10_M28$deltaABPAM28M10)
shapiro.test(delta_M10_M28$deltaFIBREPAISM28M10)
shapiro.test(delta_M10_M28$deltaCOLLAPSM28M10)
shapiro.test(delta_M10_M28$deltaFIBROSEM28M10)
shapiro.test(delta_M10_M28$deltaASPERGM28M10)
shapiro.test(delta_M10_M28$deltaMICROLM28M10)
shapiro.test(delta_M10_M28$deltaNODLM28M10)
shapiro.test(delta_M10_M28$deltaVERDLM28M10)
shapiro.test(delta_M10_M28$deltaPIEGLM28M10)


wilcox.test(delta_M10_M28$deltaBRONCSEVM28M10~1,mu=0)
wilcox.test(delta_M10_M28$deltaBRONCETENDM28M10~1,mu=0)
wilcox.test(delta_M10_M28$deltaBRONCSITM28M10~1,mu=0) #
wilcox.test(delta_M10_M28$deltaMUCETENDM28M10~1,mu=0) 
wilcox.test(delta_M10_M28$deltaMUCDENSM28M10~1,mu=0) 
wilcox.test(delta_M10_M28$deltaEPAISBRONCM28M10~1,mu=0) 
wilcox.test(delta_M10_M28$deltaABPAM28M10~1,mu=0)
wilcox.test(delta_M10_M28$deltaFIBREPAISM28M10~1,mu=0)
wilcox.test(delta_M10_M28$deltaCOLLAPSM28M10~1,mu=0)
wilcox.test(delta_M10_M28$deltaFIBROSEM28M10~1,mu=0)
wilcox.test(delta_M10_M28$deltaASPERGM28M10~1,mu=0)
wilcox.test(delta_M10_M28$deltaMICROLM28M10~1,mu=0)
wilcox.test(delta_M10_M28$deltaNODLM28M10~1,mu=0)
wilcox.test(delta_M10_M28$deltaVERDLM28M10~1,mu=0) 
wilcox.test(delta_M10_M28$deltaPIEGLM28M10~1,mu=0)


library("readxl")
data1=read_excel('C:/Users/houss/Downloads/BaseNebulamb_Baseline_M4.xlsx')
data1=data1[-1,]


data1$HEMHG=as.numeric(data1$HEMHG)
data1$HEMEOS=as.numeric(data1$HEMEOS)
data1$HEMLEU=as.numeric(data1$HEMLEU)
data1$HEMPLAQ=as.numeric(data1$HEMPLAQ)
data1$BIONA=as.numeric(data1$BIONA)
data1$BIOKA=as.numeric(data1$BIOKA)
data1$BIOAS=as.numeric(data1$BIOAS)
data1$BIOAL=as.numeric(data1$BIOAL)
data1$BIOGAGT=as.numeric(data1$BIOGAGT)
data1$BIOBILT=as.numeric(data1$BIOBILT)
data1$BIOPAL=as.numeric(data1$BIOPAL)
data1$BIOCREAT=as.numeric(data1$BIOCREAT)
data1$MYCIGET=as.numeric(data1$MYCIGET)
data1$MYCIGET=as.numeric(data1$MYCIGET)
data1$MYCIGES=as.numeric(data1$MYCIGES)
data1$UAIGG=as.numeric(data1$UAIGG)
data1$SERANARC=as.numeric(data1$SERANARC)
data1$VALQTC=as.numeric(data1$VALQTC)
data1$MYCIGET=as.numeric(data1$MYCIGET)
data1$QACQ5SMP=as.numeric(data1$QACQ5SMP)
data1$SPO2=as.numeric(data1$SPO2)
data1$VEMS=as.numeric(data1$VEMS)
data1$CVF=as.numeric(data1$CVF)
data1$VEMSCVF=as.numeric(data1$VEMSCVF)
data1$VEMSNO=as.numeric(data1$VEMSNO)
data1$ageinc=as.numeric(data1$ageinc)
data1$Duree_maladie_asthme=as.numeric(data1$Duree_maladie_asthme)
data1$duree_diagnostique_abpa=as.numeric(data1$duree_diagnostique_abpa)
data1$BMI=as.numeric(data1$BMI)
data1$suivi_exacerbation=as.numeric(data1$suivi_exacerbation)
data1$SEXE=as.factor(data1$SEXE)
data1$TABAC=as.factor(data1$TABAC)
data1$ATOPIE=as.factor(data1$ATOPIE)                
data1$ATCD_POUMON=as.factor(data1$ATCD_POUMON)
data1$ATCD_BK=as.factor(data1$ATCD_BK)
data1$ATCD_MYCAT=as.factor(data1$ATCD_MYCAT)
data1$ATCD_BPCO=as.factor(data1$ATCD_BPCO)
data1$ATCD_BK=as.factor(data1$ATCD_BK)
data1$ATCD_PNTX=as.factor(data1$ATCD_PNTX)
data1$ATCD_KPOU=as.factor(data1$ATCD_KPOU)
data1$ATCD_CHIRASP=as.factor(data1$ATCD_CHIRASP)
data1$DIABETE=as.factor(data1$DIABETE)
data1$ALCOOL=as.factor(data1$ALCOOL)
data1$EXACERB1=as.factor(data1$EXACERB1)
data1$SEROASPPOS=as.factor(data1$SEROASPPOS)
data1$SERAACP=as.factor(data1$SERAACP)
data1$SERAIGGP=as.factor(data1$SERAIGGP)
data1$PARENNORM=as.factor(data1$PARENNORM)
data1$VAL80=as.factor(data1$VAL80)
data1$DEP=as.numeric(as.character(data1$DEP))
data1$DEP80=as.factor(data1$DEP80)
data1$ASPERG=as.factor(data1$ASPERG)
data1$TOUX=as.factor(data1$TOUX)
data1$EXPECT=as.factor(data1$EXPECT)
data1$SIFFL=as.factor(data1$SIFFL)
data1$DOULT=as.factor(data1$DOULT)
data1$SMRC=as.factor(data1$SMRC)
data1$ASTCTR=as.factor(data1$ASTCTR)
data1$TRT=as.factor(data1$TRT)
data1$BRONCSEV=as.factor(data1$BRONCSEV)
data1$BRONCETEND=as.factor(data1$BRONCETEND)
data1$EPAISBRONC=as.factor(data1$EPAISBRONC)
data1$ABPA=as.factor(data1$ABPA)
data1$FIBREXT=as.factor(data1$FIBREXT)
data1$FIBREPAIS=as.factor(data1$FIBREPAIS)
data1$COLLAPS=as.factor(data1$COLLAPS)
data1$FIBROSE=as.factor(data1$FIBROSE)
data1$ASPERGCHR=as.factor(data1$ASPERGCHR)
data1$MICROL=as.factor(data1$MICROL)
data1$NODL=as.factor(data1$NODL)
data1$VERDL=as.factor(data1$VERDL)
data1$PIEGL=as.factor(data1$PIEGL)
data1$Exacerbation=as.factor(data1$Exacerbation)
data1$BRONCSIT=as.factor(data1$BRONCSIT)
data1$MUCETEND=as.factor(data1$MUCETEND)
data1$MUCDENS=as.factor(data1$MUCDENS)
data1$SUBJID=as.numeric(data1$SUBJID)
data1$Exacerbation=as.integer(data1$Exacerbation)-1



library(forcats)
data1$SERAIGGP <- fct_recode(data1$SERAIGGP,NULL="NF")
data1$SERAACP <- fct_recode(data1$SERAACP,NULL="NF")
data1$DOULT <- fct_recode(data1$DOULT,NULL="Non fait")
data1$EXPECT <- fct_recode(data1$EXPECT,NULL="Non fait")


table(data1$Exacerbation,data1$TRT)

fun=function(x){
  if (x==0){print("stable")}
  else if (x<0){print("amelioration")}
  else if (x>0){print("aggravation")}
}


#
new_data=as.data.frame(cbind(delta_M0_M4$SUBJID,delta_M0_M4$deltaMUCETENDM4M0,delta_M0_M4$deltaMUCDENSM4M0,delta_M0_M4$deltaEPAISBRONCM4M0,
                             delta_M0_M4$deltaMICROLM4M0,delta_M0_M4$deltaVERDLM4M0))
colnames(new_data)=c("SUBJID","deltaMUCETENDM4M0","deltaMUCDENSM4M0","deltaEPAISBRONCM4M0","deltaMICROLM4M0","deltaVERDLM4M0")






fun1=function(x){
  if ((x[2]>0 || x[3]>0 || x[4]>0 || x[5]>0 || x[6]>0  ) || (x[2]==0 & x[3]==0 & x[4]==0 & x[5]==0 & x[6]==0)){print("NON")} #stable amelioration: oui #aggravation amelioration :NON 
  else {print("OUI")}
}
Reponse=rep(0,132)
for (i in 1:132){Reponse[i]=fun1(new_data[i,])}

deltaMUCETENDM4M0=rep(0,132)
for (i in 1:132){
  deltaMUCETENDM4M0[i]=fun(new_data$deltaMUCETENDM4M0[i])}

deltaMUCDENSM4M0=rep(0,132)
for (i in 1:132){
  deltaMUCDENSM4M0[i]=fun(new_data$deltaMUCDENSM4M0[i])}

deltaEPAISBRONCM4M0=rep(0,132)
for (i in 1:132){
  deltaEPAISBRONCM4M0[i]=fun(new_data$deltaEPAISBRONCM4M0[i])}

deltaMICROLM4M0=rep(0,132)
for (i in 1:132){
  deltaMICROLM4M0[i]=fun(new_data$deltaMICROLM4M0[i])}

deltaVERDLM4M0=rep(0,132)
for (i in 1:132){
  deltaVERDLM4M0[i]=fun(new_data$deltaVERDLM4M0[i])}

q=integer()
for (i in 1:139){
  q=append(q,which(delta_M0_M4$SUBJID[i]==data1$SUBJID[1:139]))
}
delta_M0_M4$SUBJID
data11=data1[q,]


new_data=as.data.frame(cbind(delta_M0_M4$SUBJID,deltaMUCETENDM4M0,deltaMUCDENSM4M0,deltaEPAISBRONCM4M0,
                             deltaMICROLM4M0,deltaVERDLM4M0,Reponse,data11$Exacerbation))
freq(new_data$deltaVERDLM4M0)

colnames(new_data)=c("SUBJID","VARIATION_MUCETENDM4M0","VARIATION_MUCDENSM4M0","VARIATION_EPAISBRONCM4M0","VARIATION_MICROLM4M0","VARIATION_VERDLM4M0","Reponse","Exacerbation")


chisq.test(table(new_data$Exacerbation,new_data$Reponse))
library(rpart.plot)

fun=function(x){
  if ((x=="aggravation") || (x=="stable")) {print("Aggravation")}
  else {print("Amelioration")}
}

deltaMUCETENDM4M0=rep(0,132)
for (i in 1:132){
  deltaMUCETENDM4M0[i]=fun(new_data$VARIATION_MUCETENDM4M0[i])}

deltaMUCDENSM4M0=rep(0,132)
for (i in 1:132){
  deltaMUCDENSM4M0[i]=fun(new_data$VARIATION_MUCDENSM4M0[i])}

deltaEPAISBRONCM4M0=rep(0,132)
for (i in 1:132){
  deltaEPAISBRONCM4M0[i]=fun(new_data$VARIATION_EPAISBRONCM4M0[i])}

deltaMICROLM4M0=rep(0,132)
for (i in 1:132){
  deltaMICROLM4M0[i]=fun(new_data$VARIATION_MICROLM4M0[i])}

deltaVERDLM4M0=rep(0,132)
for (i in 1:132){
  deltaVERDLM4M0[i]=fun(new_data$VARIATION_VERDLM4M0[i])}

new_data=as.data.frame(cbind(delta_M0_M4$SUBJID,deltaMUCETENDM4M0,deltaMUCDENSM4M0,deltaEPAISBRONCM4M0,
                             deltaMICROLM4M0,deltaVERDLM4M0,Reponse,data11$Exacerbation))

colnames(new_data)=c("SUBJID","VARIATION_MUCETENDM4M0","VARIATION_MUCDENSM4M0","VARIATION_EPAISBRONCM4M0","VARIATION_MICROLM4M0","VARIATION_VERDLM4M0","Reponse","Exacerbation")

library(survival)
library(survminer)


survie=survfit(Surv(suivi_exacerbation, Exacerbation) ~  TRT, data = data1)
summary(coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(55,74,73)]))
summary(survie)
survdiff(Surv(suivi_exacerbation, Exacerbation) ~  TRT, data = data1)
ggsurvplot(survie,pval = T,xlim=c(0,650))# p-value xlim

data111=cbind(data11,new_data$Reponse)
data111$Exacerbation=as.numeric(as.character(data111$Exacerbation))

help("ggsurvplot")
survie=survfit(Surv(suivi_exacerbation, Exacerbation) ~ data111$`new_data$Reponse` , data = data111)
survdiff(Surv(suivi_exacerbation, Exacerbation) ~  data111$`new_data$Reponse`, data = data111)
ggsurvplot(survie,pval = T,xlim=c(0,650),legend.labs=c("non répondeur","répondeur"),legend=c(0.7,0.8),xlab="Temps(jours)",ylab="survie sans exacerbation")# p-value xlim
table(data111$Exacerbation,data111$`new_data$Reponse`)

data1$MYCIGET=data1$MYCIGET/100


data1$MUCETEND=relevel(data1$MUCETEND,ref="absent") 
data1$SERAIGGP=relevel(data1$SERAIGGP,ref="Non") 
modele1=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(3,74,73)])
summary(modele1)
cox.zph(modele1)
modele2=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(27,74,73)])
summary(modele2)
cox.zph(modele2)
modele3=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(30,74,73)])
summary(modele3)
cox.zph(modele3)
modele4=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(32,74,73)])
summary(modele4)
cox.zph(modele4)
modele5=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(39,74,73)])
summary(modele5)
cox.zph(modele5)
modele6=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(41,74,73)])
summary(modele6)
cox.zph(modele6)
modele7=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(59,74,73)])
summary(modele7)
cox.zph(modele7)
modele8=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(69,74,73)])
summary(modele8)
cox.zph(modele8)
levels(data1$SERAIGGP)
modele9=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(55,74,73)])
summary(modele9)
data1$TRT=relevel(data1$TRT,ref="Placebo group") 
Modele_finale=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(3,41,59,55,74,73)])
summary(Modele_finale)

library("car")
car:: Anova(Modele_finale,test.statistic="Wald")

Modele_finale=coxph(Surv(suivi_exacerbation, Exacerbation) ~ . , data = data1[,c(3,59,55,74,73)])
summary(Modele_finale)






