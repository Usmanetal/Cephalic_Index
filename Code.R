library(readxl)
cephalic <- read_excel("~/Bluetooth Exchange Folder/sagagi2.xlsx",sheet = "Sheet2")
View(cephalic)

#library(ggpubr) and library(tidyverse)
# Vertical and Horizontal median representation of both variables
#scatterplot showing correlation BPD and OFD at 2nd Trimester
meanbpd2<-cephalic%>%mutate(EG.A=str_replace_all(EG.A,"[+]","."),TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD",
  "2NDF"="2ND"))%>%filter(TRIMESTER=="2ND")%>%summarise(median_val=median(BPD))
meanofd2<-cephalic%>%mutate(EG.A=str_replace_all(EG.A,"[+]","."),TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD",
"2NDF"="2ND"))%>%filter(TRIMESTER=="2ND")%>%summarise(median_val=median(OFD))


cephalic%>%mutate(EG.A=str_replace_all(EG.A,"[+]","."),TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD","2NDF"="2ND"))%>%
  filter(TRIMESTER=="2ND")%>%ggplot(aes(BPD,OFD))+geom_point()+theme_minimal()+
  geom_hline(data= meanofd2, aes(yintercept = median_val),linetype="dashed")+
  geom_vline(data= meanbpd2, aes(xintercept = median_val),linetype="longdash")+geom_smooth(method = lm,se=FALSE,fullrange=TRUE)+
  scale_x_log10()+stat_cor(method = "spearman")

##scatterplot showing correlation BPD and OFD at 3rd Trimester

meanbpd3<-cephalic%>%mutate(EG.A=str_replace_all(EG.A,"[+]","."),
  TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD","2NDF"="2ND"))%>%
  filter(TRIMESTER=="3RD")%>%
  summarise(median_val=median(BPD))

meanofd3<-cephalic%>%mutate(EG.A=str_replace_all(EG.A,"[+]","."),
                        TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD",
                        "2NDF"="2ND"))%>%filter(TRIMESTER=="3RD")%>%
                         summarise(median_val=mean(OFD))


cephalic%>%mutate(EG.A=str_replace_all(EG.A,"[+]","."),TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD","2NDF"="2ND"))%>%
  filter(TRIMESTER=="3RD")%>%ggplot(aes(BPD,OFD))+
  geom_point()+theme_minimal()+
  geom_hline(data= meanofd3, aes(yintercept = median_val),linetype="dashed")+
  geom_vline(data= meanbpd3, aes(xintercept = median_val),linetype="longdash")+
  geom_smooth(method = lm,se=FALSE,fullrange=TRUE)+scale_x_log10()+scale_y_log10()+
  stat_cor(method = "spearman")

#Scatter plot of the cephalic index at the growth scan versus the cephalic index at the morphology scan
ci_morho<-cephalic%>%mutate(EG.A1=str_replace_all(EG.A,"[+]","."),
  TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD","2NDF"="2ND"))%>%
  mutate(CI3=(BPD/OFD)*100)%>%filter(TRIMESTER=="3RD")%>%
  select(TRIMESTER,CI3,EG.A1)
ci_growth<-cephalic%>%mutate(EG.A=str_replace_all(EG.A,"[+]","."),
  TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD","2NDF"="2ND"))%>%
  mutate(CI2=(BPD/OFD)*100)%>%filter(TRIMESTER=="2ND")

ci_medianM<-tibble(ci_growth,CI3=ci_morho[-c(199:433),]$CI3)%>%
  summarise(median_val=median(CI2))

ci_medianG<-tibble(ci_growth,CI3=ci_morho[-c(199:433),]$CI3)%>%
  summarise(median_val=median(CI3))

tibble(ci_growth,CI3=ci_morho[-c(199:433),]$CI3)%>%ggplot(aes(CI2,CI3))+
  geom_point()+theme_minimal()+scale_x_log10()+scale_y_log10()+
  labs(x="CI at morphology scan",y="CI at growth scan")+
  geom_hline(data= ci_medianG, aes(yintercept = median_val),linetype="dashed")+
  geom_vline(data= ci_medianM, aes(xintercept = median_val),linetype="longdash")+
  geom_smooth(method = lm,se=FALSE,fullrange=TRUE)+stat_cor(method = "spearman")


#Scatter plot of the change in cephalic index between growth and morphology scans versus the corresponding change
ci_growth1<-cephalic%>%mutate(EG.A=str_replace_all(EG.A,"[+]","."),
  TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD","2NDF"="2ND"))%>%
  mutate(CI2=(BPD/OFD)*100)%>%filter(TRIMESTER=="2ND")%>%
  mutate(CI3=CI2)%>%select(TRIMESTER,CI3,EG.A)

ci_morpho<-cephalic%>%mutate(EG.A1=str_replace_all(EG.A,"[+]","."),
  TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD","2NDF"="2ND"))%>%
  mutate(CI2=(BPD/OFD)*100)%>%filter(TRIMESTER=="3RD")%>%
  select(TRIMESTER,CI2,EG.A1)

ci_morpho$EG.A1<-as.numeric(ci_morpho$EG.A1)
ci_growth1$EG.A<-as.numeric(ci_growth1$EG.A)

new_data<-tibble(ci_growth1[,-1],ci_morpho[-c(199:433),])%>%
  mutate(gest_dif_wk= ci_morpho[-c(199:433),]$EG.A1-ci_growth1[,-1]$EG.A,ci_change=CI3-CI2)

new_data%>%ggplot(aes(gest_dif_wk,ci_change))+
  geom_point()+geom_smooth(method = lm,se=FALSE,fullrange=TRUE)+
  stat_cor(method = "spearman")+scale_y_continuous(limits = c(-10,10.3))+
  scale_x_continuous(limits = c(0,20))+geom_hline(yintercept = 0, linetype="dashed")+
  theme_minimal()+labs(x="difference in Gestation (week)", y= "Change in CI")

#comparing means and adding to our figure
my_comparisons<-list(c("2ND","3RD"))

cephalic%>%mutate(TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD","2NDF"="2ND"))%>%
  ggplot(aes(TRIMESTER,CI))+geom_boxplot()+geom_point(position=position_jitter())+
  theme_minimal()+stat_compare_means(comparisons = my_comparisons)+facet_wrap(~SEX)

#pairwise comparisons between morphology and growth variables
cephalic%>%mutate(TRIMESTER=recode(TRIMESTER,"2nd"="2ND","3rd"="3RD","2NDF"="2ND"))%>%
  ggpairs(columns=3:7,aes(col=TRIMESTER))+theme(panel.grid.major = element_blank(),
  axis.line = element_line(colour = "black"))+
  theme(strip.background = element_rect(fill = "white"),strip.placement = "outside")+
  theme(axis.text.x = element_blank())+scale_color_manual(values=c("blue","black"))+
  scale_fill_manual(values=c("blue","black"))

#logistic regression analysis
cephalic$sex<-cephalic$SEX
cephalic$logit<-cephalic$TRIMESTER
cephalic[cephalic$logit=="2ND"|cephalic$logit=="2nd"|cephalic$logit=="2NDF",]$logit<-"0"
cephalic[cephalic$logit=="3RD"|cephalic$logit=="3rd",]$logit<-"1"
cephalic[cephalic$sex=="F",]$sex<-"0"
cephalic[cephalic$sex=="M",]$sex<-"1"
glm(logit~sex, data=cephalic,family = "binomial")
cephalic$logit<-as.factor(cephalic$logit)
summary(glm(logit~sex, data=cephalic,family = "binomial"))
summary(glm(logit~SEX+BPD+OFD+FL+AC, data=cephalic,family = "binomial"))

#write.csv(as.data.frame(muri_logistic$coefficients),"muri2.csv")


