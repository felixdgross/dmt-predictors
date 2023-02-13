###############################
#1.1 Setup
############################## 
require(car)
require(ggplot2)
require(ggfortify)


dat <- read.csv("C:/Users/Thinkpad/Documents/Zuerich_Psychedelika/dataframe_predictors_21.1.23.csv", header = TRUE)

#1.1.1 append_longformatted additional data
#1.1.1.1 max DMT blood level 
require(dplyr)
blood_lev.dat <- read.csv("C:/Users/Thinkpad/Documents/Zuerich_Psychedelika/blood_level_DMT.csv", header = TRUE)

blood_lev.dat <- reshape(blood_lev.dat, idvar = "studid", timevar = "redcap_event_name", direction = "wide") #reshape long format to wide format
sapply(blood_lev.dat, class)   
blood_lev.dat<- as.data.frame(apply(blood_lev.dat, 2, as.numeric)) #change class to numeric

blood_lev.dat$blood_dmt_highest  <-  pmax(blood_lev.dat$blood_dmt.30, #search for highest dmt value                                             #create a new column with highest DMT levelblood_lev.dat$blood_dmt.30,  blood_lev.dat$blood_dmt.60, blood_lev.dat$blood_dmt.90,    
      blood_lev.dat$blood_dmt.120, blood_lev.dat$blood_dmt.150, blood_lev.dat$blood_dmt.180, 
      blood_lev.dat$blood_dmt.210, blood_lev.dat$blood_dmt.240, blood_lev.dat$blood_dmt.300, 
      na.rm = TRUE)

colnames(blood_lev.dat)
blood_lev_max.dat <- blood_lev.dat[c("studid", "blood_dmt_highest")]

dat$studid %in% blood_lev_max.dat$studid                                                  #studid 1019 is missing from blood_lev_max data
blood_lev_max.dat[nrow(blood_lev_max.dat) + 1,] = c(1019, mean(blood_lev_max.dat$blood_dmt_highest))  #impute missing value with mean 
dat$studid %in% blood_lev_max.dat$studid                                                  #both data frames match now

dat <- dat[order(dat$studid),]      #order both df
blood_lev_max.dat <- blood_lev_max.dat[order(blood_lev_max.dat$studid),]

all.equal(dat$studid, blood_lev_max.dat$studid)                                           #boths dfs order matches now
dat <- merge.data.frame(dat, blood_lev_max.dat, x.all = TRUE)                             #merge data frames
dat
dim(dat)
class(dat$blood_dmt_highest)
dat$blood_dmt_highest
class(blood_lev_max.dat)

mean(dat$blood_dmt_highest)==mean(blood_lev_max.dat$blood_dmt_highest) #control

#1.1.1.2acute experience ratings

#1.1.1.2.1 liking
require(dplyr)
exp.dat <- read.csv("C:/Users/Thinkpad/Documents/Zuerich_Psychedelika/acute_ratings_experience.csv", header = TRUE)

exp.dat <- exp.dat[c("studid","redcap_event_name","ap_liking")]
exp.dat <- reshape(exp.dat, idvar = "studid", timevar = "redcap_event_name", direction = "wide") #reshape long format to wide format
exp.dat<- as.data.frame(apply(exp.dat, 2, as.numeric)) #change class to numeric

dat$studid %in% exp.dat$studid
all.equal(dat$studid, exp.dat$studid)

exp.dat$exp_highest  <-  pmax(exp.dat[,2:12], na.rm = TRUE)

dat$ap_liking_highest <- exp.dat$exp_highest
dim(dat)
#1.1.1.2.2 lettinggo
exp.dat <- read.csv("C:/Users/Thinkpad/Documents/Zuerich_Psychedelika/acute_ratings_experience.csv", header = TRUE)

exp.dat <- exp.dat[c("studid","redcap_event_name","ap_letting_go")]
exp.dat <- reshape(exp.dat, idvar = "studid", timevar = "redcap_event_name", direction = "wide") #reshape long format to wide format
exp.dat<- as.data.frame(apply(exp.dat, 2, as.numeric)) #change class to numeric

exp.dat
exp.dat$exp_highest  <-  pmax(exp.dat[,2:12], na.rm = TRUE)

dat$ap_letting_go_highest <- exp.dat$exp_highest
dim(dat)
#1.1.1.2.3 psychedelic
exp.dat <- read.csv("C:/Users/Thinkpad/Documents/Zuerich_Psychedelika/acute_ratings_experience.csv", header = TRUE)

exp.dat <- exp.dat[c("studid","redcap_event_name","psychedelic")]
exp.dat <- reshape(exp.dat, idvar = "studid", timevar = "redcap_event_name", direction = "wide") #reshape long format to wide format
exp.dat<- as.data.frame(apply(exp.dat, 2, as.numeric)) #change class to numeric

exp.dat
exp.dat$exp_highest  <-  pmax(exp.dat[,2:12], na.rm = TRUE)

dat$psychedelic_highest <- exp.dat$exp_highest
dim(dat)
#1.1.1.2.4 empathogenic
exp.dat <- read.csv("C:/Users/Thinkpad/Documents/Zuerich_Psychedelika/acute_ratings_experience.csv", header = TRUE)

exp.dat <- exp.dat[c("studid","redcap_event_name","empathogenic")]
exp.dat <- reshape(exp.dat, idvar = "studid", timevar = "redcap_event_name", direction = "wide") #reshape long format to wide format
exp.dat<- as.data.frame(apply(exp.dat, 2, as.numeric)) #change class to numeric

exp.dat
exp.dat$exp_highest  <-  pmax(exp.dat[,2:12], na.rm = TRUE)

dat$empathogenic_highest <- exp.dat$exp_highest
dim(dat)
#1.1.1.3area under the curve blood_DMT data
auc.dat <- read.csv("C:/Users/Thinkpad/Documents/Zuerich_Psychedelika/excel_PK_DMT_dmt.csv", header = TRUE)

auc.dat <- reshape(auc.dat, idvar = "Subject", timevar = "Parameter", direction = "wide") #reshape long format to wide format
auc.dat <- as.data.frame(apply(auc.dat, 2, as.numeric)) #change class to numeric

auc.dat <- auc.dat[c("Subject", "Value.aucall","Value.aucinf.obs")]

auc.dat$Subject<- auc.dat$Subject + 1000
auc.dat$Subject
auc.dat <- auc.dat[order(auc.dat$Subject),]

dat$studid
auc.dat$Subject
colnames(auc.dat)
dat$studid %in% auc.dat$Subject  #Subject 1019,1038 is missing from auc data
auc.dat[nrow(auc.dat) + 1,] = c("1019", mean(auc.dat$Value.aucall),mean(auc.dat$Value.aucinf.obs))  #impute missing value with mean 
auc.dat[nrow(auc.dat) + 1,] = c("1038", mean(auc.dat$Value.aucall),mean(auc.dat$Value.aucinf.obs))  #impute missing value with mean 

all.equal(dat$studid, auc.dat$Subject) 
dat$value_aucall <- auc.dat$Value.aucall                            #merge data frames
dat$value_aucinf_obs <- auc.dat$Value.aucinf.obs

mean(dat$blood_dmt_highest)==mean(blood_lev_max.dat$blood_dmt_highest) #control
dim(dat)

write.csv(dat, "C:/Users/Thinkpad/Documents/Zuerich_Psychedelika/mergeddata_new.csv", row.names = TRUE)

#############################################################
#data merging completed: result is in mergeddata.csv
#############################################################



#1.2 Exploration and descriptives


dim(dat) #should be 31 64
summary(dat)
colnames(dat)
head(dat)
str(dat)
mean(dat$pat_weight, na.rm = TRUE)
mean(dat$pat_age, na.rm = TRUE)
sd(dat$pat_age, na.rm = TRUE)
sum(dat$psy_exp)

#1.3 Imputing some missing data with mean
dat$pat_age[is.na(dat$pat_age)] <- mean(dat$pat_age, na.rm = TRUE)
dat$pat_age
dat$pat_weight[is.na(dat$pat_weight)] <- mean(dat$pat_weight, na.rm = TRUE)
dat$pat_weight

#1.4 Z-Standardize Data 
dim(dat)  #after having run the append sections it should read 868 61                                     
str(dat)
which(colnames(dat)=="day")
which(colnames(dat)=="medianintensity")
which(colnames(dat)=="drug")
dat1<-dat[-c(12,49,31)]                       #remove the non numeric columns, in order to

dim(dat1)                                      #z-standardize
for (i in 3:61){
  dat1[,i]<-scale(dat1[,i])
}
str(dat1)

#1.5 Check criterion for normality
qqPlot(dat1$asc)
shapiro.test(dat1$asc)          #p-value < 0.05 implying that the distribution of the data is significantly different from the normal distribution. In other words, we can not assume the normality, therefore use of an GLM is indicated.
dev.off()                       #condition for auto-plot
autoplot(glmasc)                #make sure the output figure margins (window, where plot is displayed) are big enough to provide room, to displey the graphs
colnames(dat)
###############################
#2 Data Analysis

############################## 
#2.1 asc
#2.1.1 correlation asc
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
cor_dat <- dat[, c(37,4,6,5,59,60)]
chart.Correlation(cor_dat, histogram=TRUE, pch=19)

#2.1.2 asc model
glm_asc <- glm(asc ~
                pat_age + pat_weight + blood_dmt_highest + value_aucall + value_aucinf_obs +                                                                                       #physiological
                psy_exp +                                                                                                      #pre-experience
                anps_play + anps_seek + anps_care + anps_fear + anps_anger + anps_spirituality + anps_sadness +                #personality
                ipip_neuroticism + ipip_openness + 
                panas_trait_positive + panas_trait_negative +                          
                panas_state_positive + panas_state_negative                                                                                                   #momentary state                                                                                              #mental state before drug intake
                ,data = dat1)                 
summary(glm_asc)

#2.1.3 asc with random effect of studID
glm_asc_rdeff <- glm(asc ~
                pat_age + pat_weight +                                                                                         #physiological
                psy_exp +                                                                                                      #pre-experience
                anps_play + anps_seek + anps_care + anps_fear + anps_anger + anps_spirituality + anps_sadness +                #personality
                ipip_neuroticism + ipip_openness + 
                panas_trait_positive + panas_trait_negative +                          
                panas_state_positive + panas_state_negative +
                (1|studid)                                                                                                    #momentary state                                                                                              #mental state before drug intake
              ,data = dat1)                 
summary(glm_asc_rdeff)

#2.2 ceq
#2.2.1 ceqcorrelationceq
cor_dat <- dat[, c(58,4,6,5,59,60)]
chart.Correlation(cor_dat, histogram=TRUE, pch=19)

#2.2.2 ceq model
glm_ceq <- glm(ceq_globalscore ~
                pat_age + pat_weight + blood_dmt_highest + value_aucall + value_aucinf_obs +                                                                                       #physiological
                psy_exp +                                                                                                      #pre-experience
                anps_play + anps_seek + anps_care + anps_fear + anps_anger + anps_spirituality + anps_sadness +                #personality
                ipip_neuroticism + ipip_openness + 
                panas_trait_positive + panas_trait_negative +                          
                panas_state_positive + panas_state_negative                                                                                                   #momentary state                                                                                              #mental state before drug intake
              ,data = dat1)                 
summary(glm_ceq)

#2.2.3 ceq with random effect of studID
glm_ceq_rdeff <- glm(ceq_globalscore ~
                pat_age + pat_weight +                                                                                         #physiological
                psy_exp +                                                                                                      #pre-experience
                anps_play + anps_seek + anps_care + anps_fear + anps_anger + anps_spirituality + anps_sadness +                #personality
                ipip_neuroticism + ipip_openness + 
                panas_trait_positive + panas_trait_negative +                          
                panas_state_positive + panas_state_negative +
                (1|studid)                                                                                                    #momentary state                                                                                              #mental state before drug intake
              ,data = dat1)                 
summary(glm_ceq_rdeff)

#2.3 ebi
#2.3.1 ebicorrelationceq
cor_dat <- dat[, c(49,4,6,5,59,60)]
chart.Correlation(cor_dat, histogram=TRUE, pch=19)

#2.3.2 ebi model
glm_ebi <- glm(ebi_mean ~
                 pat_age + pat_weight + blood_dmt_highest + value_aucall + value_aucinf_obs +                                                                                       #physiological
                 psy_exp +                                                                                                      #pre-experience
                 anps_play + anps_seek + anps_care + anps_fear + anps_anger + anps_spirituality + anps_sadness +                #personality
                 ipip_neuroticism + ipip_openness + 
                 panas_trait_positive + panas_trait_negative +                          
                 panas_state_positive + panas_state_negative                                                                                                   #momentary state                                                                                              #mental state before drug intake
               ,data = dat1)                 
summary(glm_ebi)

#2.3.3 ebi with random effect of studID
glm_ebi_rdeff <- glm(ebi_mean ~
                       pat_age + pat_weight +                                                                                         #physiological
                       psy_exp +                                                                                                      #pre-experience
                       anps_play + anps_seek + anps_care + anps_fear + anps_anger + anps_spirituality + anps_sadness +                #personality
                       ipip_neuroticism + ipip_openness + 
                       panas_trait_positive + panas_trait_negative +                          
                       panas_state_positive + panas_state_negative +
                       (1|studid)                                                                                                    #momentary state                                                                                              #mental state before drug intake
                     ,data = dat1)                 
summary(glm_ebi_rdeff)

#2.3 acute maximum ratings

