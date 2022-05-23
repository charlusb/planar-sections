######################################################

##### script Planar sections version April, 6th 2022
##### Study 1
#####results are almost the same with the classic random effects (1+planar*straight|part) =>
##### keep the maximum of random effect, i.e intercept + interaction ??
######################################################


library(car)
library(afex)
library(ggplot2)
library(emmeans)
library(data.table)
library(stringr)


###############################################################################################
###################################### STUDY 1 ################################################
###############################################################################################

##### Non-mathematicians, two groups: a straight group (parts have to identify straight lines) and an
##### intersection group (parts have to identify planar intersections)

############## Group 1: planar section effect on identification of straight lines #############

####### participants
straight_group <- read.table("data_lambda_straight.csv", sep=";", dec=",", header=T)
participants<-aggregate(valence~participant+sex+age, data=straight_group, FUN=mean)
straight_group <- subset(straight_group, badtry==0)


####### properties of the stims
prop_all1 <- read.table("prop_curves_lambda.txt", header=T)

#### creating one df without stims on wich curvature is not defined (cubes, one cone)
prop_less1 <- subset(prop_all1, is.na(prop_all1$cmean)==F)

#### merging w participants of straight group
curve_all <- merge(straight_group, prop_all1)
curve_less <- merge(straight_group, prop_less1)



main_straight = aov_car(valence ~ planar_intersection * straight + Error(participant/(planar_intersection*straight)) , data =straight_group, anova_table = list(correction = "none", es = "pes"))
main_straight
## Anova Table (Type 3 tests)
## Response: valence
##               Effect    df  MSE         F  pes p.value
## 1       intersection 1, 22 0.07 53.32 ***  .71  <.0001
## 2              straight 1, 22 0.04 19.09 ***  .46   .0002
## 3 intersection:straight 1, 22 0.03      0.04 .002     .84
## ---

mainy=summary(emmeans(main_straight, specs=c("planar_intersection","straight"), cov.reduce=F))
################## A RENOMMER ################################################################################
##############################################################################################################

################ Planar effect on matched pairs, T-tests #######################

################ Group A: All planar, vary on straight #########################

a1=list("sphe_d_i_gc", "cyli_d_i_c","cube_d_i_tri","cube_d_i_ca")
a2=list("sphe_c_i_pc", "cyli_c_i_el","cube_c_i_tri", "cube_c_i_car")

##### average "yes" by subgroup by participants ##############################
moya1=aggregate(valence~ participant, data=subset(straight_group, straight_group$object%in%a1), FUN=mean)
moya2=aggregate(valence~ participant, data=subset(straight_group, straight_group$object%in%a2), FUN=mean)

#### straight planar 
names(moya1)[2]="SP"
#### non straight planar
names(moya2)[2]="NsP"
#### means
summary(moya1)
summary(moya2)


moya=merge(moya1, moya2, by="participant")

mean(moya$SP)
mean(moya$NsP)



##### Paired Student's T-test #####
tta=t.test(moya$SP, moya$NsP, paired=T)
tta
print("sd_groupA")
sd(moya$SP-moya$NsP)

print("means_A")
mean(moya$SP)
## [1] 0.7173913
mean(moya$NsP)
## [1] 0.5434783


##### Group B: All non-planar, vary on straight #######
### Here more elements in subgroup non planar as cylinder has two matches, one on curvature and one on lenght

#### aggregating the two matches of cylinders "d_ni_spiral" before aggregating answers
mean_spe <- aggregate(valence~participant, data=subset(straight_group, object%in%c("cyli_c_ni_spe1c","cyli_c_ni_spe1t")), FUN=mean)
names(mean_spe)[2] <- "valence"
mean_spe$object <- "cyli_c_ni_spe1"

mean_spl <- aggregate(valence~participant, data=subset(straight_group, object%in%c("cyli_c_ni_splc","cyli_c_ni_splt")), FUN=mean)
names(mean_spl)[2] <- "valence"
mean_spl$object <- "cyli_c_ni_spl"

straight_groupb <- rbind(subset(straight_group, select=c(participant, valence, object)), mean_spe, mean_spl)


#### 
b1=list("cube_d_ni_el","cyli_d_ni_spe1","cyli_d_ni_spl","cone_d_ni_rub")
b2=list("cube_c_ni_el","cone_c_ni_rub", "cyli_c_ni_spe1", "cyli_c_ni_spl" )


moyb1=aggregate(valence~ participant, data=subset(straight_groupb, straight_groupb$object%in%b1), FUN=mean)
moyb2=aggregate(valence~ participant, data=subset(straight_groupb, straight_groupb$object%in%b2), FUN=mean)
names(moyb1)[2]="SnP"
names(moyb2)[2]="nSnP"
moyb=merge(moyb1,moyb2, by="participant")


ttb=t.test(moyb$SnP, moyb$nSnP, paired=T)
ttb
print("sd_groupB")
sd(moyb$SnP-moyb$nSnP)

mean(moyb$SnP)
## [1] 0.3152174

mean(moyb$nSnP)
## [1] 0.1413043		


## 	Paired t-test

## data:  moyb$DnI and moyb$CnI
## t = 3.237, df = 22, p-value = 0.003787
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.06249071 0.28533537
## sample estimates:
## mean of the differences 
##                0.173913 

## > sd(moyb$DnI-moyb$CnI)                                                                                                                                                                                       
## [1] 0.2576641

##### Group C: All non-straight, vary on planar

c1=list("sphe_c_i_pc", "cube_c_i_rec", "cyli_c_i_el", "cone_c_i_tr")
c2=list("sphe_c_ni_ca","cube_c_ni_rec","cyli_c_ni_eld","cone_c_ni_tr")

##removing trial cyli_c_i_el (10) of participant 40 bc match is excluded (cyli_c_ni_eld)
## so participant 40 has one pair less than the others
straight_groupc <- subset(straight_group, stimsuj!="10_part40")

moyc1=aggregate(valence~ participant, data=subset(straight_groupc, straight_groupc$object%in%c1), FUN=mean)
moyc2=aggregate(valence~ participant, data=subset(straight_groupc, straight_groupc$object%in%c2), FUN=mean)

names(moyc1)[2]="nSP"
names(moyc2)[2]="nSnP"
moyc=merge(moyc1,moyc2, by="participant")

mean(moyc$nSP)
##0.5072464
mean(moyc$nSnP)
##0.134058

ttc=t.test(moyc$nSP, moyc$nSnP, paired=TRUE)
ttc

print("sd_groupC")
sd(moyc$NsP-moyc$nSnP)

## 	Paired t-test

## data:  moyc$CI and moyc$CnI
## t = 5.0896, df = 22, p-value = 4.242e-05
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.2211252 0.5252516
## sample estimates:
## mean of the differences 
##               0.3731884 


############################### Planar Section group  #########################################################
################################################################################################################
inter <- read.table("data_lambda_intersection.csv", sep=";", dec=",", header=T)
part_inter <- aggregate(valence~participant+age+sex, data=inter, FUN=mean)
inter <- subset(inter, badtry==0)

## acc 93

curve_all_int <- merge(inter, prop_all1)
curve_less_int <- merge(inter, prop_less1)


############################### Planar effect on responses ####################################################
anov = aov_car(valence ~ planar_intersection*straight + Error(participant/(planar_intersection*straight)) , data =inter, anova_table = list(correction = "none", es = "pes"))
anov
## Response: valence
##               Effect    df  MSE          F  pes p.value
## 1       intersection 1, 10 0.02 421.06 ***  .98  <.0001
## 2              straight 1, 10 0.01       0.06 .006     .81
## 3 intersection:straight 1, 10 0.01       0.99  .09     .34
## ---


a1=list("sphe_d_i_gc", "cyli_d_i_c","cube_d_i_tri","cube_d_i_ca")
a2=list("sphe_c_i_pc", "cyli_c_i_el","cube_c_i_tri", "cube_c_i_car")


moya_inter1=aggregate(valence~ participant, data=subset(inter, inter$object%in%a1), FUN=mean)
moya_inter2=aggregate(valence~ participant, data=subset(inter, inter$object%in%a2), FUN=mean)

names(moya_inter1)[2]="SP"
names(moya_inter2)[2]="nSP"

summary(moya_inter1)
summary(moya_inter2)

moya_inter=merge(moya_inter1, moya_inter2, by="participant")

####### Paired Student's T-test 
tta_inter=t.test(moya_inter$SP, moya_inter$nSP, paired=TRUE)
tta_inter
sd(moya_inter$SP-moya_inter$nSP)


## 	Paired t-test

## data:  moya_inter$DI and moya_inter$CI
## t = 0.80322, df = 10, p-value = 0.4405
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.08063684  0.17154593
## sample estimates:
## mean of the differences 
##              0.04545455 

## > sd(moya_inter$DI-moya_inter$CI)                                                                                                                                                                             
## [1] 0.1876893


####### Group B: All non Planar, vary on straight 
More elements in subgroup non planar as spirals on cylinders has two matches, one for lenght and one for curvature

#### aggregating the two matches of cylinders d_ni_spiral before aggregating answers
mean_spe <- aggregate(valence~participant, data=subset(inter, object%in%c("cyli_c_ni_spe1c","cyli_c_ni_spe1t")), FUN=mean)
names(mean_spe)[2] <- "valence"
mean_spe$object <- "cyli_c_ni_spe1"

mean_spl <- aggregate(valence~participant, data=subset(inter, object%in%c("cyli_c_ni_splc","cyli_c_ni_splt")), FUN=mean)
names(mean_spl)[2] <- "valence"
mean_spl$object <- "cyli_c_ni_spl"

interb <- rbind(subset(inter, select=c(participant, valence, object)), mean_spe, mean_spl)

b1=list("cube_d_ni_el","cyli_d_ni_spe1","cyli_d_ni_spl","cone_d_ni_rub")
b2=list("cube_c_ni_el","cyli_c_ni_spe1", "cyli_c_ni_spl","cone_c_ni_rub")


moyb1=aggregate(valence~ participant, data=subset(interb, inter$object%in%b1), FUN=mean)
moyb2=aggregate(valence~ participant, data=subset(interb, inter$object%in%b2), FUN=mean)
names(moyb1)[2]="SnP"
names(moyb2)[2]="nSnP"
moyb=merge(moyb1,moyb2, by="participant")

summary(moyb)
ttb=t.test(moyb$SnP, moyb$nSnP, paired=TRUE)
ttb
sd(moyb$SnP-moyb$nSnP)

## 	Paired t-test

## data:  moyb$DnI and moyb$CnI
## t = 1.9365, df = 10, p-value = 0.08155
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.01026859  0.14663222
## sample estimates:
## mean of the differences 
##              0.06818182 

## > sd(moyb$DnI-moyb$CnI)

## [1] 0.1167748


##### Group C: all non straight, vary on planar

c1 <- list("sphe_c_i_pc", "cube_c_i_rec", "cyli_c_i_el", "cone_c_i_tr")
c2 <- list("sphe_c_ni_ca","cube_c_ni_rec","cyli_c_ni_eld","cone_c_ni_tr")

#### removing a stim of part 51 because its match is excluded 
interc <- subset(inter, stimsuj!="3_51")

moyc1 <- aggregate(valence~ participant, data=subset(interc, inter$object%in%c1), FUN=mean)
moyc2 <- aggregate(valence~ participant, data=subset(interc, inter$object%in%c2), FUN=mean)

names(moyc1)[2] <-"nSP"
names(moyc2)[2] <-"nSnP"
moyc=merge(moyc1,moyc2, by="participant")


ttc=t.test(moyc$nSP, moyc$nSnP, paired=TRUE)
ttc

sd(moyc$nSP-moyc$nSnP)



## 	Paired t-test

## data:  moyc$CI and moyc$CnI
## t = 9.1183, df = 10, p-value = 3.679e-06
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.5266589 0.8672805
## sample estimates:
## mean of the differences 
##               0.6969697 


## > sd(moyc$CI-moyc$CnI)                                                                                                                                                                                        
## [1] 0.2535107

################################# Regression with the pool of the two groups #################
##### merging the two groups 
straight_group$group <- "straight_line"
inter$group <- "planar_intersection"
all=rbind(inter, straight_group)



########### regression analysis between % of yes in the two groups across trials ###############
## - a regression analysis between the percentage of "yes" answers in the two groups, across trials. A positive correlation would strengthen the conclusion that participants' concept of "straight line"
##orresponds to plane intersections.

##### two groups correlation of answers
straight_yes=aggregate(valence~object+planar_intersection, data=straight_group, FUN=mean)
int_yes=aggregate(valence~object+planar_intersection, data=inter, FUN=mean)
names(straight_yes)[3]="val_straight"
names(int_yes)[3]="val_inter"

all_yes=merge(straight_yes, int_yes, by=c("object", "planar_intersection"))

cor.test(all_yes$val_straight, all_yes$val_inter)

## 	Pearson's product-moment correlation

## data:  all_yes$valence.x and all_yes$valence.y
## t = 6.7907, df = 24, p-value = 5.045e-07
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.6176656 0.9118820
## sample estimates:
##       cor 
## 0.8109859

all_yes$planar_intersection=factor(all_yes$planar_intersection, levels=c("1", "0"), labels=c("Planar", "Non-planar")) 

######### anova with the two groups ######################################################
## - an Anova pooling responses of the two groups, with 3 variables for group, intersection and straightness. This Anova will serve to assess whether participants successfully modulated their answers across groups.

anov_all = aov_car(valence ~ planar_intersection * straight * group + Error(participant/(planar_intersection*straight)) , data =all, anova_table = list(correction = "none", es = "pes"))               
anov_all
## Response: valence
##                               Effect    df  MSE          F  pes p.value
## 1                              group 1, 32 0.09       2.28  .07     .14
## 2                planar_intersection 1, 32 0.06 213.60 ***  .87  <.0001
## 3          group:planar_intersection 1, 32 0.06  26.51 ***  .45  <.0001
## 4                           straight 1, 32 0.03    8.45 **  .21    .007
## 5                     group:straight 1, 32 0.03     6.94 *  .18     .01
## 6       planar_intersection:straight 1, 32 0.02       0.23 .007     .63
## 7 group:planar_intersection:straight 1, 32 0.02       0.54  .02     .47


### mixed model version
all_mix <- mixed(valence~straight*planar_intersection*group +(1+straight*planar_intersection|participant), data=all,family=binomial,check_contrasts=FALSE,method="LRT")


##                              Effect df     Chisq p.value
## 1                           straight  1      0.00    >.99
## 2                planar_intersection  1 41.91 ***  <.0001
## 3                              group  1      0.07     .79
## 4       straight:planar_intersection  1      0.19     .66
## 5                     straight:group  1    4.38 *     .04
## 6          planar_intersection:group  1 14.01 ***   .0002
## 7 straight:planar_intersection:group  1      0.47     .49



###############################################################################################
################## EXPLORATORY ANALYSES #######################################################
###############################################################################################

########### mixed model simple ###############################################################

straight_mix <- mixed(valence~straight*planar_intersection+(1+straight*planar_intersection|participant)+(1|object), data=straight_group,family=binomial,check_contrasts=FALSE,method="LRT")

## Data: straight
## Df full model: 14
##                         Effect df     Chisq p.value
## 1                     straight  1   8.90 **    .003
## 2          planar_intersection  1 20.28 ***  <.0001
## 3 straight:planar_intersection  1      0.22     .64


######## rationale: as we had a lot of factors, and models did not always converge, we tested several combinations of random effects
####### without factors as predictors, just to see which random effects were supported by models
###### in data with all stimuli, models converge when we put just planar_intersection+straight as random effect, without intercept (0+straight+planar)+(1|object)
####### in data without cubes, models converge when we put intercept+planar_intersection+straight (straight+planar_intersection)+(1|object)


######### Testing random effects and model convergence

mix_random <- mixed(valence~1+(0+straight+planar_intersection|participant)+(1|object), data=curve_all,family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")
mix_random <- mixed(valence~1+(straight+planar_intersection|participant)+(1|object), data=curve_less,family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")

####### it works with (1+planar*straight|participant)+(1|object) !!!!!

mix_random <- mixed(valence~1+(1+straight*planar_intersection|participant)+(1|object), data=curve_less,family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")


######### model with all curves
#### symetry of curve (R3), length, maximum curvature, diff min and max curvature
mix_all <- mixed(valence~sym_curvcateg+len+straight+planar_intersection+(0+straight+planar_intersection|participant)+(1|object), data=curve_all,family=binomial,check_contrasts=FALSE,method="LRT")   


######### model with only curves on which curvature degree is defined, with random effects which converged on previous models
mix_less <- mixed(valence~cmax+cmean+diff_maxmin+sym_curvcateg+len+straight+planar_intersection+(1+straight*planar_intersection|participant)+(1|object), data=curve_less,family=binomial,
check_contrasts=FALSE,method="LRT")   





########################## brouillon ##############################################################################################################

########## mixed model with sumetry for curve #####################################
mix_sym1 <- mixed(valence~straight*planar_intersection+sym_curvcateg+(1+straight*planar_intersection+sym_curvcateg|participant)+(1|object), data=curve_all,family=binomial,check_contrasts=FALSE,
method="LRT")

## Df full model: 21
##                         Effect df   Chisq p.value
## 1                     straight  1    0.81     .37
## 2          planar_intersection  1 8.63 **    .003
## 3                sym_curvcateg  1    1.47     .23
## 4 straight:planar_intersection  1    0.14     .71
## ---

mix_sym2 <- mixed(valence~straight*planar_intersection+sym_surfcateg+(1+straight*planar_intersection+sym_surfcateg|participant)+(1|object), data=curve_all,family=binomial,check_contrasts=FALSE,
method="LRT")

## Df full model: 21
##                         Effect df     Chisq p.value
## 1                     straight  1      1.28     .26
## 2          planar_intersection  1 11.30 ***   .0008
## 3                sym_surfcateg  1      1.26     .26
## 4 straight:planar_intersection  1      0.01     .93

mix_diff <- mixed(valence~straight*planar_intersection+diff_maxmin+sym_surfcateg+(straight*planar_intersection+diff_maxmin+sym_surfcateg|participant)+(1|object), data=curve_less,family=binomial,
check_contrasts=FALSE,method="LRT") 

mix_mermod <- mixed(valence~straight*planar_intersection+diff_maxmin+sym_surfcateg+(straight*planar_intersection+diff_maxmin+sym_surfcateg|participant)+(1|object), data=curve_less,family=binomial,
check_contrasts=FALSE,method="LRT", return="merMod") 



##### figure

pred_mean <- data.frame(emmeans(mix_mean,specs=c("cmean", "straight"), cov.reduce=F, type="response"))
pred_mean$straight=factor(pred_mean$straight, levels=c("y", "n"), labels=c("Straight", "Not straight"))


cm_plot=aggregate(valence~participant+cmean+straight, data=curve_less, FUN=mean)
cm_plot$straight=factor(cm_plot$straight, levels=c("y", "n"), labels=c("Straight", "Not straight"))

###### exploration of relation between mean curvature and pos answers
plot2 <- ggplot(data=cm_plot, aes(x=straight, y=valence, color=factor(cmean)))+
	geom_point(alpha=1/5, size=1.5, position=position_jitter(w=0.15,h=0))+
	  ## geom_point(data=cm_plot,aes(y=valence, color=factor(cmean)), alpha=1/5, size=1.5,position=position_jitter(w=1,h=0.5))+
        ## geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),size=0.2, width=0.2)+
	## facet_grid(~straight)+
	theme_classic()
plot2 <-themetiny(plot2)
plot2 <- plot2 + ggtitle("Relation between mean curvature and positive answers") + xlab("cmean")+ylab("% of positive answers")+
## scale_x_discrete( labels=c("Non planar intersection", "Planar intersection"))
dev.new(width=7,height=4)
plot2





########### FIGURES


##### two groups correlation of answers
straight_yes=aggregate(valence~object+planar_intersection, data=straight_group, FUN=mean)
int_yes=aggregate(valence~object+planar_intersection, data=inter, FUN=mean)
names(straight_yes)[3]="val_straight"
names(int_yes)[3]="val_inter"

all_yes=merge(straight_yes, int_yes, by=c("object", "planar_intersection"))

cor.test(all_yes$val_straight, all_yes$val_inter)

## 	Pearson's product-moment correlation

## data:  all_yes$valence.x and all_yes$valence.y
## t = 6.7907, df = 24, p-value = 5.045e-07
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.6176656 0.9118820
## sample estimates:
##       cor 
## 0.8109859

all_yes$planar_intersection=factor(all_yes$planar_intersection, levels=c("1", "0"), labels=c("Planar", "Non-planar")) 


cory <- ggplot(all_yes, aes(x=val_inter, y=val_straight)) + 
  geom_smooth(method=lm, color="#A7E4A7")+
	theme_classic()
cory <- cory + geom_point(data=all_yes, aes(x=val_inter, y=val_straight, shape=planar_intersection), size=4)  
cory <-themetiny(cory)
cory <- cory + scale_y_continuous( limits=c(-0.01,1.001))+ scale_x_continuous(limits=c(-0.01,1.001))
cory <- cory + ggtitle("Relation between the % of positive answers") + xlab("Is this a planar intersection?")+ylab("Is this a straight line?")+
dev.new(width=10,height=8)
cory

################# FIGURES ###################################################################



####### aesthetic functions
mycolors=function(ggobject){
ggobject+
scale_color_manual(values = c("#00AFBB", "#E7B800", "#CC0066", "#FF6666", "#9999FF", "#D16103","#FFDB6D" ))
}

myblues=function(ggobject){
ggobject+
scale_color_manual(values = c("#CCFFFF", "#99FFFF", "#99CCFF","#6699CC", "#336699", "#003399", "#003366" ))
}

mygreens=function(ggobject){
ggobject+
scale_color_manual(values = c("#ABFAAB", "#A7E4A7", "#8DC08D", "#33AE33", "#1D811D", "#456E45", "#1F3A1F"))
}


themetiny=function(ggobject){
ggobject+
theme(plot.title = element_text(face="bold",size=20,hjust = 0.5))+
theme(axis.text=element_text(size=15,face="bold"),                                                                                                                                                 
axis.title=element_text(size=15,face="bold"),
legend.text=element_text(size=15))
}

###### % of positive answers on the anova ###############################################################################################
pred_straight <- data.frame(emmeans(main_straight,specs=c("planar_intersection", "straight"), cov.reduce=F, type="response"))
pred_straight$straight=factor(pred_straight$straight, levels=c("X1", "X0"), labels=c("1", "0"))
pred_straight$planar_intersection=factor(pred_straight$planar_intersection, levels=c("X1", "X0"), labels=c("1", "0"))

straight_plot=aggregate(valence~participant+straight+planar_intersection, data=straight_group, FUN=mean)
straight_plot$straight=factor(straight_plot$straight, levels=c("1", "0"))
straight_plot$planar_intersection=factor(straight_plot$planar_intersection, levels=c("1", "0"))

###### % of positive answers in straight group according to the type of line : planar vs non planar intersection
plot2 <- ggplot(data=pred_straight, aes(x=planar_intersection, y=emmean))+
	 geom_point(size=4)+
	 geom_point(data=straight_plot,aes(y=valence),  shape=17, color="#C67A51", alpha=1/3, size=4	    ,position=position_jitter(w=0.1,h=0))+
        geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),size=0.8, width=0.4)+
	facet_grid(~straight)+
	theme_classic()
plot2 <-themetiny(plot2)
plot2 <- plot2 + ggtitle("Positive answers according to planar intersections") + xlab("Planar section")+ylab("% of positive answers")+
dev.new(width=10,height=10)
plot2

#### Straigth and intersection groups together ###############################


all_plot=aggregate(valence~group+object, data=all, FUN=mean)
all_plot$group=factor(all_plot$group, levels=c("planar_intersection", "straight_line"), labels=c("Is this a planar intersection?", "Is this a straight line?"))
p1=aggregate(valence~object, data=straight_group, FUN=mean)
p2=aggregate(valence~object, data=inter, FUN=mean)
p1$group="Straight line"
p2$group="Planar intersection"
plot(p1$valence, p2$valence)
names(p1)[2]="val_straight"
names(p2)[2]="val_inter"
bb=merge(p1, p2, by="object")

#### plot2
comp <- ggplot(data=p1, aes(x=object, y=val_straight, color=factor(group)))+
	geom_point(size=6,  position=position_jitter(w=0.2,h=0))+
	geom_point(data=p2, aes(x=object, y=val_inter, color=factor(group)), size=6)+
	theme_classic()
comp <-themetiny(comp)
comp <- comp + ggtitle("Answers of the two groups") + xlab("Is this a planar intersection?")+ylab("Is this a straight line?")+
         scale_x_discrete( labels=c("Non planar intersection", "Planar intersection"))
dev.new(width=7,height=4)
comp

#### plot3
comp <- ggplot(data=bb, aes(y=val_straight, x=val_inter))+
	geom_point(alpha=1/2, size=4)+
	theme_classic()
comp <-themetiny(comp)
comp <- comp+ scale_y_continuous( limits=c(-0.01,1.001))+ scale_x_continuous(limits=c(-0.01,1.001))
comp <- comp + ggtitle("Relation between the % of positive answers") + xlab("Is this a straight line?")+ylab("Is this a planar intersection?")+
dev.new(width=15,height=15)
comp





########################################################################################################
###################################### STUDY 2 #########################################################
########################################################################################################
###### dans le fichier prep mettre noms pls explicites aux critères de selection
###### renommer ces parties avec les nouvelles cols
############# participants
maths_group <- read.table("maths_time_and_acc.csv", sep=";", dec=",", header=T)
part_maths<-aggregate(acc~id+sex+age+group+included+c1+c3+c4+c5, data=maths_group, FUN=mean)


### two groups: one corresponding to the sample size of prereg used in the main analyses,  and the sample size with everyone, for exploratory analyses. 
#### all included
parts_maths_in<-subset(part_maths, included==1 & group=="maths")
maths_group_in<-subset(maths_group, included==1 & group=="maths")
#### first 38 parts included
chat<-as.vector(parts_maths_in$id)
#### R est un malade il compte pareil de 0 à 38 et de 1 à 38
chat<-sort(chat)[0:38]
parts_maths_f<-subset(parts_maths_in, id%in%chat)
maths_f<-subset(maths_group_in, id%in%chat)

############ stimuli
prop_maths <- read.table("prop_curves_maths.txt", header=T)


############## ANALYSES #########################################################################################
#################################################################################################################

### means
means_f <- aggregate(cbind(resp,acc)~planar+straight,data=maths_f,FUN=mean)
means_all <- aggregate(cbind(resp,acc)~planar+straight,data=maths_group_in,FUN=mean)

main_maths = aov_car(resp ~ planar * straight + Error(id/(planar*straight)) , data =maths_f,
anova_table = list(correction = "none", es = "pes"), check.contrasts = TRUE)
main_maths
####### ici ajouter le contrôle avec le mixed model ????

aggregate(acc~planar+straight+group+included,data=bysuj,FUN=mean)

#### sans topc
aggregate(acc~planar+straight+group+included,data=bysujnotopc,FUN=mean)


aggregate(acc~surface+straight+planar+object,data=datay,subset=group=="maths",FUN=mean)

ca=list("sphe_c_i_pc", "cyli_c_i_el", "cone_c_i_tr")
cb=list("sphe_c_ni_ca","cyli_c_ni_eld","cone_c_ni_tr")

moyca=aggregate(resp~ id+group+included, data=subset(datay, object%in%ca), FUN=mean)
moycb=aggregate(resp~ id+group+included, data=subset(datay, object%in%cb), FUN=mean)

moyca$CI <- moyca$resp
moycb$CnI <- moycb$resp
moyca$resp <- NULL
moycb$resp <- NULL

moycm=merge(moyca,moycb)
moycm$id <- factor(moycm$id)
moycm$group <- factor(moycm$group)
moycm$included <- factor(moycm$included)
moycm$diffpairC <- moycm$CI-moycm$CnI

aggregate(CI~included,data=moycm,FUN=mean)
aggregate(CnI~included,data=moycm,FUN=mean)
aggregate(diffpairC~included,data=moycm,FUN=mean)

### non-mathématiciens: diff = 33% (33 vs 67)
### mathématiciens: diff = 44% (22 vs 67)
### experts: diff = 0% (0 vs 0)



#### ma methode
ttc=t.test(moycm$CnI, moycm$CI, paired=T)
ttc
print("sd_groupA")
sd(moya$DI-moya$CI)

#############################################################################################
SUPPLEMENTARY ANALYSES 
#############################################################################################
mix_tout <- mixed(resp~straight*planar_intersection+(1+straight*planar_intersection|id)+(1|object), data=subset(datay, group="maths" & included==1),family=binomial,check_contrasts=FALSE,
method="LRT")


######## rationale: as we had a lot of factors, and models did not always converge, we tested several combinations of random effects
####### without factors as predictors, just to see which random effects were supported by models
###### in data with all stimuli, models converge when we put just planar_intersection+straight as random effect, without intercept (0+straight+planar)+(1|object)
####### in data without cubes, models converge when we put intercept+planar_intersection+straight (straight+planar_intersection)+(1|object)


############################################################################################
############### EXPLORATIONS SYM AND CURVATURE ##############################################
#############################################################################################
prop_maths <- read.table("prop_curves_maths.txt", header=T)
datay_prop = merge(subset(prop_maths, select=-c(surface, straight, planar_intersection, pair)), datay, by="object")


######### Testing random effects and model convergence

mix_random <- mixed(resp~1+(0+straight+planar|id)+(1|object), data=datay_prop,family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")
mix_random <- mixed(resp~1+(straight+planar|id)+(1|object), data=datay_prop, family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")

######### model with all curves
mix_sym1 <- mixed(resp~sym_curvcateg+straight*planar+(0+straight*planar|id)+(1|object), data=datay_prop,family=binomial,check_contrasts=FALSE,method="LRT")
mix_sym2 <- mixed(resp~sym_surfcateg+straight*planar+(0+straight*planar|id)+(1|object), data=datay_prop,family=binomial,check_contrasts=FALSE,method="LRT")
mix_diff1 <- mixed(resp~diff_maxmin+straight*planar+(0+straight*planar|id)+(1|object), data=datay_prop,family=binomial,check_contrasts=FALSE,method="LRT")
mix_diff2 <- mixed(resp~diff_maxmean+straight+planar+(0+straight+planar|id)+(1|object), data=datay_prop,family=binomial,check_contrasts=FALSE,method="LRT")
mix_av <- mixed(resp~cmean+straight*planar+(0+straight*planar|id)+(1|object), data=datay_prop,family=binomial,check_contrasts=FALSE,method="LRT")



################ STIMS PROPERTIES #################################################


##   straight planar_intersection      diff
aggregate(diff~straight+planar_intersection, data=prop_maths, FUN=mean)
###   straight planar_intersection      diff
### 1        n                   n 0.7549444
### 2        y                   n 0.2426667
### 3        n                   y 0.9411000
### 4        y                   y 0.0000000


aggregate(cmax~straight+planar_intersection, data=prop_maths, FUN=mean)
##  straight planar_intersection      cmax
 
###   straight planar_intersection     cmax
### 1        n                   n 1.274156
### 2        y                   n 0.790400
### 3        n                   y 1.424700
### 4        y                   y 0.491675
### > 

#### creating one df without stims on wich curvature is not defined ###############
prop_lessm <- subset(prop_maths, is.na(prop_maths$cmean)==F)


prop_lessm$straight=factor(prop_lessm$straight, levels=c("y", "n"), labels=c("Straight", "Not straight"))
prop_lessm$planar_intersection=factor(prop_lessm$planar_intersection, levels=c("y", "n"), labels=c("Planar", "Non planar"))

##### cmean according to difference minimum-maximum curvature/ according to Straight factor
curve_diff <- ggplot(aes(x=cmean, y=diff2, color=factor(object)), data=prop_lessm)+
	geom_point(aes(shape = straight), alpha=1/2, size=3, position=position_jitter(h=0.2))+
	facet_grid(~planar_intersection)+
	theme_classic()+
        ggtitle("Relation between distance minimum-maximum curvature and mean curvature in the stims") + xlab("Mean curvature")+ylab("Distance between min and max curvature")
curve_diff

##### cmean according to difference minimum-maximum curvature/ according to planarity factor
curve_diff2 <- ggplot(aes(x=cmean, y=diff2, color=factor(object)), data=prop_lessm)+
	geom_point(aes(shape = planar_intersection), alpha=1/2, size=3, position=position_jitter(h=0.2))+
	facet_grid(~straight)+
	theme_classic()+
        ggtitle("Relation between distance minimum-maximum curvature and mean curvature in the stims") + xlab("Mean curvature")+ylab("Distance between min and max curvature")
 
curve_diff2


##### syms according to sym / according to Straight factor
curve_sym2 <- ggplot(aes(x=sym, y=len, color=factor(object)), data=prop_all)+
	geom_point(aes(shape = planar_intersection), alpha=1/2, size=3, position=position_jitter(h=0.4))+
	facet_grid(~ straight)+
	theme_classic()+
        ggtitle("Relation between lenght and symetries in the stims") + xlab("Symetries")+ylab("lenght")
curve_sym2

##### syms according to sym / according to Planar intersection factor 
curve_sym <- ggplot(aes(x=sym, y=valence, color=factor(object)), data=curve_less)+
	geom_point(aes(shape = straight), alpha=1/2, size=3, position=position_jitter(h=0.4))+
	facet_grid(~planar_intersection)+
	theme_classic()+
        ggtitle("Relation between distance minimum-maximum curvature and symetries in the stims") + xlab("Symetries")+ylab("Distance between min and max curvature")
curve_sym




########################FIGURES ##################################################
##################################################################################


mycolors=function(ggobject){

ggobject+
scale_color_manual(values = c( "seagreen3", "#FF6666", "#9999FF", "#D16103","#FFDB6D" ))

}

themetiny=function(ggobject){

ggobject+
theme(plot.title = element_text(face="bold",size=25,hjust = 0.5))+
theme(axis.text=element_text(size=17,face="bold"),                                                                                                                                                 
axis.title=element_text(size=17,face="bold"),
legend.text=element_text(size=15),
legend.title=element_text(size=15))
}

###### % of positive answers on the mixed model ###############################################################################################
pred_straight <- data.frame(emmeans(mix_tout,specs=c("planar", "straight"), cov.reduce=F, type="response"))
pred_straight$straight=factor(pred_straight$straight, levels=c("1", "0"), labels=c("Straight", "Not straight"))

straight_plot=aggregate(resp~id+straight+planar, data=subset(datay, group=="maths" & included==1), FUN=mean)
straight_plot$straight=factor(straight_plot$straight, levels=c("1", "0"), labels=c("Straight", "Not straight"))
###### % of positive answers in straight group according to the type of line : planar vs non planar intersection
plot2 <- ggplot(data=pred_straight, aes(x=planar, y=prob))+
	  geom_point(data=straight_plot,aes(y=resp),  color="#FF9999",alpha=1/2.5, size=4,position=position_jitter(w=0.15,h=0))+
	  	geom_point(size=4)+
	## geom_point(data=straight_plot, alpha=1/5, size=4, position=position_jitter(w=0.2, h=0))+
        geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),size=0.2, width=0.2)+
	facet_grid(~straight)+
	theme_classic()
plot2 <-themetiny(plot2)
plot2 <- plot2 + ggtitle("Positive answers according to planar intersections") + xlab("Planar section")+ylab("% of positive answers")+
scale_x_discrete( labels=c("Planar intersection", "Non-planar intersection"))
dev.new(width=7,height=7)
plot2


###### according to accuracy
mix_acc <- mixed(acc~straight*planar+(1+straight*planar|id)+(1|object), data=subset(datay, group="maths" & included==1),family=binomial,check_contrasts=FALSE,
method="LRT")                                                                                                                                                                                            

pred_acc <- data.frame(emmeans(mix_acc,specs=c("planar", "straight"), cov.reduce=F, type="response"))

straight_acc=aggregate(acc~id+straight+planar, data=subset(datay, group=="maths" & included==1), FUN=mean)
straight_acc$straight=factor(straight_plot$straight, levels=c("1", "0"), labels=c("Straight", "Not straight"))

###### % of positive answers in straight group according to the type of line : planar vs non planar intersection
plot3 <- ggplot(data=pred_acc, aes(x=planar, y=prob, shape=planar))+
	geom_point(size=4)+
	  geom_point(data=straight_acc,aes(y=acc),  color="#FF9999",alpha=1/2.5, size=4,position=position_jitter(w=0.15,h=0))+
	## geom_point(data=straight_plot, alpha=1/5, size=4, position=position_jitter(w=0.2, h=0))+
        geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),size=0.2, width=0.2)+
	facet_grid(~straight)+
	theme_classic()

plot3 <- plot3 + ggtitle("Positive answers according to planar intersections") + xlab("Planar section")+ylab("% of positive answers")+
scale_x_discrete( labels=c("Planar intersection", "Non-planar intersection"))
dev.new(width=7,height=7)
plot3

#### all stims meaning of yes answer
straight_obj=aggregate(resp~straight+planar+object, data=subset(datay, group=="maths" & included==1), FUN=mean)
obj <- ggplot(data=straight_obj, aes(x=object, y=resp, color=planar, shape=straight))+
       geom_point(size=4)+
	theme_classic()+
	theme(axis.text.x = element_text(angle = 60, hjust = 1))
obj


###### response time (means)
meantime=aggregate(time~id+straight+planar, data=times, FUN=mean)
rawy=aggregate(time~straight+planar, data=times, FUN=mean)
timy <- ggplot(data=rawy, aes(x=planar, y=log(time), color=planar))+
        geom_point(data=meantime, alpha=1/5, size=2, position=position_jitter(w=0.2, h=0))+
	geom_point(size=4)+
	theme_classic()+
	facet_grid(~straight)
timy=mycolors(timy)

timy

mean_r=aggregate(time~id+resp, data=times, FUN=mean)
raw_r=aggregate(time~resp, data=times, FUN=mean)
tim_r <- ggplot(data=raw_r, aes(x=factor(resp), y=log(time)))+
        geom_point(data=mean_r, alpha=1/5, size=2, position=position_jitter(w=0.2, h=0))+
	geom_point(size=4)+
	theme_classic()
	
timy=mycolors(tim_r)

tim_r
