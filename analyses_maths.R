###################################################
###################################################

##### script planar maths 31th mars 2022 version

###################################################


library(car)
library(afex)
library(ggplot2)
library(emmeans)
library(data.table)
library(stringr)


############# participants
maths_group <- read.table("data_maths.csv", sep=";", dec=",", header=T)
maths_group <- subset(maths_group, included==1)

maths_group$planar_intersection=as.factor(maths_group$planar_intersection)
maths_group$straight=as.factor(maths_group$straight)


############## ANALYSES #########################################################################################
#################################################################################################################


bysuj <- aggregate(cbind(resp,acc)~id,data=maths_group,FUN=mean)
maths_first <- as.character(bysuj$id[1:38])

demo1 <- aggregate(acc~id+age+sex+lev+dom, data=maths_group, FUN=mean)

maths_p <- subset(maths_group, id%in%maths_first)

aggregate(resp~planar+straight+group+included,data=bysuj,FUN=mean)

##### version article
main_maths = aov_car(resp ~ planar_intersection * straight + Error(id/(planar_intersection*straight)) , data =maths_p,
anova_table = list(correction = "none", es = "pes"))
main_maths


##### version thesis
main_maths_all = aov_car(resp ~ planar_intersection * straight + Error(id/(planar_intersection*straight)) , data =maths_group,
anova_table = list(correction = "none", es = "pes"))
main_maths_all


aggregate(acc~planar_intersection+straight+group+included,data=maths_group,FUN=mean)


aggregate(acc~surface+straight+planar_intersection+object,data=maths_p,subset=group=="maths",FUN=mean)


################ Group A: All planar, vary on straight #########################

a1=list("sphe_d_i_gc", "cyli_d_i_c","cube_d_i_tri","cube_d_i_ca")
a2=list("sphe_c_i_pc", "cyli_c_i_el","cube_c_i_tri", "cube_c_i_car")

##### average "yes" by subgroup by participants ##############################
moya1=aggregate(resp~ id, data=subset(maths_group, maths_group$object%in%a1), FUN=mean)
moya2=aggregate(resp~ id, data=subset(maths_group, maths_group$object%in%a2), FUN=mean)

#### straight planar 
names(moya1)[2]="SP"
#### non straight planar
names(moya2)[2]="NsP"
#### means
summary(moya1)
summary(moya2)


moya=merge(moya1, moya2, by="id")

mean(moya$SP)
mean(moya$NsP)



##### Paired Student's T-test #####
tta=t.test(moya$SP, moya$NsP, paired=T)
tta
print("sd_groupA")
sd(moya$SP-moya$NsP)

print("means_A")
mean(moya$SP)

mean(moya$NsP)



##### Group B: All non-planar, vary on straight #######
### Here more elements in subgroup non planar as cylinder has two matches, one on curvature and one on lenght

#### aggregating the two matches of cylinders "d_ni_spiral" before aggregating answers
mean_spe <- aggregate(resp~id, data=subset(maths_group, object%in%c("cyli_c_ni_spec","cyli_c_ni_spet")), FUN=mean)
names(mean_spe)[2] <- "resp"
mean_spe$object <- "cyli_c_ni_spe"

mean_spl <- aggregate(resp~id, data=subset(maths_group, object%in%c("cyli_c_ni_splc","cyli_c_ni_splt")), FUN=mean)
names(mean_spl)[2] <- "resp"
mean_spl$object <- "cyli_c_ni_spl"

maths_groupb <- rbind(subset(maths_group, select=c(id, resp, object)), mean_spe, mean_spl)


#### 
b1=list("cube_d_ni_el","cyli_d_ni_spe","cyli_d_ni_spl","cone_d_ni_rub")
b2=list("cube_c_ni_el","cone_c_ni_rub", "cyli_c_ni_spe", "cyli_c_ni_spl" )


moyb1=aggregate(resp~ id, data=subset(maths_groupb, maths_groupb$object%in%b1), FUN=mean)
moyb2=aggregate(resp~ id, data=subset(maths_groupb, maths_groupb$object%in%b2), FUN=mean)
names(moyb1)[2]="SnP"
names(moyb2)[2]="nSnP"
moyb=merge(moyb1,moyb2, by="id")


ttb=t.test(moyb$SnP, moyb$nSnP, paired=T)
ttb
print("sd_groupB")
sd(moyb$SnP-moyb$nSnP)

mean(moyb$SnP)

mean(moyb$nSnP)



## > sd(moyb$DnI-moyb$CnI)


################## PAIR C #################################"
ca=list("sphe_c_i_pc", "cyli_c_i_el", "cone_c_i_tr")
cb=list("sphe_c_ni_ca","cyli_c_ni_eld","cone_c_ni_tr")

moyca=aggregate(resp~ id+group+included, data=subset(maths_group, object%in%ca), FUN=mean)
moycb=aggregate(resp~ id+group+included, data=subset(maths_group, object%in%cb), FUN=mean)

moyca$NsNp <- moyca$resp
moycb$NsP <- moycb$resp
moyca$resp <- NULL
moycb$resp <- NULL

moycm=merge(moyca,moycb)
moycm$id <- factor(moycm$id)
moycm$group <- factor(moycm$group)
moycm$included <- factor(moycm$included)
moycm$diffpairC <- moycm$NsP-moycm$NsNp


mean(moycm$NsP)
mean(moycm$NsNp)


#### ma methode
ttc=t.test(moycm$NsP, moycm$NsNp, paired=T)
ttc
print("sd_groupA")
sd(moya$DI-moya$CI)

#############################################################################################
SUPPLEMENTARY ANALYSES 
#############################################################################################
mix_tout <- mixed(resp~straight*planar_intersection+(1+straight*planar_intersection|id)+(1|object), data=maths_group,family=binomial,check_contrasts=FALSE,
method="LRT")


######## rationale: as we had a lot of factors, and models did not always converge, we tested several combinations of random effects
####### without factors as predictors, just to see which random effects were supported by models
###### in data with all stimuli, models converge when we put just planar_intersection+straight as random effect, without intercept (0+straight+planar)+(1|object)
####### in data without cubes, models converge when we put intercept+planar_intersection+straight (straight+planar_intersection)+(1|object)


############################################################################################
############### EXPLORATIONS SYM AND CURVATURE ##############################################
#############################################################################################
prop_maths <- read.table("prop_curves_maths.txt", header=T)
maths_prop = merge(subset(prop_maths, select=-c(surface, straight, planar_intersection, pair)), maths_group, by="object")


######### Testing random effects and model convergence

mix_random <- mixed(resp~1+(0+straight+planar_intersection|id)+(1|object), data=maths_prop,family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")
mix_random <- mixed(resp~1+(straight+planar_intersection|id)+(1|object), data=maths_prop, family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")


#### fonctionnent avec les bons effets randoms
mix_random <- mixed(resp~1+(1+straight*planar_intersection|id)+(1|object), data=maths_prop, family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")


######### model with all curves
mix_all <- mixed(resp~cmax+cmean+diff_maxmin+sym_curvcateg+len+straight+planar_intersection+(1+straight*planar_intersection|id)+(1|object), data=maths_prop,
family=binomial,check_contrasts=FALSE,method="LRT")   



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


prop_lessm$straight=factor(prop_lessm$straight, levels=c("1", "0"), labels=c("Straight", "Not straight"))
prop_lessm$planar_intersection=factor(prop_lessm$planar_intersection, levels=c("1", "0"), labels=c("Planar", "Non planar"))

##### cmean according to difference minimum-maximum curvature/ according to Straight factor
curve_diff <- ggplot(aes(x=cmean, y=diff_maxmin, color=factor(object)), data=prop_lessm)+
	geom_point(aes(shape = straight), alpha=1/2, size=3, position=position_jitter(h=0.2))+
	facet_grid(~planar_intersection)+
	theme_classic()+
        ggtitle("Relation between distance minimum-maximum curvature and mean curvature in the stims") + xlab("Mean curvature")+ylab("Distance between min and max curvature")
curve_diff

##### cmean according to difference minimum-maximum curvature/ according to planarity factor
curve_diff2 <- ggplot(aes(x=cmean, y=diff_maxmin, color=factor(object)), data=prop_lessm)+
	geom_point(aes(shape = planar_intersection), alpha=1/2, size=3, position=position_jitter(h=0.2))+
	facet_grid(~straight)+
	theme_classic()+
        ggtitle("Relation between distance minimum-maximum curvature and mean curvature in the stims") + xlab("Mean curvature")+ylab("Distance between min and max curvature")
 
curve_diff2


##### syms according to sym / according to Straight factor
curve_sym2 <- ggplot(aes(x=sym, y=len, color=factor(object)), data=prop_lessm)+
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





###### % of positive answers on the mixed model ###############################################################################################

##### pour mixed model
pred_straight <- data.frame(emmeans(mix_tout,specs=c("planar_intersection", "straight"), cov.reduce=F, type="response"))

#### pour maths, first 38
pred_straight <- data.frame(emmeans(main_maths_all,specs=c("planar_intersection", "straight"), cov.reduce=F, type="response"))
pred_straight$straight=factor(pred_straight$straight, levels=c("X1", "X0"), labels=c("1", "0"), order=T)
pred_straight$planar_intersection=factor(pred_straight$planar_intersection, levels=c("X1", "X0"), labels=c("1", "0"), order=T)

straight_plot=aggregate(resp~id+straight+planar_intersection, data=subset(maths_group, group=="maths" & included==1), FUN=mean)
straight_plot$straight=factor(straight_plot$straight, levels=c("1", "0"), order=T)


straight_plot$planar_intersection= factor(straight_plot$planar_intersection, levels=c('1','0'), order=T )
straight_plot=straight_plot[order(straight_plot$planar_intersection),]

pred_straight$planar_intersection= factor(pred_straight$planar_intersection, levels=c('1','0'), order=T )
pred_straight=pred_straight[order(pred_straight$planar_intersection),]



straight_plot$straight= factor(straight_plot$straight, levels=c('1','0'), order=T )
straight_plot=straight_plot[order(straight_plot$straight),]

pred_straight$straight= factor(pred_straight$straight, levels=c('1','0'), order=T )
pred_straight=pred_straight[order(pred_straight$straight),]

###### % of positive answers in straight group according to the type of line : planar vs non planar intersection
plot2 <- ggplot(data=pred_straight, aes(x=planar_intersection, y=emmean))+
	  geom_point(data=straight_plot,aes(y=resp),  color="#C67A51",alpha=1/3, shape=17, size=4,position=position_jitter(w=0.1,h=0))+
	  geom_point(size=4)+
        geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),size=0.8, width=0.4)+
	facet_grid(~straight)+
	theme_classic()
plot2 <-themetiny(plot2)
plot2 <- plot2 + ggtitle("Positive answers according to planar intersections") + xlab("Planar section")+ylab("% of positive answers")+
dev.new(width=10,height=10)
plot2


##################### draft ##############################################################"
#########################################################################################

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
##### script planar maths 31th mars 2022 version

###################################################


library(car)
library(afex)
library(ggplot2)
library(emmeans)
library(data.table)
library(stringr)


############# participants
maths_group <- read.table("data_maths.csv", sep=";", dec=",", header=T)
maths_group <- subset(maths_group, included==1)

maths_group$planar_intersection=as.factor(maths_group$planar_intersection)
maths_group$straight=as.factor(maths_group$straight)


############## ANALYSES #########################################################################################
#################################################################################################################


bysuj <- aggregate(cbind(resp,acc)~id,data=maths_group,FUN=mean)
maths_first <- as.character(bysuj$id[1:38])

demo1 <- aggregate(acc~id+age+sex+lev+dom, data=maths_group, FUN=mean)

maths_p <- subset(maths_group, id%in%maths_first)

aggregate(resp~planar+straight+group+included,data=bysuj,FUN=mean)

##### version article
main_maths = aov_car(resp ~ planar_intersection * straight + Error(id/(planar_intersection*straight)) , data =maths_p,
anova_table = list(correction = "none", es = "pes"))
main_maths


##### version thesis
main_maths_all = aov_car(resp ~ planar_intersection * straight + Error(id/(planar_intersection*straight)) , data =maths_group,
anova_table = list(correction = "none", es = "pes"))
main_maths_all


aggregate(acc~planar_intersection+straight+group+included,data=bysuj,FUN=mean)


aggregate(acc~surface+straight+planar_intersection+object,data=maths_p,subset=group=="maths",FUN=mean)

ca=list("sphe_c_i_pc", "cyli_c_i_el", "cone_c_i_tr")
cb=list("sphe_c_ni_ca","cyli_c_ni_eld","cone_c_ni_tr")

moyca=aggregate(resp~ id+group+included, data=subset(maths_p, object%in%ca), FUN=mean)
moycb=aggregate(resp~ id+group+included, data=subset(maths_p, object%in%cb), FUN=mean)

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


#### ma methode
ttc=t.test(moycm$CnI, moycm$CI, paired=T)
ttc
print("sd_groupA")
sd(moya$DI-moya$CI)

#############################################################################################
SUPPLEMENTARY ANALYSES 
#############################################################################################
mix_tout <- mixed(resp~straight*planar_intersection+(1+straight*planar_intersection|id)+(1|object), data=maths_group,family=binomial,check_contrasts=FALSE,
method="LRT")


######## rationale: as we had a lot of factors, and models did not always converge, we tested several combinations of random effects
####### without factors as predictors, just to see which random effects were supported by models
###### in data with all stimuli, models converge when we put just planar_intersection+straight as random effect, without intercept (0+straight+planar)+(1|object)
####### in data without cubes, models converge when we put intercept+planar_intersection+straight (straight+planar_intersection)+(1|object)


############################################################################################
############### EXPLORATIONS SYM AND CURVATURE ##############################################
#############################################################################################
prop_maths <- read.table("prop_curves_maths.txt", header=T)
maths_prop = merge(subset(prop_maths, select=-c(surface, straight, planar_intersection, pair)), maths_group, by="object")


######### Testing random effects and model convergence

mix_random <- mixed(resp~1+(0+straight+planar_intersection|id)+(1|object), data=maths_prop,family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")
mix_random <- mixed(resp~1+(straight+planar_intersection|id)+(1|object), data=maths_prop, family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")


#### fonctionnent avec les bons effets randoms
mix_random <- mixed(resp~1+(1+straight*planar_intersection|id)+(1|object), data=maths_prop, family=binomial,check_contrasts=FALSE,method="LRT", return="merMod")


######### model with all curves
mix_all <- mixed(resp~cmax+cmean+diff_maxmin+sym_curvcateg+len+straight+planar_intersection+(1+straight*planar_intersection|id)+(1|object), data=maths_prop,
family=binomial,check_contrasts=FALSE,method="LRT")   



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


prop_lessm$straight=factor(prop_lessm$straight, levels=c("1", "0"), labels=c("Straight", "Not straight"))
prop_lessm$planar_intersection=factor(prop_lessm$planar_intersection, levels=c("1", "0"), labels=c("Planar", "Non planar"))

##### cmean according to difference minimum-maximum curvature/ according to Straight factor
curve_diff <- ggplot(aes(x=cmean, y=diff_maxmin, color=factor(object)), data=prop_lessm)+
	geom_point(aes(shape = straight), alpha=1/2, size=3, position=position_jitter(h=0.2))+
	facet_grid(~planar_intersection)+
	theme_classic()+
        ggtitle("Relation between distance minimum-maximum curvature and mean curvature in the stims") + xlab("Mean curvature")+ylab("Distance between min and max curvature")
curve_diff

##### cmean according to difference minimum-maximum curvature/ according to planarity factor
curve_diff2 <- ggplot(aes(x=cmean, y=diff_maxmin, color=factor(object)), data=prop_lessm)+
	geom_point(aes(shape = planar_intersection), alpha=1/2, size=3, position=position_jitter(h=0.2))+
	facet_grid(~straight)+
	theme_classic()+
        ggtitle("Relation between distance minimum-maximum curvature and mean curvature in the stims") + xlab("Mean curvature")+ylab("Distance between min and max curvature")
 
curve_diff2


##### syms according to sym / according to Straight factor
curve_sym2 <- ggplot(aes(x=sym, y=len, color=factor(object)), data=prop_lessm)+
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





###### % of positive answers on the mixed model ###############################################################################################

##### pour mixed model
pred_straight <- data.frame(emmeans(mix_tout,specs=c("planar_intersection", "straight"), cov.reduce=F, type="response"))

#### pour maths, first 38
pred_straight <- data.frame(emmeans(main_maths_all,specs=c("planar_intersection", "straight"), cov.reduce=F, type="response"))
pred_straight$straight=factor(pred_straight$straight, levels=c("X1", "X0"), labels=c("1", "0"), order=T)
pred_straight$planar_intersection=factor(pred_straight$planar_intersection, levels=c("X1", "X0"), labels=c("1", "0"), order=T)

straight_plot=aggregate(resp~id+straight+planar_intersection, data=subset(maths_group, group=="maths" & included==1), FUN=mean)
straight_plot$straight=factor(straight_plot$straight, levels=c("1", "0"), order=T)


straight_plot$planar_intersection= factor(straight_plot$planar_intersection, levels=c('1','0'), order=T )
straight_plot=straight_plot[order(straight_plot$planar_intersection),]

pred_straight$planar_intersection= factor(pred_straight$planar_intersection, levels=c('1','0'), order=T )
pred_straight=pred_straight[order(pred_straight$planar_intersection),]



straight_plot$straight= factor(straight_plot$straight, levels=c('1','0'), order=T )
straight_plot=straight_plot[order(straight_plot$straight),]

pred_straight$straight= factor(pred_straight$straight, levels=c('1','0'), order=T )
pred_straight=pred_straight[order(pred_straight$straight),]

###### % of positive answers in straight group according to the type of line : planar vs non planar intersection
plot2 <- ggplot(data=pred_straight, aes(x=planar_intersection, y=emmean))+
	  geom_point(data=straight_plot,aes(y=resp),  color="#C67A51",alpha=1/3, shape=17, size=4,position=position_jitter(w=0.1,h=0))+
	  geom_point(size=4)+
        geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),size=0.8, width=0.4)+
	facet_grid(~straight)+
	theme_classic()
plot2 <-themetiny(plot2)
plot2 <- plot2 + ggtitle("Positive answers according to planar intersections") + xlab("Planar section")+ylab("% of positive answers")+
dev.new(width=10,height=10)
plot2


##################### draft ##############################################################"
#########################################################################################

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