#FDA Tylan Trial Performance Data
#JDY

# Library  ----------------------------------------------------------------
library(lme4)
library(car)
library(emmeans)

#set working directory
setwd("C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Raw Data")


# Import data and clean ---------------------------------------------------

dat <-  read_excel("KUN Tylosin 2021 Master SAS to Morley.xlsx")
View(dat)

dat <- clean_names(dat)

#create psuedo pen
dat$pspen <- c(1:60)

#set factors
dat$block <- as.factor(dat$block)
dat$tmt_num <-  as.factor(dat$tmt_num)
dat$lot <- as.factor(dat$lot)

summary(dat)
sum(dat$headin)
sum(dat$headout)

datcount <- dat %>% group_by(tmt_num) %>% summarise(sum(headin), sum(headout))

dat$inwtkg <- dat$inwt/2.204
summary(dat)
sd(dat$inwtkg)
# Look at data ------------------------------------------------------------

histogram(dat$headin)
histogram(dat$headout)
histogram(dat$inwt)
histogram(dat$headout)
histogram(dat$outwt)
histogram(dat$deads_all)
histogram(dat$dl)
histogram(dat$adgdi)
histogram(dat$adgdo)
histogram(dat$dmcdo)
histogram(dat$dmcdi)
histogram(dat$dmi)
histogram(dat$dof_calc_headdays)
histogram(dat$d_ppct)
histogram(dat$hcw)
histogram(dat$pr_count)
histogram(dat$ch_count)
histogram(dat$prch_count)
histogram(dat$select_count)
histogram(dat$subselect_count)
histogram(dat$yg1_count)
histogram(dat$yg2_count)
histogram(dat$yg12_count)
histogram(dat$yg3_count)
histogram(dat$yg4_count)
histogram(dat$yg5_count)
histogram(dat$yg45_count)

# ANOVA set up for production traits of continuous data --------------------------------------

##in weight
# test for normality
shapiro.test(dat$inwt) # normal


model3 <- lmer(inwt ~ tmt_num + (1|block), data = dat)
summary(model3)
Anova(model3, type = 3)

anova(model3) # no difference 0.95
emmeans(model3, pairwise~tmt_num)


##out weight
#test for normality
shapiro.test(dat$outwt) # normal

dat$outwtkg <- dat$outwt / 2.204

model4 <- lmer(outwt ~ tmt_num + (1|block), data = dat)
summary(model4)
Anova(model4, type = 3)
anova(model4) # no difference 0.99

emmeans(model4, pairwise~tmt_num)


##ADG deads in
#test for normalilty
shapiro.test(dat$adgdi) # normal
`
dat$adgdikg <- dat$adgdi / 2.204

model7 <- lmer(adgdi ~ tmt_num + (1|block), data = dat )
summary(model7)

Anova(model7) # no difference 0.97
emmeans(model7, pairwise~tmt_num)


##ADG deads out
#test for normality
shapiro.test(dat$adgdo)# normal

dat$adgdokg <- dat$adgdo / 2.204

model8 <- lmer(adgdokg ~ tmt_num + (1|block), data = dat)
summary(model8)

anova(model8)
emmeans(model8, pairwise ~ tmt_num)

##DM Conversiondeads out
#test for normality
shapiro.test(dat$dmcdo)# normal

model9 <- lmer(dmcdo ~ tmt_num + (1|block), data = dat)
summary(model9)

anova(model9) #no difference 0.95
emmeans(model9, pairwise~ tmt_num)

##DM Conversion deads in
#test for normality
shapiro.test(dat$dmcdi) # normal

model10 <-lmer(dmcdi ~ tmt_num + (1|block), data = dat)
summary(model10)

anova(model10)# not significant 0.94
emmeans(model10, pairwise~ tmt_num)

##DMI
#test for normality
shapiro.test(dat$dmi)#not normal

dat$dmikg <- dat$dmi / 2.204

#model
model28 <- glmer(dmi ~ tmt_num + (1|block), data = dat)
summary(model28)
emmeans(model28, pairwise~ tmt_num)
Anova(model28, type = 3)

kruskal.test(dat$dmi, dat$tmt_num)
friedman.test(dat$dmi, dat$tmt_num, dat$block)

##GF
dat$gf <- dat$adgdokg / dat$dmikg

model32 <- lmer(gf ~ tmt_num + (1|block), data = dat)
summary(model32)
emmeans(model32, pairwise ~ tmt_num)
Anova(model32, type = 3)

##FG
dat$fg <- dat$dmi/dat$adgdi

model39 <- lmer(fg~ tmt_num + (1|block), data = dat)
summary(model39)
emmeans(model39, pairwise~ tmt_num)
Anova(model39)

##DOF
#test for normality
shapiro.test(dat$dof_calc_headdays)# not normal
#model
model29 <- glmer(dof_calc_headdays ~ tmt_num + (1|block), data = dat)
summary(model29)
emmeans(model29, pairwise~ tmt_num)
Anova(model29, type = 3)

kruskal.test(dat$dof_calc_headdays, dat$tmt_num)

##Dressing Percent
#test for normality
shapiro.test(dat$d_ppct)#not normal
#model
model30 <-  glmer(d_ppct ~ tmt_num + (1|block), data = dat)
summary(model30)
emmeans(model30, pairwise ~ tmt_num)
Anova(model30, type = 3)

kruskal.test(dat$d_ppct, dat$tmt_num)

##HCW
#test for nomrality
shapiro.test(dat$hcw) #not normal

dat$hcwkg <- dat$hcw / 2.205

#model
model31 <- glmer(hcw ~ tmt_num + (1|block), data = dat)
summary(model31)
emmeans(model31, pairwise~ tmt_num)
Anova(model31, type = 3)

kruskal.test(dat$hcw, dat$tmt_num)

# Carcass Data as counts --------------------------------------------------
##Head in 
model1 <- glmer(headin ~ tmt_num + (1|block), family = "poisson", data = dat)
summary(model1)
emmeans(model1, pairwise~tmt_num, type = "response")
Anova(model1, type = 3)

##Head Out
model2 <- glmer(headout~tmt_num + (1|block), family = "poisson", data = dat)
summary(model2)
emmeans(model2, pairwise~tmt_num, type = "response")
Anova(model2, type = 3)

##deads
model5 <- glmer(deads_all~ tmt_num + (1|block), family = "poisson", data = dat)
summary(model5)
emmeans(model5, pairwise~tmt_num, type = "response")

##Deathloss %
model27 <- glmer(dl ~ tmt_num + (1|block), data = dat)
summary(model27)
emmeans(model27, pairwise~tmt_num, type = "response")
Anova(model27,type = 3)

##Prime
dat$propp <- dat$headout-dat$pr_count
model34 <- glmer(cbind(pr_count, propp) ~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model34)
emmeans(model34, pairwise~ tmt_num, type = "response")
Anova(model34, type = 3)

model15 <- glmer((pr_count/headout)~ tmt_num + (1|block) + (1|pspen), data = dat)  
summary(model15)

emmeans(model15, pairwise~ tmt_num, type = "response")#no differences
sjPlot::tab_model(model15)
plot_model(model15, type = "pred", terms = c("tmt_num"))



##Choice
dat$chopp <- dat$headout-dat$ch_count
model33 <- glmer(cbind(ch_count, chopp) ~ tmt_num + (1|block) + (1|pspen), family = "binomial", data = dat)
summary(model33)
emmeans(model33, pairwise~ tmt_num, type = "response")
Anova(model33, type = 3)

#model16 <- glmer((ch_count/ headout)~tmt_num + (1|block), family = "binomial", data = dat)
#summary(model16)

#emmeans(model16, pairwise~tmt_num, type = "response")
#sjPlot::tab_model(model16)

##Premium Choice
model17 <- glmer((prch_count / headout)~tmt_num + (1|block), data = dat)
summary(model17)

emmeans(model17, pairwise~tmt_num)
sjPlot::tab_model(model17)

##Select
dat$selopp <- dat$headout- dat$select_count
model35 <- glmer(cbind(select_count, selopp) ~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model35)
emmeans(model35, pairwise~ tmt_num, type = "response")
Anova(model35, type = 3)

model18 <- glmer((select_count / headout)~ tmt_num + (1|block), data = dat)
summary(model18)

emmeans(model18, pairwise~tmt_num)
sjPlot::tab_model(model18)

##Sub-select 
model19 <- glmer((subselect_count / headout)~ tmt_num + (1|block), data = dat)
summary(model19)

emmeans(model19, pairwise~ tmt_num)
sjPlot::tab_model(model19)


##YG1
dat$yg1opp <- dat$headout - dat$yg1_count
model36 <- glmer(cbind(yg1_count, yg1opp) ~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model36)
emmeans(model36, pairwise~ tmt_num, type = "response")
Anova(model36, type = 3)

model20 <- glmer((yg1_count / headout)~tmt_num + (1|block), family = "binomial", data = dat)
summary(model20)

emmeans(model20, pairwise~tmt_num)
sjPlot::tab_model(model20)

##YG2
dat$yg2opp <- dat$headout - dat$yg2_count
model37 <- glmer(cbind(yg2_count, yg2opp) ~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model37)
emmeans(model37, pairwise~ tmt_num, type = "response")
Anova(model37, type = 3)

model21 <- glmer((yg2_count / headout)~tmt_num + (1|block), data = dat)
summary(model21)

emmeans(model21, pairwise~tmt_num)
sjPlot::tab_model(model21)

##YG3
dat$yg3opp <- dat$headout - dat$yg3_count
model38 <- glmer(cbind(yg3_count, yg3opp) ~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model38)
emmeans(model38, pairwise~ tmt_num, type = "response")
Anova(model38, type = 3)


model22 <- glmer((yg3_count / headout)~tmt_num + (1|block), data = dat)
summary(model22)

emmeans(model22, pairwise~tmt_num, type = "response")
sjPlot::tab_model(model22)

##YG4
dat$yg4opp <- dat$headout - dat$yg4_count
model39 <- glmer(cbind(yg4_count, yg4opp) ~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model39)
emmeans(model39, pairwise~ tmt_num, type = "response")
Anova(model39, type = 3)

model23 <- glmer((yg4_count / headout)~tmt_num + (1|block), data = dat)
summary(model23)

emmeans(model23, pairwise~tmt_num)
sjPlot::tab_model(model23)

##YG5
dat$yg5opp <-  dat$headout - dat$yg5_count
model40 <- glmer(cbind(yg5_count, yg5opp) ~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model40)
emmeans(model40, pairwise~ tmt_num, type = "response")
Anova(model40, type = 3)
        

model24 <- glmer((yg5_count / headout)~tmt_num + (1|block), data = dat)
summary(model24)

emmeans(model24, pairwise~tmt_num)
sjPlot::tab_model(model24)

##YG1 + YG2
model25 <- glmer((yg12_count / headout)~tmt_num + (1|block), data = dat)
summary(model25)

emmeans(model25, pairwise~tmt_num)
sjPlot::tab_model(model25)

##YG4 + YG5
model26 <- glmer((yg45_count / headout)~tmt_num + (1|block), data = dat)
summary(model26)

emmeans(model26, pairwise~tmt_num)
sjPlot::tab_model(model26)

