#FDA Study
#Liver Blinded Analysis
#JDY
# Library -----------------------------------------------------------------

source("C:/Users/danie/OneDrive - West Texas A and M University/Bioninformatics/Source Scripts/Library.R")
library(sjPlot)
library(lme4)
library(lmtest)
library(stats4)
library(emmeans)
library(epitools)
library(car)
#set working directory
setwd("C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Raw Data")

#import data set
dat <- read_excel("FDA Liver Abscess Data.18JUN2023.xlsx")
View(dat)

#clean names
dat <- clean_names(dat)
names(dat)

#create unique pen identifier
dat$pspen <- c(1:60)

#set as factor
dat$tmt_num <- as.factor(dat$tmt_num)
dat$block <- as.factor(dat$block)
dat$pen <- as.factor(dat$pen)
dat$pspen <- as.factor(dat$pspen)
dat$liver_count <- as.numeric(dat$liver_count)



summary(dat)



#count livers
datcount <- dat %>% group_by(tmt_num) %>% summarise(sum(liver_count), sum(head_liver))
datcountsevere <- dat %>% group_by(tmt_num) %>% summarise(sum(liver_a_and_a_plus), sum(head_liver))
datcount_a_plus <- dat %>% group_by(tmt_num) %>% summarise(sum(liver_a_plus), sum(head_liver))

#create counts for each treatment and each class
datsum <- dat %>% group_by(tmt_num) %>% 
  summarise(sum(head_liver),sum(liver_none), sum(liver_all), sum(liver_a_minus_only), sum(liver_a_only), sum(liver_a_plus_only),sum(liver_a_plus_open), sum(liver_a_plus_adhesion), sum(liver_a_plus_open_adhesion), sum(liver_cirr),sum(liver_fluke), sum(liver_telang))


#check order of variables
table(dat$tmt_num)

#re-level so POS is the comparison group
dat$tmt_num <- relevel(dat$tmt_num, ref = "1")

#re-check order
table(dat$tmt_num)


# Individual LA Scores Logit ----------------------------------------------

#A-
histogram(dat$liver_a_minus_only)

#create difference var (what is the opposite of the out come)
dat$a_min_opp <- dat$head_liver - dat$liver_a_minus_only

model15 <- glmer(cbind(liver_a_minus_only,a_min_opp)~ tmt_num +(1|block)+ (1|pspen), family ="binomial", data = dat )
summary(model15)
Anova(model15, type = 3)

lsmeans(model15, pairwise~tmt_num, type = "response")
sjPlot::tab_model(model15, show.ci = 0.975)
plot_model(model15, type = "pred", terms = c("tmt_num"))


#A
histogram(dat$liver_a)

#new var
dat$a_opp <- dat$head_liver - dat$liver_a

model16 <- glmer(cbind(liver_a, a_opp)~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model16)
Anova(model16, type = 3)

lsmeans(model16, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model16, show.ci = 0.975)
plot_model(model16, type = "pred", terms = c("tmt_num"))

#A+ 
histogram(dat$liver_a_plus_only)

#new var
dat$a_plus_opp <- dat$head_liver - dat$liver_a_plus_only

model17 <- glmer(cbind(liver_a_plus_only, a_plus_opp)~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model17)
Anova(model17, type = 3)

lsmeans(model17, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model17, show.ci = 0.975)
plot_model(model17, type = "pred", terms = c("tmt_num"))

#Edible
histogram(dat$liver_none)

dat$edd_opp <- dat$head_liver - dat$liver_none

model18 <- glmer(cbind(liver_none, edd_opp)~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model18)
Anova(model18, type = 3)

lsmeans(model18, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model18, show.ci = 0.975)
plot_model(model18, type = "pred", terms = c("tmt_num"))


#A+Ad
histogram(dat$liver_a_plus_adhesion)

dat$a_ad_opp <- dat$head_liver - dat$liver_a_plus_adhesion

model19 <- glmer(cbind(liver_a_plus_adhesion, a_ad_opp)~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model19)
Anova(model19, type = 3)

lsmeans(model19, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model19, show.ci = 0.975)
plot_model(model19, type = "pred", terms = c("tmt_num"))

#A+op
histogram(dat$liver_a_plus_open)

dat$a_op_opp <- dat$head_liver - dat$liver_a_plus_open

model20 <- glmer(cbind(liver_a_plus_open, a_op_opp)~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model20)
Anova(model20, type = 3)

lsmeans(model20, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model20, show.ci = 0.975)
plot_model(model20, type = "pred", terms = c("tmt_num"))

#A+ad/op
histogram(dat$liver_a_plus_open_adhesion)

dat$a_opad_opp <- dat$head_liver - dat$liver_a_plus_open_adhesion

model21 <- glmer(cbind(liver_a_plus_open_adhesion, a_opad_opp)~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model21)
Anova(model21, type = 3)

lsmeans(model21, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model21, show.ci = 0.975)
plot_model(model21, type = "pred", terms = c("tmt_num"))

#Cirrhosis
histogram(dat$liver_cirr)

dat$cirr_opp <- dat$head_liver - dat$liver_cirr

model22 <- glmer(cbind(liver_cirr, cirr_opp)~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model22)
Anova(model22, type = 3)

lsmeans(model22, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model22, show.ci = 0.975)
plot_model(model22, type = "pred", terms = c("tmt_num"))

#telang
histogram(dat$liver_telang)

dat$tlang_opp <-  dat$head_liver - dat$liver_telang

model23 <- glmer(cbind(liver_telang, tlang_opp)~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model23)
Anova(model23, type = 3)

lsmeans(model23, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model23, show.ci = 0.975)
plot_model(model23, type = "pred", terms = c("tmt_num"))

#flukes
histogram(dat$flukes_total)

dat$flukes_opp <- dat$head_liver - dat$flukes_total

model24 <- glmer(cbind(flukes_total, flukes_opp)~ tmt_num + (1|block)+ (1|pspen), family = "binomial", data = dat)
summary(model24)
Anova(model24, type = 3)

lsmeans(model24, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model24, show.ci = 0.975)
plot_model(model24, type = "pred", terms = c("tmt_num"))


# Grouped Outcomes logit -----------------------------------------------------

#minor
histogram(dat$liver_a_and_a_minus)

dat$minor_opp <- dat$head_liver - dat$liver_a_minus

model25 <- glmer(cbind(liver_a_minus, minor_opp)~ tmt_num + (1|block) + (1|pspen), family = "binomial", data = dat)
summary(model25)
Anova(model25, type = 3)

lsmeans(model25, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model25, show.ci = 0.95)
plot_model(model25, type = "pred", terms = c("tmt_num"))

#major
histogram(dat$liver_a_plus)

dat$major_opp <- dat$head_liver - dat$liver_a_and_a_plus

model26 <- glmer(cbind(liver_a_and_a_plus, major_opp)~ tmt_num + (1|block) + (1|pspen), family = "binomial", data = dat)
summary(model26)
Anova(model26, type= 3)

lsmeans(model26, pairwise~tmt_num, type = "response")

sjPlot::tab_model(model26, show.ci = 0.95)
plot_model(model26, type = "pred", terms = c("tmt_num"))

#just A+
histogram(dat$liver_a_plus_only)

dat$aplus_opp <- dat$head_liver - dat$liver_a_plus_only

model30 <- glmer(cbind(liver_a_plus_only, aplus_opp) ~ tmt_num + (1|block) + (1|pspen), family = "binomial", data = dat)
summary(model30)
Anova(model30, type = 3)
lsmeans(model30, pairwise~tmt_num, type = "response")
sjPlot::tab_model(model30, show.ci = 0.95)

#all Abscesses
histogram(dat$liver_all)

dat$la_opp <- dat$head_liver - dat$liver_all

model29 <- glmer(cbind(liver_all, la_opp)~tmt_num +(1|block) + (1|pspen), family = "binomial", data = dat)
summary(model29)
Anova(model29, type = 3)

lsmeans(model29, pairwise~tmt_num, type = "response")
Anova(model29)
residuals(model29)
plot(residuals(model29))
sjPlot::tab_model(model29, show.ci = 0.95)
plot_model(model29, type = "pred", terms = c("tmt_num"))

#check all groupings for the acutal group!!!!!

# As a percent ------------------------------------------------------------
#minor
dat$minorp <- dat$liver_a_and_a_minus / dat$head_liver
histogram(dat$minorp)
summary(dat$minorp)

model27 <- glmer(minorp~ tmt_num + (1|block),  data = dat)
summary(model27)
fixef(model27)

lsmeans(model27, pairwise~tmt_num, type = "response")
sjPlot::tab_model(model27)
plot_model(model27, type = "pred", terms = c("tmt_num"))

#major
dat$majorp <- dat$liver_a_plus_only / dat$head_liver
histogram(dat$majorp)
summary(dat$majorp)


model28 <- lmer(majorp~tmt_num + (1|block), data = dat)
summary(model28)
lsmeans(model28, pairwise~tmt_num, type = "response")
Anova(model28)
sjPlot::tab_model(model28)
plot_model(model28, type = "pred", terms = c("tmt_num"))

#all abscesses
dat$all <- dat$liver_all/dat$head_liver
summary(dat$all)

model40 <- lmer(all ~ tmt_num + (1|block), data = dat)
summary(model40)
lsmeans(model40, pairwise~tmt_num)
Anova(model40)

# Individual LA scores Poisson ----------------------------------------------------

#A- livers
histogram(dat$a)

model <- glmer(liver_a_minus_only ~ tmt_num + (1|block), family = "poisson"(link = log), data = dat)
summary(model)
# output the results

gf = vcd::goodfit(dat$a, 
                  type= "poisson", 
                  method= "ML")
summary(gf)
plot(gf, main="Count data vs Poisson distribution")

lsmeans(model, pairwise~tmt_num, adjust="tukey", type="response")

sjPlot::tab_model(model)
plot_model(model, type = "pred", terms = c("tmt_num"))


### non count data
dat$ap <- (dat$a / dat$liver_count)

modelt <- lmer(ap~ tmt_num + (1|block), data = dat)
summary(modelt)

lsmeans(modelt, pairwise~ tmt_num)

plot(modelt)
residuals(modelt)
plot(residuals(modelt))

plot_model(modelt, type = "pred", terms = c("tmt_num"))


#A
histogram(dat$a_2)

model2 <- glmer(a_2 ~ tmt_num + (1|block), family = "poisson"(link=log), data = dat)
summary(model2)

gf2 = vcd::goodfit(dat$a_2, 
                  type= "poisson", 
                  method= "ML")
summary(gf2)
plot(gf2, main="Count data vs Poisson distribution")

lsmeans(model2, pairwise~tmt_num, adjust="tukey", type= "response")



sjPlot::tab_model(model2)
plot_model(model2, type = "pred", terms = c("tmt_num"))



#A+
histogram(dat$a_3)

model3 <- glmer(a_3~ tmt_num + (1|block), family = "poisson"(link = log), data = dat)
summary(model3)
gf3 = vcd::goodfit(dat$a_3, 
                   type= "poisson", 
                   method= "ML")
summary(gf3)
plot(gf3, main="Count data vs Poisson distribution")

lsmeans(model3, pairwise~tmt_num, adjust="tukey", type= "response")
sjPlot::tab_model(model3)
plot_model(model2, type = "pred", terms = c("tmt_num"))



#edible
histogram(dat$normal)

model4 <- glmer(normal~tmt_num + (1|block), family = "poisson"(link=log), data= dat)
summary(model4)

gf4 <- vcd::goodfit(dat$normal,
               type= "poisson",
               method= "ML")
summary(gf4)
plot(gf4, main="Count data vs Poisson distribution")
lsmeans(model4, pairwise~tmt_num, adjust="tukey", type="response")



sjPlot::tab_model(model4)
plot_model(model4, type = "pred", terms = c("tmt_num"))


#A+ adhesion 
histogram(dat$a_adhesion)
model5 <- glmer(a_adhesion~ tmt_num + (1|block), family = "poisson"(link = log), data = dat)
summary(model5)

gf5 <- vcd::goodfit(dat$a_adhesion,
                    type= "poisson",
                    method= "ML")
summary(gf5)
plot(gf5, main="Count data vs Poisson distribution")
lsmeans(model5, pairwise~tmt_num, adjust="tukey", type="response")



sjPlot::tab_model(model5)
plot_model(model5, type = "pred", terms = c("tmt_num"))

#A+ open
histogram(dat$a_open)

model6 <- glmer(a_open~tmt_num + (1|block), family = "poisson", data = dat)
summary(model5)


gf6 = vcd::goodfit(dat$a_open, 
                   type= "poisson", 
                   method= "ML")
summary(gf6)
plot(gf6, main="Count data vs Poisson distribution")

lsmeans(model6, pairwise~tmt_num, adjust="tukey", type="response")



sjPlot::tab_model(model6)
plot_model(model6, type = "pred", terms = c("tmt_num"))

#A+ad/op
histogram(dat$a_adhesion_open)

model7 <- glmer(a_adhesion_open~ tmt_num + (1|block), family = "poisson"(link = log), data = dat)
summary(model7)

gf7 <- vcd::goodfit(dat$a_adhesion_open,
                    type= "poisson",
                    method= "ML")
summary(gf7)
plot(gf7, main="Count data vs Poisson distribution")
lsmeans(model7, pairwise~tmt_num, adjust="tukey", type="response")



sjPlot::tab_model(model7)
plot_model(model7, type = "pred", terms = c("tmt_num"))

#contamination
histogram(dat$contamination)

model8 <- glmer(contamination~tmt_num +(1|block), family = "poisson"(link=log), data = dat)
summary(model8)

gf8 <- vcd::goodfit(dat$contamination,
             type= "poisson",
             method= "ML")
summary(gf8)
plot(gf8, main="Count data vs Poisson distribution")
lsmeans(model8, pairwise~tmt_num, adjust="tukey", type="response")

sjPlot::tab_model(model8)
plot_model(model8, type = "pred", terms = c("tmt_num"))

#cirrhosis
histogram(dat$cirrhosis)

model9 <- glmer(cirrhosis~tmt_num + (1|block), family = "poisson"(link=log), data = dat)
summary(model9)

gf9 <- vcd::goodfit(dat$cirrhosis,
                    type= "poisson",
                    method= "ML")
summary(gf9)
plot(gf9, main="Count data vs Poisson distribution")
lsmeans(model9, pairwise~tmt_num, adjust="tukey", type="response")

sjPlot::tab_model(model9)
plot_model(model9, type = "pred", terms = c("tmt_num"))

#tlang
histogram(dat$telangiectasis)

model10 <- glmer(telangiectasis~ tmt_num +(1|block), family = "poisson"(link=log), data = dat)
summary(model10)

gf10 <- vcd::goodfit(dat$telangiectasis,
                    type= "poisson",
                    method= "ML")
summary(gf10)
plot(gf10, main="Count data vs Poisson distribution")
lsmeans(model10, pairwise~tmt_num, adjust="tukey", type="response")

sjPlot::tab_model(model10)
plot_model(model10, type = "pred", terms = c("tmt_num"))

#flukes
histogram(dat$flukes_total)

model11 <- glmer(flukes_total~ tmt_num + (1|block), family = "poisson"(link=log), data = dat)
summary(model11)

gf11 <- vcd::goodfit(dat$flukes_total,
                    type= "poisson",
                    method= "ML")
summary(gf11)
plot(gf11, main="Count data vs Poisson distribution")
lsmeans(model11, pairwise~tmt_num, adjust="tukey", type="response")

sjPlot::tab_model(model11)
plot_model(model11, type = "pred", terms = c("tmt_num"))


# Grouped Outcomes --------------------------------------------------------

#Severe (A+, A+ad, A+op, and A+ad/op)
histogram(dat$liver_a_plus)

model12 <- glmer(liver_a_plus~tmt_num +(1|block), family = "poisson"(link = log), data = dat )
summary(model12)

gf12 <- vcd::goodfit(dat$liver_a_plus,
                     type= "poisson",
                     method= "ML")
summary(gf12)
plot(gf12, main="Count data vs Poisson distribution")
lsmeans(model12, pairwise~tmt_num, adjust="tukey", type="response")

sjPlot::tab_model(model12)
plot_model(model12, type = "pred", terms = c("tmt_num"))

#minor (A- and A)
histogram(dat$liver_a_and_a_minus)

model13 <- glmer(liver_a_and_a_minus~ tmt_num + (1|block), family = "poisson"(link = log), data = dat)
summary(model13)

gf13 <- vcd::goodfit(dat$liver_a_and_a_minus,
                     type= "poisson",
                     method= "ML")
summary(gf13)
plot(gf13, main="Count data vs Poisson distribution")
lsmeans(model13, pairwise~tmt_num, adjust="tukey", type="response")

sjPlot::tab_model(model13)
plot_model(model13, type = "pred", terms = c("tmt_num"))


#total liver abscess
histogram(dat$liver_all)

model14 <- glmer(liver_all~ tmt_num + (1|block) + (1|pspen), family = "poisson"(link = log), data = dat)
summary(model14)

gf14 <- vcd::goodfit(dat$liver_all,
                     type= "poisson",
                     method= "ML")
summary(gf14)
plot(gf14, main="Count data vs Poisson distribution")
lsmeans(model14, pairwise~tmt_num, adjust="tukey", type="response")

sjPlot::tab_model(model14)
plot_model(model1, type = "pred", terms = c("tmt_num"))

gc()
