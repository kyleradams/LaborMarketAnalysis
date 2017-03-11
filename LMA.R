# Kyler Adams
rm(list = ls())


library('tidyverse')
library('psych')
library('stargazer')

######### Read in the data and see what we are dealing with
dat.read <- read_csv('LMA.csv')
View(dat.read)

######### I want to clean it up a bit by removing possible values entered in error
######### There are also some structure issues
str(dat.read)

######### Remove NAs
dat <- dat.read %>% 
    na.omit()

str(dat)

######### Convert to integers rather than strings
dat$EarningsPast12Months <- as.integer(dat$EarningsPast12Months)
dat$UsualWeeklyHours <- as.integer(dat$UsualWeeklyHours)
dat$`Worked35+HoursinaTypicalWeek` <- as.integer(dat$`Worked35+HoursinaTypicalWeek`)

str(dat)

######### Remove Values with no earnings
dat <- subset(dat, dat[2] > 0)
# View(dat)
dat

######### We have gone from 64999 observations to 46071.


######### Looking into correlations
# pairs.panels(dat[1:5]) # Don't run this it takes too long


cor(dat[2:13])


str(dat)

####### sample selection criteria for paper
####### Also mention we looked at ages uner 18 or over 65
ssc.dat <- dat %>% 
    filter(`Worked40+WeeksDuringPast12Months` == 1) %>% 
    filter(`Worked35+HoursinaTypicalWeek` == 1) %>% 
    filter(`EarningsPast12Months` > 10150) %>% 
    filter(`UsualWeeklyHours` > 0)
    
mean(ssc.dat$EarningsPast12Months)
median(ssc.dat$EarningsPast12Months)
min(ssc.dat$EarningsPast12Months)
max(ssc.dat$EarningsPast12Months)
sd(ssc.dat$EarningsPast12Months)
nrow(ssc.dat)

ssc.black.dat <- dat %>% 
    filter(`Worked40+WeeksDuringPast12Months` == 1) %>% 
    filter(`Worked35+HoursinaTypicalWeek` == 1) %>% 
    filter(`EarningsPast12Months` > 10150) %>% 
    filter(`UsualWeeklyHours` > 0) %>% 
    filter(Black == 1)

mean(ssc.black.dat$EarningsPast12Months)
median(ssc.black.dat$EarningsPast12Months)
min(ssc.black.dat$EarningsPast12Months)
max(ssc.black.dat$EarningsPast12Months)
sd(ssc.black.dat$EarningsPast12Months)
nrow(ssc.black.dat)

ssc.white.dat <- dat %>% 
    filter(`Worked40+WeeksDuringPast12Months` == 1) %>% 
    filter(`Worked35+HoursinaTypicalWeek` == 1) %>% 
    filter(`EarningsPast12Months` > 10150) %>% 
    filter(`UsualWeeklyHours` > 0) %>% 
    filter(White == 1)

mean(ssc.white.dat$EarningsPast12Months)
median(ssc.white.dat$EarningsPast12Months)
min(ssc.white.dat$EarningsPast12Months)
max(ssc.white.dat$EarningsPast12Months)
sd(ssc.white.dat$EarningsPast12Months)
nrow(ssc.white.dat)



######## Loop to find all summary statistics of Earnings
ssc.summary <- NULL
for(i in 6:13) {
    
    ssc.summary <- rbind(ssc.summary, mean(subset(ssc.dat$EarningsPast12Months, ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, mean(subset(ssc.dat$EarningsPast12Months, ssc.dat$Black == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, mean(subset(ssc.dat$EarningsPast12Months, ssc.dat$White == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, median(subset(ssc.dat$EarningsPast12Months, ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, median(subset(ssc.dat$EarningsPast12Months, ssc.dat$Black == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, median(subset(ssc.dat$EarningsPast12Months, ssc.dat$White == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, min(subset(ssc.dat$EarningsPast12Months, ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, min(subset(ssc.dat$EarningsPast12Months, ssc.dat$Black == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, min(subset(ssc.dat$EarningsPast12Months, ssc.dat$White == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, max(subset(ssc.dat$EarningsPast12Months, ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, max(subset(ssc.dat$EarningsPast12Months, ssc.dat$Black == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, max(subset(ssc.dat$EarningsPast12Months, ssc.dat$White == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, sd(subset(ssc.dat$EarningsPast12Months, ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, sd(subset(ssc.dat$EarningsPast12Months, ssc.dat$Black == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, sd(subset(ssc.dat$EarningsPast12Months, ssc.dat$White == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, sum(subset(ssc.dat[,i], ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, sum(subset(ssc.dat[,i], ssc.dat$Black == 1 & ssc.dat[,i] == 1)))
    ssc.summary <- rbind(ssc.summary, sum(subset(ssc.dat[,i], ssc.dat$White == 1 & ssc.dat[,i] == 1)))

}

as.matrix(ssc.summary)


# mean(subset(ssc.dat$EarningsPast12Months, ssc.dat$White == 1 & ssc.dat[,6] == 1))
# nrow(subset(ssc.dat$EarningsPast12Months, ssc.dat[,6] == 1))


# mean(subset(ssc.dat$EarningsPast12Months, ssc.dat$Black == 1 & ssc.dat$NoHighSchoolDegree == 1))
# median(ssc.dat$EarningsPast12Months)
# min(ssc.dat$EarningsPast12Months)
# max(ssc.dat$EarningsPast12Months)
# sd(ssc.dat$EarningsPast12Months)
# nrow(ssc.dat)



######## Summary statistics of race/educ percentages
ssc.dat %>% 
    filter(White == 1 | Black == 1) %>% 
    select(c(6:14)) %>% 
    gather("educ", "n", 1:8) %>% 
    group_by(White, educ) %>% 
    summarise(nn = sum(n))


ssc.dat %>% 
    filter(White == 1 | Black == 1) %>% 
    select(c(1,4,5,14)) %>% 
    group_by(White) %>% 
    summarise(nn = sd(Age))



######## Preliminary models
######## first filter to black and white only, 
######## then use black and NHS as reference variable
ssc.dat.mod <- ssc.dat %>%
    filter(White == 1 | Black == 1) %>% 
    select(c(1:5,7:14))


######## Race Only
pmod.1 <- lm(EarningsPast12Months~White, ssc.dat.mod)
summary(pmod.1)

######## Race and Age
pmod.2 <- lm(EarningsPast12Months~White+Age, ssc.dat.mod)
summary(pmod.2)

######## Race, Age, and Sex
pmod.3 <- lm(EarningsPast12Months~White+Age+Female, ssc.dat.mod)
summary(pmod.3)

######## Race, Age, Sex, Married
pmod.4 <- lm(EarningsPast12Months~White+Age+Female+Married, ssc.dat.mod)
summary(pmod.4)

######## All Variables
pmod.5 <- lm(EarningsPast12Months~., ssc.dat.mod)
summary(pmod.5)



######## Reverse Preliminary models
######## Education Only
pmod.6 <- lm(EarningsPast12Months~HighSchoolDegreeorGED+SomeCollege+AssociatesDegree+BachelorsDegree+MastersDegree+ProfessionalDegree+Doctorate, ssc.dat.mod)
summary(pmod.6)

######## Race and Education
pmod.7 <- lm(EarningsPast12Months~HighSchoolDegreeorGED+SomeCollege+AssociatesDegree+BachelorsDegree+MastersDegree+ProfessionalDegree+Doctorate+White, ssc.dat.mod)
summary(pmod.7)

######## Race and Education And Sex
pmod.8 <- lm(EarningsPast12Months~HighSchoolDegreeorGED+SomeCollege+AssociatesDegree+BachelorsDegree+MastersDegree+ProfessionalDegree+Doctorate+
                 White+Female, ssc.dat.mod)
summary(pmod.8)

######## Race and Education And Sex And Married
pmod.9 <- lm(EarningsPast12Months~HighSchoolDegreeorGED+SomeCollege+AssociatesDegree+BachelorsDegree+MastersDegree+ProfessionalDegree+Doctorate+
                 White+Female+Married, ssc.dat.mod)
summary(pmod.9)

######## All variables
pmod.10 <- lm(EarningsPast12Months~.- UsualWeeklyHours, ssc.dat.mod)
summary(pmod.10)


######## All variables, Whites only
ssc.dat.mod.white <- ssc.dat.mod %>% 
    filter(White == 1)

pmod.11 <- lm(EarningsPast12Months~., ssc.dat.mod.white)
summary(pmod.11)

######## All variables, Blacks only
ssc.dat.mod.black <- ssc.dat.mod %>% 
    filter(White == 0)

pmod.12 <- lm(EarningsPast12Months~., ssc.dat.mod.black)
summary(pmod.12)


######## Display results
nrow(ssc.dat.mod)
nrow(ssc.dat.mod.white)
nrow(ssc.dat.mod.black)

summary(lm(EarningsPast12Months~White+Female, ssc.dat.mod))
summary(lm(EarningsPast12Months~White+Female+(White*Female), ssc.dat.mod))

######## Display results
stargazer(as.data.frame(ssc.dat.mod), type = "text", median = T, out = "d_all.html")
stargazer(as.data.frame(ssc.dat.mod.black), type = "text", median = T, out = "d_b.html")
stargazer(as.data.frame(ssc.dat.mod.white), type = "text", median = T, out = "d_w.html")
stargazer(pmod.6,pmod.7,pmod.10, type = "text", out = "asdf.html", notes = "We used OLS test test", notes.align = "l")
args(stargazer)




######## Revised models ########

######## We can see high heteroskedasticity here on all variables
bptest(pmod.10)
bptestequation <- lm(residuals(pmod.10)*residuals(pmod.10)~. - UsualWeeklyHours, ssc.dat.mod)
summary(bptestequation) #note: BP = n*RSquared of model with squared residuals as dependent variable 
vcovHC(pmod.10, type = "HC")  #the diagonal elements are the variances of the parameter estimates

#Generate the Robust standard errors
# ProfessoinalDegree, Doctorate are smaller.
sandwich_se <- diag(vcovHC(pmod.10, type = "HC"))^0.5
sandwich_se


######## Try log model to reduce heteroskedasticity
######## Lower but still not good enough
pmod.10.log <- lm(log(EarningsPast12Months)~.- UsualWeeklyHours, ssc.dat.mod)
summary(pmod.10.log)
bptest(pmod.10.log)
bptestequation <- lm(residuals(pmod.10.log)*residuals(pmod.10.log)~. - UsualWeeklyHours - EarningsPast12Months, ssc.dat.mod)
summary(bptestequation)

######## Add age^2
pmod.10.log2 <- lm(log(EarningsPast12Months)~.+I(Age^2)- UsualWeeklyHours, ssc.dat.mod)
summary(pmod.10.log2)
bptest(pmod.10.log2)
bptestequation <- lm(residuals(pmod.10.log)*residuals(pmod.10.log)~.+I(Age^2) - UsualWeeklyHours - EarningsPast12Months, ssc.dat.mod)
summary(bptestequation)


######## Create robust standard errors
robust.pmod.10.log2 <- summary(pmod.10.log2)$coefficients
sandwich_se <- diag(vcovHC(pmod.10.log2, type = "HC"))^0.5
sandwich_se
robust.pmod.10.log2 <- as.data.frame(cbind(robust.pmod.10.log2, sandwich_se))
robust.pmod.10.log2$Robust.t <- (robust.pmod.10.log2$Estimate / robust.pmod.10.log2$sandwich_se)
robust.pmod.10.log2$Robust.p <- 1 - pt(abs(robust.pmod.10.log2$Robust.t), df = 24573)



######## As above with Whites only model
pmod.11.log2 <- lm(log(EarningsPast12Months)~.+I(Age^2)- UsualWeeklyHours, ssc.dat.mod.white)
summary(pmod.11.log2)
bptest(pmod.11.log2)
bptestequation <- lm(residuals(pmod.11.log2)*residuals(pmod.11.log2)~.+I(Age^2) - UsualWeeklyHours - EarningsPast12Months, ssc.dat.mod.white)
summary(bptestequation)


######## Create robust standard errors
robust.pmod.11.log2 <- summary(pmod.11.log2)$coefficients
sandwich_se <- diag(vcovHC(pmod.11.log2, type = "HC"))^0.5
sandwich_se
robust.pmod.11.log2 <- as.data.frame(cbind(robust.pmod.11.log2, sandwich_se))
robust.pmod.11.log2$Robust.t <- (robust.pmod.11.log2$Estimate / robust.pmod.11.log2$sandwich_se)
robust.pmod.11.log2$Robust.p <- 1 - pt(abs(robust.pmod.11.log2$Robust.t), df = 24573)


######## As above with Blacks only model
pmod.12.log2 <- lm(log(EarningsPast12Months)~.+I(Age^2)- UsualWeeklyHours, ssc.dat.mod.black)
summary(pmod.12.log2)
bptest(pmod.12.log2)
bptestequation <- lm(residuals(pmod.12.log2)*residuals(pmod.12.log2)~.+I(Age^2) - UsualWeeklyHours - EarningsPast12Months, ssc.dat.mod.black)
summary(bptestequation)


######## Create robust standard errors
robust.pmod.12.log2 <- summary(pmod.12.log2)$coefficients
sandwich_se <- diag(vcovHC(pmod.12.log2, type = "HC"))^0.5
sandwich_se
robust.pmod.12.log2 <- as.data.frame(cbind(robust.pmod.12.log2, sandwich_se))
robust.pmod.12.log2$Robust.t <- (robust.pmod.12.log2$Estimate / robust.pmod.12.log2$sandwich_se)
robust.pmod.12.log2$Robust.p <- 1 - pt(abs(robust.pmod.12.log2$Robust.t), df = 24573)


stargazer(pmod.10.log2,pmod.11.log2,pmod.12.log2, type = "text", out = "revised_model.html")


round(robust.pmod.10.log2, 3)
round(robust.pmod.11.log2, 3)
round(robust.pmod.12.log2, 3)


######## all interactions
pmod.10.log2.interaction <- lm(log(EarningsPast12Months)~.+I(Age^2)-UsualWeeklyHours + 
                                   (HighSchoolDegreeorGED*White)+(SomeCollege*White)+(AssociatesDegree*White)+(BachelorsDegree*White)+
                                   (MastersDegree*White)+(ProfessionalDegree*White)+(Doctorate*White), ssc.dat.mod)
summary(pmod.10.log2.interaction)
