df <- read.csv("salary.txt",header=T)
names(df)

# Convert categorical variables into numeric categories
df$reg_num <- as.numeric(factor(df$reg , levels=c("northeast" ,
                                    "midwest", 
                                    "south",
                                    "west")))

df$city_num <- as.numeric(factor(df$city , levels=c("yes" ,
                                                  "no")))

df$deg_num <- as.numeric(factor(df$deg , levels=c("yes" ,
                                                   "no")))

df$race_num <- as.numeric(factor(df$race, levels=c("black" ,
                                                  "white", 
                                                  "other")))

# Construct training and test datasets
# Use sample 
# Set seed to 0 
set.seed(0)
index <- sample(1:nrow(df),4965,replace = F)
train.data <- df[-index,]
data <- train.data 
test.data <- df[index,]


# Quality control check
sum(train.data$race=="black")/nrow(data)
sum(test.data$race=="black")/nrow(test.data)  


# Rough model 1 
r.model.1 <- lm(wage~edu+exp+city+reg+race+deg+com,data=train.data) 
summary(r.model.1)
qqnorm(rstudent(r.model.1))
qqline(rstudent(r.model.1))


# Rough model 2 
r.model.2 <- lm(log(wage)~edu+exp+city+reg+race+deg+com,data=train.data)
summary(r.model.2)
qqnorm(rstudent(r.model.2))
qqline(rstudent(r.model.2))


############################################
# EDA (Use smootehrs)
############################################

##### com, can throw com out because the line looks very straight
plot(data$com,log(data$wage))
lines(supsmu(data$com,log(data$wage)),col=2)

##### edu, positive correlation
plot(data$edu,log(data$wage))
lines(supsmu(data$edu,log(data$wage)),col=2)

boxplot(log(data$wage)~data$edu)
lines(supsmu(data$edu,log(data$wage)),col=2)

### exp, negative parabolic correlation
plot(data$exp,log(data$wage))
lines(supsmu(data$exp,log(data$wage)),col=2)

boxplot(log(data$wage)~data$exp)
lines(supsmu(data$exp,log(data$wage)),col=2)

plot((data$exp)^2,log(data$wage))
lines(supsmu((data$exp)^2,log(data$wage)),col=2)

### emp, slight positive correlation
plot(data$emp,log(data$wage))
lines(supsmu(data$emp,log(data$wage)),col=2)

boxplot(log(data$wage)~data$emp)
lines(supsmu(data$emp,log(data$wage)),col=2)

### city, boxplot shows slight increase, needs more investigation
boxplot(log(data$wage)~data$city)

### reg, boxplot does not seem to show much correlation, needs more investigation
plot(log(data$wage)~data$reg)

### race, seems to show some correlation
plot(log(data$wage)~data$race)

### deg, seems to show positive correlation
plot(log(data$wage)~data$deg)

############################################
#### EDA (interaction plots)
############################################
city <- data$city
city_num <- data$city_num
reg <- data$reg
reg_num <- data$reg_num
race <- data$race
race_num <- data$race_num
deg <- data$deg
deg_num <- data$deg_num
wage <- data$wage
race <- data$race
emp <- data$emp
edu <- data$edu
exp <- data$exp


# City vs. region
# There exists an interaction
interaction.plot(city,reg,log(wage))
summary(lm(formula = log(data$wage) ~ city_num  + reg_num + I(city_num*reg_num), data = data))

# City vs. degree
# interaction exists
interaction.plot(city,deg,log(wage))
anova(lm(formula = log(data$wage) ~ data$city + data$deg, data = data))
anova(lm(formula = log(data$wage) ~ data$deg + data$city, data = data))
summary(lm(formula = log(data$wage) ~ city_num  + deg_num + I(city_num*deg_num), data = data))

# City vs. race
# no interaction
interaction.plot(city,race,log(wage))
anova(lm(formula = log(data$wage) ~ data$city + data$race, data = data))
anova(lm(formula = log(data$wage) ~ data$race + data$city, data = data))
# t test p value > 0.05, so no interaction
summary(lm(formula = log(data$wage) ~ city_num  + race_num + I(city_num*race_num), data = data))

# Region vs. degree
# yes, interaction exists
interaction.plot(deg,reg,log(wage))
summary(lm(formula = log(data$wage) ~ deg_num  + reg_num + I(deg_num*reg_num), data = data))

# Region vs. race
# no
interaction.plot(reg,race,log(wage))
summary(lm(formula = log(data$wage) ~ race_num  + reg_num + I(race_num*reg_num), data = data))

# Degree vs. race
# no interaction
interaction.plot(deg,race,log(wage))
summary(lm(formula = log(data$wage) ~ deg_num  + race_num + I(race_num*deg_num), data = data))

##############
# Continuous vs Categorical variable correlation check
##############

### Race vs edu
# Interaction Exists
black <- data$race=="black"
white <- data$race=="white"
other <- data$race=="other"

plot(data$edu,log(wage),col="lightgrey")
abline(lm(log(data$wage)[black]~data$edu[black]),col=2)
abline(lm(log(data$wage)[white]~data$edu[white]),col=3)
abline(lm(log(data$wage)[other]~data$edu[other]),col=4)
legend("topright",legend=c("Black","White","Other"),col=c(2,3,4),lty=c(1,1,1))

# t test shows significance
summary(lm(formula = log(data$wage) ~ race  + edu + race*edu, data = data))
summary(lm(formula = log(data$wage) ~ race  + edu + I(race_num*edu), data = data))

### Race vs exp
# Yes correlation
black <- data$race=="black"
white <- data$race=="white"
other <- data$race=="other"

plot(data$exp,log(wage),col="lightgrey")
abline(lm(log(data$wage)[black]~data$exp[black]),col=2)
abline(lm(log(data$wage)[white]~data$exp[white]),col=3)
abline(lm(log(data$wage)[other]~data$exp[other]),col=4)

# t test shows significance
summary(lm(formula = log(data$wage) ~ race  + exp + I(race_num*exp), data = data))

### Race vs emp
# No interaction Exists
black <- data$race=="black"
white <- data$race=="white"
other <- data$race=="other"

plot(data$emp, log(wage),col="lightgrey")
lines(supsmu(data$emp[black],log(data$wage)[black]),col=2)
lines(supsmu(data$emp[white],log(data$wage)[white]),col=3)
lines(supsmu(data$emp[other],log(data$wage)[other]),col=4)

# t test shows no significance
summary(lm(formula = log(data$wage) ~ race  + emp + race*emp, data = data))
summary(lm(formula = log(data$wage) ~ race  + emp + I(race_num*emp), data = data))

### City vs emp
# No interaction Exists
yes <- data$city=="yes"
no <- data$city=="no"

plot(data$emp, log(wage),col="lightgrey")
lines(supsmu(data$emp[yes],log(data$wage)[yes]),col=2)
lines(supsmu(data$emp[no],log(data$wage)[no]),col=3)

summary(lm(formula = log(data$wage) ~ city  + emp + city*emp, data = data))
summary(lm(formula = log(data$wage) ~ city  + emp + I(city_num*emp), data = data))

### City vs exp
# weak correlation but yes according to t test
yes <- data$city=="yes"
no <- data$city=="no"

plot(data$exp, log(wage),col="lightgrey")
lines(supsmu(data$exp[yes],log(data$wage)[yes]),col=2)
lines(supsmu(data$exp[no],log(data$wage)[no]),col=3)
summary(lm(formula = log(data$wage) ~ city  + exp + I(city_num*exp), data = data))

### City vs edu
# no
yes <- data$city=="yes"
no <- data$city=="no"

plot(data$edu, log(wage),col="lightgrey")
lines(supsmu(data$edu[yes],log(data$wage)[yes]),col=2)
lines(supsmu(data$edu[no],log(data$wage)[no]),col=3)

plot(data$edu,log(wage),col="lightgrey")
abline(lm(log(data$wage)[yes]~data$edu[yes]),col=2)
abline(lm(log(data$wage)[no]~data$edu[no]),col=2)

summary(lm(formula = log(data$wage) ~ city  + emp + city*emp, data = data))
summary(lm(formula = log(data$wage) ~ city  + emp + I(city_num*emp), data = data))

### Degree vs edu
# yes correlation
yes <- data$deg=="yes"
no <- data$deg=="no"

plot(data$edu, log(wage),col="lightgrey")
lines(supsmu(data$edu[yes],log(data$wage)[yes]),col=2)
lines(supsmu(data$edu[no],log(data$wage)[no]),col=3)
legend("topright",legend=c("yes","no"),col=c(2,3,4),lty=c(1,1,1))

summary(lm(formula = log(data$wage) ~ deg  + edu + deg*edu, data = data))
summary(lm(formula = log(data$wage) ~ deg  + edu + I(deg_num*edu), data = data))

### Degree vs exp
# yes correlation, but weird because quadratic
yes <- data$deg=="yes"
no <- data$deg=="no"

plot(data$exp, log(wage),col="lightgrey")
lines(supsmu(data$exp[yes],log(data$wage)[yes]),col=2)
lines(supsmu(data$exp[no],log(data$wage)[no]),col=3)
summary(lm(formula = log(data$wage) ~ deg  + exp + I(deg_num*exp), data = data))

### Degree vs emp
# no correlation
yes <- data$deg=="yes"
no <- data$deg=="no"

plot(data$emp, log(wage),col="lightgrey")
lines(supsmu(data$emp[yes],log(data$wage)[yes]),col=2)
lines(supsmu(data$emp[no],log(data$wage)[no]),col=3)
summary(lm(formula = log(data$wage) ~ deg  + emp + I(deg_num*emp), data = data))

### Region vs edu
# yes correlation, not super strong though
mw <- data$reg=="midwest"
ne <- data$reg=="northeast"
s <- data$reg=="south"
w <- data$reg=="west"

plot(data$edu, log(wage),col="lightgrey")
lines(supsmu(data$edu[mw],log(data$wage)[mw]),col=2)
lines(supsmu(data$edu[ne],log(data$wage)[ne]),col=3)
lines(supsmu(data$edu[s],log(data$wage)[s]),col=3)
lines(supsmu(data$edu[w],log(data$wage)[w]),col=3)
summary(lm(formula = log(data$wage) ~ reg  + edu + I(reg_num*edu), data = data))

### Region vs exp
# no correlation
mw <- data$reg=="midwest"
ne <- data$reg=="northeast"
s <- data$reg=="south"
w <- data$reg=="west"

plot(data$exp, log(wage),col="lightgrey")
lines(supsmu(data$exp[mw],log(data$wage)[mw]),col=2)
lines(supsmu(data$exp[ne],log(data$wage)[ne]),col=3)
lines(supsmu(data$exp[s],log(data$wage)[s]),col=3)
lines(supsmu(data$exp[w],log(data$wage)[w]),col=3)
summary(lm(formula = log(data$wage) ~ reg  + exp + I(reg_num*exp), data = data))

### Region vs emp
# literally no correlation!!
mw <- data$reg=="midwest"
ne <- data$reg=="northeast"
s <- data$reg=="south"
w <- data$reg=="west"

plot(data$exp, log(wage),col="lightgrey")
lines(supsmu(data$emp[mw],log(data$wage)[mw]),col=2)
lines(supsmu(data$emp[ne],log(data$wage)[ne]),col=3)
lines(supsmu(data$emp[s],log(data$wage)[s]),col=3)
lines(supsmu(data$emp[w],log(data$wage)[w]),col=3)
summary(lm(formula = log(data$wage) ~ reg  + emp + I(reg_num*emp), data = data))

######
# Check Correlation between continuous variables
######
cor(data.frame(log(data$wage), data$edu, poly(data$exp, degree=2), data$com, data$emp))

# Correlation betas and standard errors between education and experience
summary(lm(formula = log(data$wage) ~ data$edu, data = data))
summary(lm(formula = log(data$wage) ~ poly(data$exp, degree=2), data = data))
summary(lm(formula = log(data$wage) ~ data$edu + poly(data$exp, degree=2), data = data))

# Correlation SSR between education and experience
anova(lm(formula = log(data$wage) ~ data$edu + poly(data$exp, degree=2), data = data))
anova(lm(formula = log(data$wage) ~ poly(data$exp, degree=2) + data$edu, data = data))

# t test shows significance
inter <- I(edu * poly(exp, degree = 2))
summary(lm(formula = log(data$wage) ~ edu + poly(exp, degree = 2) + inter, data = data))


jc.model.1 <- lm(log(wage)~edu+exp+city+reg+race+deg+com,data=train.data)
AIC(jc.model.1) # 31619.73
jc.model.2 <- lm(log(wage)~edu+poly(exp, degree=2)+city+reg+race+deg,data=train.data)
AIC(jc.model.2) # 30139.33
jc.model.3 <- lm(log(wage)~edu+poly(exp, degree=2)+city+reg+race+deg
                 +edu*exp,data=train.data)
AIC(jc.model.3) # 29855.43
jc.model.4 <- lm(log(wage)~edu+poly(exp, degree=2)+city+reg+race+deg
                 +edu*exp
                 +race*edu+race*exp+deg*edu+deg*exp+reg*edu+city*exp
                 +city*reg+city*deg+reg*deg,data=train.data)
AIC(jc.model.4) # 29792.36

## AFTER AIC Model Selection
jc.model.5 <- lm(log(wage)~edu+poly(exp, degree=2)+city+race
                 +edu*exp+race*edu+reg*deg,data=train.data)
AIC(jc.model.5) # 29858.42
summary(jc.model.5)

jc.model.6 <- lm(log(wage)~edu+poly(exp, degree=2)+city+race
                 +edu*exp+race*edu+reg*edu,data=train.data)
AIC(jc.model.6) # 29844.68

# Jackson's model selected by hand
jc.model.7 <- lm(log(wage)~edu+poly(exp, degree=2)+city+reg+race+deg
                 +edu*exp*race+deg*edu+deg*exp
                 +reg*edu+city*exp
                 +city*reg+city*deg,data=train.data)
AIC(jc.model.7) # 29783.19

## PUT ON HOLD
jc.model.8 <- lm(log(wage)~edu+poly(exp, degree=2)+city+reg+race
                 +edu*exp+race*edu,data=train.data)
AIC(jc.model.8) # 29857.8

############################
# Model Selection
############################
library(leaps)
library(bestglm)

predictors=cbind(edu,poly(exp, degree=2),city,reg,race,deg
                 ,edu*exp*race_num,deg_num*edu,deg_num*exp
                 ,reg_num*edu,city_num*exp
                 ,city_num*reg_num,city_num*deg_num)
leaps.sub = leaps(x = predictors, y = log(wage))
select=leaps.sub$Cp==min(leaps.sub$Cp)
select=leaps.sub$which[select,]

regsubsets.sub=regsubsets(x=predictors,y=log(wage))
summary(regsubsets.sub,scale="Cp")
plot(regsubsets.sub,scale="Cp")

regsubsets.sub=regsubsets(x=predictors,y=log(wage))
summary(regsubsets.sub,scale="bic")
plot(regsubsets.sub,scale="bic")


############################
# Model Selection Graphs
############################
mse <- function(sm) 
  mean(sm$residuals^2)

mse(jc.model.5) # 0.2628706