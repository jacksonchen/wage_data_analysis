df <- read.csv("salary.txt",header=T)
names(df)

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
reg <- data$reg
race <- data$race
deg <- data$deg
wage <- data$wage
race <- data$race
emp <- data$emp
edu <- data$edu
exp <- data$exp


# City vs. region
# There exists an interaction
interaction.plot(city,reg,log(wage))

# City vs. degree
# very slight, probably negligble
interaction.plot(city,deg,log(wage))
anova(lm(formula = log(data$wage) ~ data$city + data$deg, data = data))
anova(lm(formula = log(data$wage) ~ data$deg + data$city, data = data))

# City vs. race
# yes, interaction exists
interaction.plot(city,race,log(wage))
anova(lm(formula = log(data$wage) ~ data$city + data$race, data = data))
anova(lm(formula = log(data$wage) ~ data$race + data$city, data = data))

# Region vs. degree
# yes, interaction exists
interaction.plot(deg,reg,log(wage))

# Region vs. race
# yes
interaction.plot(reg,race,log(wage))

# Degree vs. race
# no interaction
interaction.plot(deg,race,log(wage))

##############
# Race vs. edu
##############
plot(data$edu,log(wage),col=data$race)
plot(data$edu,log(wage),col="lightgrey")

black <- data$race=="black"
white <- data$race=="white"
other <- data$race=="other"

# smoother
plot(data$edu,log(wage),col="lightgrey")
lines(supsmu(data$edu[black],log(data$wage)[black]),col=2)
lines(supsmu(data$edu[white],log(data$wage)[white]),col=3)
lines(supsmu(data$edu[other],log(data$wage)[other]),col=4)
legend("topright",legend=c("Black","White","Other"),col=c(2,3,4),lty=c(1,1,1))

# lines
plot(data$edu,log(wage),col="lightgrey")
abline(lm(log(data$wage)[black]~data$edu[black]),col=2)
abline(lm(log(data$wage)[white]~data$edu[white]),col=3)
abline(lm(log(data$wage)[other]~data$edu[other]),col=4)
legend("topright",legend=c("Black","White","Other"),col=c(2,3,4),lty=c(1,1,1))

##################################
#  Is the interaction significant?
##################################

r.model.3 <- lm(log(wage)~edu+edu*race+exp+city+reg+race+deg+com,data=train.data)
summary(r.model.3)

# AIC 
AIC(r.model.1)
AIC(r.model.3)

# Correlation between education and experience
cor(data.frame(log(data$wage), data$edu, data$exp, data$com, data$emp))
cor.test(data$edu, data$exp)

# Correlation betas and standard errors between education and experience
summary(lm(formula = log(data$wage) ~ data$exp, data = data))
summary(lm(formula = log(data$wage) ~ data$edu + data$exp, data = data))
summary(lm(formula = log(data$wage) ~ data$edu + data$exp + data$com, data = data))

# Correlation SSR between education and experience
anova(lm(formula = log(data$wage) ~ data$edu + data$exp, data = data))
anova(lm(formula = log(data$wage) ~ data$exp + data$edu, data = data))

yes <- data$deg == "yes"


jc.model.1 <- lm(log(wage)~edu+exp+city+reg+race+deg+com,data=train.data)
jc.model.2 <- lm(log(wage)~edu+exp+city+reg+race+deg+com+,data=train.data)