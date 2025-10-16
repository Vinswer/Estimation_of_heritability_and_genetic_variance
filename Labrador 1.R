library(readr)
library(dplyr)
library(car)
library(lme4)
library(lmerTest) 

Labrador <- read_csv("Labrador.csv", col_types = cols(`Dog-ID` = col_character(), `Sire-ID` = col_character(), `Dam-ID` = col_character(),
                                                      Littersize = col_number(), nr_male = col_number(), nr_fem = col_number(), 
                                                        Pmale = col_number(), Pfemale = col_number(), 
                                                        litternr = col_number(), TestAge = col_number(), 
                                                        courage = col_number(), PerPmale = col_number()))
View(Labrador)

hist(Labrador$courage)
summary(Labrador) # 2111 dogs, 218 NAs for litter size, nr (and proportion) males & females, etc.
length(Labrador$`Dog-ID`)

## Replace "." by NA
Labrador[Labrador == "."] <- NA

# Sires
length(unique(Labrador$`Sire-ID`)) # 230 sire
length(Labrador$`Dog-ID`[which(is.na(Labrador$`Sire-ID`))])  # 18 missing sires
Labrador[which(is.na(Labrador$`Sire-ID`)),]
max(table(Labrador$`Sire-ID`)) # 1 sire has 83 offspring
min(table(Labrador$`Sire-ID`)) # 1 sire has 1 offspring
hist(table(Labrador$`Sire-ID`), breaks = 100)
table(table(Labrador$`Sire-ID`)) # 58 sires have 1 offspring, ... 2 sires have more than 60 offspring (popular sires)

# Dams
length(unique(Labrador$`Dam-ID`)) # 329 dams
length(Labrador$`Dog-ID`[which(is.na(Labrador$`Dam-ID`))])  # 9 missing dams
Labrador[which(is.na(Labrador$`Dam-ID`)),]
hist(table(Labrador$`Dam-ID`), breaks = 50)
table(table(Labrador$`Dam-ID`)) # 65 females have 1 offspring (ie no FS) ; max = 38 offspring

#Litters
length(unique(Labrador$litternr)) # 552 litters
hist(table(Labrador$litternr))
table(table(Labrador$litternr)) # Most litters have only 1 offspring with records ; 1 litter with 11 offspring
table(Labrador$Littersize) # Different values from previously because previous command considers only offspring that have a record
hist(Labrador$Littersize) # "True" litter size is approximately normal


# Sex 
table(Labrador$Sex)   # Hane = male ; Tik = female
tapply(Labrador$courage, Labrador$Sex, mean, na.rm = T)
aggregate(courage ~ Sex, Labrador, mean)
tapply(Labrador$courage, Labrador$Sex, sd, na.rm = T)
boxplot(Labrador$courage ~ Labrador$Sex)
t.test(courage ~ Sex, Labrador) # significant difference between mean courage between sexes

hist(Labrador$Pmale, breaks = 20)
hist(Labrador$Pfemale, breaks = 20)

plot(courage ~ Pmale, data = Labrador)
plot(courage ~ Pfemale, data = Labrador) 
hist(Labrador$TestAge)    # 1 outlier for TestAge
hist(Labrador$Littersize)
hist(as.numeric(Labrador$TestYear)) # Also 1 dog test quite later (2008)
# Dog 10405 is from a litter (1546) with other offspring. These dogs have been tested in 2002 → probably a typo between 2002 and 2008
Labrador[which(Labrador$TestD == "20/08/08"),]
which(Labrador$TestD == "20/08/08") # row 992
Labrador <- Labrador[-992,]

hist(Labrador$TestAge, breaks=50)
hist(Labrador$Littersize)
av.Age <- mean(Labrador$TestAge, na.rm = T)
sd.Age <- sd(Labrador$TestAge, na.rm = T)
av.Age - 4*sd.Age
av.Age + 4*sd.Age
Labrador[which(Labrador$TestAge > 950),]
Labrador[which(Labrador$litternr == 1422),] # Only animal of the litter tested in 2003 (others: 2001) → typo??
# Other animals with test age > 1000 have no siblings





plot(courage ~ Littersize, data = Labrador)
table(Labrador$Littersize)
hist(Labrador$Littersize)

table(Labrador$Sunhours)
boxplot(Labrador$courage ~ Labrador$Sunhours)

hist(table(Labrador$`Sire-ID`), breaks = 100)
barplot(table(Labrador$`Sire-ID`))

### convert to factors
# What do we want as factor?
Labrador$TestYear <- as.factor(Labrador$TestYear)
Labrador$Sex <- as.factor(Labrador$Sex)
Labrador$Region <- as.factor(Labrador$Region)
Labrador$AgeGr <- as.factor(Labrador$AgeGr)
Labrador$litternr <- as.factor(Labrador$litternr)
Labrador$PerPmale <- as.factor(Labrador$PerPmale)
Labrador$Littersize <- as.numeric(Labrador$Littersize)
Labrador$Pmale <- as.numeric(Labrador$Pmale)



# Fit some linear models with only fixed effects (1 factor)
lms1 <- lm(courage ~ Sex, data = Labrador)
summary(lms1)
Anova(lms1)  # Sex seems significant

lms2 <- lm(courage ~ Pmale, data = Labrador)
summary(lms2)
Anova(lms2)  # 

lms3 <- lm(courage ~ Region, data = Labrador)
summary(lms3)
Anova(lms3)  # 

lms4 <- lm(courage ~ Littersize, data = Labrador)
summary(lms4)
Anova(lms4)  # No effect of litter size alone


# Choice of fixed and random effects:
# Fixed effects - factors:  Sex, Region, Age groupe, sun hours
# Fixed effects - covariates: littersize, proportion of male,  Age at testing

# Random effects: Sire, Dams, litter number, postal code, test day, test year


# Let's try with a more complete model. There are quite many variables, so we keep out the interactions and random effects first:
lm1 <- lm(courage ~ Sex + Littersize + Pmale + I(Pmale^2) + PerPmale + Region + TestAge + I(TestAge^2) + AgeGr  + Sunhours, data = Labrador)
Anova(lm1) # highest p-val for Littersize

lm2 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + PerPmale + Region + TestAge + I(TestAge^2) + AgeGr  + Sunhours, data = Labrador)
Anova(lm2) # highest p-val for PerPmale

lm3 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + I(TestAge^2) + AgeGr  + Sunhours, data = Labrador)
Anova(lm3) # highest p-val for AgeGr

lm4 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + I(TestAge^2)  + Sunhours, data = Labrador)
Anova(lm4) # highest p-val for quadratic of TestAge

lm5 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge  + Sunhours, data = Labrador)
Anova(lm5) # highest p-val for Sunhours

lm6 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge , data = Labrador)
Anova(lm6) # all variables are significant


#### Let's test the interactions by pairs:
lm.i1 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + Sex:Pmale, data = Labrador)
Anova(lm.i1) # intercation not significant
lm.i2 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + Sex:I(Pmale^2), data = Labrador)
Anova(lm.i2)
lm.i3 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + Sex:Region, data = Labrador)
Anova(lm.i3)
lm.i4 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + Sex:TestAge, data = Labrador)
Anova(lm.i4)
lm.i5 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + Pmale:I(Pmale^2), data = Labrador)
Anova(lm.i5)
lm.i6 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + Pmale:Region, data = Labrador)
Anova(lm.i6)
lm.i7 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + Pmale:TestAge, data = Labrador)
Anova(lm.i7)
lm.i8 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + I(Pmale^2):Region, data = Labrador)
Anova(lm.i8)
lm.i9 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + I(Pmale^2):TestAge, data = Labrador)
Anova(lm.i9)
lm.i10 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + Region:TestAge, data = Labrador)
Anova(lm.i10)


# None of the interaction are significant → we continue with the model lm5 without interaction

#### Now we include random effects - Mixed model
lm6 <- lm(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + TestYear, data = Labrador)
summary(lm6)


lmr1 <- lmer(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + (1|TestYear) + (1|litternr) + (1|PostalCode) + (1|TestD), data =Labrador)
Anova(lmr1) # Fixed effects (same as previously)
rand(lmr1)



par(mfrow = c(2,2))
plot(lm6) # quite unusual patterns due to considering courage as continuous data
plot(lmr1) 


#####
## Sire model
data_sire <- Labrador[is.na(Labrador$`Sire-ID`) == FALSE, ]
lm.sire <- lmer(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge 
                + (1|TestYear) + (1|`Sire-ID`) + (1|litternr) + (1|PostalCode) + (1|TestD), data = na.omit(data_sire))
summary(lm.sire)
Anova(lm.sire) # Fixed effects significant
rand(lm.sire)  # Sire and year effects not very significant → should we keep year?

(VCE1 <- as.data.frame(VarCorr(lm.sire)))     #the var-covar estimated by ReML
vsire <- VCE1$vcov[4]
ve1 <- VCE1$vcov[6]
vp1 <- vsire + ve1           
h2s <- (4*vsire)/vp1                
h2s  # 0.10

plot(lm.sire1)

# Without TestYear
lm.sire2 <- lmer(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge 
                + (1|`Sire-ID`) + (1|litternr) + (1|PostalCode) + (1|TestD), data = na.omit(data_sire))
summary(lm.sire2)
rand(lm.sire2)  # Sire and year effects not very significant → should we keep year?

(VCE1.2 <- as.data.frame(VarCorr(lm.sire2)))     #the var-covar estimated by ReML
vsire1.2 <- VCE1.2$vcov[4]
ve1.2 <- VCE1.2$vcov[5]
vp1.2 <- vsire1.2 + ve1.2           
h2s2 <- (4*vsire1.2)/vp1.2                
h2s2  # 0.12 
# Not a very big difference without Test year, but I think we should keep it
anova(lm.sire, lm.sire2) # Not significant difference between the 2 models


# Sire - dam model
data_dam <- data_sire[is.na(data_sire$`Dam-ID`) == FALSE, ]
lm.sd <- lmer(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + 
                (1|TestYear) + (1|`Sire-ID`) + (1|`Dam-ID`) + (1|litternr) + (1|PostalCode) + (1|TestD), data = na.omit(data_dam))
summary(lm.sd)
rand(lm.sd)  

(VCE2 <- as.data.frame(VarCorr(lm.sd)))      #the var-covar estimated by ReML
vdam2 <- VCE2$vcov[4]
vsire2 <- VCE2$vcov[5]
ve2 <- VCE2$vcov[7]
vp2 <- vdam2 + vsire2 + ve2           
(h2.2 <-(4*vsire2)/vp2)    # 0.10
(c2 <-(vdam-vsire)/vp2)   # -0.02 Negative 



lm.sd2 <- lmer(courage ~ Sex + Pmale + I(Pmale^2) + Region + TestAge + 
                (1|`Sire-ID`) + (1|`Dam-ID`) + (1|litternr) + (1|PostalCode) + (1|TestD), data = na.omit(data_dam))
summary(lm.sd2)
rand(lm.sd2)  # Sire, dam and year effects not significant 

# Check assumptions 
par(mfrow=c(2,2))
plot(lm6)
plot(lm.sd)
par(mfrow=c(1,1))
qqnorm(resid(lm.sd))    
qqline(resid(lm.sd))

#