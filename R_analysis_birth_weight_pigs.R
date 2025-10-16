########Case 2 - example analysis birth weight pigs Piter Bijma########
setwd("M:/WRK/onderwijs/MSLS/2024/voorbeeld rapport Case 2")    #note: R uses slash-forward instead of slash back
library("lme4")                                                
library("car")                                                 #for Type II and Type III anova's
library(lmerTest)                                              #for getting P-values from lmer models

###############READ THE DATA########################################################################################

data.pigs <- read.table("Pig-BW-Data1.csv", sep = ",", header = T, stringsAsFactors = F, 
                       na.strings = ".")

#########RAW DATA INVENTORY AND ADJUSTMENTS / FILTERING.############################################################
#you have 3539 entries of 8 variables

#first have a look at the data, click the data.pigs in the Data window of R-studio
summary(data.pigs)

#Observe that all variables, except birth date and birth month are integers. Not what you want I guess?
#I guess you would want to fit those effects potentially as a factor, so consider making factors of them.
data.pigs$ANIMAL<-as.factor(data.pigs$ANIMAL)
data.pigs$SIRE<-as.factor(data.pigs$SIRE)
data.pigs$DAM<-as.factor(data.pigs$DAM)
data.pigs$LINE<-as.factor(data.pigs$LINE)
data.pigs$BIRTH_YEAR<-as.factor(data.pigs$BIRTH_YEAR)

#With the table statement, you can make an inventory of factors, e.g. check for duplicate records
table(data.pigs$ANIMAL)
table(table(data.pigs$ANIMAL))           #See whether you understand this statement; you find one duplicate animal
duplicates<-which(table(data.pigs$ANIMAL)==2)
duplicates                               #animal ID 3839 occurs twice. In the sorted (!) animal list this is the 2435th ID
#check whether both duplicate records are identical, of so drop one of them, otherwise drop both entries.
data.pigs[data.pigs$ANIMAL==3839,]
#here you see both rows are identical. So drop the second one. using -rownr drops that entire row.
data.pigs<-data.pigs[-3539,]             #drop row 3539 from the data frame, you get 3538 rows now.
table(table(data.pigs$ANIMAL))           #Check that the duplicate record has indeed been removed.

length(unique(data.pigs$SIRE))
table(data.pigs$SIRE)                  #there are 264 sires in total. Observe there is a sire with id 0, with 28 offspring 
max(table(data.pigs$SIRE))             #the sire with most offspring has 94 offspring                  
min(table(data.pigs$SIRE))             #there are also sires with only 1 offspring
hist(table(data.pigs$SIRE))
which(data.pigs$SIRE==0)                #these 28 sires have id 0, but 0 usually means missing
#Think about what would happen if we leave this as it is, and do a genetic analysis??
#set the 0 sire to NA in these records
data.pigs$SIRE[data.pigs$SIRE==0]<-NA  #set those sires to NA, meaning NotAvailable (i.e. missing)
table(data.pigs$SIRE,useNA="always")   #now there are zero sires with id 0, they are now NA
mean(table(data.pigs$SIRE))
table(table(data.pigs$SIRE))
#there are 16 sires with 1 offspring, 15 with two offspring, 18 with three offspring, etc., and one sire with 94 offspring
#clearly the data are not balanced by sire! Thus Anova for estimating h2 is a no go.
#the 16 sires with only one offspring could be dropped because they don't add to the estimation of heritability (no half sibs)
#but let's keep them, because they may add to the better estimation of fixed effects.
#(the first 0 1 column is because R remembers the id 0, there is not really a sire 0 anymore, this is an "artifact")

length(unique(data.pigs$DAM))         #1203 dams
mean(table(data.pigs$DAM))
table(table(data.pigs$DAM))           #many dams with a single offspring only, 3 dams with as many as 14!
hist(table(data.pigs$DAM))
which(data.pigs$DAM==0)               #no missing dams it seems.
#If we would be certain that complete litters were weighted, we could drop the small litters (few offspring per dam)
#as they are not representative. However, here we don't know, and there are quite many dams with few offspring.
#so let's keep all in.

table(data.pigs$LINE)                 #
#line 8 is by far the largest. We could decide to base the entire analysis on line 8 only, 
#because heritability might differ between lines. But we will lose quite a bit of data,
#particularly from lines 3 and 7. Line 2 contributes only 185 records. 
#I decided to drop line 2, but this decision is somewhat arbitrary.
data.pigs<-data.pigs[data.pigs$LINE!=2,]
table(data.pigs$LINE)                 
#3353 observations left

table(data.pigs$BIRTH_YEAR,data.pigs$BIRTH_MONTH)             
#there is one record in 1986, that is strange, all the rest is about 10 years later
#this is record 2449. If you look at the records arount is, they are all 1996
#that suggests this is a typo. So could correct this record, or drop it. Here I correct it.
data.pigs$BIRTH_DATE[data.pigs$BIRTH_DATE=="17/08/1986"]<-"17/08/1996"
data.pigs$BIRTH_YEAR[data.pigs$BIRTH_YEAR=="1986"]<-"1996"
table(data.pigs$BIRTH_YEAR,data.pigs$BIRTH_MONTH)             #check
#Furthermore observe there are almost no pigs born in 1997, this could be a reason to talk
#to the people who provided the data, maybe some part of it got lost.
#the rest looks OK.

#now we move to your y-variable, which is quantitative
hist(data.pigs$WEIGHT)                #this looks really strange
data.pigs[data.pigs$WEIGHT>5,]         #have a closer look, an extremely heavy piglet
#this looks like a typo, where 19.8 instead of 1.98 has been entered.
#one option is to drop the record, the second option is to correct it, guessing this is indeed what happened. 
#I decided to drop it, to be on the safe side. This is largely a matter of taste. 
data.pigs<-data.pigs[data.pigs$WEIGHT<5,]         #keep only the ones below 5 (using !data.pigs$WEIGHT>5) would drop the high one, thus do the same
#note (check!) that the number of entries dropped by one.

hist(data.pigs$WEIGHT)                #this looks much better; note: y does not need to be normally distributed! (e does)
summary(data.pigs$WEIGHT)

#complex check for the advanced student: do all records of each sire belong to a single line?
sire_line<-table(data.pigs$SIRE,data.pigs$LINE)  #number of records per sire per line
lines_per_sire<-sire_line!=0                     #check for how many lines a sire has records (should not exceed 1), logical
lines_per_sire<-apply(lines_per_sire,1,sum)                   #there should only be one TRUE per sire, so the sum should be 1 for each sire (TRUE=1, FALSE=0)
max(lines_per_sire)                                           #if this is one, then there is no sire with records for more than one line.

#same for dams
dam_line<-table(data.pigs$DAM,data.pigs$LINE) 
lines_per_dam<-dam_line!=0 
lines_per_dam<-apply(lines_per_dam,1,sum)
max(lines_per_dam)

#each sire and each dam is linked to only one line, thus OK.

#now the data looks fine, so we can start building a model.


####STATISTICAL MODEL DEVELOPMENT###################
#There is no strict recipe for developing a model. If the number of potential explanatory 
#variables is limited, you could try to fit the full interaction between all terms, and then
#drop the least significant interaction term, and continue doing this until all remaining terms are significant.  
#But in field data, often the number of potential explanatory variables is too large to fit the full interaction
#in that case you can e.g. start by fitting only main effects, and then test interactions only for 
#the main effects that are significant.
#In any case, play around sufficiently to get a feel for the data. Try to interpret what you see.

#for the genetic analysis, we need a mixed model, because we want to estimate a (genetic) variance.

#But first, to get a feel for the data, I start with a linear model without genetics, fitting one factor at a time

model1<-lm(WEIGHT ~ LINE, data=data.pigs)
summary(model1)                                
Anova(model1)                  #clear evidence for a line effect. Line 7 and 8 are about 0.15 and 0.25 grams lighter than line 3

model2<-lm(WEIGHT ~ BIRTH_MONTH, data=data.pigs)
summary(model2)                                
Anova(model2)                  #No indications for a month effect, but beware we fit month across year, good idea?

model3<-lm(WEIGHT ~ BIRTH_YEAR, data=data.pigs)
summary(model3)                                
Anova(model3)                  #No indications for a year effect

model4<-lm(WEIGHT ~ BIRTH_MONTH*BIRTH_YEAR, data=data.pigs)
summary(model4)                                
Anova(model4)                  #clearly significant effect of month*year, so there are month effects, but 
#the same month in different years does not have the same effect.
#the residual df is 3321, so we have lots of "degrees of freedom" left, so we can try fitting the full interaction

model5<-lm(WEIGHT ~ LINE*BIRTH_MONTH*BIRTH_YEAR, data=data.pigs)
summary(model5)                                
Anova(model5)                 
#clearly there is a very large line effect.
#there is no evidence at all for a main month effect, we can drop that.
#the other model terms are significant or close to significant

model6<-lm(WEIGHT ~ LINE + BIRTH_YEAR + BIRTH_MONTH:LINE + LINE:BIRTH_YEAR + BIRTH_MONTH:BIRTH_YEAR + LINE:BIRTH_MONTH:BIRTH_YEAR, data=data.pigs)
summary(model6)                                
Anova(model6)                 
#Drop LINE:BIRTH_MONTH

model7<-lm(WEIGHT ~ LINE + BIRTH_YEAR + LINE:BIRTH_YEAR + BIRTH_MONTH:BIRTH_YEAR + LINE:BIRTH_MONTH:BIRTH_YEAR, data=data.pigs)
summary(model7)                                
Anova(model7)                 
#Drop LINE:BIRTH_YEAR

model8<-lm(WEIGHT ~ LINE + BIRTH_YEAR + BIRTH_MONTH:BIRTH_YEAR + LINE:BIRTH_MONTH:BIRTH_YEAR, data=data.pigs)
summary(model8)                                
Anova(model8)                 
#all remaining fixed effects are significant. We could check now whether the q-q plot of residuals is roughly OK.
#before going to the random effects.

plot(model8)          #this all looks OK
hist(resid(model8)) 

#now go to mixed models including random genetic effects.
#note: we cannot fit an animal model in R with lmer, because R cannot handle a relationship matrix.
#(try fitting an animal model to see what happens. 
#so here we fit a sire or sire + dam model.

#first try a sire model
#first drop the entries (rows) with missing sire
data.pigs<-data.pigs[is.na(data.pigs$SIRE)==FALSE,]        #only keep records where the sire is not missing, check you lose 28 records
model9<-lmer(WEIGHT ~ LINE + BIRTH_YEAR + BIRTH_MONTH:BIRTH_YEAR + LINE:BIRTH_MONTH:BIRTH_YEAR + (1|SIRE), data=data.pigs)
summary(model9)           
Anova(model9)                     #fixed effects remain significant
rand(model9)                      #very significant sire effect.

VCE<-as.data.frame(VarCorr(model9))      #the var-covar estimated by ReML
vsire<-VCE$vcov[1]
ve<-VCE$vcov[2]
vp<-vsire+ve           
h2<-(4*vsire)/vp                        #suggest a heritability of 0.33
h2

#however, the sire variance maybe inflated because the data contain full sibs as well (some sire and dam)
#to avoid this, we should check fitting the dam

model10<-lmer(WEIGHT ~ LINE + BIRTH_YEAR + BIRTH_MONTH:BIRTH_YEAR + LINE:BIRTH_MONTH:BIRTH_YEAR + (1|SIRE) + (1|DAM), data=data.pigs)
summary(model10)           
Anova(model10)                     #fixed effects remain kind of significant, no changes needed.
rand(model10)                      #very significant sire and particularly dam effect.
plot(model10)

VCE<-as.data.frame(VarCorr(model10))      #the var-covar estimated by ReML
vdam<-VCE$vcov[1]
vsire<-VCE$vcov[2]
ve<-VCE$vcov[3]
vp<-vdam+vsire+ve           
h2<-(4*vsire)/vp                        #suggest a heritability of ~0.28
c2<-(vdam-vsire)/vp                     #common litter prop var explained; assumes we ignore dominance
#so if we would not have fitted the dam, then we'd probably overestimate the heritability based on the sire.

#some final checks
plot(model10)                  #here you see a clear relationship between the fitted values and the residuals
#but that is not a problem for a mixed model! (but should be tested for the fixed-effects part of the model, as we did)
#with random effects, you expect a positive relationship between the BLUP of the random effects and the residuals.
#that is what we see here, and which is OK.
hist(resid(model8))            #that looks very nice, almost to good to be true.

qqnorm(resid(model10))
qqline(resid(model10))         #this looks quite satisfying, a few deviations in the lower tail, but that are just few data points

hist(ranef(model10)$SIRE[,1])  #this looks OK, no extreme values of estimated sire effects or so, approx normal dist.
hist(ranef(model10)$DAM[,1])   #also this looks OK, no extreme values of estimated dam effects or so, approx normal dist.

#Conclusion: we estimate a heritability of 0.28 for birth weight and a common litter c2 of 0.25
#It is not so easy in lmer to get a standard error for these estimates, so we don't do that here.
#software like ASREML could do that for you.

#Discussion
#what would happen if we ignore LINE?
model11<-lmer(WEIGHT ~ BIRTH_YEAR + BIRTH_MONTH:BIRTH_YEAR + (1|SIRE) + (1|DAM), data=data.pigs) 
summary(model11)           
Anova(model11)                     
rand(model11)                      
plot(model11)

qqnorm(resid(model11))
qqline(resid(model11))         #Note that absence of the line effect is not very visible here! 

VCE<-as.data.frame(VarCorr(model11))      #the var-covar estimated by ReML
vdam<-VCE$vcov[1]
vsire<-VCE$vcov[2]
ve<-VCE$vcov[3]
vp<-vdam+vsire+ve           
h2<-(4*vsire)/vp                        #suggest a heritability of ~0.48!


#testing the impact of fitting the biggest interaction as random
model12<-lmer(WEIGHT ~ LINE + (1|BIRTH_YEAR) + (1|BIRTH_MONTH:BIRTH_YEAR) + (1|LINE:BIRTH_MONTH:BIRTH_YEAR) + (1|SIRE) + (1|DAM), data=data.pigs)
summary(model12)           
Anova(model12)                     #fixed effects remain kind of significant, no changes needed.
rand(model12)                      #very significant sire and particularly dam effect.
plot(model12)

VCE<-as.data.frame(VarCorr(model12))      #the var-covar estimated by ReML
vdam<-VCE$vcov[1]
vsire<-VCE$vcov[2]
ve<-VCE$vcov[3]
vp<-vdam+vsire+ve           
h2<-(4*vsire)/vp                        #suggest a heritability of ~0.28
c2<-(vdam-vsire)/vp                     #common litter prop var explained; assumes we ignore dominance
#so if we would not have fitted the dam, then we'd probably overestimate the heritability based on the sire.


#DONE.










