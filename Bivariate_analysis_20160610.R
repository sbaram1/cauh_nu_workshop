
### Data import 
AZ = read.csv("C:/Dataset/Bivariate analysis.csv")
AZ

### 변수명 확인
names(AZ)

summary(AZ$AGE)
attach(AZ)
summary(AGE)


#### 1.How do I assess an association between two dichotomous variables
install.packages("descr") 
library(descr)

###Cross table (1)
CrossTable(MALE, N_3MO_POOR, prop.r = F, prop.c =T, prop.t = F, 
                prop.chisq = F)

###Cross table (2)
crosstab(MALE, N_3MO_POOR, prop.c =T)

###Chi-square test
crosstab(MALE, N_3MO_POOR, prop.c =T, chisq=T)

###Fisher's exact test
crosstab(HX_HF, N_3MO_POOR, prop.c =T, chisq=T)
crosstab(HX_HF, N_3MO_POOR, prop.c =T, chisq=T, fisher=T)

#### 2.How do I test an association between a nominal variable and a
####dichotomous variable or between two nominal variables

####a nominal variable × a dichotomous variable
crosstab(TOAST, N_3MO_POOR, prop.c =T, chisq=T, fisher=T)

####a nominal variable × a nominal variable
crosstab(TOAST, N_MRS3MO, prop.c =T, chisq=T, fisher=T)

fisher.test(table(TOAST, N_MRS3MO), simulate.p.value = T, B=10000)


#### 3. How do I test an association between a dichotomous variable and an ordinal variable?
####Shift analysis: Wilcoxon Mantel Haenszel (WMH) test 

install.packages("lazyWeave") 
library(lazyWeave)

mantel.test(HX_HTN, N_MRS3MO, col.scores="midrank")

####Cochran-Armitage linear test
mantel.test(N_MRS3MO, HX_HTN)


#### 4.How do I test an association involving a continuous variable? 
###Shape of a distribution: Histogram

install.packages("FSA")      
library(FSA)

hist(TG ~ factor(N_3MO_POOR), xlab='TG', col='blue4')

hist(AGE ~ factor(N_3MO_POOR), xlab='Age', col='blue4')

###Normality test
shapiro.test(AGE[N_3MO_POOR==0])
shapiro.test(AGE[N_3MO_POOR==1])


##### 5. How do I test an association of a dichotomous variable with ancontinuous variable?
### 5.1 Normally distributed continuous variable

install.packages("psych")  
library(psych)

describeBy (AGE, N_3MO_POOR)

##평균과 표준편차
cbind(mean(AGE[N_3MO_POOR==0]), sd(AGE[N_3MO_POOR==0]))
cbind(mean(AGE[N_3MO_POOR==1]), sd(AGE[N_3MO_POOR==1]))

###분산동질성 검정
bartlett.test(AGE ~ N_3MO_POOR)

###t-test
t.test (AGE ~ N_3MO_POOR, var.equal = T) 

### 5.2 Non-normally distributed continuous variable

##Box-plot

par(mai = c(0.8, 1.2, 0.5,0.5))
boxplot (TG ~ factor(N_3MO_POOR), ylab="TG")
boxplot (TG ~ factor(N_3MO_POOR), outline=F, ylab="TG")

###기술통계량
library(Hmisc)
describe(TG) 

tapply(TG,N_3MO_POOR,summary)


###Wilcoxon rank sum test
wilcox.test (TG ~ N_3MO_POOR)

#### 6. How do I test an association of a nominal variable with a continuous variable 
### 6.1 Normally distributed continuous variable

###평균과 표준편차
library(psych)
describeBy (AGE, N_MRS3MO)

###ANOVA
a1 = aov(AGE ~ factor(N_MRS3MO))
summary(a1)

###Multiple comparison
TukeyHSD(a1)

### 6.2 Non-Normally distributed continuous variable

##Box-plot

par(mai = c(0.8, 1.2, 0.5,0.5))
boxplot (FBS ~ factor(N_MRS3MO), outline=F, ylab="FBS")

###기술통계량

tapply(FBS, N_MRS3MO, summary)

####Kruskal-Wallis test
kruskal.test(FBS ~ factor(N_MRS3MO))

####Dunn's Kruskal-Wallis Multiple Comparisons.
install.packages("dunn.test")
library(dunn.test)
library(FSA)

dunnTest(FBS ~ factor(N_MRS3MO))
