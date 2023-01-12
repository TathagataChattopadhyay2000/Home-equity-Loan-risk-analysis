
# Home Equity Loan (Exploratory Data Analysis) 
# --------------------------------------------


# 1) DATA COLLECTION
# ------------------------------------------------------------------------------

# The data set HMEQ reports characteristics and delinquency information for 
# 5,960 home equity loans. A home equity loan is a loan where the obligor uses 
# the equity of his or her home as the underlying collateral.

library(readxl)
hmeq <- read_excel("C:/Users/TATHAGATA/Desktop/hmeq.xlsx")

# 2) DATA CLEANING
# ------------------------------------------------------------------------------
summary(hmeq)

# Counting NA/Null values in each column:
fna <- function(x){
  sum(is.na(x))
}

sapply(hmeq, fna)

sum(is.na(hmeq))

# Removing NA/Null values:
h <- na.omit(data.frame(hmeq))
str(h)

# Unique Values
fu <- function(x){
  unique(x)
}

sapply(h[,-c(2,3,4,10,13)], fu)

# Data types : Qualitative/Quantitative 

# Qualitative

# Nominal
# -------
# a) REASON: DebtCon = debt consolidation; HomeImp = home improvement
# b) JOB: Occupational categories
# c) BAD: 1 = applicant defaulted; 0 = applicant paid loan

# Quantitative

# Discrete
# --------
# a) YOJ: Years at present job
# b) DEROG: Number of major derogatory reports
# c) DELINQ: Number of delinquent credit lines
# d) NINQ: Number of recent credit inquiries
# e) CLNO: Number of credit lines

# Continuous
# ----------
# a) LOAN: Amount of the loan request
# b) MORTDUE: Amount due on existing mortgage
# c) VALUE: Value of current property
# d) CLAGE: Age of oldest credit line in months
# e) DEBTINC: Debt-to-income ratio

# 3) UNIVARIATE ANALYSIS
# ------------------------------------------------------------------------------
library(ggstatsplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(ggthemes)
library(plotrix)

# Discrete Variables
# ------------------

# 1) Relevant Box-Plots:

# Years at present job

ggplot(data = h,aes(x="", y=h$Years.at.present.job)) +
  geom_boxplot(fill = "purple") +
  stat_summary(fun = mean, geom="point", shape=20, size=7, color="red", fill="red") +
  scale_fill_viridis(discrete = TRUE, alpha=0.7) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=16)
  ) +
  ggtitle("YEARS AT PRESENT JOB") +
  xlab("Years at present job")+ylab("Values")

# Number of Credit Lines

ggplot(data = h,aes(x="", y=Number.of.credit.lines)) +
  geom_boxplot(fill = "#1B9E77") +
  stat_summary(fun = mean, geom="point", shape=20, size=7, color="red", fill="red") +
  scale_fill_brewer(palette="Accent") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=16)
  ) +
  ggtitle("NUMBER OF CREDIT LINES") +
  xlab("Number of credit lines")+ylab("Values")

# 2) Frequency Bar-Plots:

par(mfrow=c(1,1))
# Number of major derogatory reports

plot(table(h$Number.of.major.derogatory.reports),
     col="blue",xlab="Number of major derogatory reports",ylab="Frequency")
title(main = "Number of major derogatory reports")

# Number of delinquent credit lines

plot(table(h$Number.of.delinquent.credit.lines),
     col="red",xlab="Number of major derogatory reports",ylab="Frequency")
title(main = "Number of delinquent credit lines")

# Number of recent credit inquiries

plot(table(h$Number.of.recent.credit.inquiries),
     col="yellow",xlab="Number of major derogatory reports",ylab="Frequency")
title(main = "Number of recent credit inquiries")

# Continuous Variables
# --------------------

# Amount of the loan request

layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0.5, 3.1, 1.1, 2.1))
boxplot(h$LOAN , horizontal=TRUE , xaxt="n", 
        col=rgb(0.5,0.6,0.4,0.5) , frame=T)
par(mar=c(4, 4, 1.1, 2.1))
hist(h$LOAN ,col=rgb(0,0.8,1,0.8) , border=T,breaks=40,
     freq = FALSE, main="Loan" , xlab="Loan",
     ylab = "Frequency")
lines(density(h$LOAN),type = "l")

# Amount due on existing mortgage

layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0.5, 3.1, 1.1, 2.1))
boxplot(h$MORTDUE , horizontal=TRUE , xaxt="n", 
        col=rgb(0.5,0.6,0.4,0.5) , frame=T)
par(mar=c(4, 4, 1.1, 2.1))
hist(h$MORTDUE ,col=rgb(1,0.3,0,0.8) , border=T,breaks=40,
     freq = FALSE, main="Amount due on existing mortgage" , xlab="Mortgage",
     ylab = "Frequency")
lines(density(h$MORTDUE),type = "l")

# Value of current property

layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0.5, 3.1, 1.1, 2.1))
boxplot(h$VALUE , horizontal=TRUE , xaxt="n", 
        col=rgb(0.5,0.6,0.4,0.5) , frame=T)
par(mar=c(4, 4, 1.1, 2.1))
hist(h$VALUE ,col=rgb(1,0,1,0.8) , border=T,breaks=40,
     freq = FALSE, main="Value of current property" , xlab="Property",
     ylab = "Frequency")
lines(density(h$VALUE),type = "l")

# Age of oldest credit line in months

layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0.5, 3.1, 1.1, 2.1))
boxplot(h$Age.of.oldest.credit.line.in.months , horizontal=TRUE , xaxt="n", 
        col=rgb(0.5,0.6,0.4,0.5) , frame=T)
par(mar=c(4, 4, 1.1, 2.1))
hist(h$Age.of.oldest.credit.line.in.months ,col=rgb(0.4,0.8,0.2,0.8),
     border=T,breaks=40,
     freq = FALSE, main="Age of oldest credit line in months" ,
     xlab="Age of oldest credit line in months", ylab = "Frequency")
lines(density(h$Age.of.oldest.credit.line.in.months),type = "l")

# Debt-to-income ratio 

layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0.5, 3.1, 1.1, 2.1))
boxplot(h$Debt.to.income.ratio , horizontal=TRUE , xaxt="n", 
        col=rgb(0.8,0.8,0,0.5) , frame=T)
par(mar=c(4, 4, 1.1, 2.1))
hist(h$Debt.to.income.ratio ,col=rgb(0.2,0.8,0.5,0.8) , border=T,breaks=40,
     freq = FALSE, main="Debt-to-income ratio" , xlab="Debt-to-income ratio",
     ylab = "Frequency")
lines(density(h$Debt.to.income.ratio),type = "l")

dev.off()

# NOMINAL VARIABLES
# -----------------

# Occupational Categories

# Bar plot

h %>%
  ggplot(aes(JOB, fill = JOB))+
  geom_bar(position = "dodge", alpha = 0.5)+
  theme_clean()+stat_count(geom = "text", aes(label = ..count..), vjust = -0.5)+
ggtitle("Occupational Categories") +
  xlab("Occupational Categories")+ylab("Count")

# Pie chart

h1 <- h %>%
  group_by(JOB) %>%
  summarise(Count = n()) 


h2 <- as.numeric(h1$Count)
lab <- paste0(round(h2/sum(h2) * 100, 2), "%")
pie3D(h2,col = hcl.colors(length(h2), "Spectral"),border = "white",shade = 0.5,
    labels = c("Mgr(13.38%)","Office(17.15%)","Other(38.23%)",
               "ProfExe(26.72%)","Sales(1.58%)","Self(2.94%)"),explode = 0.05,
    theta = 1)
title(main = "Occupational Categories")


# Reason for the loan (Home Imp or Debt Cons.)

# Bar plot

h %>%
  ggplot(aes(REASON, fill = REASON))+
  geom_bar(position = "dodge", alpha = 0.5)+
  theme_clean()+stat_count(geom = "text", aes(label = ..count..), vjust = -0.5)+
ggtitle("Reason for the loan") +
  xlab("Reason for the loan")+ylab("Count")

# Pie chart

h3 <- h %>%
  group_by(REASON) %>%
  summarise(Count = n()) 

h4 <- as.numeric(h3$Count)
lab1 <- paste0(round(h4/sum(h4) * 100, 2), "%")
pie3D(h4,col = hcl.colors(length(h4), "Spectral"),border = "white",shade = 0.5,
labels = c("Debt consolidation(70.42%)","Home improvement(29.58%)"),explode = 0.05,theta = 1)
title(main = "Reason for the loan")

# Number of Loans defaulted or paid back

# Bar Plot

h %>%
  ggplot(aes(hname, fill = hname))+
  geom_bar(position = "dodge", alpha = 0.5)+
  theme_clean()+stat_count(geom = "text", aes(label = ..count..), vjust = -0.5)+
ggtitle("Number of Loans defaulted or paid back") +
  xlab("Number of Loans defaulted or paid back")+ylab("Values")  

# Pie chart

h5 <- h %>%
  group_by(BAD) %>%
  summarise(Count = n()) 

h6 <- as.numeric(h5$Count)
lab2 <- paste0(round(h6/sum(h6)*100,2),"%")
pie3D(h6,col = c("purple","pink"),border = "white",shade = 0.5,
    labels = c("Repaid(91.08%)","Bad(8.92%)"),theta = 1) #theta = 1
title(main = "Number of Loans defaulted or paid back")


# BIVARIATE ANALYSIS
# ------------------------------------------------------------------------------

# Renaming BAD values of 0 and 1 with "REPAID" and "BAD" to help with further 
# analysis.
hname <- ifelse(h$BAD==1,"BAD","REPAID")
h$hname <- hname

# Correlation matrix 

library(GGally)
ggpairs(h, title="CORRELOGRAM")

# Scatter plots for variables with high correlation (+ve or -ve)


library( rgl )
library(magick)

colors <- c("royalblue1", "darkcyan")
plot3d(h$MORTDUE,h$VALUE,col = colors, type = "p",xlab="Value",
       ylab="Mortgage Due")
play3d(spin3d(axis = c(0, 0, 0), rpm = 10), duration = 60)

colors <- c("red", "yellow")
plot3d(h$LOAN,h$VALUE,col = colors, type = "p",xlab="Value", ylab="Loan")
play3d(spin3d(axis = c(0, 0, 1), rpm = 7), duration = 60)

colors <- c("pink", "purple")
plot3d(h$MORTDUE,h$Number.of.credit.lines,col = colors, type = "p",
       xlab = "Number of credit lines", ylab = "Mortgage Due")
play3d(spin3d(axis = c(0, 0, 1), rpm = 7), duration = 60)

# Multiple Bar Diagram (JOB)

h %>%
ggplot(aes(fill=JOB, y=LOAN, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("Loan amount for various occupational categories") +
  xlab("JOB")+ylab("Loan amount")
  
h %>%
  ggplot(aes(fill=JOB, y=VALUE, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("Property value for various occupational categories") +
  xlab("JOB")+ylab("Property value")  
h %>%
  ggplot(aes(fill=JOB, y=MORTDUE, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("Mortgage Value for various occupational categories") +
  xlab("JOB")+ylab("Mortgage Value")  
h %>%
  ggplot(aes(fill=JOB, y=Age.of.oldest.credit.line.in.months, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("Oldest credit Line for various occupational categories") +
  xlab("JOB")+ylab("Oldest credit Line (Months)")  
h %>%
  ggplot(aes(fill=JOB, y=Number.of.credit.lines, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("No. of credit Lines for various occupational categories") +
  xlab("JOB")+ylab("No. of credit Lines")  
h %>%
  ggplot(aes(fill=JOB, y=Debt.to.income.ratio, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("Debt-income Ratio for various occupational categories") +
  xlab("JOB")+ylab("Debt-income Ratio")  

# Multiple Bar Diagram (REASON)

h %>%
  ggplot(aes(fill=BAD, y=BAD, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("Loan amount for different reasons") +
  xlab("Reason")+ylab("Loan amount") 
h %>%
  ggplot(aes(fill=REASON, y=VALUE, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("Property Value for different reasons") +
  xlab("Reason")+ylab("Property Value") 
h %>%
  ggplot(aes(fill=REASON, y=MORTDUE, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("Mortgage due for different reasons") +
  xlab("Reason")+ylab("Mortgage due") 
h %>%
  ggplot(aes(fill=REASON, y=Age.of.oldest.credit.line.in.months, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("Age of oldest credit lines for different reasons") +
  xlab("Reason")+ylab("Oldest credit line(months)") 
h %>%
  ggplot(aes(fill=REASON, y=Number.of.credit.lines, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("No. of credit lines for different reasons") +
  xlab("Reason")+ylab("No. of credit lines")
h %>%
  ggplot(aes(fill=REASON, y=Debt.to.income.ratio, x=hname)) + 
  geom_bar(position="dodge", stat="identity",color = "black")+theme_economist()+
  ggtitle("Debt-income ratio for different reasons") +
  xlab("Reason")+ylab("Debt-income ratio") 

# VIOLIN BOX-PLOTS

ggbetweenstats(data = h,x = hname,y = Debt.to.income.ratio,
               outlier.tagging = TRUE)+
labs(x = "CREDIT STATUS",y = "DEBT INCOME RATIO",
                  title = "Credit status based on Debt income ratio") 

ggbetweenstats(data = hmeq,x = BAD,y = LOAN,outlier.tagging = TRUE)+
labs(x = "CREDIT STATUS",y = "Loan amount",
     title = "Credit status based on Loan") 

ggbetweenstats(data = h,x = hname,y = MORTDUE,outlier.tagging = TRUE)+
labs(x = "CREDIT STATUS",y = "Mortgage due",
     title = "Credit status based on due mortgage") 

ggbetweenstats(data = h,x = hname,y = VALUE,outlier.tagging = TRUE)+
labs(x = "CREDIT STATUS",y = "Property Value",
     title = "Credit status based on Property value") 

ggbetweenstats(data = h,x = hname,y = Age.of.oldest.credit.line.in.months,
               outlier.tagging = TRUE)+
labs(x = "CREDIT STATUS",y = "Oldest credit line(months)",
     title = "Credit status based on Oldest credit line") 

ggbetweenstats(data = h,x = hname,y = Number.of.credit.lines,
               outlier.tagging = TRUE)+
labs(x = "CREDIT STATUS",y = "Number of Credit Lines",
     title = "Credit status based on Number of Credit Lines") 

# BOX-PLOTS AFTER OUTLIER REMOVAL

# ------------------------------------------------------------------------------
data <- h$LOAN
quartiles <- quantile(data, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
loan1 <- subset(data, data > Lower & data < Upper)

data1 <- h$MORTDUE
quartiles <- quantile(data1, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data1)
Lower1 <- quartiles[1] - 1.5*IQR
Upper1 <- quartiles[2] + 1.5*IQR 
mort1 <- subset(data, data > Lower1 & data < Upper1)

data2 <- h$VALUE
quartiles <- quantile(data2, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data2)
Lower2 <- quartiles[1] - 1.5*IQR
Upper2 <- quartiles[2] + 1.5*IQR 
val1 <- subset(data2, data2 > Lower2 & data2 < Upper2)

data3 <- h$Age.of.oldest.credit.line.in.months
quartiles <- quantile(data3, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data3)
Lower3 <- quartiles[1] - 1.5*IQR
Upper3 <- quartiles[2] + 1.5*IQR 
oldcredit1 <- subset(data3, data3 > Lower3 & data3 < Upper3)

data4 <- h$Number.of.credit.lines
quartiles <- quantile(data4, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data4)
Lower4 <- quartiles[1] - 1.5*IQR
Upper4 <- quartiles[2] + 1.5*IQR 
ncredit1 <- subset(data4, data4 > Lower4 & data4 < Upper4)

data5 <- h$Debt.to.income.ratio
quartiles <- quantile(data5, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data5)
Lower5 <- quartiles[1] - 1.5*IQR
Upper5 <- quartiles[2] + 1.5*IQR 
DIR1 <- subset(data5, data5 > Lower5 & data5 < Upper5)

#-------------------------------------------------------------------------------

par(mfrow = c(2,3))

boxplot(loan1,xlab = "Loan",ylab = "Value", col = "#B2182B")

boxplot(mort1,xlab = "Mortgage Due",ylab = "Value", col = "#D6604D")

boxplot(val1,xlab = "Property Value",ylab = "Value", col = "#F4A582" )

boxplot(oldcredit1,xlab = "Age of Oldest credit line (months)",ylab = "Value", 
        col = "#92C5DE")

boxplot(ncredit1,xlab = "No. of credit lines",ylab = "Value", col = "#4393C3")

boxplot(DIR1,xlab = "Debt-Income ratio",ylab = "Value", col = "#2166AC")

# COUNT PLOTS

ggplot(h,aes(hname,Number.of.delinquent.credit.lines))+
  geom_count()+ theme_light()+
  ggtitle("NUMBER OF DELINQUENT CREDIT LINES") +
  xlab("Credit status")+ylab("Number of delinquent credit lines")

ggplot(h,aes(hname,Number.of.major.derogatory.reports))+geom_count()
  geom_count()+ theme_light()+
  ggtitle("NUMBER OF MAJOR DEROGATORY REPORTS") +
  xlab("Credit status")+ylab("Number of major derogatory reports")

ggplot(h,aes(hname,Number.of.recent.credit.inquiries))+geom_count()+
  geom_count()+ theme_light()+
  ggtitle("NUMBER OF RECENT CREDIT INQUIRIES") +
  xlab("Credit status")+ylab("Number of recent credit inquiries")





# ------------------------------------------------------------------------------




subset(h,select = c("JOB","BAD"))
d1 <- data.frame(h$JOB)









































































