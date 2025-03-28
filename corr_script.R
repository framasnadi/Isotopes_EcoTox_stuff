# -------------------
# Correlation analyses script - Ecotox course 2023
# -------------------

# List of packages to install
packages_to_install <- c("readxl", "readr", "dplyr", "tidyr", "ggplot2", "PerformanceAnalytics", "ggpubr", "rstatix")
# Check if packages are already installed; if not, install them
for (package in packages_to_install) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}
# load the R packages
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(ggpubr)
library(rstatix)


# 1. Load the data into R
setwd("C:/Users/frma6502/Desktop/SU_Postdoc/ecotox2023") # set the folder with the database file

cont <- "PCB118"  # change the name according to the Contaminant you want to analyse (Excel sheet name)

db.yr <- read_excel("ecotox_db.flt.xlsx", 
              sheet = cont, ) 
head(db.yr, 12)   #shows the first 12 rows of the db 
str(db.yr)        #info on the db structure
summary(db.yr)    #some basic stats on the variables (min value, max value, mean etc.)

##########################
# 2. Visualize the Data
# Let’s create a plot  to visualize all the timeseries available for the contaminant of interest, in this example PCB-118
pd <- position_dodge(0.5)
ggplot(db.yr %>% pivot_longer(cols = 3:8 ,values_to = "value", names_to = "Specie-Loc"), aes(x = as.factor(Year), y = value, group = `Specie-Loc`, color=`Specie-Loc` )) + geom_point(position = pd)+geom_line()+ geom_smooth(method = "loess", se = F, linetype="solid", size= 0.7)+geom_hline(yintercept = 0, linetype = "dotted")  + ggtitle(paste(cont))+ theme(axis.text.x = element_text(angle=90, hjust=1))+ ylab("avg.Value") + xlab("Year")

##########################
# 3. Check for outlier
# A boxplot helps to visualize a quantitative variable by displaying five common location summary (minimum, median, first and third quartiles and maximum) and any observation that was classified as a suspected outlier. In case, to remove the ouliers go in the excel file and manually remove them. ..but always make a copy of the original db!
db <- db.yr %>% dplyr::select(-Year,-CONT) # remove the first two columns for the following analyses 
boxplot(db , main="Box Plot")
 which(db$CLUP_46H0 %in% c(boxplot.stats(db$CLUP_46H0)$out)) # row 38 of CLUP_46H0 could be a potential outlier
 which(db$CLUP_UTLA %in% c(boxplot.stats(db$CLUP_UTLA)$out)) #  row 12 of CLUP_UTLA could be a potential outlier
 which(db$GADU_SEGO %in% c(boxplot.stats(db$GADU_SEGO)$out)) #  row 12 of GADU_SEGO could be a potential outlier
# More Outliers detection technique can be found here:  https://statsandr.com/blog/outliers-detection-in-r/

##########################
# 4. Correlation analyses
##########################
# the function cor.test() is used to perform Spearman's rho or Kendall rank correlation tau or Pearson's product-moment correlation value, see more info here:  https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/cor.test
cor.test(db$CLUP_UTLA, db$GADU_SEGO,method = "pearson" ) # you can change method from "pearson" to "spearman" or "kendall"

# otherwise you can run all correlations at once with cor_mat()
 cor.mat <- db %>% cor_mat( method = "pearson") # you can change method from "pearson" to "spearman" or "kendall"
 cor.mat %>% cor_get_pval()
 restable <- cor.mat %>% cor_gather();restable
write.xlsx(restable, paste("restable_",cont, ".xlsx"), rowNames = F) #save the table with all cor values and p-values directly as an Excel file (easy to use to create tables for your report)

# and with chart.Correlation() you can have a summary plot with all cor values and p-values.
#On top the (absolute) value of the correlation plus the result of the cor.test as stars. On bottom, the bivariate scatterplots, with a fitted line. p-values: *=p≤0.05, **=p≤0.01, ***=p≤0.001
chart.Correlation(db   , histogram=T, pch=5, method = c("pearson"));mtext(cont, side=3, line=3)

# Another method using ggpubr package to visualize and plot the correlations one by one
# Scatter plot with correlation coefficient
ggscatter(db, x = "CLUP_UTLA", y = "GADU_SEGO",  # Here you need to manually select the time series to compare
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE, # Add confidence interval
               title = cont,
)+ stat_cor(method = "pearson",p.accuracy = 0.05) # Add correlation coefficient and p-value, as before you can change method from "pearson" to "spearman" or "kendall"




