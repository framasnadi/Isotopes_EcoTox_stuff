# -------------------
# Breakpoints analyses - Ecotox course 2023
# -------------------

# List of packages to install
packages_to_install <- c("readxl", "segmented")
# Check if packages are already installed; if not, install them
for (package in packages_to_install) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}
# load the R packages
library(readxl)
library(segmented)

# 1. Load the data into R
setwd("C:/Users/frma6502/Desktop/SU_Postdoc/ecotox2023") # set the folder with the data file

cont <- "HG"  # change the name according to the Contaminant you want to analyse (Excel sheet name)
db.yr <- read_excel("ecotox_db.flt.xlsx", 
                    sheet = cont, ) 

# -------------------
# Step 2: Visualize the Data
# -------------------
# Let’s create a scatterplot to visualize the timeseries, in this case level of Hg in CLUP_LAND 
plot(db.yr$Year,db.yr$CLUP_LAND , pch=16, col='steelblue', type = "b")
# We can see that Hg levels in CLUP LAND appear to increase until 2000 and then decrease

# -------------------
# 3. Fit the Piecewise Regression Model
# -------------------
# Piecewise regression is a regression method we often use when there are clear “breakpoints” in a dataset.
# first fit simple linear regression model:
fit <- lm(CLUP_LAND ~ Year, data=db.yr); summary(fit)
# fit piecewise regression model to original lm model, estimating a breakpoint at x=2000
my.seg <- segmented(fit, 
                    seg.Z = ~ Year, 
                    psi = list(Year = c( 2000)))  # Year 2000 can be a good candidate to test a breakpoint. When not providing estimates for the breakpoints "psi = NA" can be used.
# view summary of segmented model
summary(my.seg)
# get the breakpoints
my.seg$psi
my.seg$psi[,2]  # estimated breakpoint(s) year(s) by the model
# get the slopes
slope(my.seg)

# -------------------
# Step 4: Visualize the Final Piecewise Regression Model
# -------------------
#plot original data
plot(db.yr$Year,db.yr$CLUP_LAND , pch=16, col='steelblue')
#add segmented regression model
plot(my.seg, add=T)

# in this case could be interesting to re-run the Correlation Analyses between CLUP_LAND and GADU_SEGO before and after year 1997!

