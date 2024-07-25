###################################################### - 
# Project Information
# Author
###################################################### - 

###################### - 
# Packages ---- 
###################### -

# Packages
library(correlation) #An alternative package for correlations; nicer looking output
library(flextable)   
library(ggResidpanel)
library(rio) 
library(tidyverse) 
library(marginaleffects)
library(modelsummary) 

###################### - 
# Data ---- 
###################### -
demdata <- import("data/demdata.rds")

###################### - 
# Data Cleaning  ---- 
###################### -
 
## Democracy DV: distribution ----
summary(demdata$v2x_polyarchy)

# Graphical display
ggplot(demdata, aes(x = v2x_polyarchy)) + 
  geom_histogram(color = 'black', fill = 'white') + 
  labs(title = 'Polyarchy Score Distribution', 
       x = NULL, y = 'Count')

## Corruption ----
summary(demdata$cpi)

ggplot(demdata, aes(x = cpi)) + 
  geom_histogram(color = 'black', fill = 'white') + 
  labs(title = 'Corruption Distribution', 
       x = NULL, y = 'Count')

## Inequailty ----
summary(demdata$gini_2011)

## Region (factor variable) ----

# Creating a four-category region variable
demdata <- demdata |> 
  mutate(
    region = case_when( 
      e_regiongeo %in% c(1:4) ~ 1, 
      e_regiongeo %in% c(5:9) ~ 2, 
      e_regiongeo %in% c(10:15) ~ 3, 
      e_regiongeo %in% c(16:19) ~ 4))

# Which category has the most observations? 
# Note: might not be the same one here once we have controls in the model!
table(demdata$region)

# Create a factor variable
demdata <- demdata |> 
  mutate(region_fct = factor(region, 
                    levels=c(2,3,1,4), #<2>
                    labels=c("Africa", "Asia", "Europe", "Americas")))


###################### - 
# Descriptive & Bivariate Analyses ---- 
###################### -

## Descriptives for the continuous variables all at once
# using the describe() function the psych package

demdata |> 
  select(v2x_polyarchy, cpi, gini_2011) |> 
  psych::describe()

## Maybe a table? Lots of potential packages here; this uses 
## datasummary() from the `modelsummary` package
## Would create a table with three rows (one for each variable)
## and columns for mean, sd, min and max values, and the 25 and 75 pct values
## See: https://modelsummary.com/vignettes/datasummary.html

datasummary(
  (`Polyarchy` = v2x_polyarchy) + (`Corruption` = cpi) + 
    (`Inequality (2011)` = gini_2011) ~ Mean + SD + Min + Max + P25 + P75, 
  data = demdata)

## Their correlations
demdata |> 
  select(v2x_polyarchy, cpi, gini_2011) |> 
  correlation()

## DV Mean (SD), N by Region
demdata |> 
  group_by(region_fct) |> 
  summarize(
    mean = mean(v2x_polyarchy, na.rm = T), 
    sd = sd(v2x_polyarchy, na.rm = T), 
    n = n())

###################### - 
# Models, Assumptions, Tables ---- 
###################### -

## Regression Model ----

m1 <- lm(v2x_polyarchy ~ cpi, data = demdata)
m2 <- lm(v2x_polyarchy ~ cpi + region_fct, data = demdata)

### Assumptions ----

## Various plots
assumption_plots <- resid_panel(m2, plots = c("resid", "qq", "hist"))
assumption_plots
  #can save via ggsave
ggsave("assumption_plots.png", 
       plot = assumption_plots, 
       height = 8, width = 12)

#Added variable plots
car::avPlots(m2)
  #need to use Base R ways of saving this, unfortunately
  #either via syntax like that below or via the menu
  #see: http://www.sthda.com/english/wiki/creating-and-saving-graphs-r-base-graphs

#open a file
png("avplots.png")
#create the plot
car::avPlots(m2)
# close the file
dev.off()

## Multicollinearity
car::vif(m2)

### Predicted Values ----
predictions(m2, 
            newdata = datagrid(cpi = c(12, 38.5, 88)))

## Regression Table ----

reg_table <- modelsummary(m2, 
                          estimate = "{estimate}{stars}\n{std.error}", 
                          statistic = NULL, 
                          gof_map = c("nobs", "r.squared", "adj.r.squared"), 
                          notes = list("Notes: OLS Coefficients with SEs in Parentheses",
                                       "* p < 0.05; ** p < 0.01; *** p < 0.001"),
                          output = 'flextable')

reg_table <- reg_table |> 
  hline(i = nrow_part(reg_table) - 3) |>  
  align(i = 1:nrow_part(reg_table), j = 2:ncol_keys(reg_table), 
        align = 'center') |>  
  align(align = 'center', part = 'header') 

reg_table
