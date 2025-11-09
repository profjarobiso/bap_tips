#' ---
#' code-annotations: hover
#' reference-location: margin
#' toc-depth: 4
#' title: analysis_clustering
#' ---
#' 

#| message: false
#| warning: false
#| code-fold: true
#| code-summary: Show the code

# Packages
library(flextable)
library(rio) # <1> 
library(tidyverse) #<2> 
library(modelsummary) #<3>
library(marginaleffects)
library(broom)
library(parameters)
library(patchwork)

#| message: false
#| warning: false
#| code-fold: true
#| code-summary: Show the code
#| fig-cap: Distribution of Individual-Level Responses
#| label: fig-indiv-dist

# Load our data
wvs <- import("./data/wvs_small.rda")

#Some cleaning 
wvs <- wvs |> 
  mutate(
    country_name = sjlabelled::as_label(B_COUNTRY), 
    country_name = as.character(country_name)) #<1>
  
#A plot
wvs |> 
  filter(!is.na(Q252)) |> 
  group_by(Q252) |> 
  tally() |> 
  ggplot(aes(x = Q252, y = n)) + 
  geom_col() + 
  labs(x = "Regime Satisfaction\n1 = Not Satisfied at All, 10 = Completely Satisfied", 
       y = "Count") +
  scale_x_continuous(breaks = c(1:10)) + 
  theme_minimal()



#| message: false
#| warning: false
#| code-fold: true
#| code-summary: Show the code
#| fig-cap: Distribution of Country Averages
#| label: fig-country-dist

#Country Averaegs
country_avgs <- wvs |> 
  group_by(country_name) |> 
  summarize(dem_satis = mean(Q252, na.rm = T))

#Plot
ggplot(country_avgs, aes(x = dem_satis)) + 
  geom_histogram(fill = 'white', color = 'black') + 
  labs(y = "Mean Democratic Satisfaction within Country", 
       x = "Regime Satisfaction") + 
  scale_x_continuous(limits = c(1, 10), 
                     breaks = seq(from = 1, to = 10, by = 1)) + 
  theme_minimal()


#| message: false
#| warning: false
#| code-fold: true
#| code-summary: Show the code
#| fig-cap: Distribution of Individual Responses by Country
#| label: fig-indiv-country-dist
#| fig-height: 12

wvs |> 
  filter(!is.na(Q252)) |> 
  group_by(country_name, Q252) |> 
  tally() |> 
  ggplot(aes(x = Q252, y = n)) + 
  geom_col() + 
  facet_wrap(~ country_name) + 
  labs(x = "Regime Satisfaction\n1 = Not Satisfied at All, 10 = Completely Satisfied", 
       y = "Count") +
  scale_x_continuous(breaks = c(1,3,5,7,9)) + 
  theme_minimal()


# Filter
nz <- wvs |> 
  filter(country_name == "New Zealand")

#run our model
model_nz <- lm(Q252 ~ Q50, data = nz)

#summary of coefficients
tidy(model_nz)

#predicted values via avg_predictions()
avg_predictions(model_nz, variables = "Q50") # <1> 


#run our model
model_all_indiv <- lm(Q252 ~ Q50 + country_name, data = wvs)

#summary of coefficients
tidy(model_all_indiv)



modelsummary(model_all_indiv, 
             stars = T,
             coef_map = c(
               "(Intercept)" = "Intercept", 
               "Q50" = "Personal Economic Situation"), 
             vcov = ~country_name, #<1>
             gof_map = c("nobs", "r.squared", "adj.r.squared", "vcov.type"),  #<2>            
             notes = list("Linear regression coefficients with standard errors in parentheses. Standard errors are clustered by country."))


#Without clustering standard errors: 
modelsummary(model_all_indiv, output ='modelsummary_list')$tidy |> 
  select(term, estimate, std.error) |> 
  slice(2)

#With clustering of standard errors
modelsummary(model_all_indiv, output ='modelsummary_list', 
             vcov = ~country_name)$tidy |> 
  select(term, estimate, std.error) |> 
  slice(2)




predictions(model_all_indiv, 
            newdata = datagrid(Q50 = c(1, 5, 6, 8, 10)), 
            vcov = ~country_name)  


wvs |> 
  select(country_name, Q252, unemploytotal) |> 
  na.omit() |> 
  head()


#Aggregate the data 
aggregated <- wvs |> 
  group_by(country_name) |>
  summarize(reg_satis = mean(Q252, na.rm = T), 
            unemploy = mean(unemploytotal, na.rm = T))

#Take a look
aggregated

#| message: false
#| warning: false
#| code-fold: true
#| code-summary: Show the code
#| fig-cap: Unemployment & Regime Satisfaction
#| label: fig-unemployment

#Bivariate plot
ggplot(aggregated, aes(x = unemploy, y = reg_satis)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(y = "Mean Regime Satisfaction", 
       x = "Unemployment (% of total labor force)") + 
  theme_minimal() + 
  scale_y_continuous(limits = c(1, 10), 
                     breaks = seq(from = 1, to = 10, by = 1))



#Model
model_aggregated <- lm(reg_satis ~ unemploy, data = aggregated)

#Coefficients
tidy(model_aggregated)



avg_predictions(model_aggregated, variables = "unemploy")

#| message: false
#| warning: false

library(lme4)
library(lmerTest)
library(performance)
library(parameters)


mixed_null <- lmer(Q252 ~ 1 + (1 | country_name), data = wvs)


summary(mixed_null)


mean(aggregated$reg_satis, na.rm = T)


sd(aggregated$reg_satis, na.rm = T)


icc(mixed_null)


# Only individual
mixed_indiv <- lmer(Q252 ~ Q50 + (1 | country_name), data = wvs)

#Only aggregate
mixed_agg <- lmer(Q252 ~ unemploytotal + (1 | country_name), data = wvs)

#Both
mixed_both <- lmer(Q252 ~ Q50 + unemploytotal + (1 | country_name), data = wvs)


# Via Summary
summary(mixed_indiv)

#Via parameters::parameters()
parameters(mixed_indiv)


predictions(mixed_indiv, 
            newdata = datagrid(Q50 = c(1, 3, 5, 7, 9, 10)))


summary(mixed_agg)


predictions(mixed_agg, newdata = datagrid(unemploytotal = c(1:10)))


#Uses the parameters() command and some filtering to focus
#our attention on the random effects

parameters(mixed_null) |> 
  filter(Parameter != "(Intercept)")


#Individual 
standardise_parameters(mixed_indiv)

#Country
standardise_parameters(mixed_agg)

#| code-fold: true
#| code-summary: Show the code
#| label: tbl-comparison
#| tbl-cap: Comparison of Different Methods of Analysing Clustered Data

#See the chapter on 'regression table formatting suggesion' 
#for an explanation of the flextable code

#List
model_comps <- list(
  "Indiv w/FE" = model_all_indiv, 
  "Aggregated" = model_aggregated, 
  "Multi-level" = mixed_indiv, 
  "Multi-Level" = mixed_agg, 
  "Multi-Level" = mixed_both)

#Table with some flextable formatting
model_comps_table <- modelsummary(model_comps, 
             estimate = "{estimate}{stars}\n{std.error}", 
             statistic = NULL, 
             gof_map = c("nobs", "r.squared", "adj.r.squared", 
                         "r2.marginal", "r2.conditional"),
             vcov = c(~ country_name, "classical", "classical", 
                      "classical", "classical"), 
             coef_map = c(
               "(Intercept)" = "Intercept", 
               "Q50" = "Personal Financial Situation", 
               "unemploy" = "Country Unemployment Rate", 
               "unemploytotal" = "Country Unemployment Rate"), 
             notes = list("Notes: OLS or multi-level model coefficients with SEs in parentheses. Country fixed effect estimates omitted from FE model. FE model clusters SEs by country.", 
                          "* p < 0.05; ** p < 0.01; *** p < 0.001"),
             output = 'flextable')
             
             
model_comps_table |> 
  hline(i = nrow_part(model_comps_table) - 3) |>
  align(i = 1:nrow_part(model_comps_table), j = 2:ncol_keys(model_comps_table), align = 'center') |>
  align(align = 'center', part = 'header') |> 
  autofit()



# Different intercept, same slope
coef(mixed_indiv)

#| message: false
#| warning: false
#| code-fold: true
#| code-summary: Show the code
#| fig-cap: Correlation between Macro and Micro Economic Indicators
#| label: fig-econ

#Get the aggregated data
econ_data <- wvs |> 
  group_by(country_name) |> 
  summarize(personal = mean(Q50, na.rm = T), 
            country = mean(unemploytotal, na.rm =T))

#Correlation between them using the correlation package
econ_corr <- correlation::correlation(econ_data)

#Plot
ggplot(econ_data, aes(x = country, y = personal)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(x = "Country Unemployment Rate",
       y = "Country Average Personal Financial Situation") + 
  theme_minimal() + 
  scale_y_continuous(limits = c(1,10), 
                     breaks = c(1:10)) + 
  geom_text(x = 3, 
            y = 8, 
            label = paste("Correlation =", 
                              round(econ_corr[1,3],2), 
                              sep = " "))


#Center the variable within country
wvs <- wvs |> 
  group_by(country_name) |> 
  mutate(personal_country_mean = mean(Q50, na.rm = T)) |> 
  ungroup() |> #<1>
  mutate(personal_center = Q50 - personal_country_mean)



wvs |> 
  select(Q50, personal_center) |> 
  psych::describe()

#| message: false
#| warning: false
#| fig-height: 12

ggplot(wvs, aes(x = personal_center, y =Q50)) + 
  geom_point() + 
  facet_wrap(~ country_name) + 
  labs(x = "Centered Financial Satisfaction", 
       y = "Original Scale") + 
  theme_minimal()

#| code-fold: true
#| code-summary: Show the code
#| label: tbl-comparison1
#| tbl-cap: Comparison with Centered IV

#Fit model with centered variable
mixed_both_center <- lmer(Q252 ~ personal_center + unemploytotal + 
                            (1 | country_name), data = wvs)

#Table
#List
model_comps1 <- list(
  "Indiv w/FE" = model_all_indiv, 
  "Aggregated" = model_aggregated, 
  "Multi-level" = mixed_indiv, 
  "Multi-Level" = mixed_agg, 
  "Multi-Level" = mixed_both, 
  "Multi-Level" = mixed_both_center)

#Table with some flextable formatting
model_comps_table1 <- modelsummary(model_comps1, 
             estimate = "{estimate}{stars}\n{std.error}", 
             statistic = NULL, 
             gof_map = c("nobs", "r.squared", "adj.r.squared", 
                         "r2.marginal", "r2.conditional"),
             vcov = c(~ country_name, "classical", "classical", 
                      "classical", "classical", "classical"), 
             coef_map = c(
               "(Intercept)" = "Intercept", 
               "Q50" = "Personal Financial Situation", 
               "personal_center" = "Personal Financial Situation (Centered)",
               "unemploy" = "Country Unemployment Rate", 
               "unemploytotal" = "Country Unemployment Rate"), 
             notes = list("Notes: OLS or multi-level model coefficients with SEs in parentheses. Country fixed effect estimates omitted from FE model. Clustered standard errors not taken into account in the first model.", 
                          "* p < 0.05; ** p < 0.01; *** p < 0.001"),
             output = 'flextable')
             
             
model_comps_table1 |> 
  hline(i = nrow_part(model_comps_table1) - 3) |>
  align(i = 1:nrow_part(model_comps_table1), j = 2:ncol_keys(model_comps_table1), align = 'center') |>
  align(align = 'center', part = 'header') |> 
  autofit()



#Mixed Model with interaction
mixed_interaction <- lmer(Q252 ~ personal_center*unemploytotal + 
                            (1 | country_name), data = wvs)

#Results
summary(mixed_interaction)
parameters(mixed_interaction)

#| message: false
#| warning: false
#| code-fold: true
#| code-summary: Show the code
#| fig-cap: Predicted Values and AMEs from the Interaction Model
#| label: fig-interactions

# The syntax below uses some R syntax that is a little bit more advanced
# than taught in Statistics I and II. Basically, I'm storing the values 
# I want to make predictions form in a data object and then 
# directly accessing those values in other syntax calls, instead of writing them 
# down and then manually entering them. 

##Finding 1 SD < mean, mean, 1 SD > mean for the two variables
#Uses psych::describe to create a dataframe with the mean and sd
mean_data <- wvs |> 
  select(personal_center, unemploytotal) |> 
  psych::describe()

# I use some base R notation here (the [] stuff) to pass the 
#values from the mean_data df into a vector. 
# The first number in brackets is the row number for an observation and the second is the column number. So, [1,3] means: get me the value in row 1, column 3. 
# I could do all of this manually as well, e.g., unemploy_sdbelow <- 6.22 - 4.07, etc., but I want to get on with it! See the R book on interactions for doing this manually

personal_values <- c(mean_data[1,3] - mean_data[1,4], # mean - sd
                     mean_data[1,3], #mean, 
                     mean_data[1,3] + mean_data[1,4]) #mean + sd

unemploy_values <- c(mean_data[2,3] - mean_data[2,4], #mean + sd
                     mean_data[2,3], #mean, 
                     mean_data[2,3] + mean_data[2,4]) #mean + sd

## Predicted Values
# Personal by Country
#Also uses some base R stuff to simplify getting min to max values for
#unemployment. 
plot1 <- predictions(mixed_interaction, 
            newdata = datagrid(personal_center = c(-6:6), 
                               unemploytotal = unemploy_values)) |> 
  mutate(unemploytotal = factor(unemploytotal, 
                                labels = c("1 SD < Mean", 
                                           "Mean", 
                                           "1 SD > Mean"))) |>
  ggplot(aes(x = personal_center, y = estimate, linetype = unemploytotal)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + 
  labs(x = "(Centered) Personal Financial Situation", 
       y = "Predicted Value", 
       linetype = "Country Unemployment") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  guides(linetype = guide_legend(nrow = 2))

# Country by Personal
plot2 <- predictions(mixed_interaction, 
            newdata = datagrid(personal_center = personal_values, 
                               unemploytotal = mean_data[2,8]:mean_data[2,9])) |> 
  mutate(personal_center = factor(personal_center, 
                                labels = c("1 SD < Mean", 
                                           "Mean", 
                                           "1 SD > Mean"))) |>
  ggplot(aes(x = unemploytotal, y = estimate, linetype = personal_center)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + 
  labs(x = "Country Unemployment Rate", 
       y = "Predicted Value", 
       linetype = "Personal Financial") + 
  theme_bw() + 
  theme(legend.position = "bottom")  + 
  guides(linetype = guide_legend(nrow = 2))


##Slopes
plot3 <- avg_slopes(mixed_interaction, 
                    variables = "personal_center", 
                    by = "unemploytotal") |> 
  ggplot(aes(x = unemploytotal, y = estimate)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2) + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') + 
  labs(x = "Country Unemployment",
       y = "AME for Personal Financial Situation") + 
  theme_bw()
 
plot4 <- avg_slopes(mixed_interaction, 
           variables = "unemploytotal", 
           by = "personal_center") |> 
  ggplot(aes(x = personal_center, y = estimate)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') + 
  labs(x = "(Centered) Personal Financial Situation",
       y = "AME for Country Unemployment") + 
  theme_bw()
 
#Combining
plot3 + plot4 + plot1 + plot2


