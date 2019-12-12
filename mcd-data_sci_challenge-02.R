## McD Data Science Analysis 01
## Details in `Data Scientist Assignment 2 - Brief.pdf`; provided dataset in repo
## Dec 2019
## Khalil H Najafi
##
## ABSTRACT:
## Analyzing the outcome of a limited time promotion; units sold, units projected
## Key questions
##      1) Did the Ultimate Big Mac meet business objectives?
##      2) What other data would you curate to enhance the data set?
##         How would you approach it?
##      3) What recommendations would you make to McDonald's in future?
##      4) How can the analysis be expanded?
##         What impacts would it have on the recommendations?


# # # # # # # # # #

###### FUNCTIONS & WORKSPACE ######
# REQUIRED LIBRARIES
libraries <- c("data.table", "tidyverse", "lubridate", "shiny", "miniUI")
lapply(libraries, require, character.only = T)
rm(libraries)




###### DATASET ######

###### >>> Dataset Creation ######

## Raw Datasets

# Dataset containing forecasted units sold for selected items, weekly
# NB: for non ultimate Big Mac, forecast is based on non-promotional presence
dat_raw_forecast <- fread("./Data/data-forecast_units_sold_weekly.csv")

# Dataset containing actual units sold for selected items, weekly
dat_raw_actual <- fread("./Data/data-actual_units_sold_weekly.csv")

## Tidying
dat_forecast <- as_tibble(dat_raw_forecast) %>%
        melt(id.vars = "item") %>%
        rename(t = variable, units = value) %>%
        as_tibble()

dat_actual <- dat_raw_actual %>%
        melt(id.vars = "item") %>%
        rename(t = variable, units = value) %>%
        as_tibble()





# # # # # # # # # #



#### 1 - PERFORMANCE ANALYSIS ####
# To assess whether the objective was met, the analysis can be defined by:
#       prop of difference of weekly actual total (f) to
#       weekly forecast total (g) over UBM total is <= 0.65 (δ)
# f - g = δ ; |δ|/UBM <= 0.65 (p) ; p_mean <= 0.65

## Non UBM Weekly Forecast Totals (non_ubm_wft) ; (f)
f <- dat_forecast %>%
        filter(item != "Ultimate Big Mac Limited Time Offer") %>%
        group_by(t) %>%
        summarise(total_units_fc = sum(units))

## Non UBM Weekly Actual Totals (non_ubm_wat) ; (g)
g <- dat_actual %>%
        filter(item != "Ultimate Big Mac Limited Time Offer") %>%
        group_by(t) %>%
        summarise(total_units_act = sum(units))

## Difference of Non UBM Weekly Actual to Forecast ; (δ)
d <- g %>%
        left_join(f) %>%
        mutate(d = total_units_act - total_units_fc) %>%
        select(t, d)

## Proportion of (absolute) difference to UBM actual ; (p)
p <- dat_actual %>%
        filter(item == "Ultimate Big Mac Limited Time Offer") %>%
        left_join(d) %>%
        mutate(p = abs(d)/units,
               p_inv = 1 - p) %>%
        select(t, p, p_inv)

## p and p_inv determine the success of the promotion based on a priori expectations/objectives


## Comparing UBM actual v UBM forecast (dat_ubm)

#UBM forecasted
UBM_f <- dat_forecast %>%
        filter(item == "Ultimate Big Mac Limited Time Offer") %>%
        rename(units_f = units)

#UBM actual v UBM forecasted, by units and percentage
dat_UBM <- dat_actual %>%
        filter(item == "Ultimate Big Mac Limited Time Offer") %>%
        left_join(UBM_f) %>%
        mutate(delta = units - units_f,
               delta_perc = delta/units_f * 100)

#Average difference (percentage)
mean(dat_UBM$delta_perc)


