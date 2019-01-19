library(rsample)
library(broom)
library(tidyverse)

# Create a dataset of resampled/bootstraps using rsample
# There's other functions available here too for cross-validation, monte-carlo etc.
car_bootstaps <- bootstraps(mtcars, times = 1000)

# Now we create a helper function which fits an lm model to a given dataset
# We use the new `analysis()` function which unpacks the "rsample" data object and 
# just gets the training/analysis set from it
fit_lm_on_car_bootstraps <- function(which_split_to_fit) {
  lm(mpg ~ drat + disp, data = analysis(which_split_to_fit))
}

# Use purrr to run this function across the analysis set of 
# each of the 1000 resampled datasets
car_bootstaps <- car_bootstaps %>%
  mutate(fit_lm = purrr::map(splits, fit_lm_on_car_bootstraps))

# Again use purrr but this time we pull the model fit statistics
# into another column using broom

# Here we're pulling out estimates of the parameters but we could use the exact
# same process to run a prediction of each of the fitted models against 
# each of the assessment/training resampled data sets to get holdout estimates of performance
car_bootstaps <- car_bootstaps %>%
  mutate(coef_info = purrr::map(fit_lm, broom::tidy))

# Unnest all the bootstrap coefficients into nice, flat, tidy data frame
bootstrap_coefficients <- car_bootstaps %>% 
  tidyr::unnest(coef_info)

# Add some summary statistics on the model parameter estimates
bootstrap_coefficients <- bootstrap_coefficients %>%
  group_by(term) %>%
  mutate(median_est = median(estimate),
         upper = quantile(estimate, probs = 0.95),
         lower = quantile(estimate, probs = 0.05)) %>%
  ungroup()

# Plot out our bootstrapped parameter estimates
bootstrap_coefficients %>%
  ggplot(aes(x = estimate)) + 
  facet_wrap(~ term, scales = "free") + 
  geom_histogram(bins = 30) +
  geom_vline(aes(xintercept = median_est)) +
  geom_rect(data = bootstrap_coefficients[1:3,], 
          aes(xmin = lower, xmax = upper, ymin = 0, ymax = Inf), fill = "red", alpha =0.1)
