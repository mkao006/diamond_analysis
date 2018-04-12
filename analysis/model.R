library(lme4)
library(dplyr)
library(magrittr)
library(ggplot2)

model_output_path = "../data/lmer.rds"
data_path = "../data/processed_diamonds.Rda"
diamonds_processed = readRDS(data_path)

########################################################################
## Model exploratory
########################################################################

train_test_split <- function(data, train_prop, seed){
    sample_size = NROW(data)
    train_size = floor(sample_size * train_prop)
    train_index = sample(sample_size, size = train_size)
    test_index = setdiff(1:sample_size, train_index)

    train = data[train_index, ]
    test = data[test_index, ]
    list(training_set = train, test_set = test)
}

## Create model data for training and testing
diamond_model_data =
    diamonds_processed %>%
    select(., -carat_adjusted_price) %>%
    train_test_split(., train_prop = 0.75, seed = 587)



## Create a simple regression model
model_lm = lm(price ~ poly(carat, 2) + clarity + color + cut +
                  depth + polish + shape + symmetry + tablesize + x + y + z,
              data = diamond_model_data$training_set)
pred_lm = predict(model_lm, newdata = diamond_model_data$test_set)

## Append the prediction so we can see how we perform
diamonds_result = diamonds_processed
diamonds_result$pred_lm = predict(model_lm, newdata = diamonds_result)

ggplot(data = diamonds_result, aes(x = pred_lm, y = price)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, col = "red")


## This shows that the linear model underestimates the price of the
## premium diamonds in with high color grade (D, E) and also high
## clarity (VVS2, VVS1, IF). Also overestimating the price of low
## grade diamonds.
ggplot(data = diamonds_result, aes(x = pred_lm, y = price)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, col = "red") +
    facet_grid(clarity ~ color)


## Use Linear Mixed model, but removed a few variables
model_lme = lmer(log(price) ~ (1 + log(carat)|clarity:color:cut) + shape,
              data = diamond_model_data$training_set)
pred_lme = exp(predict(model_lme, newdata = diamond_model_data$test_set))

## The result improved, however there the prediction at the extreme
## low end are poor with several values being negative. The high end
## market appears to be fine, the discrepencies are likely to be
## variation in valuation.
diamonds_result$pred_lme = exp(predict(model_lme, newdata = diamonds_result))
ggplot(data = diamonds_result, aes(x = pred_lme, y = price)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, col = "red")

## After accounting or the between group differences, we can see the
## fit is now much better and accomodates to the difference classes of
## diamonds.
ggplot(data = diamonds_result, aes(x = pred_lme, y = price)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, col = "red") +
    facet_grid(clarity ~ color)


## NOTE (Michael): A closer inspection shows that some of the grading
##                 may not be ideal. The clarity of the following
##                 diamon is terribly flawed and thus the model gave a
##                 significantly higher price then the actual price.
##
## https://www.jamesallen.com/loose-diamonds/round-cut/1.00-carat-d-color-i1-clarity-very-good-cut-sku-3463671

## NOTE (Michael): This diamond has a much lower price then predicted
##                 because it exhibits strong fluorence. Huge
##                 discrepency with the second diamond, while the only
##                 thing separate them is the degree of fluorenscence.
##
## https://www.jamesallen.com/loose-diamonds/round-cut/2.41-carat-d-color-if-clarity-excellent-cut-sku-2770221
## https://www.jamesallen.com/loose-diamonds/round-cut/2.45-carat-d-color-if-clarity-excellent-cut-sku-2114348

## Save the model to app
saveRDS(model_lme, file = model_output_path)
