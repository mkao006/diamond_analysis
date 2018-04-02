library(ggplot2)
library(magrittr)
library(dplyr)
library(lme4)

data_path = "./data/diamonds_ja.csv"

clarity_level = c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF")
cut_level = c("Good", "Very Good", "Excellent", "True Hearts")
color_level = toupper(letters)[4:26]
polish_level = c("GD", "VG", "EX")
symmetry_level = c("FR", "GD", "VG", "EX")

common_shape = c("round", "oval", "emerald", "princess", "cushion modified",
                 "pear", "cushion", "radiant", "heart", "marquise", "asscher")

########################################################################
## Preprocessing
########################################################################

diamonds = read.csv(data_path, stringsAsFactors = FALSE)

## Recode cut and polish
diamonds[diamonds$cut == "Ideal", "cut"] = "Excellent"

## Recode polish
diamonds[diamonds$polish == "ID", "polish"] = "EX"

## Recode Symmetry
diamonds[diamonds$symmetry == "ID", "symmetry"] = "EX"


diamonds_processed =
    diamonds %>%
    ## Remove missing price and unclassified entries.
    subset(., !is.na(price) & cut != '' & polish != '' & symmetry != '') %>%
    ## Select relevant variables
    select(., c(id, carat, clarity, color, cut, depth, polish, price,
                shape, symmetry, tablesize, x, y, z)) %>%
    ## Remove inferior quality diamonds with very few observations
    subset(., color %in% toupper(letters)[4:8] & symmetry != 'FR' & clarity != "I1") %>%
    ## Remove Large diamonds
    subset(., carat < 2.5) %>%
    ## transform variables to factor
    transform(., clarity = factor(clarity, levels = clarity_level)) %>%
    transform(., cut = factor(cut, levels = cut_level)) %>%
    transform(., color = factor(color, levels = color_level)) %>%
    transform(., polish = factor(polish, levels = polish_level)) %>%
    transform(., symmetry = factor(symmetry, levels = symmetry_level)) %>%
    transform(., shape = factor(shape)) %>%
    transform(., carat_adjusted_price = sqrt(price)/carat) %>% 
    ## Select only common shapes
    subset(., shape %in% common_shape)



########################################################################
## Exploratory analysis
########################################################################


## carat vs price
##
## NOTE (Michael): The square root of the price appears to be linear
##                 coorelated to the carat up to approximately 2 carat
ggplot(data = diamonds_processed, aes(x = carat, y = price)) +
    geom_point() + geom_smooth()


ggplot(data = diamonds_processed, aes(x = carat, y = price)) +
    geom_point() +
    geom_smooth() +
    scale_y_sqrt()

## Due to the rarity of large diamonds, the price variation are also
## much greater.
ggplot(data = diamonds_processed, aes(x = carat, y = price)) +
    geom_point() +
    geom_smooth() +
    scale_y_sqrt() +
    facet_wrap(~shape)

## After further accounting for clarity and color, we can see a strong
## linear between the carat and the square root of the price.
ggplot(data = diamonds_processed, aes(x = carat, y = price)) +
    geom_point() +
    geom_smooth() +
    scale_y_sqrt() +
    facet_grid(clarity ~ color)


## color vs price
##
## NOTE (Michael): The initial plot shows the lower grade color gave
##                 rise to higher prices, however, after accounting
##                 for the carat size, the average price appears to
##                 reflect the expected price of better color.
##
## NOTE (Michael): The price appears to diminishes exponentially as
##                 the color grade drops.

ggplot(data = diamonds_processed, aes(x = color, y = price)) +
    scale_y_log10() + 
    geom_violin()

ggplot(data = diamonds_processed, aes(x = color, y = carat_adjusted_price)) +
    geom_violin()


## clarity vs price
##
## NOTE (Michael): Again, similar to the color, the price need to
##                 account for the carat size. It appears that the
##                 lower grade diamonds are generally accounted for
##                 with a greater size.
ggplot(data = diamonds_processed, aes(x = clarity, y = price)) +
    scale_y_log10() + 
    geom_violin()

ggplot(data = diamonds_processed, aes(x = clarity, y = carat_adjusted_price)) +
    geom_violin()


## clarity vs price
##
## NOTE (Michael): Again, similar to the color, the price need to
##                 account for the carat size. 
ggplot(data = diamonds_processed, aes(x = cut, y = price)) +
    scale_y_log10() + 
    geom_violin()

ggplot(data = diamonds_processed, aes(x = cut, y = carat_adjusted_price)) +
    geom_violin()

ggplot(data = diamonds_processed, aes(x = cut, y = carat_adjusted_price)) +
    geom_boxplot()



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

mape <- function(x, y){
    mean(abs((x - y)/x)) * 100
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
mape_lm = mape(pred_lm, diamond_model_data$test_set$price)

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
model_lme = lmer(price ~ (1 + poly(carat, 2)|clarity:color:cut) + polish + shape + symmetry,
              data = diamond_model_data$training_set)
pred_lme = predict(model_lme, newdata = diamond_model_data$test_set)
mape_lme = mape(pred_lme, diamond_model_data$test_set$price)

## The result improved, however there the prediction at the extreme
## low end are poor with several values being negative. The high end
## market appears to be fine, the discrepencies are likely to be
## variation in valuation.
diamonds_result$pred_lme = predict(model_lme, newdata = diamonds_result)
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
##                 because it exhibits strong fluorence.
##
## https://www.jamesallen.com/loose-diamonds/round-cut/2.41-carat-d-color-if-clarity-excellent-cut-sku-2770221

