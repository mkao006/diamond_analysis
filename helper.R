########################################################################
## Preliminary setup
########################################################################

## Load model
model = readRDS("data/lmer.rds")
data_path = "data/processed_diamonds.Rda"
diamonds_processed = readRDS(data_path)
diamonds_processed$prediction = exp(predict(model, newdata = diamonds_processed))

## Set variable level
model_var = model@frame
shape_level = levels(model_var$shape)
clarity_level = levels(model_var$clarity)
cut_level = levels(model_var$cut)
color_level = levels(model_var$color)
selectionChoice = c("clarity", "color", "cut", "polish", "shape", "symmetry")

## Calculate model summary
r2 = with(diamonds_processed, round(cor(price, prediction)^2, 2))
rmse = with(diamonds_processed, round(mean(sqrt((price - prediction)^2)), 2))
n_diamonds = NROW(diamonds_processed)

## Define the base plot
base_prediction_plot =
    ggplot(data = diamonds_processed, aes(x = prediction, y = price)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, col = "red") +
    xlab("Predicted Price") +
    ylab("Retail Price")



########################################################################
## Helper functions
########################################################################

upgrade <- function(input_data, quality){
    switch(quality,
           cut =
               input_data %>%
               transform(cut = cut_level[min(length(cut_level),
                                             which(cut_level == input_data[[quality]]) + 1)]),
           color =
               input_data %>%
               transform(color = color_level[min(length(color_level),
                                                 which(color_level == input_data[[quality]]) + 1)]),
           clarity =
               input_data %>%
               transform(clarity = clarity_level[min(length(clarity_level),
                                                     which(clarity_level == input_data[[quality]]) + 1)]),
           carat =
               input_data %>%
               transform(carat = carat + 0.1))
}

downgrade <- function(input_data, quality){
    switch(quality,
           cut =
               input_data %>%
               transform(cut = cut_level[max(1,
                                             which(cut_level == input_data[[quality]]) - 1)]),
           color =
               input_data %>%
               transform(color = color_level[max(1,
                                                 which(color_level == input_data[[quality]]) - 1)]),
           clarity =
               input_data %>%
               transform(clarity = clarity_level[max(1,
                                                     which(clarity_level == input_data[[quality]]) - 1)]),
           carat =
               input_data %>%
               transform(carat = carat - 0.1))
}

neighbour_change <- function(input_data, model){
    input.df = data.frame(input_data)
    expected_price = exp(predict(model, newdata = input.df))
    qualities = c("cut", "color", "clarity", "carat")
    change.df =
        lapply(qualities,
               function(x) rbind(upgrade(input.df, x), downgrade(input.df, x))) %>%
        do.call(rbind, .)
    price_change = exp(predict(model, newdata = change.df)) - expected_price
    data.frame(quality = rep(qualities, each = 2),
               change = rep(c("up", "down"), 4),
               price_change = price_change)
    
}

shape_category <- function(value){
    factor(ifelse(value > 0, 1, ifelse(value < 0, -1, 0)))
}
