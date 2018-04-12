library(ggplot2)

data_path = "../data/processed_diamonds.Rda"
diamonds_processed = readRDS(data_path)

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


## Take log-log relationship
ggplot(data = diamonds_processed, aes(x = carat, y = price)) +
    scale_y_log10() +
    scale_x_log10() +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_grid(clarity ~ color)
