library(ggplot2)
library(magrittr)
library(dplyr)
library(lme4)

data_path = "../data/diamonds_ja.csv"
output_path = "../data/processed_diamonds.Rda"

clarity_level = c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF")
cut_level = c("Good", "Very Good", "Excellent", "True Hearts")
color_level = rev(toupper(letters)[4:26])
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
    transform(., clarity = ordered(clarity, levels = clarity_level)) %>%
    transform(., cut = ordered(cut, levels = cut_level)) %>%
    transform(., color = ordered(color, levels = color_level)) %>%
    transform(., polish = ordered(polish, levels = polish_level)) %>%
    transform(., symmetry = ordered(symmetry, levels = symmetry_level)) %>%
    transform(., shape = factor(shape)) %>%
    transform(., carat_adjusted_price = sqrt(price)/carat) %>% 
    ## Select only common shapes
    subset(., shape %in% common_shape) %>%
    saveRDS(., file = output_path)
