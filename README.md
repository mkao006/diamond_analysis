# Diamond Analysis

This repository is the home of a diamond analysis shiny app which
helps people to understand the pricing of diamonds.


Use this app with precaution and the author takes no responsiility of
the actions taken.

## Data

The data are extracted from the web pages of [James
Allen](https://www.jamesallen.com/), however, the author is not
affiliated with the company.

The script `scraper.py` scraps the price from the website and save the
result in the `data` directory.


## The app

There are three sections to web app.

* Price Derivatives
* Find a Bargain
* Predictive Analysis

### Price Derivatives

This page the user to selecte a diamond with specific quality its
expected price, further, the price given a change to each of the 4C
quality are also provided.

![price_derivative](https://user-images.githubusercontent.com/1054320/38766292-d8f00778-4023-11e8-884d-660ed774bf12.png)


### Find a Bargain

In this page, the user can find the optimal diamond on James Allen
based on a given set of minimum criteria.

![find_a_bargain](https://user-images.githubusercontent.com/1054320/38766302-00f0479c-4024-11e8-8855-2cd35665ed80.png)

### Predictive Analysis

The predicted value and the retail price are illustrated in this
component. This shows the power and the flaws of the model.

![predictive_analysis](https://user-images.githubusercontent.com/1054320/38766305-27287286-4024-11e8-9617-860748076f93.png)

## Analysis

The data processing, exploratory analysis and the models can all be
found under the `analysis` directory.


## Deployment

To launch the app, simply issue the follow command:

```r
shiny::runApp()
```

Alternatively, one can deploy the app on
[shinyapps](https://www.shinyapps.io/).

```r
rsconnect::deployApp(appFiles = c("data/", "app.R", "helper.R"))
```

Please ensure that `rsconnect` has been set up correctly, the
instruction can be found
[here](https://shiny.rstudio.com/articles/shinyapps.html).


