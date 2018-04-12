library(shiny)
library(ggplot2)

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
## Define UI
########################################################################

## Analysis Tab
analysis_tab = tabPanel("Analysis",
                        sidebarLayout(
                            sidebarPanel(
                                h2("Predicted vs Retail Price"),
                                p("Shown here is the breakdown of the predicted price of a Linear Mixed Model vs the retail price as advertised on James Allen."),
                                p("A total of ", format(n_diamonds, big.mark = ","),
                                  " diamonds were included in the model."),
                                h3("Model Summary:"),
                                p("R-squared:", r2),
                                p("RMSE:", rmse),
                                br(),
                                br(),
                                selectInput(inputId = "x_axis",
                                            label = "X-axis",
                                            choices = selectionChoice,
                                            selected = "clarity"),
                                selectInput(inputId = "y_axis",
                                            label = "Y-axis",
                                            choices = selectionChoice,
                                            selected = "color"),
                                checkboxInput(inputId = "to_log",
                                              label = "Log the axis?",
                                              value = TRUE),
                                code("Disclaimer: Every model has it's flaw, the author takes no responsibility of the results")
                            ),
                            mainPanel(
                                plotOutput("prediction_expected_plot"))
                        ))

## Comparison Tab
compare_tab = tabPanel("Diamond Comparison",
                       sidebarLayout(
                           sidebarPanel(
                               ## Shape
                               selectInput(inputId = "shape",
                                           label = "Shape",
                                           choices = shape_level,
                                           selected = "round"),
                               ## Carat
                               numericInput(inputId = "carat",
                                            label = "Carat",
                                            value = 0.5),
                               ## Clarity
                               selectInput(inputId = "clarity",
                                           label = "Clarity",
                                           choices = clarity_level),

                               ## Cut
                               selectInput(inputId = "cut",
                                           label = "Cut",
                                           choices = cut_level),

                               ## Color
                               selectInput(inputId = "color",
                                           label = "Color",
                                           choices = color_level)
                           ),
                           mainPanel(
                               fluidRow(
                                   column(4, "Decrease"),
                                   column(4, textOutput(outputId = "expectedPrice")),
                                   column(4, "Increase")
                               )
                           )))

## Find a bargain tab
bargain_tab = tabPanel("Find a Bargain",
                       sidebarLayout(
                           sidebarPanel(),
                           mainPanel()
                       ))


## Put everything together
ui <- navbarPage("Diamond Analysis",
                 analysis_tab,
                 compare_tab,
                 bargain_tab
                 )



########################################################################
## Define server
########################################################################

server <- function(input, output){
    output$prediction_expected_plot =
        renderPlot({
            if(input$x_axis == input$y_axis)
                stop("X and Y axis can not be the same")

            ## Add facet
            facet_formula = formula(paste0(input$x_axis, " ~ ", input$y_axis))
            output =
                base_prediction_plot +
                facet_grid(facet_formula)

            ## Log plot
            if(input$to_log)
                output = output +
                    scale_y_log10() +
                    scale_x_log10()
            output
        }, height = 650)

    output$expectedPrice = renderText({
        newdf = data.frame(shape = input$shape,
                           carat = input$carat,
                           clarity = input$clarity,
                           cut = input$cut,
                           color = input$color)
        exp(predict(model, newdata = newdf))
    })
}


########################################################################
## Build app
########################################################################

shinyApp(ui = ui, server = server)
