library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
source("helper.R")

########################################################################
## Define UI
########################################################################

## Analysis Tab
analysis_tab = tabPanel("Analysis",
                        sidebarLayout(
                            sidebarPanel(
                                h2("Predicted vs Retail Price"),
                                p("Shown here is the breakdown of the predicted price of a Linear Mixed Model vs the retail price as advertised on James Allen. The work is only a personal project and is not affiliated with James Allen"),
                                p("A total of ", format(n_diamonds, big.mark = ","),
                                  " diamonds were included in the model."),
                                h3("Model Summary:"),
                                p("R-squared:", r2),
                                p("RMSE:", rmse),
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
                               h2("Price change"),
                               p("This tab shows the price change of an up or downgrade given the current selection. All prices are quoted in USD."),
                               p("Note: The change in carat is measured in 0.1 carat"),
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
                                           choices = clarity_level,
                                           selected = "VVS1"),

                               ## Cut
                               selectInput(inputId = "cut",
                                           label = "Cut",
                                           choices = cut_level,
                                           selected = "Very Good"),

                               ## Color
                               selectInput(inputId = "color",
                                           label = "Color",
                                           choices = color_level,
                                           selected = "G")
                           ),
                           mainPanel(
                               h3(textOutput("expected_price")),
                               plotOutput("change_plot")
                           )))

## Find a bargain tab
bargain_tab = tabPanel("Find a Bargain",
                       sidebarLayout(
                           sidebarPanel(
                               selectInput(inputId = "shape",
                                           label = "Shape",
                                           choices = shape_level,
                                           selected = "round"),
                               ## Carat
                               numericInput(inputId = "carat",
                                            label = "Minimum Carat Size",
                                            value = 0.5),
                               ## Clarity
                               selectInput(inputId = "clarity",
                                           label = "Minimum Clarity Level",
                                           choices = clarity_level,
                                           selected = "VVS1"),

                               ## Cut
                               selectInput(inputId = "cut",
                                           label = "Minim Cut Level",
                                           choices = cut_level,
                                           selected = "Very Good"),

                               ## Color
                               selectInput(inputId = "color",
                                           label = "Minim Color Level",
                                           choices = color_level,
                                           selected = "G"),

                               ## Price range
                               sliderInput(inputId = "price_range",
                                           label = "Price Range",
                                           min = price_range[1],
                                           max = price_range[2],
                                           value = c(0, 5000))
                           ),
                           mainPanel(
                               tableOutput('bargain_table')
                           )
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

    output$expected_price = renderText({
        newdf = data.frame(shape = input$shape,
                           carat = input$carat,
                           clarity = input$clarity,
                           cut = input$cut,
                           color = input$color)
        paste0("The expected price of the current selection is: $",
               round(exp(predict(model, newdata = newdf))))
    })

    output$change_plot = renderPlot({
        data.frame(shape = input$shape,
                   carat = input$carat,
                   clarity = input$clarity,
                   cut = input$cut,
                   color = input$color) %>%
            neighbour_change(., model = model) %>% {
                max_price_change = abs(max(.$price_change))
                ggplot(data = .,
                       aes(x = price_change, y = quality)) +
                    geom_point(size = 10, aes(color = change, shape = shape_category(price_change))) +
                    geom_line(arrow = arrow(length = unit(0.30, "cm"),
                                            ends = "both", type = "closed")) +
                    xlim(limits = c(-max_price_change, max_price_change)) +
                    geom_vline(xintercept = 0, linetype = "longdash") +
                    ylab("") +
                    xlab("Expected Price Change") +
                    scale_shape_manual(values = c("-1" = 60, "0" = 73, "1" = 62),
                                       breaks = c(-1, 0, 1)) +
                    guides(color=FALSE, shape = FALSE) +
                    theme(axis.text = element_text(size = 15))
            }
    }, height = 600)

    output$bargain_table = renderTable({
        diamonds_processed %>%
            subset(carat >= input$carat &
                   as.numeric(clarity) >= which(clarity_level == input$clarity) &
                   as.numeric(cut) >= which(cut_level == input$cut) &
                   as.numeric(color) >= which(color_level == input$color) &
                   shape == input$shape & 
                   price >= input$price_range[1] &
                   price <= input$price_range[2]) %>%
            arrange(., desc(gain)) %>%
            select(., c(id, carat, cut, color, clarity, symmetry, polish, price, gain, url)) %>%
            unique %>%
            transform(url = paste0("<a href='", url, "' target='_blank'> Go to Diamond </a>")) %>%
            head(., 10)
    }, sanitize.text.function = function(x) x)
}


########################################################################
## Build app
########################################################################

shinyApp(ui = ui, server = server)
