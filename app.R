library(shiny)

## Load model
model <- readRDS("data/lmer.rds")

## Set variable level
model_var = model@frame
shape_level = levels(model_var$shape)
clarity_level = levels(model_var$clarity)
cut_level = levels(model_var$cut)
color_level = levels(model_var$color)
polish_level = levels(model_var$polish)
symmetry_level = levels(model_var$symmetry)

## Define UI
ui <- fluidPage(
    titlePanel("Diamond Price Prediction"),

    sidebarLayout(


        sidebarPanel(
            ## Shape
            selectInput(inputId = "shapeSelection",
                        label = "Shape",
                        choices = shape_level,
                        selected = "round"),
            ## Carat
            numericInput(inputId = "caratInput",
                         label = "Carat",
                         value = 0.5),
            ## Clarity
            selectInput(inputId = "claritySelection",
                        label = "Clarity",
                        choices = clarity_level),

            ## Cut
            selectInput(inputId = "cutSelection",
                        label = "Cut",
                        choices = cut_level),

            ## Color
            selectInput(inputId = "colorSelection",
                        label = "Color",
                        choices = color_level)
        ),

        mainPanel(
            fluidRow(
                column(4, "Decrease"),
                column(4, textOutput(outputId = "expectedPrice")),
                column(4, "Increase")
            )

        )
    )
)

## Define server
server <- function(input, output){

    output$expectedPrice = renderText({
        newdf = data.frame(shape = input$shapeSelection,
                           carat = input$caratInput,
                           clarity = input$claritySelection,
                           cut = input$cutSelection,
                           color = input$colorSelection)
        print(newdf)
        exp(predict(model, newdata = newdf))
    })
}

shinyApp(ui = ui, server = server)
