#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(HALtere)

# Define UI for application that draws a histogram
fluidPage(
    id = "navbarPage",
    title =img(src = "www/hex-HALtere.png"),
    windowTitle = "HALtere",

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        sidebarPanel(
          selectInput("collection",
                      "Collection:",
                      c("BIOEENVIS","EVS_UMR5600","OSR")),
            sliderInput("years",
                        "years:",
                        min = 1970,
                        max = 2025,
                        value=c(2020,2025)),
            selectInput("sizevar",
                        "Size nodes according to:",
                        choices=c("nrefs")),
            selectInput("colorvar",
                        "Color nodes according to:",
                        choices=c("affiliation","name")),
          sliderInput("number_of_nodes",
                      "Number of nodes to display:",
                      min=0,
                      max=100,
                      value=100),
            sliderInput("number_of_names",
                        "Number of names to display:",
                        min=0,
                        max=50,
                        value=20),
            radioButtons("doctype",
                         "Type of document",
                         c("articles only","all documents"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotly::plotlyOutput("collab_graph",
                                 width = "100%",
                                 height = "600px",)
        )
    )
)
