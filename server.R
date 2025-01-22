#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    r_get_data_authors=reactive({
      input$collection
      datadir=glue::glue("data/{input$collection}")
      data_authors=readRDS(glue::glue("{datadir}/data_authors.RDS")) %>%
        dplyr::filter(producedDateY_i>=input$years[1] & producedDateY_i<=input$years[2])
    })
    r_get_data_crossed_authors=reactive({
      input$collection
      input$years
      input$doctype

      datadir=glue::glue("data/{input$collection}")
      data_crossed_authors=readRDS(glue::glue("{datadir}/crossed_authors.RDS")) %>%
        dplyr::filter(producedDateY_i>=input$years[1] & producedDateY_i<=input$years[2])
      if(input$doctype=="ART"){
        data_crossed_authors=data_crossed_authors %>%
          dplyr::filter(docType_s=="ART")
      }
      data_crossed_authors=data_crossed_authors %>%
        dplyr::group_by(val1,val2,affiliation) %>%
        dplyr::summarise(n=sum(n),
                  nrefs=sum(nrefs),
                  .groups="drop")
      return(data_crossed_authors)
    })

    r_get_graph=reactive({
      crossed_auth=r_get_data_crossed_authors()
      build_network(crossed_auth,
                   number_of_nodes=input$number_of_nodes)
    })
    output$collab_graph<- plotly::renderPlotly({
      graph=r_get_graph()
      plot_network(graph,
                   number_of_names=input$number_of_names,
                   sizevar=input$sizevar,
                   colorvar=input$colorvar)
    })

    observeEvent(input$collection,{
      datadir=input$collection
      data_authors=readRDS(glue::glue("data/{datadir}/data_authors.RDS"))
      range_years=range(data_authors$producedDateY_i)
      updateSliderInput(session,"years",
                        min=range_years[1],
                        max=range_years[2],
                        value=range_years)
    },ignoreInit=TRUE,ignoreNULL=TRUE)
      # max_number_of_names=data_authors$name %>% unique() %>% length()
      # print(max_number_of_names)
      # updateSliderInput(session,"number_of_nodes",
      #                   min=0,
      #                   max=max_number_of_names,
      #                   value=min(c(100,max_number_of_names)))
      # updateSliderInput(session,"number_of_names",
      #                   min=0,
      #                   max=max_number_of_names,
      #                   value=min(c(20,max_number_of_names))
}
