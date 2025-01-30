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

  r_get_data_groups=reactive({
    input$collection
    datadir=glue::glue("data/{input$collection}")
    groups=input$groups
    data_groups=readRDS(glue::glue("{datadir}/data_{groups}.RDS")) %>%
      dplyr::filter(producedDateY_i>=input$years[1] & producedDateY_i<=input$years[2])
  })
  r_get_data_crossed=reactive({
    input$collection
    input$years
    input$doctype
    groups=input$groups
    datadir=glue::glue("data/{input$collection}")

    data_crossed=readRDS(glue::glue("{datadir}/crossed_{groups}.RDS")) %>%
      dplyr::filter(producedDateY_i>=input$years[1] & producedDateY_i<=input$years[2])
    if(input$doctype=="ART"){
      data_crossed=data_crossed %>%
        dplyr::filter(docType_s=="ART")
    }
    return(data_crossed)
  })

  r_get_graph=reactive({
    data_crossed=r_get_data_crossed()
    data_groups=r_get_data_groups()
    build_network(data_crossed=data_crossed,
                  data_groups=data_groups,
                  number_of_nodes=input$number_of_nodes)
  })
  output$collab_graph<- plotly::renderPlotly({
    graph=r_get_graph()
    plot_network(graph,
                 number_of_names=input$number_of_names,
                 namevar=input$namevar,
                 sizevar=input$sizevar,
                 colorvar=input$colorvar)
  })
  output$table_nodes <- DT::renderDT({
    graph=r_get_graph()
    graph$nodes
  })
  output$table_edges <- DT::renderDT({
    graph=r_get_graph()
    graph$edges
  })
  r_get_text=reactive({
    collection=input$collection
    years=input$years

    data=readRDS(glue::glue("data/{collection}/text.RDS")) %>%
      dplyr::filter(producedDateY_i>=input$years[1],
                    producedDateY_i<=input$years[2]) %>%
      na.omit()
    data
  })
  output$word_graph=renderPlot({
    data=r_get_text()
    data_freq=mixr::tidy_frequencies(data,lemma,top_freq=25)
    mixr::plot_frequencies(data_freq, cat=lemma, frequency=freq)

  })

  ######################"

  observeEvent(input$collection,{
    datadir=input$collection
    publications=readRDS(glue::glue("data/{datadir}/publications.RDS"))
    range_years=range(publications$producedDateY_i)
    updateSliderInput(session,"years",
                      min=range_years[1],
                      max=range_years[2],
                      value=range_years)
  })
  observeEvent(input$groups,{
    max_number_of_names=r_get_data_groups() %>% dplyr::pull(name) %>% unique() %>% length()
    updateSliderInput(session,"number_of_nodes",
                      min=0,
                      max=max_number_of_names,
                      value=min(c(400,max_number_of_names)))
    updateSliderInput(session,"number_of_names",
                      min=0,
                      max=max_number_of_names,
                      value=min(c(50,max_number_of_names)))
  })

}
