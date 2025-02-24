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
  r_get_ref_authors=reactive({
    publis=readRDS(glue::glue("{datadir}/publications.RDS")) %>%
      dplyr::select(id_ref,abstract=en_abstract_s,title=title_en,keywords=keywords_s) %>%
      dplyr::mutate_all(tidyr::replace_na,replace="") %>%
      tidyr::unite("text",2:4,remove=FALSE)
    data_ref_authors=readRDS(glue::glue("{datadir}/data_ref_authors.RDS")) %>%
      dplyr::mutate(title_en=dplyr::case_when(title_en==""~title_s,
                                              TRUE~title_en)) %>%
      dplyr::select(id_ref,
                    title=title_en,
                    journal=journalTitle_s,
                    author=name,
                    affiliation,
                    docType=docType_s,
                    year=producedDateY_i,
                    keywords=keywords_s) %>%
      dplyr::filter(year>=input$years[1],
                    year<=input$years[2]) %>%
      dplyr::left_join(publis)
    if(input$doctype=="ART"){data_ref_authors=data_ref_authors %>% dplyr::filter(docType=="ART")}
    if(input$textsearch!=""){
      varsearch=rlang::sym(input$varsearch)
      data_ref_authors = data_ref_authors %>%
        dplyr::filter(stringr::str_detect(!!varsearch,input$textsearch))
    }
    data_ref_authors %>%
      tidyr::unite("authors",all_of(c("author","affiliation")),sep=" (") %>%
      dplyr::mutate(authors=paste0(authors,")")) %>%
      dplyr::group_by(dplyr::across(-authors)) %>%
      tidyr::nest() %>%
      dplyr::mutate(authors=purrr::map_chr(data, ~paste0(.$authors,collapse="; "))) %>%
      dplyr::select(-data) %>%
      dplyr::ungroup()
  })
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
    graph$nodes %>%
      dplyr::select(name,name_simplified, affiliation, lemma, nrefs,betweenness, x,y)
  })
  output$table_edges <- DT::renderDT({
    graph=r_get_graph()
    graph$edges %>%
      dplyr::select(namefrom,nameto, nlinks, affiliation, lemma, nrefs)
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
  output$wordfreq=renderPlot({
    data=r_get_text()
    data_freq=HALtere::tidy_frequencies(data,lemma,top_freq=25)
    HALtere::plot_frequencies(data_freq, cat=lemma, frequency=freq)
  })
  output$word_period=renderPlot({
    data=r_get_text()
    years=range(data$producedDateY_i)
    years=sort(c(years,input$cutyear))
    labels=paste0(years[1:(length(years)-1)],"-",years[2:(length(years))])
    data2=data %>%
      dplyr::ungroup()%>%
      dplyr::mutate(catYear=cut(producedDateY_i,
                                breaks=years,labels=FALSE)) %>%
      dplyr::mutate(catYear=labels[catYear])

    if(input$freq_or_spec=="specificity"){
      data_freq=HALtere::tidy_specificities(data2,lemma,catYear,top_spec=input$number_of_words)
      plot=HALtere::plot_specificities(data_freq, lemma, catYear)
    }

    if(input$freq_or_spec=="frequency"){
      data_freq=data2 %>%
        dplyr::group_by(lemma,catYear) %>%
        dplyr::tally() %>%
        dplyr::group_by(catYear)  %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::mutate(id=1:dplyr::n()) %>%
        dplyr::filter(id<input$number_of_words) %>%
        dplyr::ungroup() %>%
        na.omit()
      plot=ggplot2::ggplot(data_freq,ggplot2::aes(x=-id,y=n,fill=catYear))+
        ggplot2::geom_bar(stat="identity",alpha=0.5)+
        ggplot2::facet_wrap(ggplot2::vars(catYear),scales="free")+
        ggplot2::scale_x_discrete(breaks=NULL)+
        ggplot2::coord_flip()+
        ggplot2::geom_text(ggplot2::aes(label =lemma,
                                        y = 0), hjust = 0)+
        ggplot2::labs(x="catYear",y="frequency")+
          ggplot2::theme(legend.position="none",)
    }
    plot
  })

  output$ref_authors=DT::renderDataTable({
    result=r_get_ref_authors() %>%
      dplyr::select(-text) %>%
      unique()
    result=result%>%
      dplyr::select(title,authors,journal, docType, year, keywords,abstract)
    if(!input$show_abstract){
      result=result %>% dplyr::select(-abstract)
    }
    result=result %>%
      DT::datatable() %>%
      DT::formatStyle(columns ="title",width='600px') %>%
      DT::formatStyle(columns ="authors",width='600px') %>%
      DT::formatStyle(columns ="keywords",width='600px')
    if(input$show_abstract){
        result=result %>%
          DT::formatStyle(columns ="abstract",width='1200px')
    }
    result
  })
  output$plot_ref=renderPlot({
    result=r_get_ref_authors() %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(n=dplyr::n_distinct(id_ref))
    ggplot2::ggplot(result, ggplot2::aes(x=year, y=n))+
      ggplot2::geom_col(fill="turquoise")+
      ggplot2::scale_x_continuous(limits=input$years)
  })

  ######################"

  observeEvent(input$collection,{
    datadir=input$collection
    publications=readRDS(glue::glue("data/{datadir}/publications.RDS"))
    range_years=range(publications$producedDateY_i)
    updateSliderInput(session,"years",
                      min=range_years[1],
                      max=range_years[2],
                      value=c(range_years[2]-10, range_years[2]))
  })
  observeEvent(input$years,{
    updateSliderInput(session,"cutyear",
                      min=input$years[1],
                      max=input$years[2],
                      value=round(input$years[1]+(input$years[2]-input$years[1])/2))
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
