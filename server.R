#
# Server logic of HALtere Shiny app (query-based version)
#

library(shiny)

function(input, output, session) {

  global <- reactiveValues(datapath = getwd())

  output$dir <- renderText({
    global$datapath
  })


  # ---------------------------------------------------------------------------
  # App state
  # ---------------------------------------------------------------------------

  rv <- reactiveValues(
    state = "ready"
  )

  # ---------------------------------------------------------------------------
  # Root directory
  # ---------------------------------------------------------------------------


  r_root_dir <- reactive({
      #system.file("data_HALtere",package="HALtere")
      "inst/data_HALtere"
  })

  # ---------------------------------------------------------------------------
  # Dataset selection UI
  # ---------------------------------------------------------------------------

  output$ui_pub_list_selection <- renderUI({
    print("in ui_pub_list_selection")
    files_included <- list.files(r_root_dir(), full.names = FALSE)
    selectInput(
      "pub_list_name",
      "Publications of :",
      choices = list.dirs(
        r_root_dir(),
        full.names = FALSE,
        recursive = FALSE
      )
    )

  })



  # ---------------------------------------------------------------------------
  # Create new dataset UI
  # ---------------------------------------------------------------------------

  output$ui_create_dir <- renderUI({
    if (!isTRUE(input$create_new_dir)) {
      return(NULL)
    }

    tagList(
      textInput("new_pub_list_name", "Nom du nouveau répertoire", value = ""),
      textInput("pub_list_query", "Requête HAL (query)"),
      actionButton("create_data", "Créer les données")
    )
  })


  # ---------------------------------------------------------------------------
  # Create dataset
  # ---------------------------------------------------------------------------


  r_selected_dataset <- reactive({
    req(rv$state == "ready")
    if(is.null(input$pub_list_name)){
      pub_list_name="BIOEENVIS"
    }else{
      pub_list_name=input$pub_list_name
    }
    selected_dataset <- file.path(r_root_dir(),pub_list_name)
    print("in r_selected_dataset")
    print(selected_dataset)
    selected_dataset
  })


  # ---------------------------------------------------------------------------
  # Ref authors
  # ---------------------------------------------------------------------------

  r_get_ref_authors <- reactive({

    datadir <- r_selected_dataset()

    data_ref_authors <- readRDS(
      glue::glue("{datadir}/data_ref_authors.RDS")
    )
    publications <- readRDS(
      glue::glue("{datadir}/publications.RDS")
    )

    result <- show_ref_authors(publications,
                               data_ref_authors,
                               yearmin=input$years[1],
                               yearmax=input$years[2])

    if (input$doctype == "articles only") {
      result <- result %>% dplyr::filter(docType == "ART")
    }

    if (input$textsearch != "") {
      varsearch <- rlang::sym(input$varsearch)

      result <- result %>%
        dplyr::filter(
          stringr::str_detect(!!varsearch, input$textsearch)
        )
    }

    result
  })

  # ---------------------------------------------------------------------------
  # Groups
  # ---------------------------------------------------------------------------

  r_get_data_groups <- reactive({

    datadir <- r_selected_dataset()
    groups <- input$groups

    data_groups <- readRDS(
      glue::glue("{datadir}/data_{groups}.RDS")
    ) %>%
      dplyr::filter(
        producedDateY_i >= input$years[1] &
        producedDateY_i <= input$years[2]
      )

    data_words <- readRDS(
      glue::glue("{datadir}/data_words.RDS")
    )

    data_words <- data_words %>%
      dplyr::filter(!is.na(lemma_completed)) %>%
      dplyr::filter(
        producedDateY_i >= input$years[1] &
          producedDateY_i <= input$years[2]
      )

    if (groups == "labs") {
      data_words <- data_words %>%
        dplyr::mutate(
          name = affiliation,
          name_simplified = affiliation_simplified
        )
    }

    spec <- data_words %>%
      dplyr::group_by(name, name_simplified, lemma_completed) %>%
      dplyr::summarise(q = dplyr::n(), .groups = "drop") %>%
      dplyr::group_by(name, name_simplified) %>%
      dplyr::mutate(k = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(lemma_completed) %>%
      dplyr::mutate(m = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(n = dplyr::n() - m) %>%
      dplyr::mutate(
        p = purrr::pmap_dbl(list(q = q, k = k, m = m, n = n), phyper)
      ) %>%
      dplyr::mutate(p = (1 - p) * log(2)) %>%
      dplyr::mutate(spec = -log2(p)) %>%
      dplyr::arrange(name, desc(spec), desc(q), m) %>%
      dplyr::group_by(name) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(data, ~ .x[1, ])) %>%
      tidyr::unnest(cols = c("data")) %>%
      dplyr::ungroup()

    data_groups %>%
      dplyr::left_join(
        spec %>% dplyr::select(name, lemma = lemma_completed, spec),
        by = "name"
      ) %>%
      dplyr::mutate(
        lemma = dplyr::case_when(
          is.na(lemma) ~ name,
          TRUE ~ lemma
        )
      )
  })

  # ---------------------------------------------------------------------------
  # Crossed data
  # ---------------------------------------------------------------------------

  r_get_data_crossed <- reactive({

    datadir <- r_selected_dataset()

    groups <- input$groups

    data_crossed <- readRDS(
      glue::glue("{datadir}/crossed_{groups}.RDS")
    ) %>%
      dplyr::filter(
        producedDateY_i >= input$years[1] &
          producedDateY_i <= input$years[2]
      )

    if (input$choose_group) {

      elems_in_the_graph <- data_crossed %>%
        dplyr::filter(val1 == input$chosen_group) %>%
        dplyr::pull(val2)

      data_crossed <- data_crossed %>%
        dplyr::filter(
          val1 %in% c(input$chosen_group, elems_in_the_graph)
        )
    }

    if (input$doctype == "articles only") {
      data_crossed <- data_crossed %>%
        dplyr::filter(docType_s == "ART")
    }

    data_crossed
  })

  # ---------------------------------------------------------------------------
  # Graph
  # ---------------------------------------------------------------------------

  r_get_graph <- reactive({
    data_crossed <- r_get_data_crossed()
    data_groups <- r_get_data_groups()
    build_network(
      data_crossed = data_crossed,
      data_groups = data_groups,
      number_of_nodes = input$number_of_nodes
    )
  })

  output$collab_graph <- plotly::renderPlotly({

    graph <- r_get_graph()

    plot_network(
      graph,
      number_of_names = input$number_of_names,
      namevar = input$namevar,
      sizevar = input$sizevar,
      colorvar = input$colorvar
    )
  })

  output$table_nodes <- DT::renderDT({
    graph <- r_get_graph()

    graph$nodes %>%
      dplyr::select(
        name, name_simplified,
        affiliation, lemma,
        nrefs, betweenness, x, y
      )
  })

  output$table_edges <- DT::renderDT({
    graph <- r_get_graph()

    graph$edges %>%
      dplyr::select(
        namefrom, nameto,
        nlinks, affiliation,
        lemma, nrefs
      )
  })

  # ---------------------------------------------------------------------------
  # Text
  # ---------------------------------------------------------------------------

  r_get_text <- reactive({

    datadir <- r_selected_dataset()

    data <- readRDS(
      glue::glue("{datadir}/text.RDS")
    ) %>%
      dplyr::filter(
        producedDateY_i >= input$years[1],
        producedDateY_i <= input$years[2]
      ) %>%
      na.omit()

    if (input$doctype == "articles only") {
      data <- data %>% dplyr::filter(docType_s == "ART")
    }

    data
  })

  output$wordfreq <- renderPlot({

    data <- r_get_text()

    data_freq <- HALtere::tidy_frequencies(
      data,
      lemma_completed,
      top_freq = 25
    )

    HALtere::plot_frequencies(
      data_freq,
      cat = lemma_completed,
      frequency = freq
    )
  })

  output$word_period <- renderPlot({

    data <- r_get_text()

    years <- range(data$producedDateY_i)
    years <- sort(c(years, input$cutyear))

    labels <- paste0(
      years[1:(length(years) - 1)],
      "-",
      years[2:(length(years))]
    )

    data2 <- data %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        catYear = cut(
          producedDateY_i,
          breaks = years,
          labels = FALSE
        )
      ) %>%
      dplyr::mutate(catYear = labels[catYear])

    if (input$freq_or_spec == "specificity") {

      data_freq <- HALtere::tidy_specificities(
        data2,
        lemma,
        catYear,
        top_spec = input$number_of_words
      )

      plot <- HALtere::plot_specificities(
        data_freq,
        lemma,
        catYear
      )
    }

    if (input$freq_or_spec == "frequency") {

      data_freq <- data2 %>%
        dplyr::group_by(lemma, catYear) %>%
        dplyr::tally() %>%
        dplyr::group_by(catYear) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::mutate(id = 1:dplyr::n()) %>%
        dplyr::filter(id < input$number_of_words) %>%
        dplyr::ungroup() %>%
        na.omit()

      plot <- ggplot2::ggplot(
        data_freq,
        ggplot2::aes(x = -id, y = n, fill = catYear)
      ) +
        ggplot2::geom_bar(stat = "identity", alpha = 0.5) +
        ggplot2::facet_wrap(ggplot2::vars(catYear), scales = "free") +
        ggplot2::scale_x_discrete(breaks = NULL) +
        ggplot2::coord_flip() +
        ggplot2::geom_text(
          ggplot2::aes(label = lemma, y = 0),
          hjust = 0
        ) +
        ggplot2::labs(x = "catYear", y = "frequency") +
        ggplot2::theme(legend.position = "none")
    }

    plot
  })

  # ---------------------------------------------------------------------------
  # Outputs tables
  # ---------------------------------------------------------------------------

  output$ref_authors <- DT::renderDataTable({

    result <- r_get_ref_authors() %>%
      dplyr::select(-text) %>%
      unique() %>%
      dplyr::select(
        title, authors, journal,
        docType, year, keywords, abstract
      )

    if (!input$show_abstract) {
      result <- result %>% dplyr::select(-abstract)
    }

    DT::datatable(result) %>%
      DT::formatStyle(columns = "title", width = "600px") %>%
      DT::formatStyle(columns = "authors", width = "600px") %>%
      DT::formatStyle(columns = "keywords", width = "600px") %>%
      {
        if (input$show_abstract) {
          DT::formatStyle(., columns = "abstract", width = "1200px")
        } else .
      }
  })

  output$plot_ref <- renderPlot({

    r_selected_dataset()

    result <- r_get_ref_authors() %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(n = dplyr::n_distinct(id_ref))

    ggplot2::ggplot(result, ggplot2::aes(x = year, y = n)) +
      ggplot2::geom_col(fill = "turquoise") +
      ggplot2::scale_x_continuous(limits = input$years)
  })

  ## Observe/updates

  ######################"
  observe({
    groups_names=r_get_data_groups() %>%
      dplyr::pull(name) %>%
      unique()
    updateSelectInput(session,
                      "chosen_group",
                      choices=groups_names,
                      selected=groups_names[1])
  })
  observe({
    datadir <- r_selected_dataset()
    publications <- readRDS(glue::glue("{datadir}/publications.RDS"))

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
  observeEvent(input$pub_list_name,{
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
