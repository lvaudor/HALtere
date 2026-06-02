#' Builds network of co-publications by group (defined by data_groups, which can correspond to either people or labs)
#' @param data_crossed data_crossed (by author or affiliation)
#' @param data_groups data_authors
#' @param number_of_nodes the number of nodes
#' @return a graph
#' @export
#' @examples
#' data=extract_publications("BIOEENVIS", nmax=200)
#' data_ref_authors=tidy_ref_authors(data)
#'
#' data_groups=tidy_groups(data_ref_authors,type="people")
#' data_crossed=HALtere::cross(data_groups)
#' graph=build_network(data_crossed,data_groups,number_of_nodes=200)
#'
#' data_groups=tidy_groups(data_ref_authors,type="labs")
#' data_crossed=HALtere::cross(data_groups)
#' graph=build_network(data_crossed,data_groups,number_of_nodes=200)
#'
#' plot_network(graph, colorvar="name")
build_network=function(data_crossed,
                       data_groups,
                       number_of_nodes=60
                       ){
  # Combien de refs au total (dependant de type de doc et année)
  data_summary=data_groups %>%
    dplyr::group_by(across(-c(docType_s,producedDateY_i,nrefs))) %>%
    dplyr::summarise(nrefs=sum(nrefs),.groups="drop") %>%
    dplyr::group_by(name) %>%
    dplyr::top_n(n=1,nrefs) %>%
    dplyr::ungroup()

  # Combien de liens entre chaque paire de noeuds
  data=data_crossed %>%
    dplyr::group_by(val1,val2) %>%
    dplyr::summarise(nlinks=sum(nlinks),
            .groups="drop")
  # nrefs min to be included as node
  keep_nodes=data_summary %>%
    dplyr::arrange(desc(nrefs)) %>%
    unique() %>%
    dplyr::pull(name) %>%
    .[1:number_of_nodes]
  # filter data to keep only nodes with enough references
  data_filtered=data %>%
    dplyr::filter(val1 %in% keep_nodes,
                  val2 %in% keep_nodes)
  g=tidygraph::as_tbl_graph(data_filtered)
  layout <- ggraph::create_layout(g, layout = "fr")
  nodes <- tibble::as_tibble(layout)
  edges <- tibble::as_tibble(g, "edges")
  edges <- tibble::tibble(
    from=edges$from,
    to=edges$to,
    xfrom=nodes$x[edges$from],
    xto=nodes$x[edges$to],
    yfrom=nodes$y[edges$from],
    yto=nodes$y[edges$to],
    namefrom=nodes$name[edges$from],
    nameto=nodes$name[edges$to],
    nlinks=edges$nlinks) %>%
    dplyr::left_join(data_summary,by=c("namefrom"="name"),relationship = "many-to-many") %>%
    dplyr::mutate(hover=glue::glue("{namefrom}-\n{nameto}:\n{nlinks} links")) %>%
    dplyr::mutate(width=round(dplyr::percent_rank(nlinks)*10+0.5)) %>%
    dplyr::mutate(line_color= "rgb(200,200,200)")

  nodes=nodes %>%
    dplyr::mutate(betweenness=igraph::betweenness(g))
  nodes= nodes %>%
    dplyr::left_join(data_summary,
                     by=c("name"))
  return(list(nodes=nodes,edges=edges))
}


#' Plots network of co-publications by group (defined by data_groups, which can correspond to either people or labs
#' @param data a tibble produced with the cross() function
#' @param number_of_nodes the number of nodes to display
#' @param number_of_names the number of names to display,
#' @param margin the number of standard deviations to keep nodes
#' @param colorvar the variable to take into account to define node color
#' @param sizevar the variable to take into account to define node size
#' @param colors the colors to interpolate to define palette
#' @return a tibble
#' @export
#' @examples
#' dat=extract_publications("BIOEENVIS", nmax=200)
#' data_ref_authors=tidy_ref_authors(dat)
#'
#' data_groups=tidy_groups(data_ref_authors,type="people")
#' data_crossed=HALtere::cross(data_groups, data_ref_authors)
#' graph=build_network(data_crossed,data_groups,number_of_nodes=200)
#'
#' data_groups=tidy_groups(data_ref_authors,type="labs")
#' data_crossed=HALtere::cross(data_groups, data_ref_authors)
#' graph=build_network(data_crossed,data_groups,number_of_nodes=200)
#'
#' plot_network(graph, colorvar="name")
plot_network=function(graph,
                      number_of_names=30,
                      namevar="name",
                      colorvar="name",
                      sizevar="nrefs",
                      colors=c("red", "yellow", "blue")){
  nodes=graph$nodes
  edges=graph$edges
  colorvar=rlang::sym(colorvar)
  sizevar=rlang::sym(sizevar)
  namevar=rlang::sym(namevar)
  palROB <- grDevices::colorRampPalette(colors)
  keep_names=nodes %>%
    dplyr::arrange(desc(betweenness)) %>%
    dplyr::pull(name) %>%
    unique() %>%
    .[1:number_of_names]
  fmatch=function(vec){match(vec, sort(unique(vec)))}
  nodes=nodes %>%
    dplyr::mutate(name_to_display=dplyr::case_when(name %in% keep_names ~!!namevar,
                                                   TRUE~"")) %>%
    dplyr::mutate(marker_color=fmatch(!!colorvar),
                  size=dplyr::percent_rank(!!sizevar)*10)
  if(all(c("affiliation","lemma") %in% colnames(nodes))){
    nodes=nodes %>%
      dplyr::mutate(hover=glue::glue("Name: {name}
                                      Specificity: {lemma}
                                      Affiliation: {affiliation}
                                      Number of references: {nrefs}"))
  }else{
    nodes=nodes %>%
      dplyr::mutate(hover=glue::glue("Name: {name}
                                      Number of references: {nrefs}"))
  }
  ncols=max(nodes$marker_color)
  cols=palROB(ncols)
  # remove nodes that are too marginal
  nodes_to_display=nodes %>%
    dplyr::mutate(marker_color=cols[nodes$marker_color])
  # Créer le graphique interactif avec plotly
  fig <- plotly::plot_ly() %>%
    # Ajouter les arêtes
    plotly::add_segments(
      data=edges,
      x = ~xfrom,
      y = ~yfrom,
      xend = ~xto,
      yend = ~yto,
      line = list(
        color = ~line_color,  # Utiliser la couleur dynamique
        width = ~width        # Largeur dynamique
      )
    ) %>%
    # Ajouter les nœuds avec info en hover
    plotly::add_trace(
      x = ~nodes_to_display$x,
      y = ~nodes_to_display$y,
      type = 'scatter',
      mode = 'markers+text',
      text= ~nodes_to_display$name_to_display,
      hovertext= ~nodes_to_display$hover,
      marker = list(
        size = ~nodes_to_display$size,
        color = ~nodes_to_display$marker_color,
        line = list(width = 0)  # Pas de bordure
      ),
      textfont = list(
        color = 'black',
        size = 15
      ),
      textposition = 'center',
      showlegend=FALSE
    ) %>%
    plotly::layout(
      xaxis = list(
        title = "",         # Pas de titre pour l'axe x
        showticklabels = FALSE,  # Masquer les étiquettes de l'axe x
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "",         # Pas de titre pour l'axe x
        showticklabels = FALSE,  # Masquer les étiquettes de l'axe x
        showgrid = FALSE,
        zeroline = FALSE
      )
    )
  return(fig)
}
