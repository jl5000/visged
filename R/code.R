

#' Create node labels for pedigree and descendancy charts
#' 
#' This function creates brief summaries of individuals and relationships formatted
#' for the DiagrammeR::mermaid() function.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the Individual or Family Group record.
#'
#' @return A string describing a node to use in the DiagrammeR::mermaid() function.
#' @tests
#' expect_equal(node_label(tidyged::sample555, "@I1@"),
#' "I1(<b>Robert Eugene Williams</b><br>b. 2 Oct 1822<br>Weston, Madison, Connecticut, United States of America<br>d. 14 Apr 1905<br>Stamford, Fairfield, Connecticut, United States of America)")
#' expect_equal(node_label(tidyged::sample555, "@I3@"),
#' "I3(<b>Joe Williams</b><br>b. 11 Jun 1861<br>Idaho Falls, Bonneville, Idaho, United States of America<br> - Still living)")
#' expect_equal(node_label(tidyged::sample555, "@F1@"),
#' "F1(m. Dec 1859<br>Rapid City, Pennington, South Dakota, United States of America)")
node_label <- function(gedcom, xref) {
  
  #I1(<b>Joe Billy Bloggs</b><br>b. 1980<br>Somewhere<br>d. 2100<br>Somewhere else)
  if (tidyged::is_indi(gedcom, xref)) {
    
    alive <- nrow(dplyr::filter(gedcom, record == xref, level == 1, tag == "DEAT")) == 0
    
    dob <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", 2, "BIRT") %>% 
      stringr::str_to_title()
    pob <- tidyged.internals::gedcom_value(gedcom, xref, "PLAC", 2, "BIRT")
    dod <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", 2, "DEAT") %>% 
      stringr::str_to_title()
    pod <- tidyged.internals::gedcom_value(gedcom, xref, "PLAC", 2, "DEAT")
    
    birth <- ifelse(dob == "" | pob == "", paste0(dob, pob), paste0(dob, "<br>", pob))
    death <- ifelse(dod == "" | pod == "", paste0(dod, pod), paste0(dod, "<br>", pod))
    
    paste0(xref, 
           "(",
           "<b>", tidyged::describe_indi(gedcom, xref, TRUE), "</b>", "<br>",
           "b. ", birth, "<br>",
           ifelse(alive, " - Still living", paste0("d. ", death)),
           ")") %>% 
      stringr::str_replace_all("@", "")
    
  } else { #family group
    
    married <- nrow(dplyr::filter(gedcom, record == xref, tag == "MARR")) > 0
    
    dom <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", 2, "MARR") %>% 
      stringr::str_to_title()
    pom <- tidyged.internals::gedcom_value(gedcom, xref, "PLAC", 2, "MARR")
    
    marr <- ifelse(dom == "" | pom == "", paste0(dom, pom), paste0(dom, "<br>", pom))
    
    paste0(xref, 
           "(",
           ifelse(married, paste0("m. ", marr), "Relationship"),
           ")") %>% 
      stringr::str_replace_all("@", "")
    
  }
  
}

#' Create node styles for pedigree and descendancy charts
#' 
#' This function creates node styles formatted for the DiagrammeR::mermaid() function.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the Individual or Family Group record.
#'
#' @return A string describing a node style to use in the DiagrammeR::mermaid() function.
#' @tests
#' expect_equal(node_style(tidyged::sample555, "@F1@"),
#' "style F1 fill:lightgrey, stroke:black")
#' expect_equal(node_style(tidyged::sample555, "@I1@"),
#' "style I1 fill:lightblue, stroke:black")
node_style <- function(gedcom, xref) {
  
  if (tidyged::is_indi(gedcom, xref)) {
    
    gender <- tidyged.internals::gedcom_value(gedcom, xref, "SEX", 1)
    
    style <- paste0("style ", xref, " fill:", ifelse(gender == "M", "lightblue", "pink"), ", stroke:black")
    
  } else {
    
    style <- paste0("style ", xref, " fill:lightgrey, stroke:black")
  }
  
  stringr::str_replace_all(style, "@", "")
}


#' Generate a pedigree chart
#'
#' @param gedcom A tidyged object.
#' @param individual An xref or character string identifying an Individual record.
#'
#' @return A pedigree chart showing the ancestors of the individual.
#' @export
pedigree_chart <- function(gedcom,
                          individual = character()) {
  
  xrefs <- tidyged::get_ancestors(gedcom, individual, 
                                  include_individual = TRUE, 
                                  include_siblings = FALSE,
                                  include_families = TRUE, 
                                  include_supp_records = FALSE)
  
  
  # get the parents / spouses of all these records
  get_spouses <- function(gedcom, fams) {
    dplyr::filter(gedcom, record == fams, tag %in% c("HUSB","WIFE"))$value
  }
  
  links <- tibble::tibble(to = xrefs) %>% 
    dplyr::mutate(from = purrr::map_if(to, ~ tidyged::is_indi(gedcom, .x),
                                               ~  tidyged::get_families_as_child(gedcom, .x),
                                               .else = ~  get_spouses(gedcom, .x))) %>% 
    tidyr::unnest(from) %>% 
    dplyr::filter(from != "") %>% 
    dplyr::mutate(from = purrr::map_chr(from, node_label, gedcom = gedcom)) %>% 
    dplyr::mutate(to = purrr::map_chr(to, node_label, gedcom = gedcom)) %>% 
    dplyr::mutate(links = paste0(from,"-->",to)) %>% 
    dplyr::pull(links) %>% 
    paste(collapse = "; ")
  
  styles <- purrr::map_chr(xrefs, node_style, gedcom = gedcom) %>% 
    paste(collapse = "; ")

  DiagrammeR::mermaid(paste0("graph TB;", links, ";", styles))

}

#' Generate a descendancy chart
#'
#' @param gedcom A tidyged object.
#' @param individual An xref or character string identifying an Individual record.
#'
#' @return A descendancy chart showing the descendants of the individual.
#' @export
descendancy_chart <- function(gedcom,
                           individual = character()) {
  
  xrefs <- tidyged::get_descendants(gedcom, individual, 
                                  include_individual = TRUE, 
                                  include_spouses = FALSE,
                                  include_families = TRUE, 
                                  include_supp_records = FALSE)
  
  # get the marriages / children of all these records
  get_children <- function(gedcom, famc) {
    dplyr::filter(gedcom, record == famc, tag == "CHIL")$value
  }
  
  links <- tibble::tibble(from = xrefs) %>% 
    dplyr::mutate(to = purrr::map_if(from, ~ tidyged::is_indi(gedcom, .x),
                                       ~  tidyged::get_families_as_spouse(gedcom, .x),
                                       .else = ~ get_children(gedcom, .x))) %>% 
    tidyr::unnest(to) %>% 
    #dplyr::filter(from != "") %>% 
    dplyr::mutate(from = purrr::map_chr(from, node_label, gedcom = gedcom)) %>% 
    dplyr::mutate(to = purrr::map_chr(to, node_label, gedcom = gedcom)) %>% 
    dplyr::mutate(links = paste0(from,"-->",to)) %>% 
    dplyr::pull(links) %>% 
    paste(collapse = "; ")

  styles <- purrr::map_chr(xrefs, node_style, gedcom = gedcom) %>% 
    paste(collapse = "; ")
  
  DiagrammeR::mermaid(paste0("graph TB;", links, ";", styles))
  
}
