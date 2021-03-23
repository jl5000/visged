

node_label <- function(gedcom, xref) {
  
  #I1(<b>Joe Billy Bloggs</b><br>b. 1980<br>Somewhere<br>d. 2100<br>Somewhere else)
  if (tidyged::is_indi(gedcom, xref)) {
    
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
           "d. ", death,
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

node_style <- function(gedcom, xref) {
  
  if (tidyged::is_indi(gedcom, xref)) {
    
    gender <- tidyged.internals::gedcom_value(gedcom, xref, "SEX", 1)
    
    style <- paste0("style ", xref, " fill:", ifelse(gender == "M", "lightblue", "pink"), ", stroke:black")
    
  } else {
    
    style <- paste0("style ", xref, " fill:lightgrey, stroke:black")
  }
  
  stringr::str_replace_all(style, "@", "")
}


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