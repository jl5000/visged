

node_label <- function(gedcom, xref) {
  
  #I1[<b>Joe Billy Bloggs</b><br>b. 1980<br>Somewhere<br>d. 2100<br>Somewhere else]
  if (tidyged::is_indi(gedcom, xref)) {
    
    paste0(xref, "(<b>", tidyged::describe_indi(gedcom, xref, TRUE), "</b>)") %>% 
      stringr::str_replace_all("@", "")
    
  } else { #family group
    
    xref %>% 
      stringr::str_replace_all("@", "")
    
  }
  
  
  # 
  # if(dob == "" & pob == "") {
  #   birth <- ""
  # } else {
  #   birth <- dob
  #   birth <- ifelse(birth == "", pob, paste0(birth, "<br>", pob))
  # }
  # if(dod == "" & pod == "") {
  #   death <- ""
  # } else {
  #   death <- dod
  #   death <- ifelse(death == "", pod, paste0(death, "<br>", pod))
  # }
  # 
  # paste0(stringr::str_replace_all(xref, "@", ""),
  #        "(",
  #        "<b>",full_name,"</b>",
  #        "<br>",
  #        paste0("b. ", birth),
  #        "<br>",
  #        paste0("d. ", death),
  #        ")")
  
}



ascendency_chart <- function(gedcom,
                          individual = character(),
                          include_siblings = FALSE) {
  
  xrefs <- tidyged::get_ancestors(gedcom, individual, 
                                  include_individual = TRUE, 
                                  include_siblings = include_siblings,
                                  include_families = TRUE, 
                                  include_supp_records = FALSE)
  
  
  # get the parents / famc of all these records
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
  
  DiagrammeR::mermaid(paste0("graph TB;", links))

}

