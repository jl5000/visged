

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
#' "I1(\"<b>Robert Eugene Williams</b><br>b. 2 Oct 1822<br>Weston, Madison, Connecticut, United States of America<br>d. 14 Apr 1905<br>Stamford, Fairfield, Connecticut, United States of America\")")
#' expect_equal(node_label(tidyged::sample555, "@I3@"),
#' "I3(\"<b>Joe Williams</b><br>b. 11 Jun 1861<br>Idaho Falls, Bonneville, Idaho, United States of America<br> - Still living\")")
#' expect_equal(node_label(tidyged::sample555, "@F1@"),
#' "F1(\"<b>Married</b><br>Dec 1859<br>Rapid City, Pennington, South Dakota, United States of America\")")
node_label <- function(gedcom, xref) {
  quot = '"'
  
  #I1(<b>Joe Billy Bloggs</b><br>b. 1980<br>Somewhere<br>d. 2100<br>Somewhere else)
  if (tidyged::is_indi(gedcom, xref)) {
    
    alive <- nrow(dplyr::filter(gedcom, record == xref, level == 1, tag == "DEAT")) == 0
    
    dob <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", 2, "BIRT") |> 
      stringr::str_to_title()
    pob <- tidyged.internals::gedcom_value(gedcom, xref, "PLAC", 2, "BIRT")
    dod <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", 2, "DEAT") |> 
      stringr::str_to_title()
    pod <- tidyged.internals::gedcom_value(gedcom, xref, "PLAC", 2, "DEAT")
    
    if(dob == "" | pob == "") birth <- paste0(dob, pob) else birth <- paste0(dob, "<br>", pob)
    if(dod == "" | pod == "") death <- paste0(dod, pod) else death <- paste0(dod, "<br>", pod)
    if(alive) death_str <- " - Still living" else death_str <- paste0("d. ", death)
    
    paste0(xref, 
           "(", "\"",
           "<b>", tidyged::describe_indi(gedcom, xref, TRUE), "</b>", "<br>",
           "b. ", birth, "<br>",
           death_str,
           "\"", ")") |> 
      stringr::str_replace_all("@", "")
    
  } else { #family group
    
    gedcom <- tidyged::insert_explicit_marr_types(gedcom, xref)
    
    eng <- nrow(dplyr::filter(gedcom, record == xref, tag == "ENGA")) > 0
    div <- nrow(dplyr::filter(gedcom, record == xref, tag %in% c("DIV","DIVF"))) > 0
    relship <- nrow(dplyr::filter(gedcom, record == xref, tag == "MARR")) > 0
    married <- nrow(dplyr::filter(gedcom, record == xref, tag == "TYPE", 
                                  value %in% c("marriage","civil","religious","common law"))) > 0
    
    dor <- por <- ""
    
    if(div){
      rel <- "Divorced"
      dor <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", 2, "DIV")
      if(dor == "") dor <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", 2, "DIVF")
      dor <- stringr::str_to_title(dor)
    } else if(married){
      rel <- "Married"
      marr_rows <- tidyged.internals::identify_section(gedcom, 1, "MARR", xrefs = xref)
      marr_secs <- gedcom |> 
        dplyr::slice(marr_rows) |> 
        dplyr::filter(tag %in% c("MARR","TYPE","DATE","PLAC")) |> 
        dplyr::mutate(marr = tag == "MARR",
                      marr_no = cumsum(marr))
      for(i in seq_len(max(marr_secs$marr_no))){
        sec <- dplyr::filter(marr_secs, marr_no == i)
        if(nrow(dplyr::filter(sec, tag == "TYPE", value %in% c("marriage","civil","religious","common law")))>0)
          break
      }
      dor <- tidyged.internals::gedcom_value(sec, xref, "DATE", 2, "MARR") |> 
        stringr::str_to_title()
      por <- tidyged.internals::gedcom_value(sec, xref, "PLAC", 2, "MARR")
    } else if(eng){
      rel <- "Engaged"
      dor <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", 2, "ENGA") |> 
        stringr::str_to_title()
    } else if (relship) {
      rel <- "Relationship"
      marr_rows <- tidyged.internals::identify_section(gedcom, 1, "MARR", xrefs = xref)
      marr_secs <- gedcom |> 
        dplyr::slice(marr_rows) |> 
        dplyr::filter(tag %in% c("MARR","TYPE","DATE","PLAC")) |> 
        dplyr::mutate(marr = tag == "MARR",
                      marr_no = cumsum(marr))
      for(i in seq_len(max(marr_secs$marr_no))){
        sec <- dplyr::filter(marr_secs, marr_no == i)
        if(nrow(dplyr::filter(sec, tag == "TYPE", value %in% c("marriage","civil","religious","common law")))==0)
          break
      }
      dor <- tidyged.internals::gedcom_value(sec, xref, "DATE", 2, "MARR") |> 
        stringr::str_to_title()
    } else {
      rel <- "Unknown"
    }
    
    if(dor == "" | por == "") details <- paste0(dor, por) else details <- paste0(dor, "<br>", por)
    
    paste0(xref, 
           "(", "\"",
           "<b>", rel, "</b>", "<br>",
           details,
           "\"", ")") |> 
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
    
    style <- paste0("style ", xref, " fill:", dplyr::if_else(gender == "M", "lightblue", "pink"), ", stroke:black")
    
  } else {
    
    style <- paste0("style ", xref, " fill:lightgrey, stroke:black")
  }
  
  stringr::str_replace_all(style, "@", "")
}


#' Generate a family group chart
#'
#' @param gedcom A tidyged object.
#' @param family An xref identifying a Family group record.
#' @param birth_only Whether to only show biological children.
#'
#' @return A chart showing a family group.
#' @export
family_group_chart <- function(gedcom, family, birth_only = FALSE) {
  
  spou <- tidyged::get_famg_partners(gedcom, family)
  chil <- tidyged::get_famg_children(gedcom, family, birth_only)
  
  get_chil_pedi <- function(chil_xref, gedcom){
    
    famc_sec <- tidyged.internals::identify_section(gedcom, 1, "FAMC", family, xrefs= chil_xref)
    
    ged_famc <- dplyr::slice(gedcom, famc_sec)
    
    tidyged.internals::gedcom_value(ged_famc, 
                                    record_xref = chil_xref, 
                                    tag = "PEDI", 
                                    level = 2, 
                                    after_tag = "FAMC")
  }
  
  pedi <- purrr::map_chr(chil, get_chil_pedi, gedcom = gedcom)
  pedi[pedi == ""] <- "birth"

  links <- dplyr::bind_rows(
    tibble::tibble(from = spou, to = family, linktype = "-->"),
    tibble::tibble(from = family, to = chil, linktype = as.character(dplyr::if_else(pedi == "birth",
                                                                                    "-->",
                                                                                    paste0("-. ", pedi, " .->"))))
  ) |> 
    dplyr::mutate(from = purrr::map_chr(from, node_label, gedcom = gedcom)) |> 
    dplyr::mutate(to = purrr::map_chr(to, node_label, gedcom = gedcom)) |> 
    dplyr::mutate(links = paste0(from,linktype,to)) |> 
    dplyr::pull(links) |> 
    paste(collapse = "; ")
  
  styles <- purrr::map_chr(c(spou, chil, family), node_style, gedcom = gedcom) |> 
    paste(collapse = "; ")
  
  DiagrammeR::mermaid(paste0("graph TB;", links, ";", styles))
  
}
