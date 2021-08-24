
timevis_data_indi <- function(gedcom, xref) {
  
  xref_indi <- xref
  
  facts <- tidyged::df_indi_facts(gedcom, xref) %>% 
    dplyr::mutate(xref = xref_indi)
  
  facts_fams <- get_facts_fams(gedcom, xref)
  
  facts %>% 
    dplyr::bind_rows(facts_fams) %>%
    dplyr::filter(!is.na(DATE)) %>%
    dplyr::filter(!stringr::str_detect(DATE, tidyged.internals::reg_custom_value())) %>% 
    create_box_dates() %>% 
    create_box_text(gedcom) %>%
    create_hover_text() %>%
    create_box_appearance() %>% 
    # change all xrefs to xref of this individual (family ones will be for spouse)
    dplyr::mutate(xref = xref_indi)
  
}

create_box_dates <- function(facts) {
  
  facts %>% 
    tidyr::separate(DATE, into = c("start", "end"), sep = "AND|TO", remove = FALSE, fill = "right") %>%
    # start is guaranteed to have at least one date
    dplyr::mutate(dplyr::across(c(start, end), 
                                ~ stringr::str_extract(., 
                                                       tidyged.internals::reg_date(only=FALSE)) %>% 
                                  unlist())) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(start = tidyged.internals::parse_gedcom_date(start)) %>%
    dplyr::mutate(end = tidyged.internals::parse_gedcom_date(end)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(is_alive = sum(fact_type == "Death") == 0,
                  earliest = min(start, na.rm = TRUE),
                  latest = dplyr::if_else(is_alive, Sys.Date(), max(start, end, na.rm = TRUE))) %>% 
    dplyr::mutate(end = dplyr::if_else(stringr::str_detect(DATE, "^TO|^BEF") & earliest != start, 
                                       start, end), # move start to end
                  start = dplyr::if_else(stringr::str_detect(DATE, "^TO|^BEF") & earliest != start, 
                                         earliest, start),
                  end = dplyr::if_else(stringr::str_detect(DATE, "^FROM|^AFT") & latest != start & is.na(end),
                                       latest, end)) %>%
    dplyr::mutate(dplyr::across(c(start, end), as.character)) %>% 
    dplyr::select(-is_alive, -earliest, -latest)
  
}

create_box_text <- function(facts, gedcom) {
  
  facts_with_place <- c("Residence","Birth","Death","Census",
                        "Adult christening","Christening","Baptism",
                        "Bar-mitzvah","Bas-mitzvah","Burial",
                        "Confirmation","Cremation","First communion",
                        "Emigration","Immigration","Naturalization",
                        "Graduation")
  
  facts_needing_description <- c("Caste","Academic achievement","Physical description",
                                 "National ID number","Nationality",
                                 "Number of children","Number of relationships",
                                 "Occupation","Property","Religion","Nobility title")
  
  facts_with_spouse <- c("Annulment","Divorce","Divorce filed","Engagement", 
                         "Marriage banns","Marriage contract","Marriage license", 
                         "Marriage settlement","Relationship")
  
  facts %>% 
    dplyr::mutate(xref_names = purrr::map_chr(xref, tidyged::describe_indi,
                                              gedcom=gedcom, name_only = TRUE)) %>%
    dplyr::mutate(content = fact_type) %>% 
    dplyr::mutate(content = dplyr::if_else(stringr::str_detect(content, "^Other"), description, content),
                  description = dplyr::if_else(content == description, "", description)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::if_else(is.na(.),NA_character_,.))) %>% # fix for case_when
    dplyr::mutate(second_line = dplyr::case_when(content %in% facts_with_place ~ dplyr::coalesce(PLAC,CITY,STAE,CTRY),
                                                 content %in% facts_needing_description ~ description,
                                                 content %in% facts_with_spouse ~ xref_names,
                                                 TRUE ~ TYPE)) %>%
    dplyr::mutate(content = dplyr::if_else(content == "Relationship" & !is.na(TYPE), 
                                           paste0(content, " (", TYPE, ")"), content)) %>% 
    dplyr::mutate(content = paste0("<b>", content, "</b>")) %>% 
    dplyr::mutate(content = dplyr::if_else(!is.na(second_line), paste0(content, "<br>", second_line),content))
  
}


create_hover_text <- function(facts) {
  
  unique_missing_str <- "&&GLFYSKK"
  
  facts %>% 
    dplyr::mutate(AGE = dplyr::if_else(is.na(AGE), AGE, paste("Age:", AGE)), 
                  CAUS = dplyr::if_else(is.na(CAUS), CAUS, paste("Cause:", CAUS)),
                  AGNC = dplyr::if_else(is.na(AGNC), AGNC, paste("With:", AGNC)),
                  LATI = dplyr::if_else(is.na(LATI), LATI, paste("Latitude:", LATI)),
                  LONG = dplyr::if_else(is.na(LONG), LONG, paste("Longitude:", LONG))) %>% 
    dplyr::mutate(description = dplyr::if_else(description == "Y", NA_character_, description)) %>% 
    dplyr::mutate(dplyr::across(c(DATE,TYPE,description,AGE,CAUS,AGNC,ADR1,ADR2,ADR3,CITY,STAE,CTRY,LATI,LONG),
                                ~dplyr::if_else(is.na(.),unique_missing_str,.))) %>% 
    dplyr::mutate(title = paste(DATE,TYPE,description,AGE,CAUS,AGNC,ADR1,ADR2,ADR3,CITY,STAE,CTRY,LATI,LONG,sep="\n")) %>% 
    dplyr::mutate(title = stringr::str_remove_all(title,paste0("\n",unique_missing_str))) %>%
    dplyr::mutate(title = stringr::str_replace_all(title, "\n{2,10}", "\n"))
  
}

create_box_appearance <- function(facts) {
  
  facts %>% 
    dplyr::mutate(type = dplyr::if_else(is.na(end), "point", "range"),
                  style = dplyr::if_else(stringr::str_detect(DATE, "BET|BEF|AFT|ABT|CAL|EST"), 
                                         "opacity: 0.5;", 
                                         NA_character_))
}

get_facts_fams <- function(gedcom, xref) {
  
  fams <- tidyged::get_families_as_spouse(gedcom, xref) %>% 
    purrr::set_names(.)
  
  if(length(fams) == 0) return(tibble::tibble())
  
  facts_fams <- purrr::map_dfr(fams, tidyged::df_famg_facts, gedcom = gedcom, .id = "xref_fams")
  
  if(nrow(facts_fams) == 0) return(tibble::tibble())
  
  facts_fams %>% 
    dplyr::mutate(role = purrr::map_chr(xref_fams,
                                        ~ dplyr::filter(gedcom, record == .x, value == xref)$tag), #role of this indi
                  AGE = dplyr::if_else(role == "HUSB", HUSB.AGE, WIFE.AGE), #age of this indi
                  xref = purrr::map2_chr(role, xref_fams, #xref of spouse
                                         ~dplyr::if_else(.x == "HUSB",
                                                         tidyged.internals::gedcom_value(gedcom, .y, "WIFE", 1),
                                                         tidyged.internals::gedcom_value(gedcom, .y, "HUSB", 1)))) %>% 
    dplyr::select(-xref_fams, -role, -HUSB.AGE, -WIFE.AGE)
}

#' Construct a fact timeline for one or more individuals
#' 
#' @details Only facts with associated dates are plotted.
#'
#' @param gedcom A tidyged object.
#' @param xrefs The xrefs of the individuals to be plotted.
#'
#' @return A timevis object.
#' @export
timevis_indi <- function(gedcom, xrefs) {

  timevis_data <- unique(xrefs) %>% 
    purrr::set_names(.) %>%
    purrr::map_dfr(timevis_data_indi, gedcom = gedcom) %>%
    dplyr::mutate(group = purrr::map_chr(xref, tidyged::describe_indi, gedcom = gedcom, name_only = TRUE)) 
  
  timevis_groups <- tibble::tibble(id = unique(timevis_data$group), content = id, style = "font-weight: bold;")

  timevis::timevis(timevis_data, groups = timevis_groups)
  
}

#' Construct a fact timeline for a family group
#' 
#' @details Only facts with associated dates are plotted.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the family group to be plotted.
#'
#' @return A timevis object.
#' @export
timevis_famg <- function(gedcom, xref){
  
  c(dplyr::filter(gedcom, level == 1, record == xref, tag == "HUSB")$value,
    dplyr::filter(gedcom, level == 1, record == xref, tag == "WIFE")$value,
    dplyr::filter(gedcom, level == 1, record == xref, tag == "CHIL")$value) %>% 
    timevis_indi(gedcom = gedcom)
  
}
