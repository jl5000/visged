
timevis_data_indi <- function(gedcom, xref) {
  
  unique_missing_str <- "&&GLFYSKK"
  xref_indi <- xref # used at the end
  
  facts <- tidyged::fact_summary_indi(gedcom, xref) %>% 
    dplyr::mutate(xref = xref)
  
  facts_fams <- get_facts_fams(gedcom, xref)
  
  is_alive <- sum(facts$fact_type == "Death") == 0
  dob <- dplyr::filter(facts, fact_type == "Birth")$DATE[1]
  dod_or_today <- ifelse(is_alive,                     
                         as.character(Sys.Date()), 
                         dplyr::filter(facts, fact_type == "Death")$DATE[1])
  
  facts %>% 
    dplyr::bind_rows(facts_fams) %>%
    dplyr::filter(!is.na(DATE)) %>% 
    dplyr::mutate(xref_names = purrr::map_chr(xref, tidyged::describe_indi,
                                              gedcom=gedcom, name_only = TRUE)) %>% 
    # sort out dates
    dplyr::mutate(style = ifelse(stringr::str_detect(DATE, "BET|BEF|AFT|ABT|CAL|EST"), "opacity: 0.5;", NA_character_),
                  AGE = ifelse(is.na(AGE), AGE, paste("Age:", AGE)), 
                  CAUS = ifelse(is.na(CAUS), CAUS, paste("Cause:", CAUS)),
                  AGNC = ifelse(is.na(AGNC), AGNC, paste("With:", AGNC)),
                  LATI = ifelse(is.na(LATI), LATI, paste("Latitude:", LATI)),
                  LONG = ifelse(is.na(LONG), LONG, paste("Longitude:", LONG))) %>% 
    tidyr::separate(DATE, into = c("start", "end"), sep = "AND|TO", remove = FALSE, fill = "right") %>%
    dplyr::mutate(dplyr::across(c(start, end), 
                                ~ stringr::str_extract(., 
                                                       tidyged.internals::reg_date(only=FALSE)) %>% 
                                  unlist())) %>% 
    dplyr::mutate(start = purrr::map_chr(start, ~as.character(tidyged.internals::parse_gedcom_date(.x)))) %>%
    dplyr::mutate(end = purrr::map_chr(end, ~as.character(tidyged.internals::parse_gedcom_date(.x)))) %>% 
    dplyr::mutate(end = ifelse(stringr::str_detect(DATE, "^TO|^BEF"), start, end), # move start to end
                  start = ifelse(stringr::str_detect(DATE, "^TO|^BEF"), dob, start),
                  end = ifelse(is.na(end) & stringr::str_detect(DATE, "^FROM|^AFT"), 
                               dod_or_today, end)) %>%
    # Populate box text
    dplyr::mutate(content = fact_type) %>% 
    dplyr::mutate(content = ifelse(stringr::str_detect(content, "^Other"), description, content),
                  description = ifelse(content == description, "", description)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(is.na(.),NA_character_,.))) %>% # fix for case_when
    dplyr::mutate(second_line = dplyr::case_when(content %in% c("Residence","Birth","Death","Census",
                                                                "Adult christening","Christening","Baptism",
                                                                "Bar-mitzvah","Bas-mitzvah","Burial",
                                                                "Confirmation","Cremation","First communion",
                                                                "Emigration","Immigration","Naturalization",
                                                                "Graduation") ~ 
                                                   dplyr::coalesce(PLAC,CITY,STAE,CTRY),
                                                 content %in% c("Caste","Academic achievement","Physical description",
                                                                "National ID number","Nationality",
                                                                "Number of children","Number of relationships",
                                                                "Occupation","Property","Religion","Nobility title") ~ 
                                                   description,
                                                 content %in% c("Annulment","Divorce","Divorce filed","Engagement", 
                                                                "Marriage banns","Marriage contract","Marriage license", 
                                                                "Marriage settlement","Relationship") ~
                                                   xref_names,
                                                 TRUE ~ TYPE)) %>%
    dplyr::mutate(content = ifelse(content == "Relationship" & !is.na(TYPE), 
                                   paste0(content, " (", TYPE, ")"), content)) %>% 
    dplyr::mutate(content = paste0("<b>", content, "</b>")) %>% 
    dplyr::mutate(content = ifelse(!is.na(second_line), paste0(content, "<br>", second_line),content)) %>%
    # Populate hover text
    dplyr::mutate(description = ifelse(description == "Y", NA_character_, description)) %>% 
    dplyr::mutate(dplyr::across(c(DATE,TYPE,description,AGE,CAUS,AGNC,ADR1,ADR2,ADR3,CITY,STAE,CTRY,LATI,LONG),
                                ~ifelse(is.na(.),unique_missing_str,.))) %>% 
    dplyr::mutate(title = paste(DATE,TYPE,description,AGE,CAUS,AGNC,ADR1,ADR2,ADR3,CITY,STAE,CTRY,LATI,LONG,sep="\n")) %>% 
    dplyr::mutate(title = stringr::str_remove_all(title,paste0("\n",unique_missing_str))) %>%
    dplyr::mutate(title = stringr::str_replace_all(title, "\n{2,10}", "\n")) %>%
    # appearance
    dplyr::mutate(type = ifelse(is.na(end), "point", "range")) %>% 
    # change all xrefs to xref of this individual (family ones will be for spouse)
    dplyr::mutate(xref = xref_indi)
  
}


get_facts_fams <- function(gedcom, xref) {
  
  fams <- tidyged::get_families_as_spouse(gedcom, xref) %>% 
    purrr::set_names(.)
  
  if(length(fams) == 0) return(tibble::tibble())
  
  facts_fams <- purrr::map_dfr(fams, tidyged::fact_summary_famg, gedcom = gedcom, .id = "xref_fams")
  
  if(nrow(facts_fams) == 0) return(tibble::tibble())
  
  facts_fams %>% 
    dplyr::mutate(role = purrr::map_chr(xref_fams,
                                        ~ dplyr::filter(gedcom, record == .x, value == xref)$tag), #role of this indi
                  AGE = ifelse(role == "HUSB", HUSB.AGE, WIFE.AGE), #age of this indi
                  xref = purrr::map2_chr(role, xref_fams, #xref of spouse
                                         ~ifelse(.x == "HUSB",
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
