

#' Construct a fact timeline for an individual
#' 
#' @details Only facts with associated dates are plotted.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the individual.
#'
#' @return A timevis object.
#' @export
timevis_indi <- function(gedcom, xref) {

  unique_missing_str <- "&&GLFYSKK£"
    
  facts <- gedcom %>% 
    tidyged::fact_summary_indi(xref) 
  
  is_alive <- sum(facts$fact_type == "Death") == 0
  dob <- dplyr::filter(facts, fact_type == "Birth")$DATE[1] 
  dod_or_today <- ifelse(is_alive, 
                         as.character(Sys.Date()), 
                         dplyr::filter(facts, fact_type == "Death")$DATE[1])
  
  facts %>% 
    dplyr::filter(!is.na(DATE)) %>% 
    # sort out dates
    dplyr::mutate(qualifier = ifelse(stringr::str_detect(DATE, "BET|BEF|AFT"), "uncertain", "certain"),
                  AGE = ifelse(is.na(AGE), AGE, paste("Age:", AGE))) %>% 
    tidyr::separate(DATE, into = c("start", "end"), sep = "AND|TO", remove = FALSE, fill = "right") %>%
    dplyr::mutate(dplyr::across(c(start, end), 
                                ~ stringr::str_extract(., 
                                                       tidyged.internals::reg_date(only=FALSE)) %>% 
                                  unlist())) %>% 
    dplyr::mutate(start = purrr::map_chr(start, ~as.character(tidyged.internals::parse_gedcom_date(.x)))) %>%
    dplyr::mutate(end = purrr::map_chr(end, ~ifelse(is.na(.x),
                                                    NA_character_,
                                                    as.character(tidyged.internals::parse_gedcom_date(.x))))) %>% 
    dplyr::mutate(end = ifelse(stringr::str_detect(DATE, "^TO|^BEF"), start, end), # move start to end
                  start = ifelse(stringr::str_detect(DATE, "^TO|^BEF"), dob, start),
                  end = ifelse(is.na(end) & stringr::str_detect(DATE, "^FROM|^AFT"), 
                               dod_or_today, end)) %>%
    # Populate box text
    dplyr::rename(content = fact_type) %>% 
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
                                                 content %in% c("Caste","Education","Physical description",
                                                                "National ID number","Nationality",
                                                                "Number of children","Number of relationships",
                                                                "Occupation","Property","Religion","Nobility title") ~ 
                                                   description,
                                                 TRUE ~ TYPE)) %>%
    dplyr::mutate(content = paste0("<b>", content, "</b>")) %>% 
    dplyr::mutate(content = ifelse(!is.na(second_line), paste0(content, "<br>", second_line),content)) %>%
    # Populate hover text
    dplyr::mutate(dplyr::across(c(DATE,AGE,description,TYPE,ADR1,ADR2,ADR3,CITY,STAE,CTRY), 
                                ~ifelse(is.na(.),unique_missing_str,.))) %>% 
    dplyr::mutate(title = paste(DATE,AGE,description,TYPE,ADR1,ADR2,ADR3,CITY,STAE,CTRY,sep="\n")) %>% 
    dplyr::mutate(title = stringr::str_remove_all(title,paste0("\n",unique_missing_str))) %>%
    dplyr::mutate(title = stringr::str_replace_all(title, "\n{2,10}", "\n")) %>%
    timevis::timevis()

}


