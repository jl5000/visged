# Stop the NOTES from R CMD CHECK when creating columns with mutate()
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("level", "record", "tag", "value", ".", "to", "from",
                           "ADR1", "ADR2", "ADR3", "AGE", "CITY", "content", "CTRY", "DATE", 
                           "description", "end", "fact_type", "id", "second_line", "STAE", "start", 
                           "title", "TYPE"))


