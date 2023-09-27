# CREATED BY: FOUZIA FAROOQ
# DATE: 09/27/2023

# CODE GENERATOR: 

library(readr)
library(dplyr)
# do the select to get

df <- read.csv('data_out/mat_anemia.csv')
outfile <- 'text_to_paste.txt'
# Clear the file
cat() %>% write_lines(outfile, append = FALSE)


varnames <- df %>% select(MOMID, SITE, starts_with(c("M08_CBC_", "M06_HB_"))) %>% names()
 
for (var in varnames) {
  var_prefix <- substring(var, 1, 5)
  c(paste0('"Missing ', var, ' = NA, n (%)" ='),
    paste0('    paste0(sum(is.na(', var,')),'),
    '" (",',
    paste0('         format(round(sum(is.na(', var, '))/n()*100, 2),nsmall=2, digits=3),'),
    '         ")")) %>%') %>%
    write_lines(outfile, append = TRUE)
}
