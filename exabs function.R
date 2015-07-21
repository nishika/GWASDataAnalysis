
exabs = function (url){
  
  library(rvest)
  library(rvest)
  library(XML)
  library(xml2)
  library(stringr)
  
  input <- read_html(url) %>%
    html_children() %>%        ##this line downward is for text clean up. 
    html_children() %>%
    html_children() %>%
    
    html_text() %>%
    
    str_replace_all("\t"," ") %>%
    str_replace_all("\n", " ")
 
}
