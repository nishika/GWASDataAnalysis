
exabs = function (string){
  library(rvest)
  input <- read_html(string) %>%
    html_children() %>%
    html_children() %>%
    html_children() %>%
    
    html_text() %>%
    
    str_replace_all("\t"," ") %>%
    str_replace_all("\n", " ")
 
}
