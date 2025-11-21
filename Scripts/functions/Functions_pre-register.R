homogenize_months <- function(column) {
  # Crear un diccionario para traducir
  month_dict <- c(
    "januari" = "January",
    "februari" = "February",
    "maart" = "March",
    "april" = "April",
    "jan" = "January",
    "feb" = "February",
    "mar" = "March",
    "apr" = "April"
  )
  
  # Limpiar y traducir
  clean_column <- tolower(column) %>% 
    stringr::str_replace_all("[-/]", " ") %>% 
    stringr::str_replace_all("\\?", "") %>% 
    stringr::str_replace_all(" \\d{4}", "") %>% 
    stringr::str_squish()
  
  # Extraer meses usando el diccionario
  extracted_months <- purrr::map(clean_column, function(row) {
    found_months <- names(month_dict)[sapply(names(month_dict), function(month) grepl(month, row))]
    unique(month_dict[found_months])
  })
  
  # Crear una columna estandarizada con el primer mes detectado
  main_month <- sapply(extracted_months, function(x) if (length(x) > 0) x[1] else NA)
  
  list(main_month = main_month, all_months = extracted_months)
}
