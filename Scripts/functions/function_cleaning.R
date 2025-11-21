options(scipen=999)
library(dplyr)
library(dplyr)
library(ggplot2)
library(haven)
library(readr)
library(stringdist)
library(stringr)
library(tools)
# School Name Homogenization
library(stringr)
library(dplyr)

# Function to identify duplicates and flag rows to drop
flag_duplicates <- function(data) {
  data %>%
    # Create a group identifier for duplicates
    mutate(group_id = paste0(ipaddr, "_", substr(G01Q01, 1, 11))) %>%
    group_by(group_id) %>%
    # Create a column 'to_drop' to flag rows based on the rules
    mutate(
      # Identify the row to keep (to_drop = 0) and others (to_drop = 1)
      to_keep = case_when(
        any(is.na(PTF10)) ~ ifelse(is.na(PTF10), 1, 0),
        TRUE ~ as.integer(startdate == min(startdate))
      ),
      # Ensure only one row is kept per group (arising from ties)
      to_keep = ifelse(row_number() == which.max(to_keep), 1, 0)
    ) %>%
    ungroup() %>%
    # Add 'to_drop' column (inverse of 'to_keep')
    mutate(to_drop = ifelse(to_keep == 1, 0, 1)) %>%
    select(-to_keep)
}

# Function to resolve duplicates based on the described rules
library(dplyr)

# Function to resolve duplicates based on the described rules
resolve_duplicates <- function(data) {
  data %>%
    # 1. Create group_id as before
    mutate(group_id = paste0(ipaddr, "_", substr(G01Q01, 1, 11))) %>%
    
    # 2. Arrange the data within each group.  This is crucial!
    group_by(group_id) %>%
    arrange(group_id, is.na(PTF10), startdate, .by_group = TRUE) %>%
    
    # 3. Take the first row in each group (now properly ordered)
    slice(1) %>% #Much easier after arrange
    
    # 4. Put the `startdate` value into Q1
    mutate(Q1 = startdate) %>%
    
    # 5. Cleanup
    ungroup() %>%      # Remove grouping
    select(-group_id)  # Remove group_id
}
# resolve_duplicates <- function(data) {
#   data %>%
#     # Create a group identifier for duplicates
#     mutate(group_id = paste0(ipaddr, "_", substr(G01Q01, 1, 11))) %>%
#     group_by(group_id) %>%
#     summarise(Q1_ = Q1[which.max(startdate)],  across(everything(), ~ if (n() == 1) .[1] else {# Keep all columns from the row based on the rules
#       if (any(is.na(PTF10))) {
#         row <- which(is.na(PTF10))[1]  # Row where PTF10 is NA
#       } else {
#         row <- which.min(startdate)  # Row with the earliest startdate
#       }
#       .[row]
#     }), .groups = "drop" )    %>%
#     mutate(Q1 =Q1_)%>% 
#     select(-group_id) %>% 
#     select(-Q1_) 
# }

 
number_by_group <- function(table_, column) {
  # Create counts table and convert it to a data frame
  counts <- table(as.character(table_[[column]]))
  
  # Create a data frame for ggplot, converting counts to data frame
  df <- as.data.frame(counts)
  colnames(df) <- c("group", "count")  # Rename columns to 'group' and 'count'
  
  group_names <- c("Control Group:\nTraditional Path (TP)", 
                   "Treatment:\nReduced TP + AI", #General Purpose
                   "Treatment:\nTailored AI in the \nBelgian Tax System")
  # Assign group names to the data frame
  df$group <- factor(df$group, levels = c("1", "2", "3"), labels = group_names)
  
  # ggplot bar plot with improved styling
  plot_ = ggplot(df, aes(x = group, y = count, fill = group)) +
    geom_bar(stat = "identity") +  # Use stat = "identity" to map y to the counts directly
    labs(
      title = "Number of Participants by Group",
      x = "Groups",
      y = "Number of Participants"
    ) +
    theme_minimal() +  # Use a minimal theme for a cleaner look
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for readability
      axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis title
      axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis title
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and style the title
      legend.position = "none" 
    ) +
    scale_fill_manual(values = c("steelblue", "lightcoral", "darkseagreen"))  # Custom colors
  
  print(df)  # Print the data frame to check the counts
  print(plot_)  # Print the plot
  return(plot_)  # Return the plot
}
fractions_by_group_print <- function(table_,
                                     column, 
                                     filename = "fractions") {  
  # Define file paths  
  text_filename <- paste0("Metadata/", filename, ".txt")  
  plot_filename <- paste0("Graph/", filename, ".png")  
  
  # Ensure directories exist  
  if (!dir.exists("Metadata")) dir.create("Metadata")  
  if (!dir.exists("Graph")) dir.create("Graph")  
  
  # Create counts table and convert it to a data frame  
  counts <- table(as.character(table_[[column]]))  
  counts_df <- as.data.frame(counts)  
  colnames(counts_df) <- c("group", "counts")  
  
  # Calculate the proportions  
  fractions <- prop.table(counts)  
  
  # Convert fractions to a data frame  
  df <- as.data.frame(fractions)  
  colnames(df) <- c("group", "fraction")  
  
  # Merge counts and fractions  
  df <- merge(df, counts_df, by = "group")  
  
  # Assign group names  
  group_names <- c("Control Group:\nTraditional Path (TP)",  
                   "Treatment:\nReduced TP + AI",  
                   "Treatment:\nTailored AI in the \nBelgian Tax System")  
  df$group <- factor(df$group, levels = c("1", "2", "3"), labels = group_names)  
  
  # Save data frame to text file  
  write.table(df, file = text_filename, row.names = FALSE, sep = "\t")  
  
  # Create ggplot bar plot  
  plot_ <- ggplot(df, aes(x = group, y = fraction, fill = group)) +  
    geom_bar(stat = "identity") +  
    labs(  
      title = "Fraction of Participants by Group",  
      x = "Groups",  
      y = "Fraction of Participants"  
    ) +  
    theme_minimal() +  
    theme(  
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  
      axis.title.x = element_text(size = 12, face = "bold"),  
      axis.title.y = element_text(size = 12, face = "bold"),  
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
      legend.position = "none"  
    ) +  
    scale_fill_manual(values = c("steelblue", "lightcoral", "darkseagreen"))  
  
  # Save plot as PNG  
  ggsave(filename = plot_filename, plot = plot_, width = 8, height = 6, dpi = 300)  
  
  print(df)  # Print the data frame  
  print(plot_)  # Print the plot  
  
  return(plot_)  # Return the plot  
}


fractions_by_group <- function(table_, column) { 
  # Create counts table and convert it to a data frame 
  counts <- table(as.character(table_[[column]])) 
  
  # Calculate the proportions 
  fractions <- prop.table(counts) 
  
  # Create a data frame for ggplot, converting fractions to data frame 
  df <- as.data.frame(fractions) 
  colnames(df) <- c("group", "fraction")  # Rename columns to 'group' and 'fraction'
  
  group_names <- c("Control Group:\nTraditional Path (TP)", 
                   "Treatment:\nReduced TP + AI", #General Purpose
                   "Treatment:\nTailored AI in the \nBelgian Tax System")
  
  # Assign group names to the data frame
  df$group <- factor(df$group, levels = c("1", "2", "3"), labels = group_names)
  
  # ggplot bar plot with improved styling
  plot_ = ggplot(df, aes(x = group, y = fraction, fill = group)) + 
    geom_bar(stat = "identity") +  # Use stat = "identity" to map y to the fractions directly
    labs(
      title = "Fraction of Participants by Group", 
      x = "Groups", 
      y = "Fraction of Participants"
    ) + 
    theme_minimal() +  # Use a minimal theme for a cleaner look
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for readability
      axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis title
      axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis title
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and style the title
      legend.position = "none"  # Hide legend
    ) + 
    scale_fill_manual(values = c("steelblue", "lightcoral", "darkseagreen"))  # Custom colors
  
  print(df)  # Print the data frame to check the fractions
  print(plot_)  # Print the plot
  return(plot_)  # Return the plot
}

translations_ls <- c(
  "Helemaal mee oneens" = 1,
  "Mee oneens" = 2,
  "Neutraal" = 3,
  "Mee eens" = 4,
  'Helemaal mee eens' = 5
)
translations <- c(
  "Jongen" = "Boy",
  "Meisje" = "Girl",
  "X"="",
  
  "Algemeen Secundair Onderwijs (ASO)"="General Secondary Education" ,
  "Technisch Secundair Onderwijs (TSO)"="Technical Secondary Education",
  "Beroepssecundair Onderwijs (BSO)"="Vocational Secondary Education",
  "Kunstsecundair Onderwijs (KSO)"="Secondary Education in the Arts",
  "Anders"="Other",
  
  "Mee oneens" = "Disagree",
  "Neutraal" = "Neutral",
  "Mee eens" = "Agree",
  "Helemaal mee oneens" = "Strongly Disagree",
  'Helemaal mee eens' = "Strongly Agree",
  
  "Hogeschool/universiteitsdiploma of hoger"  = "Higher education degree or university degree",
  "Diploma secundair onderwijs/middelbare school" = "Secondary education diploma/high school diploma",
  "Geen secundair onderwijs/middelbare school niet afgemaakt"  = "No secondary education/high school not completed", 
  "Ik weet het niet"="I don't know",
  "Geen van bovenstaande" = "None of the above",
  "Progressief belastingsysteem" = "Progressive tax system",
  "Proportioneel belastingsysteem (vlaktaks)" = "Proportional tax system (flat tax)",
  "Degressief belastingsysteem" = "Degressive tax system",
  "Beroepskosten" = "Professional expenses",
  "Belastingvrije som" =  "Tax-free allowance",
  "Het nationaal gemiddeld loon" = "The national average wage",
  "De nationale gemiddelde loon"="The national average wage",
  "Aantal kinderen ten laste" = "Number of dependents (children)",
  "Meer dan 80%" = "Over 80%",
  "70% of meer, maar minder dan 80%" = "70% to 79%",
  "60% of meer, maar minder dan 70%"      ="60% to 69%",            
  "50% of meer, maar minder dan 60%"= "50% to 59%",
  "Minder dan 50%"   = "Under 50%",
     'Frans'= 'French',
  'Nederlands'='Dutch',
  "In de gewone klas met mijn leraar economie" ="In the regular classroom with my economics teacher",
  "Thuis"   = "At home", 
  "In studie" = "In study",
  "In de gewone klas maar niet met mijn gebruikelijke leraar" = "In the regular class but not with my usual teacher"
  
)

# Function to translate responses
translate_responses <- function(responses, translations) {
  sapply(responses, function(x) {
    if (is.na(x)) {
      return(NA)  # Preserve NA values
    } else if (x %in% names(translations)) {
      return(translations[x])  # Translate if in dictionary
    } else {
      return(x)  # Keep original if no translation available
    }
  })
}


##### Full Name Homogenization (Pretest and Posttest) ####
homogenize_name <- function(name_vector) {
  name_vector <- toupper(name_vector)
  name_vector <- gsub("\\s+", "_", name_vector)  # Replace multiple spaces with underscores
  return(name_vector)
}

##### City Homogenization ####
library(stringr)
library(tools)

##### City Homogenization Function ####
homogenize_cities <- function(city_vector, address_vector = NULL) {
  
  # Create a city lookup table
  city_lookup <- c(
    "gent" = "Gent",
    "9000" = "Gent",
    "9050" = "Gentbrugge",
    "gentbrugge" = "Gentbrugge",
    "getnbrugge" = "Gentbrugge",
    "destelbergen" = "Destelbergen",
    "molenbeek" = "Molenbeek",
    "sint-jans-molenbeek" = "Molenbeek",
    "1080 sint-jans-molenbeek" = "Molenbeek",
    "molembeek" = "Molenbeek",
    "laken/brussel" = "Brussels",
    "laeken/ brussels" = "Brussels",
    "brussel" = "Brussels",
    "bruxelles" = "Brussels",
    "brussels" = "Brussels",
    "torhout" = "Torhout",
    "wevelgem" = "Wevelgem",
    "genk" = "Genk",
    "3600 genk" = "Genk",
    "3600" = "Genk",
    "3590" = "Diepenbeek",
    "ieper" = "Ypres",
    "antwerpen" = "Antwerp",
    "berchem" = "Berchem",
    "turnhout" = "Turnhout",
    "geel" = "Geel",
    "diksmuide" = "Diksmuide",
    "tienen" = "Tienen",
    "dworp" = "Dworp",
    "waregem" = "Waregem",
    "melle" = "Melle",
    "halle" = "Halle",
    "sint-niklaas" = "Sint-Niklaas",
    "sint niklaas" = "Sint-Niklaas",
    "9100" = "Sint-Niklaas",
    "willebroek" = "Willebroek",
    "kapelle-op-den-bos" = "Kapelle-op-den-Bos",
    "laakdal" = "Laakdal",
    "meerhout" = "Meerhout",
    "tessenderlo" = "Tessenderlo",
    "hasselt" = "Hasselt",
    "leuven" = "Leuven",
    "mechelen" = "Mechelen",
    "temse" = "Temse",
    "heist op den berg" = "Heist-op-den-Berg",
    "heist-op-den-berg" = "Heist-op-den-Berg",
    "2220" = "Heist-op-den-Berg",
    "wiekevorst" = "Wiekevorst",
    "ekeren" = "Ekeren",
    "kapellen" = "Kapellen",
    "merelbeke" = "Merelbeke",
    "mol" = "Mol",
    "gistel" = "Gistel",
    "beernem" = "Beernem",
    "brugge" = "Brugge",
    "arendonk" = "Arendonk",
    "evere" = "Evere",
    "lennik" = "Lennik",
    "1190" = "Brussels", # Assuming this is Brussels as it's a Brussels postcode
    "diepenbeek" = "Diepenbeek",
    "balen" = "Balen",
    "gentbrugge" = "Gentbrugge",
    "loommel"= "Lommel",
    "lommel" = "Lommel",
    "aarschot" = "Aarschot",
    "aalst" = "Aalst",
    "malle" = "Malle",
    "vorselaar" = "Vorselaar",
    "sint-kruis" = "Sint-Kruis",
    "bilzen" = "Bilzen",
    "tildonk" = "Tildonk",
    "londerzeel" = "Londerzeel",
    "ieper" = "Ieper",
    "oostende" = "Oostende",
    "zaventem" = "Zaventem",
    "lier" = "Lier",
    "heusden" = "Heusden",
    "brasschaat" = "Brasschaat",
    "lokeren" = "Lokeren",
    "lochristi" = "Lochristi",
    "avelgem" = "Avelgem",
    "merksem" = "Merksem",
    "kortrijk" = "Kortrijk",
    "duffel" = "Duffel",
    "maasmechelen" = "Maasmechelen",
    "sint-truiden" = "Sint-Truiden",
    "maldegem" = "Maldegem",
    "anderlecht" = "Anderlecht",
    "deurne" = "Deurne",
    "kuurne" = "Kuurne",
    "deinze" = "Deinze",
    "leopoldsburg" = "Leopoldsburg",
    "aalter" = "Aalter",
    "roeselare" = "Roeselare",
    "etterbeek" = "Etterbeek",
    "ronse" = "Ronse",
    "overijse" = "Overijse",
    "wijnegem" = "Wijnegem",
    "blankenberge" = "Blankenberge",
    "de panne" = "De Panne",
    "9050 gentbrugge" =  "Gentbrugge"   ,
    "1080" = "Molenbeek", 
    "1500 halle"   = "Halle" ,
    "sint-jans-molenbeek"="Molenbeek" ,
    "sint-jans molenbeek" ="Molenbeek" ,
    "2400 mol" = "Mol",
    "kapelle/op/den/bos" = "Kapelle-op-den-Bos",
    "kapelle op den bos" = "Kapelle-op-den-Bos",
    "kappelle op den bos" = "Kapelle-op-den-Bos",
    "kapellen op den bos" = "Kapelle-op-den-Bos",
    "kapelle-op-den-bod" = "Kapelle-op-den-Bos",
    "kapelle op den bos" = "Kapelle-op-den-Bos",
    "kappelle-op-den-bos" = "Kapelle-op-den-Bos",
    "sint-niklaas 9100" =  "Sint-Niklaas" ,
    "2180 ekeren" =  "Ekeren"   ,
    "1850 grimbergen" ="Grimbergen"  ,
    "ghent" ="Gent"   ,
    "lennick"  =   "Lennik" ,
    "molenbeek brussel"  = "Molenbeek" 
    
  )
  
  # Pre-processing step: Clean up inconsistencies
  
  # 1. Remove extra spaces within city names (e.g., "Kapelle - Op -Den - Bos")
  city_vector <- stringr::str_replace_all(city_vector, "\\s*-\\s*", "-") # Replace "-"-separated words surrounded by spaces with just "-"
  city_vector <- stringr::str_replace_all(city_vector, "\\s+", " ")  # Replace multiple spaces with single spaces
  city_vector <- trimws(city_vector) # Trim leading/trailing spaces
  
  # 2. Handle "Halle 1500" and "2400 Mol" by removing the postcode
  city_vector <- stringr::str_replace(city_vector, "^(\\w+)\\s+\\d+$", "\\1") #Remove post code from string start with string
  
  
  # Convert to lowercase
  city_vector_lower <- tolower(city_vector)
  
  # Homogenize using the lookup table
  homogenized_cities <- city_lookup[city_vector_lower]
  
  # Handle unmatched cities
  unmatched_indices <- is.na(homogenized_cities)
  
  #Attempt to extract city from address if provided and still unmatched
  if(!is.null(address_vector) && any(unmatched_indices)){
    extracted_cities <- extract_city_from_address(address_vector[unmatched_indices])
    homogenized_cities[unmatched_indices] <- extracted_cities
    unmatched_indices <- is.na(homogenized_cities) #Update unmatched indices
  }
  
  # If still unmatched, use the original (lowercase) city
  homogenized_cities[unmatched_indices] <- city_vector_lower[unmatched_indices]
  
  # Convert to Title Case
  homogenized_cities <- tools::toTitleCase(homogenized_cities)
  
  # Remove names from the vector
  return(unname(homogenized_cities))
}

##### Helper function to extract city from address ####
extract_city_from_address <- function(address_vector) {
  extracted_cities <- rep(NA_character_, length(address_vector)) #Initialize with NAs
  for (i in seq_along(address_vector)) {
    address <- address_vector[i]
    
    # Regular expression to find a city name after a comma or digits followed by space
    match <- stringr::str_extract(address, "(?:,|^|\\d+\\s+)(\\w+)$")
    
    if (!is.na(match)) {
      city <- stringr::str_trim(match)
      extracted_cities[i] <- tolower(city)
    }
  }
  return(extracted_cities)
}


#####  School Name Homogenization  ####

homogenize_school_name <- function(school_names) {
  
  # 1. Uppercase and Initial Cleaning
  cleaned_names <- school_names %>%
    toupper() %>%
    str_replace_all("[[:punct:]]", "") %>%  # Remove punctuation
    str_replace_all("\\s+", " ") %>%        # Condense multiple spaces
    str_trim()                             # Trim leading/trailing spaces
  
  # 2. Targeted Replacements (using regex for efficiency and accuracy)
  cleaned_names <- cleaned_names %>%
    str_replace_all("^GO\\s*!?\\s*4\\s*(CITY|C!TY)", "GO4CITY") %>%  #GO! 4 variations
    str_replace_all("^GO\\s*!", "GO ") %>%                         # GO! variations
    str_replace_all("IVG\\s?SCHOOL|IVG\\s?SCHOLEN|IVGSCHOLEN|IVGSCHOOL", "IVG-SCHOOL") %>%  # All IVG variations
    str_replace_all("SINT\\s?JOZEFS?COLLEGE\\s?(TORHOUT)?", "SINT-JOZEFSCOLLEGE TORHOUT") %>%  # Jozef variations
    str_replace_all("SINT\\s?PAULUSCOLLEGE\\s?(WEVELGEM)?", "SINT PAULUSCOLLEGE WEVELGEM") %>%  # Paulus variations
    str_replace_all("SINT\\s?JOZEFINSTITUUT\\s?(BOKRIJK)?", "SINT JOZEFINSTITUUT BOKRIJK") %>%   # Jozefinstituut variations
    str_replace_all("ATHENEUM\\s?GENTBRUGGE", "ATHENEUM GENTBRUGGE") %>% #Gentbrugge variations
    str_replace_all("^GO\\sATHENEUM", "GO ATHENEUM") %>%           # Standardize "GO ATHENEUM"
    str_replace_all("SINT REMBERT COLLEGE TORHOUT", "SINT-REMBERT COLLEGE TORHOUT") %>%   #consistent dash
    str_replace_all("KOGEKA","KOGEKA")      #KOGEKA
  
  # 3. Lookup Table (Optimized for speed – using a named vector)
  #This include every name in the schools vector and the names
  #In test vector
  lookup_table <- c(
    "ASO SPIJKKER" = "ASO Spijkker",
    "DAMIAANINSTITUUT" = "Damiaaninstituut",
    "SANCTA MARIA LEUVEN" = "Sancta Maria Leuven",
    "GO LYCEUM AALST" = "GO! Lyceum Aalst",
    "SINTPAULUSINSTITUUT" = "Sint-Paulusinstituut",
    "SINT JAN BERCHMANSCOLLEGE" = "Sint Jan Berchmanscollege",
    "CAMPUS FENIX" = "Campus Fenix",
    "KOGEKA SINT MARIA GEEL" = "KOGEKA Sint Maria Geel",
    "SINTPAULUSINSTITUUT HERZELE" = "Sint-Paulusinstituut Herzele",
    "SINTNORBERTUS ANTWERPEN" = "Sint-Norbertus Antwerpen",
    "IVGSCHOOL" = "IVG-school",
    "IVG"      ="IVG-school",     #More IVG
    "IVG SCHOOL" = "IVG-school",
    "IVGSCHOOL GENT" = "IVG-school",
    "KARDINAAL VAN ROEY INSTITUUT" = "Kardinaal Van Roey Instituut",
    "SINTJOZEFSCOLLEGE" = "Sint-Jozefscollege", #correct Jozef
    "SINTJOZEFSCOLLEGE TORHOUT"="Sint-Jozefscollege Torhout",
    "MIA" = "MIA",
    "CAMPUS HEMELVAART" = "Campus Hemelvaart",
    "GO ATHENEUM TIENEN" = "GO! Atheneum Tienen",
    "SINTBARBARACOLLEGE" = "Sint-Barbaracollege",
    "MORETUS" = "Moretus",
    "VRIJZE ISRAELITISCHE SCHOOL VOOR SECUNDAIR ONDERWIJS YAVNE" = "Vrijze Israëlitische School voor Secundair Onderwijs Yavne",
    "HEILIFGRAFINSTITUUT" = "Heilig-Grafinstituut",#correct
    "VRIJ HANDELS EN SPORTINSTITUUT" = "Vrij Handels- en Sportinstituut",
    "SINTGODELIEVECOLLEGE" = "Sint-Godelievecollege",
    "SECUNDAIRE HANDELSSCHOOL SINT LODEWIJK ANTWERPEN" = "Secundaire Handelsschool Sint Lodewijk Antwerpen",
    "SINT ANGELA TILDONK" = "Sint Angela Tildonk",
    "VIRGO SAPIENS" = "Virgo Sapiens",
    "HEILIGE FAMILIE IEPER" = "Heilige Familie Ieper",
    "SINTANDREASINSTITUUT" = "Sint-Andreasinstituut",
    "SIVIBU" = "SIVIBU",
    "ZAVO" = "Zavo",
    "SINTURSULA INSTITUUT" = "Sint-Ursula Instituut",
    "SINTJAN BERCHMANSCOLLEGE MOL" = "Sint-Jan Berchmanscollege Mol",
    "SINTANNACOLLEGE" = "Sint-Annacollege",
    "HEILIG HART EN COLLEGE" = "Heilig Hart en College",
    "GO AMAZE" = "GO! A-Maze",
    "HOGESCHOOL PXL DEP EDUCATION LERARENOPLEIDING" = "Hogeschool PXL - dep. education (lerarenopleiding)",
    "HEILIGHARTCOLLEGE" = "Heilig-Hartcollege",
    "SINTFRANCISCUSCOLLEGE" = "Sint-Franciscuscollege",
    "ONZELIEVEVROUWLYCEUM GENK" = "Onze-Lieve-Vrouwlyceum Genk",
    "MATER DEI BRASSCHAAT" = "Mater Dei Brasschaat",
    "CAMPUS SINTJAN BERCHMANS MOL" = "Campus Sint-Jan Berchmans Mol",
    "CAMPUS HAST" = "Campus Hast",
    "VLOT CAMPUS SINTLODEWIJK" = "VLOT campus Sint-Lodewijk",
    "VLOT CAMPUS SLC" = "VLOT campus Sint-Lodewijk",
    "EDUGOLO" = "Edugolo",
    "SINT PAULUSSCHOOL CAMPUS SJB" = "Sint Paulusschool campus SJB",
    "SINTLUDGARDISSCHOOL" = "Sint-Ludgardisschool",
    "HOGESCHOOL UCLL CAMPUS DIEPENBEEK" = "Hogeschool UCLL (Campus Diepenbeek)",
    "UC LEUVEN LIMBURG" = "UC Leuven Limburg",
    "VLOT VZW" = "VLOT vzw",
    "HEILIGE FAMILIE BERCHEM WILFAM" = "Heilige Familie Berchem (Wilfam)",
    "GO ATHENEUM MXM" = "GO! Atheneum MXM",
    "FUTURA SO" = "Futura SO",
    "VHSI" = "VHSI",
    "GULDENSPORENCOLLEGE KAAI" = "Guldensporencollege Kaai",
    "SINTNORBERTUSINSTITUUT" = "Sint-Norbertusinstituut",
    "GO ATHENEUM DIKSMUIDE" = "Go Atheneum Diksmuide",
    "X PLUS LOMMEL" = "X Plus Lommel",
    "TA BRASSCHAAT" = "TA Brasschaat",
    "FORUM DAVINCI" = "Forum DaVinci",
    "GO HANDELSSCHOOL AALST" = "GO! Handelsschool Aalst",
    "GO MAXWELL" = "GO! Maxwell",
    "GO HET KOMPAS" = "GO Het Kompas",
    "ATHENEUM COURTMANSLAAN" = "Atheneum Courtmanslaan",
    "GO ATHENEUM IEPER" = "GO! atheneum Ieper",
    "GO COOVI" = "GO! Coovi",
    "GO ATHENEUM SPRONK" = "GO! atheneum spronk",
    "COLLEGE IEPER" = "College Ieper",
    "SINTJOZEFINSTITUUT BOKRIJK" = "Sint-Jozefinstituut Bokrijk",
    "GO ATHENEUM GENTBRUGGE" = "GO! Atheneum Gentbrugge",
    "HASPO CENTRUM" = "Hasp-O Centrum",
    "GO ATHENEUM AVELGEM" = "GO! atheneum Avelgem",
    "CAMPUS SINTJAN BERCHMANS" = "Campus Sint-Jan Berchmans",
    "GO4CITY" = "Go4city",  #Go4City
    "GOCITY" = "Go4city",
    "GO 4CITY"  ="Go4city",
    "SINTDONATUS SECUNDAIR" = "Sint-Donatus secundair",
    "SPES NOSTRA KUURNE" = "Spes Nostra Kuurne",
    "NIEUWEN BOSCH HUMANIORA" = "Nieuwen Bosch Humaniora",
    "SINTJOZEFSCOLLEGE TORHOUT" = "Sint-Jozefscollege Torhout", #Correct Jozef college
    "LEIEPOORT CAMPUS SINT HENDRIK" = "Leiepoort campus Sint Hendrik",
    "BOVENBOUW SINTMICHIEL" = "Bovenbouw Sint-Michiel",
    "EMMAUS AALTER" = "Emmaüs Aalter",
    "HASPO CENTRUM SINTTRUIDEN" = "Hasp-o Centrum Sint-Truiden",
    "KVRI VORSELAAR" = "KVRI Vorselaar",
    "ATHENEUM HALLE" = "Atheneum Halle",
    "HEILIGHART COLLEGE HALLE" = "Heilig-Hart & College Halle",
    "TSAAM CAMPUS CARDIJN" =  "t Saam Campus Cardijn", #correct t saam and dash
    "LEIEPOORT SINT HENDRIK" = "Leiepoort Sint Hendrik",
    "SINTPAULUSCOLLEGE" = "Sint-Pauluscollege",
    "SINT PAULUS COLLEGE"   = "Sint-Pauluscollege",    #Paulus
    "SINT PAULUS COLLEGE WEVELGEM" = "Sint Pauluscollege Wevelgem",
    "SINTJOZEFKLEINSEMINARIE" = "Sint-Jozef-Klein-Seminarie",
    "GOTOPSPORTSCHOOL GENT" = "GO!Topsportschool Gent",
    "KBC GET A TEACHER" = "KBC Get a Teacher",
    "MIA BRUGGE" = "MIA Brugge",
    "EMMAUS SECUNDAIRE SCHOOL AALTER" = "Emmaus secundaire school Aalter",
    "MSKA ROESELARE" = "MSKA Roeselare",
    "KONINKLIJK ATHENEUM ETTERBEEL" = "Koninklijk Atheneum Etterbeel",
    "KATONDVLA" = "KatOndVla",
    "GLORIEUX RONSE" = "Glorieux Ronse",
    "SCHEPPERSINSTITUUT DEURNEANTWERPEN" = "Scheppersinstituut Deurne-Antwerpen",
    "SPORTSCHOOL HASSELT" = "Sportschool Hasselt",
    "NUO TIENEN" = "Nuo Tienen",
    "IMMACULATA INSTITUUT" = "Immaculata instituut",
    "SINTJOZEF SINTPIETER" = "Sint-Jozef Sint-Pieter",
    "SINTMARTINUSCOLLEGE" = "Sint-Martinuscollege",
    "BROEDERS" = "Broeders",
    "GO4CITY MOLENBEEK" = "GO!4CITY - Molenbeek",
    "CLW OOSTENDE" = "CLW Oostende",
    "HEILIG GRAF" = "Heilig Graf",
    "CAMPUS IMMACULATA DE PANNE" = "Campus Immaculata De Panne",
    "SINTDIMPNACOLLEGE" = "Sint-Dimpnacollege",
    "VRIJ HANDELS EN SPORTINSTITUUT" = "Vrij Handels- en Sportinstituut",
    "SINTJOZEFINSTITUUT" = "Sint-Jozefinstituut",
    "SINTJOZEFINSTITUUTBE" = "Sint-Jozefinstituut",   #More jozefinstituut
    "SINTJOZEFINSTITUUT GENK" = "Sint-Jozefinstituut",
    "SINTJOZEFINSTITUUT BOKRIJKI" = "Sint-Jozefinstituut Bokrijk",
    "SINTJOZEFINSTITUUTBOKRIJK" = "Sint-Jozefinstituut Bokrijk",
    "SINTPAULUSGENT" = "SintPaulusGent",
    "ANNUNTIAINSTITUUT WIJNEGEM" = "Annuntia-Instituut Wijnegem",
    "VLOT CAMPUS SINTLODEWIJK" = "VLOT! campus Sint-Lodewijk",
    "KADEURNE" = "KADeurne",
    "VISO ROESELARE" = "Viso Roeselare",
    "KOBOS" = "Kobos",
    "HEILIGE FAMILIE SINT WILLEBRORD" = "Heilige Familie - Sint Willebrord",
    "GO ATHENEUM GENTBRUGGE OOEIVAARSNEST" = "Go! Atheneum Gentbrugge Ooievaarsnest",
    "SINT REMBERT COLLEGE TORHOUT" = "Sint-Rembert College Torhout",
    "SINTJOZEF COLLEGE"   = "Sint-Jozefscollege Torhout",
    "SINTJOZEFS COLLEGE" = "Sint-Jozefscollege Torhout",
    "COLLEGE SINTREMBERT" = "Sint-Rembert College Torhout",
    "SINTJOZEFSCOLLEGE SINTREMBERT"  = "Sint-Rembert College Torhout"
    
    
  )
  
  # Use the lookup table; unmatched names will remain in cleaned uppercase
  homogenized_names <- lookup_table[cleaned_names]
  
  # 4. Handle Unmatched: Convert unmatched to Title Case
  unmatched_indices <- is.na(homogenized_names)
  homogenized_names[unmatched_indices] <- tools::toTitleCase(tolower(cleaned_names[unmatched_indices]))
  
  return(unname(homogenized_names)) # Remove names from the vector
}


#####  Course Group Homogenization (More robust)   ####
homogenize_course_group <- function(course_vector) {
  course_vector <- tolower(course_vector)
  course_vector <- str_replace_all(course_vector, "[[:punct:]]", "") # Remove punctuation
  course_vector <- str_squish(course_vector)  # Remove extra whitespace
  course_lookup <- c(
    "economie wiskunde" = "Economie-Wiskunde",
    "economie moderne talen" = "Economie-Moderne Talen",
    "bedrijfswetenschappen" = "Bedrijfswetenschappen",
    "bedrijfsorganisatie" = "Bedrijfsorganisatie",
    "moderne talen wetenschappen" = "Moderne Talen-Wetenschappen",
    "moderne talen" = "Moderne Talen",
    "6 commerciële organisatie" = "Commerciële Organisatie",
    "commerciële organisatie" = "Commerciële Organisatie",
    "ecmt" = "Economie-Moderne Talen"  #Or whatever makes sense for your data
    
  )
  homogenized_course_names <- course_lookup[course_vector]
  unmatched_indices <- is.na(homogenized_course_names)
  homogenized_course_names[unmatched_indices] <- course_vector[unmatched_indices]
  homogenized_course_names <- tools::toTitleCase(homogenized_course_names)
  return(unname(homogenized_course_names))
}

##### Define function to anonymize a column ####
anonymize_column <- function(df, column_name) {
  df %>%
    mutate(!!sym(column_name) := sapply(!!sym(column_name), function(x) digest(x, algo = "sha256")))
}



filter_and_select <- function(df,  keyword) {
  # Define the columns to select
  search_column= "G01Q01"
  selected_columns <- c("Q1", "G01Q03", "G01Q05", "G01Q01", 'id')
  
  # Check if columns exist in the dataframe
  missing_columns <- setdiff(selected_columns, colnames(df))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing:", paste(missing_columns, collapse = ", ")))
  }
  
  if (!(search_column %in% colnames(df))) {
    stop("The specified search column does not exist in the dataframe.")
  }
  
  # Filter and select columns
  df %>%
    filter(grepl(keyword, !!sym(search_column), ignore.case = TRUE)) %>%
    select(all_of(selected_columns))
}



fix_treatment_status <- function(df, treatment_column, ambiguous_treatment_column, stu_name, correct_treatment_value) {
  # Check if required columns exist
  if (!("G01Q01" %in% names(df))) {
    stop("Error: 'G01Q01' column not found in the dataframe.")
  }
  
  if (!(treatment_column %in% names(df))) {
    stop(paste0("Error: Treatment column '", treatment_column, "' not found in the dataframe."))
  }
  
  # Create ambiguous_treatment column if it doesn't exist
  if (!(ambiguous_treatment_column %in% names(df))) {
    df[[ambiguous_treatment_column]] <- FALSE  # Initialize with FALSE
  }
  
  # Apply the fixes
  df <- df %>%
    mutate(
      !!treatment_column := if_else(
        G01Q01 == stu_name,
        correct_treatment_value, # Update with correct value
        !!treatment_column  # Keep original if G01Q01 != stu_name
      ),
      !!ambiguous_treatment_column := if_else(G01Q01 == stu_name, TRUE, !!ambiguous_treatment_column)
    )
  
  return(df)
}


homogenize_school_names <- function(school_names) {
  # Convert all school names to lowercase to ensure case-insensitive matching.
  cleaned_names <- tolower(school_names)
  
  # Remove punctuation and extra whitespace to standardize the format.
  cleaned_names <- gsub("[[:punct:]]", " ", cleaned_names)
  cleaned_names <- gsub("\\s+", " ", cleaned_names)
  cleaned_names <- trimws(cleaned_names)
  
  # Create a detailed mapping from observed variations to a single, standardized name.
  school_mapping <- c(
    "heilig graf" = "Heilig Graf", "heiliggraf" = "Heilig Graf", "heiliggraf paterstraat" = "Heilig Graf",
    "campus hemelvaart" = "Campus Hemelvaart",
    "go atheneum tienen" = "GO! Atheneum Tienen", "atheneum tienen" = "GO! Atheneum Tienen", "goatheneumtienen" = "GO! Atheneum Tienen",
    "sint pauluscollege wevelgem" = "Sint-Pauluscollege Wevelgem", "sintpaulus college wevelgem" = "Sint-Pauluscollege Wevelgem", "sint pauluscollege" = "Sint-Pauluscollege", "spwe" = "Sint-Pauluscollege Wevelgem",
    "go atheneum gentbrugge" = "GO! Atheneum Gentbrugge", "atheneum gentbrugge" = "GO! Atheneum Gentbrugge",
    "sint jozefinstituut bokrijk" = "Sint-Jozefsinstituut Bokrijk",
    "yavne" = "Yavne",
    "ivg school" = "IVG-School", "ivgschool" = "IVG-School", "ivg school gent" = "IVG-School Gent", "ivscool" = "IVG-School",
    "go4city" = "Go4City",
    "sint jozefscollege torhout" = "Sint-Jozefscollege Torhout",
    "sint rembert college torhout" = "Sint-Rembert College Torhout",
    "kobos" = "Kobos", "kobos m" = "Kobos", "kobos secundair" = "Kobos Secundair",
    "go campus halle" = "GO! Campus Halle", "go atheneum halle" = "GO! Atheneum Halle", "atheneum halle" = "GO! Atheneum Halle", "ka halle" = "GO! Atheneum Halle", "koninklijk atheneum halle" = "GO! Atheneum Halle",
    "ksom campus sint jan berchmans" = "KSOM Campus Sint-Jan Berchmans", "ksom campus sintjan berchmans" = "KSOM Campus Sint-Jan Berchmans",
    "sjb mol" = "Sint-Jan Berchmanscollege Mol", "sint janberchmanscollege" = "Sint-Jan Berchmanscollege", "sintjan berchmanscollege" = "Sint-Jan Berchmanscollege", "sintjan berchmans college" = "Sint-Jan Berchmanscollege", "sintjanberchmanscollege" = "Sint-Jan Berchmanscollege", "sintjans berchmanscollege" = "Sint-Jan Berchmanscollege", "sint jan berchmanscollege" = "Sint-Jan Berchmanscollege", "sintjan bergmanscollege" = "Sint-Jan Berchmanscollege", "sint jan berghmanscollege" = "Sint-Jan Berchmanscollege", "sintjan berghmans college" = "Sint-Jan Berchmanscollege",
    "vrij handelsen sportinstituut" = "Vrij Handels- en Sportinstituut", "vhsi" = "Vrij Handels- en Sportinstituut",
    "ucll" = "UCLL",
    "topsportschool gent" = "Topsportschool Gent", "topsport gent" = "Topsportschool Gent",
    "virgosapiens" = "Virgo Sapiens", "virgo sapiens" = "Virgo Sapiens",
    "sint godelievecollege" = "Sint-Godelievecollege", "sigo" = "Sint-Godelievecollege", "sintgoddelievecollege" = "Sint-Godelievecollege", "sintgodelieve college sigo" = "Sint-Godelievecollege", "sigo gistel" = "Sint-Godelievecollege", "sintgoddelieve college" = "Sint-Godelievecollege",
    "go atheneum avelgem" = "GO! Atheneum Avelgem", "go atheneum avelgem" = "GO! Atheneum Avelgem",
    "go spronk" = "Atheneum Spronk", "spronk lennik" = "Atheneum Spronk", "atheneum spronk" = "Atheneum Spronk",
    "sint dimpna" = "Sint-Dimpna College", "sintdimpna" = "Sint-Dimpna College", "sdgc" = "Sint-Dimpna College", "sint dimpna college" = "Sint-Dimpna College",
    "heilig hartcollege" = "Heilig Hartcollege", "heilig hart college" = "Heilig Hartcollege", "heilighartcollege heistopdenberg" = "Heilig Hartcollege Heist-op-den-Berg",
    "go atheneum" = "GO! Atheneum",
    "atheneum" = "Atheneum",
    "sintbarbara" = "Sint-Barbaracollege", "sint barbara college" = "Sint-Barbaracollege",
    "college" = "College",
    "moretus" = "Moretus Ekeren", "moretus ekeren" = "Moretus Ekeren", "moretusekeren" = "Moretus Ekeren",
    "sancta maria leuven" = "Sancta Maria Leuven", "sancta maria" = "Sancta Maria",
    "edugo lochristi" = "Edugo Lochristi", "edugo" = "Edugo",
    "haspocentrum" = "Hasp-O Centrum", "hasp o centrum" = "Hasp-O Centrum",
    "futura secundair wervik" = "Futura Secundair", "futura secundair" = "Futura Secundair", "futura" = "Futura",
    "sint franciscus college" = "Sint-Franciscuscollege", "sint franciscuscollege" = "Sint-Franciscuscollege", "sfc" = "Sint-Franciscuscollege",
    "sint andreas instituut oostende" = "Sint-Andreasinstituut Oostende", "sintandreas" = "Sint-Andreasinstituut", "sint andreasinstituut" = "Sint-Andreasinstituut",
    "scheppersinstituut" = "Scheppersinstituut", "scheppersinstituutdeurne" = "Scheppersinstituut Deurne",
    "sint maria geel" = "Sint-Maria Geel", "sint maria" = "Sint-Maria",
    "zavo" = "ZAVO",
    "mska roeselare" = "MSKA Roeselare",
    "heilig hart en college halle" = "Heilig Hart & College Halle", "heilig hartcollege halle" = "Heilig Hartcollege Halle", "hhchalle" = "Heilig Hartcollege Halle", "hhc halle" = "Heilig Hartcollege Halle", "heilighart" = "Heilig Hart",
    "technisch instituut heilige familie ieper" = "Heilige Familie Ieper", "heilige familie" = "Heilige Familie",
    "sint paulusinstituut" = "Sint-Paulusinstituut",
    "mska" = "MSKA",
    "bovenbouw sint michiel" = "Bovenbouw Sint-Michiel",
    "sint norbertusinstituut" = "Sint-Norbertusinstituut", "sintnorbertus instituut" = "Sint-Norbertusinstituut", "snor" = "Sint-Norbertusinstituut",
    "topsport voskenslaan" = "Topsportschool Gent", "ka voskenslaan topsport" = "Topsportschool Gent",
    "atheneum courtmanslaan" = "Atheneum Courtmanslaan",
    "vlot" = "VLOT!",
    "virgo sapiens" = "Virgo Sapiens",
    "tabrasschaat" = "Technisch Atheneum Brasschaat", "ta brasschaat" = "Technisch Atheneum Brasschaat",
    "sinthendrik" = "Leiepoort Campus Sint-Hendrik", "leiepoort campus sinthendrik" = "Leiepoort Campus Sint-Hendrik",
    "spes nostra" = "Spes Nostra",
    "coovi" = "COOVI",
    "broederschool handel" = "Broederschool Handel", "broederschool" = "Broederschool",
    "kardinaal van roey instituut" = "Kardinaal Van Roey Instituut", "kvri" = "Kardinaal Van Roey Instituut",
    "sint martinuscollege overijse" = "Sint-Martinuscollege Overijse", "sint martinuscollege" = "Sint-Martinuscollege",
    "ksd" = "KSDiest",
    "spectrumcollege" = "Spectrumcollege",
    "sint ursula instituut" = "Sint-Ursula Instituut",
    "sint agnesinstituut" = "Sint-Agnesinstituut",
    "go atheneum diksmuide" = "GO! Atheneum Diksmuide",
    "methodeschool het kompas" = "Methodeschool Het Kompas",
    "hast" = "HAST",
    "immaculata" = "Immaculata",
    "sint angela tildonk" = "Sint-Angela Tildonk",
    "sivi" = "SIVI",
    "forum da vinci" = "Forum Da Vinci",
    "sint clara college" = "Sint-Clara College",
    "onze lieve vrouw presentatie" = "Onze-Lieve-Vrouw-Presentatie", "olvp" = "Onze-Lieve-Vrouw-Presentatie",
    "richtpunt campus buggenhout" = "Richtpunt Campus Buggenhout",
    "atheneum sint truiden" = "Atheneum Sint-Truiden",
    "smik" = "Sancta Maria Instituut Kasterlee",
    "c" = "Unknown",
    "sint jozef sint pieter" = "Sint-Jozef Sint-Pieter Blankenberge",
    "mia" = "MIA"
  )
  
  # Use the mapping to replace the cleaned names with the standardized names.
  homogenized_names <- school_mapping[cleaned_names]
  
  # If a name was not found in the mapping, keep the original (cleaned) name.
  unmapped_indices <- is.na(homogenized_names)
  homogenized_names[unmapped_indices] <- cleaned_names[unmapped_indices]
  
  return(homogenized_names)
}


homogenize_school_names_pro <- function(school_names) {
  
  # 1. CLEAN THE INPUT DATA
  # Convert to lowercase, replace all punctuation with spaces, and collapse multiple spaces.
  cleaned_names <- tolower(school_names)
  cleaned_names <- gsub("[[:punct:]]", " ", cleaned_names)
  cleaned_names <- gsub("\\s+", " ", cleaned_names)
  cleaned_names <- trimws(cleaned_names)
  
  # Create a second version for processing by removing generic, non-identifying terms.
  # This helps make the pattern matching more accurate.
  processed_names <- gsub("\\bcollege\\b|\\bcampus\\b|\\binstituut\\b|\\bschool\\b|\\batheneum\\b|\\bonderwijs\\b|\\bsecundair\\b", "", cleaned_names)
  processed_names <- trimws(processed_names) # Trim whitespace again after removal.
  
  # Initialize the output vector. We'll fill this in as we find matches.
  homogenized_names <- rep(NA, length(school_names))
  
  # 2. DEFINE THE PATTERNS FOR EACH OF THE 58 SCHOOLS
  # This list is the core of the function. Each entry is a standardized name,
  # and its value is a vector of unique string patterns to identify it.
  school_patterns <- list(
    "Heilig Graf" = c("heilig graf", "heiliggraf"),
    "Campus Hemelvaart" = "hemelvaart",
    "GO! Atheneum Tienen" = "tienen",
    "Sint-Pauluscollege Wevelgem" = c("paulus.*wevelgem", "spwe"),
    "GO! Atheneum Gentbrugge" = "gentbrugge",
    "Sint-Jozefsinstituut Bokrijk" = "bokrijk",
    "Yavne" = "yavne",
    "IVG-School" = c("ivg", "ivgschool"),
    "Go4City" = "go4city",
    "Sint-Jozefscollege Torhout" = "jozef.*torhout",
    "Sint-Rembert College Torhout" = "rembert",
    "Kobos" = "kobos",
    "GO! Atheneum Halle" = "go.*halle",
    "KSOM Campus Sint-Jan Berchmans" = "ksom",
    "Vrij Handels- en Sportinstituut" = c("vrij.*handel.*sport", "vhsi"),
    "UCLL" = "ucll",
    "Topsportschool Gent" = c("topsport.*gent", "voskenslaan"),
    "Virgo Sapiens" = c("virgo.*sapiens", "virgosapiens", "virgo.*londerzeel"),
    "Sint-Godelievecollege" = c("godelieve", "sigo"),
    "GO! Atheneum Avelgem" = c("avelgem", "kaavelgem"),
    "Atheneum Spronk" = "spronk",
    "Sint-Dimpna College Geel" = c("dimpna.*geel", "sdgc"),
    "Heilig Hartcollege Heist-op-den-Berg" = "heist",
    "Heilig Hartcollege Halle" = c("hart.*halle", "hhc"),
    "Sint-Barbaracollege" = "barbara",
    "Sint-Jan Berchmanscollege Mol" = c("jan.*berchmans.*mol", "sjb.*mol"),
    "Moretus Ekeren" = "moretus",
    "Sancta Maria Leuven" = "sancta.*maria.*leuven",
    "Edugo Lochristi" = c("edugo", "edugolo", "eduolo"),
    "Hasp-O Centrum" = c("hasp o", "haspo"),
    "Futura Secundair Wervik" = "futura",
    "Sint-Franciscuscollege" = c("franciscus", "sfc"),
    "Sint-Andreasinstituut Oostende" = "andreas.*oostende",
    "Scheppersinstituut Deurne" = "scheppers",
    "Sint-Maria Geel" = "maria.*geel",
    "ZAVO" = "zavo",
    "MSKA Roeselare" = c("mska", "tant"),
    "Heilige Familie Ieper" = "familie.*ieper",
    "Sint-Paulusinstituut Herzele" = c("paulus.*herzele", "spi"),
    "Bovenbouw Sint-Michiel" = "michiel",
    "Sint-Norbertusinstituut Duffel" = c("norbertus.*duffel", "snor"),
    "Atheneum Courtmanslaan" = "courtmanslaan",
    "VLOT!" = "vlot",
    "Technisch Atheneum Brasschaat" = c("brasschaat", "kta"),
    "Leiepoort Campus Sint-Hendrik" = c("leiepoort", "hendrik"),
    "Spes Nostra Kuurne" = c("spes.*nostra", "spesnostra"),
    "COOVI" = "coovi",
    "Broederschool Handel" = "broeder",
    "Kardinaal Van Roey Instituut" = c("roey", "kvri"),
    "Sint-Martinuscollege Overijse" = c("martinus.*overijse", "smo"),
    "KSDiest" = c("ksd", "ksdiest"),
    "Spectrumcollege" = "spectrum",
    "Sint-Jan Berchmanscollege Malle" = "sjb.*malle",
    "Sint-Ursula Instituut" = "ursula",
    "GO! Atheneum Diksmuide" = "diksmuide",
    "Methodeschool Het Kompas" = "kompas",
    "HAST" = "hast",
    "Immaculata" = "immaculata",
    "Sint-Angela Tildonk" = "angela",
    "Sint-Anna" = "sint anna",
    "SIVI" = "sivi",
    "Forum Da Vinci" = "da vinci",
    "Sint-Clara College" = "clara",
    "Onze-Lieve-Vrouw-Presentatie" = c("presentatie", "olvp"),
    "Richtpunt Campus Buggenhout" = "buggenhout",
    "Atheneum Sint-Truiden" = c("sinttruiden", "speelhof"),
    "Sancta Maria Instituut Kasterlee" = c("maria.*kasterlee", "smik"),
    "Sint-Jozef Sint-Pieter Blankenberge" = "sint jozef sint pieter",
    "MIA" = "mia"
  )
  
  # 3. MATCH NAMES TO PATTERNS
  # Loop through each school and its patterns, finding matches in the yet-unassigned names.
  for (school_name in names(school_patterns)) {
    patterns <- school_patterns[[school_name]]
    for (pattern in patterns) {
      matches <- grepl(pattern, processed_names) & is.na(homogenized_names)
      if (any(matches)) {
        homogenized_names[matches] <- school_name
      }
    }
  }
  
  # 4. HANDLE UNMATCHED AND SPECIAL CASES
  # If any names remain unmatched, return their original form to allow for manual review.
  unmatched_indices <- is.na(homogenized_names)
  homogenized_names[unmatched_indices] <- school_names[unmatched_indices]
  
  # Manually correct specific edge cases that are hard to catch with patterns.
  homogenized_names[school_names == "c"] <- "Unknown"
  homogenized_names[school_names == "caroline pauwels"] <- "Unknown (Individual's Name)"
  
  return(homogenized_names)
}

get_network_part <- function(ip_vector) {
  # Usamos expresión regular para quedarnos con los dos primeros octetos
  sub("^([0-9]+\\.[0-9]+)\\..*", "\\1", ip_vector)
}
