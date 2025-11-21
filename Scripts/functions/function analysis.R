

fractions_by_perception = function(table_, column, main){
  counts <- table(as.character(table_[[column]]))
  
  
  # Calculate the proportions
  fractions <- prop.table(counts) 
  # Create a bar plot for fractions
  plot_ = barplot(fractions, 
                  main = main , 
                  xlab = "Groups", 
                  ylab = "Fraction of Participants", 
                  col = c("steelblue", "lightcoral", "darkseagreen"),
                  ylim = c(0, 1)) # Ensures the y-axis goes from 0 to 1
  
  
  print(fractions)
  # print(plot_)
  return(plot_)
}

# Wrapper function to generate perception plots
generate_perception_plot <- function(data, column, treatment_state, main_base, separate = TRUE) {
  require(ggplot2)
  
  # Define group-specific details (including colors, alpha, and labels)
  group_definitions <- list(
    list(group = "1", main = "\nControl Group", color = "skyblue", alpha = 1, label = "Control Group"),
    list(group = "2", main = "\nAI+Reduced Path", color = "coral", alpha = 0.7, label = "AI+Reduced Path"),
    list(group = "3", main = "\nTailored AI", color = "lightgreen", alpha = 0.5, label = "Tailored AI")
  )
  
  # Function to add line breaks every three words
  add_line_breaks <- function(str) {
    words <- unlist(strsplit(str, " "))
    if (length(words) <= 3) {
      return(str)
    }
    new_str <- ""
    for (i in seq_along(words)) {
      new_str <- paste0(new_str, words[i], " ")
      if (i %% 3 == 0 && i != length(words)) {
        new_str <- paste0(new_str, "\n")
      }
    }
    trimws(new_str)
  }
  
  # Calculate proportions and create a data frame for plotting
  plot_data <- do.call(rbind, lapply(group_definitions, function(def) {
    group_data <- subset(data, data[[treatment_state]] == def$group)
    counts <- table(as.character(group_data[[column]]))
    fractions <- prop.table(counts)
    data.frame(
      group = names(fractions),
      fraction = as.numeric(fractions),
      treatment_group = def$group,
      main = paste0(main_base, def$main),  # Add main_base to main
      color = def$color,
      alpha = def$alpha,
      label = def$label,
      stringsAsFactors = FALSE
    )
  }))
  
  # Apply the line break function
  plot_data$group <- sapply(plot_data$group, add_line_breaks)
  plot_data$group <- factor(plot_data$group, levels = unique(plot_data$group))
  
  # Define the base plot and common attributes
  base_plot <- ggplot(plot_data, aes(x = group, y = fraction, fill = treatment_group, alpha = treatment_group)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(y = "Fraction of Participants", x = "") +  # Remove x-axis label
    ylim(0, 1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = setNames(plot_data$color, plot_data$treatment_group),
                      breaks = sapply(group_definitions, function(x) x$group),
                      labels = sapply(group_definitions, function(x) x$label)) +
    scale_alpha_manual(values = setNames(plot_data$alpha, plot_data$treatment_group),
                       breaks = sapply(group_definitions, function(x) x$group),
                       labels = sapply(group_definitions, function(x) x$label))
  
  if (separate) {
    # Add title only if main_base is not empty
    if (main_base != "") {
      base_plot <- base_plot + facet_wrap(~main)
    } else {
      base_plot <- base_plot + facet_wrap(~label)
    }
    
    print(base_plot + guides(alpha = "none"))
  } else {
    # Add title only if main_base is not empty
    if (main_base != "") {
      base_plot <- base_plot + labs(title = main_base)
    }
    
    print(base_plot + guides(fill = guide_legend(title = "Groups"), alpha = guide_legend(title = "Groups")))
  }
}
#### generate plots and metadata####
generate_perception_plot <- function(data, column, treatment_state, main_base, separate = TRUE) {
  require(ggplot2)
  require(dplyr) # Add dplyr dependency
  
  # Define group-specific details (including colors, alpha, and labels)
  group_definitions <- list(
    list(group = "1", main = "\nControl Group", color = "skyblue", alpha = 1, label = "Control Group"),
    list(group = "2", main = "\nAI+Reduced Path", color = "coral", alpha = 0.7, label = "AI+Reduced Path"),
    list(group = "3", main = "\nTailored AI", color = "lightgreen", alpha = 0.5, label = "Tailored AI")
  )
  
  # Function to add line breaks every three words
  add_line_breaks <- function(str) {
    words <- unlist(strsplit(str, " "))
    if (length(words) <= 3) {
      return(str)
    }
    new_str <- ""
    for (i in seq_along(words)) {
      new_str <- paste0(new_str, words[i], " ")
      if (i %% 3 == 0 && i != length(words)) {
        new_str <- paste0(new_str, "\n")
      }
    }
    trimws(new_str)
  }
  
  # Calculate proportions and create a data frame for plotting
  plot_data <- do.call(rbind, lapply(group_definitions, function(def) {
    group_data <- subset(data, data[[treatment_state]] == def$group)
    counts <- table(as.character(group_data[[column]]))
    fractions <- prop.table(counts)
    data.frame(
      group = names(fractions),
      fraction = as.numeric(fractions),
      treatment_group = def$group,
      main = paste0(main_base, def$main),  # Add main_base to main
      color = def$color,
      alpha = def$alpha,
      label = def$label,
      stringsAsFactors = FALSE
    )
  }))
  
  # Apply the line break function
  plot_data$group <- sapply(plot_data$group, add_line_breaks)
  plot_data$group <- factor(plot_data$group, levels = unique(plot_data$group))
  
  # Calculate summary statistics (e.g., mean fraction by treatment group)
  summary_stats <- plot_data %>%
    group_by(treatment_group) %>%
    summarise(
      mean_fraction = mean(fraction, na.rm = TRUE),
      sd_fraction = sd(fraction, na.rm = TRUE)
    )
  
  # Define the base plot and common attributes
  base_plot <- ggplot(plot_data, aes(x = group, y = fraction, fill = treatment_group, alpha = treatment_group)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(y = "Fraction of Participants", x = "") +  # Remove x-axis label
    ylim(0, 1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = setNames(plot_data$color, plot_data$treatment_group),
                      breaks = sapply(group_definitions, function(x) x$group),
                      labels = sapply(group_definitions, function(x) x$label)) +
    scale_alpha_manual(values = setNames(plot_data$alpha, plot_data$treatment_group),
                       breaks = sapply(group_definitions, function(x) x$group),
                       labels = sapply(group_definitions, function(x) x$label))
  
  if (separate) {
    # Add title only if main_base is not empty
    if (main_base != "") {
      base_plot <- base_plot + facet_wrap(~main)
    } else {
      base_plot <- base_plot + facet_wrap(~label)
    }
    
    plot <- base_plot + guides(alpha = "none")
    print(plot)
  } else {
    # Add title only if main_base is not empty
    if (main_base != "") {
      base_plot <- base_plot + labs(title = main_base)
    }
    
    plot <- base_plot + guides(fill = guide_legend(title = "Groups"), alpha = guide_legend(title = "Groups"))
    print(plot)
  }
  
  # Return the plot and the metadata
  return(list(
    plot = plot,
    data = plot_data,
    summary_stats = summary_stats,
    column = column,
    treatment_state = treatment_state,
    main_base = main_base,
    separate = separate
  ))
}



##### save plot ####
save_perception_plot_metadata <- function(Graph, title ){
  title_=gsub(pattern = "\n", replacement = "_", title)
  ggsave(file.path('Graph/', paste0(title_, ".png") ), Graph$plot)
  write.csv(Graph$data, file.path('Metadata/',  paste0(title_, ".csv")), row.names = FALSE)
}

test_distibution_score <- function(data, score_col, state, title="") {
  # Calculate the mean and median values of the score column
  data$score_col <- data[[score_col]] # assign the col to data$score_col
  mean_score <- mean(data$score_col, na.rm = TRUE)
  median_score <- median(data$score_col, na.rm = TRUE)
  print(paste0("Mean: ", mean_score, 
               "\nMedian: ", median_score, 
               "\nSD: ", sd(data$score_col, na.rm = TRUE))
  )
  if (state == 'pretest') {
    when <- 'At Baseline'
  } else {
    when <- 'After Treatment'
  }
  
  # Create the density plot
  plot <- ggplot(data, aes(x = .data$score_col)) + #Use .data$ to refer to the col
    geom_density(aes(y = ..scaled..), fill = "steelblue",alpha = 0.2) + 
    geom_vline(xintercept = mean_score, linetype = "dashed", color = "red", size = 0.5) + 
    geom_vline(xintercept = median_score, linetype = "dashed", color = "blue", size = 0.5) + 
    labs(
      title = paste0("Density Distribution of\n", title," Scores", '\n', when),
      x = "Score",
      y = "Density"
    ) +
    annotate( "text", x = mean_score, y = 0, label = "Mean", 
              hjust = 0, vjust = -1, color = "red", size = 3, fontface = "italic"
    ) + 
    annotate("text", x = median_score, y = 0.5, label = "Median", 
             hjust = 0, vjust = -1, color = "blue", size = 3, fontface = "italic"
    ) + 
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      plot.margin = margin(10, 10, 20, 10)
    )
  
  return(plot)
}

# test_distibution_score <- function(data, score_col, state, title="") {
#   
#   mean_score <- mean(data[[score_col]], na.rm = TRUE)
#   median_score <- median(data[[score_col]], na.rm = TRUE)
#   
#   if (state == 'pretest') {
#     when <- 'At Baseline'
#   } else {
#     when <- 'After Treatment'
#   }
#   
#   # Create the density plot
#   plot <- ggplot(data, aes(x = .data[[score_col]])) + 
#     geom_density(aes(y = ..scaled..), fill = "steelblue",alpha = 0.2) + 
#     geom_vline(xintercept = mean_score, linetype = "dashed", color = "red", size = 0.5) + 
#     geom_vline(xintercept = median_score, linetype = "dashed", color = "blue", size = 0.5) + 
#     labs(
#       title = paste0("Density Distribution of\n", title," Scores", '\n', when),
#       x = "Score",
#       y = "Density"
#     ) +
#     annotate( "text", x = mean_score, y = 0, label = "Mean", 
#               hjust = 0, vjust = -1, color = "red", size = 3, fontface = "italic"
#     ) + 
#     annotate("text", x = median_score, y = 0.5, label = "Median", 
#              hjust = 0, vjust = -1, color = "blue", size = 3, fontface = "italic"
#     ) + 
#     theme_minimal() + 
#     theme(
#       plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
#       axis.title.x = element_text(size = 12, face = "bold"),
#       axis.title.y = element_text(size = 12, face = "bold"),
#       plot.margin = margin(10, 10, 20, 10)
#     )
#   
#   return(plot)
# }


plot_density_by_group <- function(data, score_col, group_col, title="") {
  if (title=="") {
    title=score_col
  }
  data$state = data[[group_col]]
  # Relabel groups
  group_labels <- c(
    "Control Group:\nTraditional Path (TP)",
    "Treatment:\nReduced TP + AI",
    "Treatment:\nTailored AI in the \nBelgian Tax System"
  )
  data[[group_col]] <- factor(data[[group_col]], levels = c(1, 2, 3), labels = group_labels)
  # Calculate group sizes
  group_sizes <- data %>%
    group_by(state) %>%
    summarise(total_n = n())
  # Calculate means for each group
  mean_values <- tapply(data[[score_col]], data[[group_col]], mean, na.rm = TRUE)
  
  # Prepare the plot
  plot <- ggplot(data, aes(x = .data[[score_col]], fill = .data[[group_col]])) +
    # geom_density(alpha = 0.6) +  # Density plot with transparency
    # geom_density(aes(y = ..density.. ), alpha = 0.6) +  # Normalize density for proportions
    geom_density(
      aes(y = ..density../group_sizes$total_n[as.numeric(as_factor(state))]),
      alpha = 0.3
    ) +
    geom_vline(
      aes(xintercept = mean_values[.data[[group_col]]], color = .data[[group_col]]), 
      linetype = "dashed", size = 0.8
    ) +  # Dashed mean lines
    scale_fill_manual(
      values = c("steelblue", "lightcoral", "darkseagreen"), 
      name = "Groups"
    ) +  # Custom colors for density
    scale_color_manual(
      values = c("steelblue", "lightcoral", "darkseagreen"), 
      guide = "none"  # Use the same colors for the dashed lines
    ) +
    labs(
      title = paste0( "Density Distribution of\n", title  ,"by Group"),
      x = "Score",
      y = "Density"
    ) +
    theme_minimal(base_size = 12) +  # Professional theme
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centered and styled title
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.spacing = unit(0.8, "cm"),
      legend.key.size = unit(1.2, "cm"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10)
    )
  
  return(plot)
}

plot_rct_density <- function(data, score_col, group_col, title_ = "") {
  
  # 1. Data Preparation
  titulo = if_else(title_ == "", paste("Density Distribution of\n", score_col, "by Group"), title_)
  print(titulo)
  # Rename for easier handling
  data$score = data[[score_col]]
  data$group = data[[group_col]]
  
  # data <- data %>%
  #   rename(score = {{score_col}}, 
  #          group = {{group_col}})
  
  # Define group labels
  group_labels <- c(
    "1" = "Control Group:\nTraditional Path (TP)",
    "2" = "Treatment:\nReduced TP + AI",
    "3" = "Treatment:\nTailored AI in the \nBelgian Tax System"
  )
  
  # Relabel groups for better legend/display
  data$group <- factor(data$group, levels = c(1, 2, 3), labels = group_labels)
  
  # 2. Calculate Group Statistics (Means)
  
  group_means <- data %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(mean_score = mean(score, na.rm = TRUE),.groups="drop")
  
  # 3. Create the Density Plot
  
  plot <- ggplot(data, aes(x = score, fill = group)) +
    geom_density(alpha = 0.7) +  # Density plot with transparency
    geom_vline(data = group_means, aes(xintercept = mean_score, color = group),
               linetype = "dashed", size = 1) +  # Add mean lines
    scale_fill_manual(values = c("skyblue", "lightgreen", "salmon"),
                      name = "Group") +  # Custom colors for fills
    scale_color_manual(values = c("skyblue", "lightgreen", "salmon"),
                       guide = "none") + # Same for mean lines; hide color legend
    labs(
      title = titulo,
      x = "Score",
      y = "Density"
    ) +
    theme_minimal(base_size = 14) +  # Clean theme
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"  # Put legend at the bottom
    )
  
  return(plot)
}

descriptive_stats_by_group <- function(data, score_col, group_col, latex = FALSE) {
  
  # Use column names directly
  data = data[,c(group_col,score_col)]
  data = na.omit(data)
 
  colnames(data)= c('group', 'score')
  # Define group labels
  group_labels <- c(
    "1" = "Control Group: Traditional Path (TP)",
    "2" = "Treatment: Reduced TP + AI",
    "3" = "Treatment: Tailored AI in the Belgian Tax System"
  )
  
  # Calculate descriptive statistics
  group_stats <- data %>%
    group_by(group) %>%
    summarize(
      mean_score = mean(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      n_participants = n(),
      .groups = "drop"
    ) %>%
    mutate(group_label = group_labels[as.character(group)])
  
  # Print descriptive statistics
  if (latex) {
    # LaTeX table output
    cat("\\begin{tabular}{lccc}\n")
    cat("\\hline\n")
    cat("Group & Mean & Standard Deviation & N Participants \\\\\n")
    cat("\\hline\n")
    for (i in 1:nrow(group_stats)) {
      cat(
        paste0(
          group_stats$group_label[i], " & ",
          sprintf("%.2f", group_stats$mean_score[i]), " & ",
          sprintf("%.2f", group_stats$sd_score[i]), " & ",
          group_stats$n_participants[i], " \\\\\n"
        )
      )
    }
    cat("\\hline\n")
    cat("\\end{tabular}\n")
  } else {
    # Regular output
    for (i in 1:nrow(group_stats)) {
      cat(
        sprintf(
          "%s (group %s) has a mean of %.2f, a standard deviation of %.2f, and %d participants.\n",
          group_stats$group_label[i],
          as.character(group_stats$group[i]),
          group_stats$mean_score[i],
          group_stats$sd_score[i],
          group_stats$n_participants[i]
        )
      )
    }
  }
  
  return(group_stats)
}

##### Plot and download ####

save_analysis_output <- function(data, score_col, group_col, analysis_name) {
  # 1. Descriptive Stats and LaTeX Table
  descriptive_stats_text <- capture.output(descriptive_stats_by_group(data, score_col, group_col, latex = TRUE))
  
  # 2. Density Plot
  plot_filename <- file.path("Graph", paste0(analysis_name, ".png"))
  plot <- plot_rct_density(data, score_col, group_col, paste0("Distribution of ", analysis_name))  # Pass title
  
  ggsave(filename = plot_filename, plot = plot, width = 8, height = 6, units = "in")  # Save plot
  
  # 3. Linear Model Summary
  linear_model <- lm(data = data, as.formula(paste0(score_col, "~ factor(", group_col, ")")))
  linear_model_summary <- capture.output(summary(linear_model))
  
  # 4. Combine All Metadata and Save to File
  metadata <- c(
    "Analysis Name:" = analysis_name,
    "Score Column:" = score_col,
    "Group Column:" = group_col,
    "Descriptive Statistics (LaTeX Table):\n" = descriptive_stats_text,
    "\nLinear Model Summary:\n" = linear_model_summary
  )
  print(descriptive_stats_text)
  print( linear_model_summary)
  
  metadata_filename <- file.path("Metadata", paste0(analysis_name, ".txt"))
  write(paste(names(metadata), metadata, collapse = "\n"), file = metadata_filename)
  
  cat(paste0("Analysis for '", analysis_name, "' saved to 'Graph' and 'Metadata' folders.\n"))
  print(plot)
}


descriptive_stats_by_group <- function(data, score_col, group_col, latex = FALSE) {
  
  # Use column names directly
  data = data[,c(group_col,score_col)]
  data = na.omit(data)
  
  colnames(data)= c('group', 'score')
  # Define group labels
  group_labels <- c(
    "1" = "Control Group: Traditional Path (TP)",
    "2" = "Treatment: Reduced TP + AI",
    "3" = "Treatment: Tailored AI in the Belgian Tax System"
  )
  
  # Calculate descriptive statistics
  group_stats <- data %>%
    group_by(group) %>%
    summarize(
      mean_score = mean(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      n_participants = n(),
      .groups = "drop"
    ) %>%
    mutate(group_label = group_labels[as.character(group)])
  
  # Print descriptive statistics
  if (latex) {
    # LaTeX table output
    cat("\\begin{tabular}{lccc}\n")
    cat("\\hline\n")
    cat("Group & Mean & Standard Deviation & N Participants \\\\\n")
    cat("\\hline\n")
    for (i in 1:nrow(group_stats)) {
      cat(
        paste0(
          group_stats$group_label[i], " & ",
          sprintf("%.2f", group_stats$mean_score[i]), " & ",
          sprintf("%.2f", group_stats$sd_score[i]), " & ",
          group_stats$n_participants[i], " \\\\\n"
        )
      )
    }
    cat("\\hline\n")
    cat("\\end{tabular}\n")
  } else {
    # Regular output
    for (i in 1:nrow(group_stats)) {
      cat(
        sprintf(
          "%s (group %s) has a mean of %.2f, a standard deviation of %.2f, and %d participants.\n",
          group_stats$group_label[i],
          as.character(group_stats$group[i]),
          group_stats$mean_score[i],
          group_stats$sd_score[i],
          group_stats$n_participants[i]
        )
      )
    }
  }
}

# plot_rct_density <- function(data, score_col, group_col, title = "Distribution by Treatment Group") {
#   # Ensure group_col is a factor
#   data[[group_col]] <- as.factor(data[[group_col]])
#   
#   # Create the density plot with ggplot2
#   ggplot(data, aes(x = .data[[score_col]], fill = .data[[group_col]])) +
#     geom_density(alpha = 0.5) +
#     labs(
#       title = title,
#       x = score_col,
#       y = "Density",
#       fill = group_col
#     ) +
#     theme_minimal() +
#     scale_fill_brewer(palette = "Pastel1")
# }
#  
categorize_fortnight <- function(date_column) {
  # Ensure the column is in Date format
  date_column <- as.Date(date_column)
  
  # Extract year and month
  year_month <- format(date_column, "%Y-%m")
  
  # Extract day of the month
  day_of_month <- as.numeric(format(date_column, "%d"))
  
  # Determine the fortnight category
  fortnight <- ifelse(day_of_month <= 14, 2 * (as.numeric(format(date_column, "%m")) - 1), 
                      2 * (as.numeric(format(date_column, "%m")) - 1) + 1)
  
  # Create a categorical variable with a unique ID for each fortnight
  category <- paste(year_month, fortnight, sep = "-")
  # category <- 
  
  return(category)
}




# Load necessary libraries
library(dplyr)
library(tidyr)
library(knitr)
# Consider library(kableExtra) for enhanced styling

descriptive_stats_by_group_transposed <- function(data,
                                                  score_cols,
                                                  group_col, 
                                                  digits = 3, # Control rounding
                                                  caption = "Descriptive Statistics by Group", # Add caption
                                                  label = "tab:desc_stats", # Add label
                                                  latex_output = TRUE, # Control output type
                                                  file_name = "") {
  group_labels <- c(
    "1" = "Control Group: Traditional Path (TP)",
    "2" = "Treatment: Reduced TP + AI",
    "3" = "Treatment: Tailored AI in the Belgian Tax System"
  )
  # --- Input Validation (Basic Example) ---
  if (!group_col %in% names(data)) {
    stop("Grouping column '", group_col, "' not found in data.")
  }
  if (!all(score_cols %in% names(data))) {
    missing_cols <- score_cols[!score_cols %in% names(data)]
    stop("Score columns not found in data: ", paste(missing_cols, collapse=", "))
  }
  
  # --- Data Preparation ---
  # Subset data - select only necessary columns
  data_subset <- data[, c(group_col, score_cols)]
  
  # Rename group column internally for consistency
  colnames(data_subset)[1] <- 'group_internal'
  
  # Convert group column to factor *using the provided labels*
  # This ensures correct ordering and naming later
  data_subset$group_internal <- factor(data_subset$group_internal,
                                       levels = names(group_labels),
                                       labels = group_labels)
  
  # Handle NAs during summarization rather than upfront listwise deletion
  # data_subset <- na.omit(data_subset) # Consider implications: removes entire row if ANY score is NA
  
  # --- Calculate Descriptive Statistics ---
  group_stats <- data_subset %>%
    pivot_longer(cols = all_of(score_cols), # Use all_of for clarity with select helpers
                 names_to = "Variable",
                 values_to = "score") %>%
    group_by(group_internal, Variable) %>%
    summarize(
      N = sum(!is.na(score)), # Calculate N for clarity
      Mean = mean(score, na.rm = TRUE),
      SD = sd(score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Format the display string (Mean (SD))
    mutate(
      stat_display = ifelse(N > 0, # Handle cases with no data for a group/var
                            paste0(
                              sprintf(paste0("%.", digits, "f"), Mean),
                              " (",
                              sprintf(paste0("%.", digits, "f"), SD),
                              ")"
                              # Optional: Add N, e.g. paste0(..., "\nN=", N)
                            ),
                            "-" # Placeholder for missing data
      )
    ) %>%
    select(group_internal, Variable, stat_display) # Keep only needed columns
  
  # --- Reshape Data: Variables as rows, Groups as columns ---
  wide_stats <- group_stats %>%
    pivot_wider(id_cols = Variable, # Rows defined by Variable
                names_from = group_internal, # New columns from group names
                values_from = stat_display) # Values are the formatted strings
  
  # Order rows based on the original score_cols order
  wide_stats <- wide_stats[match(score_cols, wide_stats$Variable),]
  
  
  # --- Generate Output ---
  if (latex_output) {
    # Use knitr::kable for robust LaTeX generation
    # booktabs = TRUE gives nicer rules (\toprule, \midrule, \bottomrule)
    latex_table <- kable(wide_stats,
                         format = "latex",
                         booktabs = TRUE,
                         caption = caption,
                         label = label,
                         align = c('l', rep('c', ncol(wide_stats) - 1)), # Left align variable, center others
                         escape = FALSE) # Prevent escaping of LaTeX special chars if group labels have them
    
    # Optional: Add styling with kableExtra
    # library(kableExtra)
    # latex_table <- kable_styling(latex_table, latex_options = c("striped", "hold_position"))
    
    
    # Save to file if requested
    if (nzchar(file_name)) { # Check for non-empty string
      dir.create(dirname(file_name), showWarnings = FALSE, recursive = TRUE) # Create directory if needed
      # Ensure file has .tex extension if not provided
      if (!endsWith(tolower(file_name), ".tex")) {
        file_name <- paste0(file_name, ".tex")
      }
      writeLines(latex_table, file_name)
      message("LaTeX table saved to: ", file_name) # Provide confirmation
    }
    
    # Return the LaTeX code as a single string
    return(paste(latex_table, collapse = "\n"))
    
  } else {
    # Return the reshaped data frame if LaTeX is not requested
    return(wide_stats)
  }
}
# descriptive_stats_by_group_transposed(
#   data = pretest,
#   score_cols = statistics,
#   group_col = "Q1", 
#   latex_output = TRUE,            # Equivalent to your latex = T
#   file_name = "Tables/descriptive_stats" # Specify path and base name
#   # Optional: Add a caption and label for the LaTeX table
#   # caption = "Descriptive Statistics for Pre-Test Scores by Group (Q1)",
#   # label = "tab:pretest_desc"
# )
generate_perception_plot_with_test = function(data, column, treatment_state, main_base, separate = TRUE) {
  # Use requireNamespace for checking package availability without loading globally if preferred
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' needed for this function to work. Please install it.", call. = FALSE)
  }
  library(ggplot2) # Load ggplot2 for use
  library(dplyr)   # Load dplyr for use
  
  # --- Input Checks (Basic) ---
  if (!column %in% names(data)) stop("Column '", column, "' not found in data.")
  if (!treatment_state %in% names(data)) stop("Treatment state column '", treatment_state, "' not found in data.")
  if (!is.factor(data[[column]]) && !is.character(data[[column]])) {
    warning("Column '", column, "' is not character or factor. Attempting conversion for table.")
    # data[[column]] <- as.factor(data[[column]]) # Be careful with automatic conversion
  }
  if (!is.factor(data[[treatment_state]]) && !is.character(data[[treatment_state]])) {
    warning("Column '", treatment_state, "' is not character or factor. Attempting conversion for table.")
    # data[[treatment_state]] <- as.factor(data[[treatment_state]])
  }
  # --- End Input Checks ---
  
  
  # --- Calculate Contingency Table and Perform Test ---
  contingency_counts <- NULL
  test_result_obj <- NULL
  test_result_obj_raw <- NULL # Initialize raw object store
  test_method <- NULL
  test_statistic <- NULL
  test_p_value <- NULL
  test_warning_msg <- NULL
  
  tryCatch({
    # Create contingency table of COUNTS using xtabs for safety
    formula_str <- paste0("~ `", column, "` + `", treatment_state, "`") # Use backticks for non-standard names
    contingency_counts <- xtabs(as.formula(formula_str), data = data, addNA = FALSE) # Exclude NA counts by default
    
    if (is.null(contingency_counts) || length(contingency_counts) == 0 || any(dim(contingency_counts) < 1) ) {
      warning("Contingency table could not be created or is empty.")
      test_warning_msg <- "Contingency table could not be created or is empty."
      print("Contingency table empty or could not be created.") # DEBUG PRINT
    } else {
      # --- Debugging Test Execution ---
      print("Contingency Table created:") # DEBUG PRINT
      print(contingency_counts)           # DEBUG PRINT
      print("Attempting Chi-squared test (raw)...") # DEBUG PRINT
      test_result_obj_raw <- tryCatch(chisq.test(contingency_counts), error=function(e) {print(paste("RAW TEST FAILED:",e$message)); return(NULL)}) # Run without suppress/tryCatch first for debug
      if(!is.null(test_result_obj_raw)){
        print("Test object created (raw):") # DEBUG PRINT
        print(str(test_result_obj_raw)) # Print structure # DEBUG PRINT
      }
      # --- End Debugging ---
      
      print("Attempting Chi-squared test with suppress/tryCatch...") # DEBUG PRINT
      test_result_obj <- suppressWarnings(
        tryCatch(
          chisq.test(contingency_counts),
          error = function(e) {
            warning("Chi-squared test failed: ", e$message)
            return(list(error = TRUE, message = e$message))
          }
        )
      )
      print("Test object after suppress/tryCatch:") # DEBUG PRINT
      print(str(test_result_obj)) # <<<< DEBUG PRINT 1
      
      # Check if the test itself produced an error object from our tryCatch
      if (inherits(test_result_obj, "list") && !is.null(test_result_obj$error)) {
        test_warning_msg <- paste("Chi-squared test failed:", test_result_obj$message)
        test_result_obj <- NULL # Clear the result object on error
        print(paste("Test failed with error:", test_warning_msg)) # <<<< DEBUG PRINT 2
      } else if (is.null(test_result_obj) || !inherits(test_result_obj, "htest")) {
        # Added check if test_result_obj is not even a valid htest object
        test_warning_msg <- "Chi-squared test did not return a valid test object."
        print(test_warning_msg) # DEBUG PRINT
        test_result_obj <- NULL # Ensure it's NULL if invalid
      } else {
        print("Test succeeded (returned htest object), attempting to extract results...") # <<<< DEBUG PRINT 3
        
        # Check specifically for the approximation warning *after* running
        test_warn <- tryCatch(chisq.test(contingency_counts), warning=function(w) {w})
        if (!is.null(test_warn) && grepl("Chi-squared approximation may be incorrect", test_warn$message)) {
          test_warning_msg <- "Chi-squared approximation may be incorrect (check expected counts). Consider Fisher's Exact Test."
          print("Approximation warning detected.") # <<<< DEBUG PRINT 4
        } else {
          print("No approximation warning detected.") # DEBUG PRINT
        }
        
        # --- Debugging Assignments ---
        print("Assigning method...") # DEBUG PRINT
        test_method <- test_result_obj$method
        print(paste("test_method:", ifelse(is.null(test_method), "NULL", test_method))) # DEBUG PRINT
        
        print("Assigning statistic...") # DEBUG PRINT
        test_statistic <- test_result_obj$statistic
        print(paste("test_statistic:", ifelse(is.null(test_statistic), "NULL", test_statistic))) # DEBUG PRINT
        
        print("Assigning p.value...") # DEBUG PRINT
        test_p_value <- test_result_obj$p.value
        print(paste("test_p_value:", ifelse(is.null(test_p_value), "NULL", test_p_value))) # DEBUG PRINT
        # --- End Debugging Assignments ---
        
        print("Finished extracting results.") # <<<< DEBUG PRINT 5
      }
    }
  }, error = function(e) {
    warning("Error during contingency table creation or testing: ", e$message) # Corrected
    test_warning_msg <- paste("Error during processing:", e$message) # Corrected
    print(paste("Error caught in outer tryCatch:", e$message)) # Corrected
  })
  
  # --- Prepare data_test output list ---
  print("Preparing final data_test list...") # <<<< DEBUG PRINT 7
  print(paste("Value of test_method before list creation:", ifelse(is.null(test_method), "NULL", test_method))) # DEBUG PRINT
  print(paste("Value of test_statistic before list creation:", ifelse(is.null(test_statistic), "NULL", test_statistic))) # DEBUG PRINT
  print(paste("Value of test_p_value before list creation:", ifelse(is.null(test_p_value), "NULL", test_p_value))) # DEBUG PRINT
  
  data_test_output <- list(
    contingency_table_counts = contingency_counts, # The count table needed for the test
    test_method = test_method,                     # e.g., "Pearson's Chi-squared test"
    test_statistic_value = test_statistic,         # The calculated chi-squared value
    p_value = test_p_value,                        # The test p-value
    warning_message = test_warning_msg,            # Any warnings (e.g., about small counts)
    suggested_interpretation = ifelse(is.null(test_p_value), "Test could not be performed.",
                                      ifelse(test_p_value > 0.10, "No significant difference detected (p > 0.10) -> Groups likely balanced.",
                                             ifelse(test_p_value > 0.05, "Marginally significant difference (0.05 < p <= 0.10).",
                                                    "Significant difference detected (p <= 0.05) -> Groups likely unbalanced."))),
    how_to_interpret = "Compare p-value to significance level (e.g., 0.05 or 0.10). High p-value (> alpha) suggests balance (no significant difference). Low p-value (<= alpha) suggests imbalance.",
    # Store the raw object from the initial test run for inspection
    full_test_object = if(exists("test_result_obj_raw")) test_result_obj_raw else NULL
  )
  # --- End Test Section ---
  
  
  # --- Plotting Section (largely unchanged) ---
  # Define group-specific details (including colors, alpha, and labels)
  group_definitions <- list(
    list(group = "1", main = "\nControl Group", color = "skyblue", alpha = 1, label = "Control Group"),
    list(group = "2", main = "\nAI+Reduced Path", color = "coral", alpha = 0.7, label = "AI+Reduced Path"),
    list(group = "3", main = "\nTailored AI", color = "lightgreen", alpha = 0.5, label = "Tailored AI")
  )
  
  # Filter group_definitions to only include groups present in the data
  present_groups <- as.character(sort(unique(data[[treatment_state]])))
  group_definitions <- group_definitions[sapply(group_definitions, function(def) def$group %in% present_groups)]
  if(length(group_definitions) == 0) {
    warning("No matching group definitions found for groups in data. Plotting might fail.")
    # Allow to proceed, maybe plotting fails gracefully or user has few groups
  }
  
  
  # Function to add line breaks every three words
  add_line_breaks <- function(str) {
    str <- as.character(str)
    words <- unlist(strsplit(str, " "))
    if (length(words) <= 3) {
      return(str)
    }
    new_str <- ""
    for (i in seq_along(words)) {
      new_str <- paste0(new_str, words[i], " ")
      if (i %% 3 == 0 && i != length(words)) {
        new_str <- paste0(new_str, "\n")
      }
    }
    trimws(new_str)
  }
  
  # Calculate proportions and create a data frame for plotting
  plot_data <- do.call(rbind, lapply(group_definitions, function(def) {
    group_data <- subset(data, as.character(data[[treatment_state]]) == def$group)
    if (nrow(group_data) > 0 && column %in% names(group_data)) {
      # Use the specific column for table, ensure it's treated as factor/character
      counts <- table(as.character(group_data[[column]]))
      if(sum(counts) > 0) {
        fractions <- prop.table(counts)
        data.frame(
          group = names(fractions),
          fraction = as.numeric(fractions),
          treatment_group = def$group,
          main = paste0(main_base, def$main),
          color = def$color,
          alpha = def$alpha,
          label = def$label,
          stringsAsFactors = FALSE
        )
      } else { NULL }
    } else { NULL }
  }))
  
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    warning("No data available for plotting after processing.")
    # Return only test data if plotting fails
    # Make sure data_test_output exists even if tests failed
    if(!exists("data_test_output")){
      data_test_output <- list(warning_message = "Plotting data empty and test data unavailable.")
    }
    return(list(data_test = data_test_output))
  }
  
  
  plot_data$group <- sapply(plot_data$group, add_line_breaks)
  # Create factor levels based on the order of appearance for each treatment group
  plot_data$group <- factor(plot_data$group, levels = unique(plot_data$group))
  
  
  summary_stats <- plot_data %>%
    dplyr::group_by(treatment_group) %>%
    dplyr::summarise(
      mean_fraction = mean(fraction, na.rm = TRUE),
      sd_fraction = sd(fraction, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create named vectors for scales AFTER filtering group_definitions
  # Ensure names are based on the actual group identifiers used
  group_ids <- sapply(group_definitions, function(x) x$group)
  color_values <- setNames(sapply(group_definitions, function(x) x$color), group_ids)
  alpha_values <- setNames(sapply(group_definitions, function(x) x$alpha), group_ids)
  label_values <- setNames(sapply(group_definitions, function(x) x$label), group_ids)
  
  
  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = fraction, fill = treatment_group, alpha = treatment_group)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(preserve = "single")) +
    ggplot2::labs(y = "Fraction of Participants", x = "") +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "bottom") +
    # Use breaks and labels arguments in scale_* functions
    ggplot2::scale_fill_manual(name = "Groups",
                               values = color_values,
                               breaks = group_ids,
                               labels = label_values) +
    ggplot2::scale_alpha_manual(name = "Groups",
                                values = alpha_values,
                                breaks = group_ids,
                                labels = label_values)
  
  if (separate) {
    if (!missing(main_base) && nzchar(main_base)) {
      # Facet by 'label' which should correspond to group label via group_definitions
      base_plot <- base_plot + ggplot2::facet_wrap(~label, scales = "free_x") # Use label from plot_data
      base_plot <- base_plot + ggplot2::ggtitle(main_base)
    } else {
      base_plot <- base_plot + ggplot2::facet_wrap(~label, scales = "free_x")
    }
    plot <- base_plot + ggplot2::guides(alpha = "none", fill = "none")
    
  } else {
    if (!missing(main_base) && nzchar(main_base)) {
      base_plot <- base_plot + ggplot2::labs(title = main_base)
    }
    plot <- base_plot
  }
  
  print(plot)
  
  # Return list including the new data_test element
  return(invisible(list(
    plot = plot,
    data = plot_data,
    summary_stats = summary_stats,
    data_test = data_test_output,
    column = column,
    treatment_state = treatment_state,
    main_base = main_base,
    separate = separate
  )))
}

descriptive_stats_by_group <- function(data_df, 
                                       score_cols, 
                                       group_col, 
                                       latex_caption = "Descriptive Statistics by Group",
                                       latex_label = "tab:desc_stats_by_group",
                                       latex_notes_line1 = "Values are Mean (Standard Deviation) [N per cell].", # Adjusted this default
                                       latex_notes_line2 = "Scales are 1-5 Likert unless otherwise noted.",
                                       save_to_file_path = NULL) {
  
  # Ensure necessary columns exist
  if (!group_col %in% colnames(data_df)) {
    stop(paste("Group column '", group_col, "' not found in data.", sep=""))
  }
  missing_scores <- setdiff(score_cols, colnames(data_df))
  if (length(missing_scores) > 0) {
    stop(paste("Score column(s) not found in data:", paste(missing_scores, collapse=", ")))
  }
  
  # --- Calculate overall N per group BEFORE any NA filtering for specific scores ---
  # This N reflects how many observations are in each group in the input data_df
  # for the specified group_col.
  # We need to ensure the group_col is treated as a factor with the right labels for this summary.
  
  temp_group_col_name <- "group_for_overall_n" # Temporary unique name
  data_df[[temp_group_col_name]] <- factor(data_df[[group_col]]) # Ensure it's a factor
  
  group_labels_map <- c(
    "1" = "Control (Traditional Path)",
    "2" = "Treatment 1 (Reduced TP + AI)",
    "3" = "Treatment 2 (Tailored AI)"
  )
  # Apply labels if Q1 is numeric-like factor levels "1", "2", "3"
  if(all(levels(data_df[[temp_group_col_name]]) %in% names(group_labels_map))){
    levels(data_df[[temp_group_col_name]]) <- group_labels_map[levels(data_df[[temp_group_col_name]])]
  }
  
  
  overall_n_per_group <- data_df %>%
    group_by(.data[[temp_group_col_name]]) %>%
    summarise(N_overall = n(), .groups = "drop") %>%
    rename(group_label = .data[[temp_group_col_name]]) # Rename for easier merging/lookup later
  
  # Construct the N per group note string
  n_per_group_notes <- paste0("Overall N per group: ", 
                              paste(apply(overall_n_per_group, 1, function(row) {
                                paste0(row["group_label"], " (N=", row["N_overall"], ")")
                              }), collapse = "; "), ".")
  
  # --- Proceed with existing logic for cell-specific stats ---
  data_subset <- data_df[, c(group_col, score_cols)] # Use original group_col name here
  colnames(data_subset)[colnames(data_subset) == group_col] <- 'group_internal' # Rename for internal consistency
  
  data_subset$group_internal <- as.character(data_subset$group_internal)
  
  group_stats <- data_subset %>%
    pivot_longer(cols = -group_internal, names_to = "variable_raw", values_to = "score") %>%
    group_by(group_internal, variable_raw) %>%
    summarize(
      n_cell = sum(!is.na(score)), 
      mean_score = mean(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      group_label_for_cols = group_labels_map[group_internal],
      variable_display = gsub("pre_", "", variable_raw),
      variable_display = gsub("_", " ", variable_display),
      variable_display = tools::toTitleCase(tolower(variable_display))
    )
  
  desired_group_order <- group_labels_map[sort(unique(names(group_labels_map)))]
  group_stats$group_label_for_cols <- factor(group_stats$group_label_for_cols, levels = desired_group_order)
  
  table_data <- group_stats %>%
    select(variable_display, group_label_for_cols, n_cell, mean_score, sd_score) %>%
    pivot_wider(names_from = group_label_for_cols, 
                values_from = c(n_cell, mean_score, sd_score),
                names_vary = "slowest",
                names_glue = "{.value}_{group_label_for_cols}")
  
  unique_group_display_labels <- group_labels_map[sort(as.character(unique(group_stats$group_internal)))]
  
  # --- LaTeX Output Generation ---
  latex_output <- capture.output({
    cat("\\begin{table}[htbp]\n")
    cat("\\centering\n")
    cat("\\begin{threeparttable}\n")
    cat(paste0("\\caption{", latex_caption, "}\n"))
    cat(paste0("\\label{", latex_label, "}\n"))
    cat("\\scriptsize\n")
    
    col_format <- paste0("@{}l", paste(rep("c", length(unique_group_display_labels)), collapse = ""), "@{}")
    cat(paste0("\\begin{tabular}{", col_format, "}\n"))
    cat("\\toprule\n")
    
    header_row1 <- paste("Variable", paste(unique_group_display_labels, collapse = " & "), sep = " & ")
    cat(paste0(header_row1, " \\\\\n"))
    
    mean_sd_n_header <- paste(rep("Mean (SD) [N]", length(unique_group_display_labels)), collapse = " & ")
    cat(paste("", mean_sd_n_header, sep = " & "), " \\\\\n")
    cat("\\midrule\n")
    
    for (i in 1:nrow(table_data)) {
      row_name <- table_data$variable_display[i]
      row_values_str <- c()
      for (group_lab in unique_group_display_labels) {
        n_col_name <- paste0("n_cell_", group_lab)
        mean_col_name <- paste0("mean_score_", group_lab)
        sd_col_name <- paste0("sd_score_", group_lab)
        
        if (n_col_name %in% colnames(table_data) &&
            mean_col_name %in% colnames(table_data) && 
            sd_col_name %in% colnames(table_data) &&
            !is.na(table_data[[n_col_name]][i]) && table_data[[n_col_name]][i] > 0) {
          
          sd_val <- table_data[[sd_col_name]][i]
          sd_str <- if (is.na(sd_val)) "NA" else sprintf("%.3f", sd_val)
          
          val_str <- paste0(sprintf("%.3f", table_data[[mean_col_name]][i]), 
                            " (", sd_str, ")",
                            " [", table_data[[n_col_name]][i], "]")
        } else if (n_col_name %in% colnames(table_data) && !is.na(table_data[[n_col_name]][i]) && table_data[[n_col_name]][i] == 0) {
          val_str <- "- (-) [0]"
        } else {
          val_str <- "NA (NA) [NA]"
        }
        row_values_str <- c(row_values_str, val_str)
      }
      cat(paste(row_name, paste(row_values_str, collapse = " & "), sep = " & "), " \\\\\n")
    }
    
    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    
    # Table notes - now includes the overall N per group
    cat("\\begin{tablenotes}\n")
    if (!is.null(latex_notes_line1) && latex_notes_line1 != "") {
      cat(paste0("  \\item[] \\textit{Notes:} ", latex_notes_line1, "\n"))
    }
    if (!is.null(latex_notes_line2) && latex_notes_line2 != "") {
      cat(paste0("  \\item[] ", latex_notes_line2, "\n"))
    }
    cat(paste0("  \\item[] ", n_per_group_notes, "\n")) # Add overall Ns here
    cat("\\end{tablenotes}\n")
    
    cat("\\end{threeparttable}\n")
    cat("\\end{table}\n")
  })
  
  latex_string <- paste(latex_output, collapse = "\n")
  
  if (!is.null(save_to_file_path)) {
    dir_name <- dirname(save_to_file_path)
    if (!dir.exists(dir_name) && dir_name != ".") {
      dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
    }
    writeLines(latex_string, save_to_file_path)
    cat(paste("\nLaTeX table saved to:", save_to_file_path, "\n"))
  }
  
  return(latex_string)
}



baseline_balance_categorical_to_latex <- function(data_df,
                                                  categorical_vars_to_test,
                                                  group_col,
                                                  var_name_dictionary = NULL, # New argument for the dictionary
                                                  latex_caption = "Baseline Balance Tests for Categorical Variables",
                                                  latex_label = "tab:baseline_balance_categorical",
                                                  save_to_file_path = NULL) {
  
  results_list <- list()
  
  if (!is.factor(data_df[[group_col]])) {
    data_df[[group_col]] <- factor(as.character(data_df[[group_col]]))
  }
  
  for (var_name_raw in categorical_vars_to_test) { # Changed loop variable name for clarity
    if (!var_name_raw %in% colnames(data_df)) {
      warning(paste("Categorical variable '", var_name_raw, "' not found in data. Skipping.", sep = ""))
      next
    }
    
    current_var_data <- data_df[[var_name_raw]] # Use a temporary variable for the data column
    if (!is.factor(current_var_data)) {
      current_var_data <- factor(as.character(current_var_data))
    }
    
    # Use a temporary dataframe for the chi-squared test to avoid modifying data_df in loop
    temp_df_for_test <- data.frame(
      var_to_test = current_var_data,
      group = data_df[[group_col]]
    )
    temp_df_for_test <- na.omit(temp_df_for_test) # Remove NAs only for this specific test
    
    contingency_table <- table(temp_df_for_test$var_to_test, temp_df_for_test$group)
    contingency_table_cleaned <- contingency_table[rowSums(contingency_table) > 0, colSums(contingency_table) > 0]
    
    chi_sq_stat_str <- "NA"
    df_str <- "NA"
    p_value_str <- "NA"
    
    if (nrow(contingency_table_cleaned) < 2 || ncol(contingency_table_cleaned) < 2) {
      warning(paste("Skipping Chi-squared test for '", var_name_raw, "' due to insufficient levels/groups after NA removal or cleaning.", sep=""))
    } else {
      test_result <- tryCatch({
        chisq.test(contingency_table_cleaned, simulate.p.value = FALSE) 
      }, warning = function(w){
        message(paste("Warning for variable '", var_name_raw, "': ", w$message, ". Attempting simulation.", sep=""))
        chisq.test(contingency_table_cleaned, simulate.p.value = TRUE, B = max(2000, 10*ncol(contingency_table_cleaned)*nrow(contingency_table_cleaned)))
      }, error = function(e) {
        warning(paste("Error in Chi-squared test for '", var_name_raw, "': ", e$message, ". Skipping.", sep = ""))
        return(NULL)
      })
      
      if (!is.null(test_result)) {
        chi_sq_stat_str <- sprintf("%.2f", test_result$statistic)
        df_str <- as.character(test_result$parameter)
        p_val_numeric <- test_result$p.value
        p_value_str <- if (p_val_numeric < 0.001) "<0.001" else sprintf("%.3f", p_val_numeric)
      } else {
        chi_sq_stat_str <- "Error"
        df_str <- "Error"
        p_value_str <- "Error"
      }
    }
    
    # Use dictionary for display name, otherwise clean the raw name
    display_name <- if (!is.null(var_name_dictionary) && var_name_raw %in% names(var_name_dictionary)) {
      var_name_dictionary[[var_name_raw]]
    } else {
      # Fallback cleaning if name not in dictionary
      cleaned_var_name_step1 <- gsub("pre_", "", var_name_raw)
      cleaned_var_name_step2 <- gsub("_", " ", cleaned_var_name_step1)
      # Handle special characters like [SQ001]
      cleaned_var_name_step2 <- gsub("\\[SQ\\d{3}\\]", "", cleaned_var_name_step2) 
      cleaned_var_name_step3 <- tolower(cleaned_var_name_step2)
      tools::toTitleCase(cleaned_var_name_step3)
    }
    
    results_list[[var_name_raw]] <- data.frame(
      Variable = display_name, # Use the display name
      Chi_Squared_Statistic = chi_sq_stat_str,
      df = df_str,
      p_value = p_value_str,
      stringsAsFactors = FALSE
    )
  }
  
  final_table_df <- NULL
  if(length(results_list) > 0) {
    final_table_df <- bind_rows(results_list)
  } else {
    final_table_df <- data.frame(Variable=character(), Chi_Squared_Statistic=character(), df=character(), p_value=character())
    warning("No categorical variables were processed. Output table will be empty.")
  }
  
  # --- LaTeX Output Generation (remains largely the same) ---
  latex_output <- capture.output({
    cat("\\begin{table}[htbp]\n")
    cat("\\centering\n")
    cat("\\begin{threeparttable}\n")
    cat(paste0("\\caption{", latex_caption, "}\n"))
    cat(paste0("\\label{", latex_label, "}\n"))
    cat("\\scriptsize\n")
    
    cat("\\begin{tabular}{@{}lcc@{}}\n") 
    cat("\\toprule\n")
    cat("Variable & Test Statistic ($\\chi^2$(df)) & p-value \\\\\n") 
    cat("\\midrule\n")
    
    if(nrow(final_table_df) > 0) {
      for (i in 1:nrow(final_table_df)) {
        stat_df_combined <- final_table_df$Chi_Squared_Statistic[i]
        if(final_table_df$df[i] != "NA" && final_table_df$df[i] != "Error" && !is.na(final_table_df$df[i])) {
          stat_df_combined <- paste0("$\\chi^2$(", final_table_df$df[i], ") = ", final_table_df$Chi_Squared_Statistic[i])
        } else if (final_table_df$Chi_Squared_Statistic[i] != "NA" && final_table_df$Chi_Squared_Statistic[i] != "Error") {
          stat_df_combined <- paste0("$\\chi^2$ = ", final_table_df$Chi_Squared_Statistic[i])
        }
        
        cat(paste(
          final_table_df$Variable[i],
          stat_df_combined, 
          final_table_df$p_value[i],
          sep = " & "
        ), " \\\\\n")
      }
    } else {
      cat("  &  &  \\\\ % No data rows\n")
    }
    
    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    
    cat("\\begin{tablenotes}\n")
    cat("  \\item[] \\textit{Notes:} Pearson's Chi-squared tests for independence between each baseline characteristic and the assigned treatment group.\n")
    cat("\\end{tablenotes}\n")
    
    cat("\\end{threeparttable}\n")
    cat("\\end{table}\n")
  })
  
  latex_string <- paste(latex_output, collapse = "\n")
  
  if (!is.null(save_to_file_path)) {
    dir_name <- dirname(save_to_file_path)
    if (!dir.exists(dir_name) && dir_name != ".") {
      dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
    }
    writeLines(latex_string, save_to_file_path)
    cat(paste("\nLaTeX table saved to:", save_to_file_path, "\n"))
  }
  
  return(latex_string)
}

library(dplyr)
library(stringr)
categorize_education <- function(education_field) {
  if (!is.character(education_field)) {
    stop("Input 'education_field' must be a character vector.")
  }
  
  sapply(education_field, function(field) {
    if (is.na(field)) {
      return(NA_character_)
    }
    
    # Normalize the string for robust matching
    cleaned_field <- tolower(field)
    # Replace common Flemish accented characters with their unaccented equivalents
    cleaned_field <- chartr("éèêëöüï", "eeeeouii", cleaned_field)
    # Remove all punctuation and numbers
    cleaned_field <- str_replace_all(cleaned_field, "[^a-z\\s]", "")
    # Remove extra spaces
    cleaned_field <- str_squish(cleaned_field)
    
    category <- case_when(
      # --- Specific / Junk / Uncategorized Terms (prioritize to catch errors early) ---
      str_detect(cleaned_field, "alle zesdejaars doorstroom|nvt|jijij|dcxfvgx|tzrfjzrtj|karton en beton|lawschool|cor|como|coovi|beor|coor|bcs|v") ~ "Other/Unspecified",
      str_detect(cleaned_field, "leerkracht$") ~ "Other/Unspecified", # Standalone "Leerkracht" without a field
      
      # --- 1. Humanities & Languages (includes Classics, Communication) ---
      # Put these first as they are often combined but define the stream strongly
      str_detect(cleaned_field, "latijn|grieks|talen|taal|communicatie|moderne|humanities") ~ "Humanities & Languages",
      
      # --- 2. Arts & Sports ---
      str_detect(cleaned_field, "kunst|dans|architectur|sport") ~ "Arts & Sports",
      
      # --- 3. Care & Social Studies (includes Welfare, Health, Social Sciences, Pedagogy) ---
      str_detect(cleaned_field, "humane wetenschap|humane|opvoeding|begeleiding|welzijn|gezondheid|verzorging|zorg|gedragswetenschap") ~ "Care & Social Studies",
      
      # --- 4. STEM (Science, Technology, Engineering, Math, IT) ---
      str_detect(cleaned_field, "wiskunde|wetenschap|technologie|informatica|elektromechanica|biotech") ~ "STEM",
      
      # --- 5. Economics & Business (includes Commerce, Management, Finance, Organization) ---
      str_detect(cleaned_field, "economie|bedrijf|handel|commercieel|financie|organisatie|logistiek") ~ "Economics & Business", # Added logistiek here
      
      # --- 6. Vocational & Applied Skills (specific trades/practical skills) ---
      str_detect(cleaned_field, "restaurant|keuken|bakkerij|slagerij|tuinbouw|applicatie|data") ~ "Vocational & Applied Skills",
      
      # Catch-all for anything not matched by specific patterns
      TRUE ~ "Other/Unspecified"
    )
    return(category)
  }, USE.NAMES = FALSE)
}




baseline_balance_tests_to_latex <- function(data_df,
                                            vars_to_test,
                                            group_col,
                                            control_group_level = "1", 
                                            t1_level = "2",
                                            t2_level = "3",
                                            latex_caption = "Baseline Balance Tests for Continuous Variables",
                                            latex_label = "tab:baseline_balance_continuous",
                                            save_to_file_path = NULL) {
  
  results_list <- list()
  original_group_col_name <- group_col 
  
  if (!is.factor(data_df[[original_group_col_name]])) {
    data_df[[original_group_col_name]] <- factor(as.character(data_df[[original_group_col_name]]))
  }
  if (!control_group_level %in% levels(data_df[[original_group_col_name]])) {
    stop(paste("Control group level '", control_group_level, "' not found in '", original_group_col_name, 
               "'. Available levels: ", paste(levels(data_df[[original_group_col_name]]), collapse=", "), sep=""))
  }
  data_df[[original_group_col_name]] <- relevel(data_df[[original_group_col_name]], ref = control_group_level)
  
  # Simpler internal column names for results_list and final_table_df
  internal_col_names <- c("Variable", "Control_Est_SE", "T1_Est_SE", "T2_Est_SE")
  
  # Display names for LaTeX table header
  display_col_headers <- c(
    "Variable",
    "Intercept (Control Group Mean)",
    "Treatment 1: Reduced TP + AI", 
    "Treatment 2: Tailored AI"      
  )
  
  expected_term_t1 <- paste0(original_group_col_name, t1_level)
  expected_term_t2 <- paste0(original_group_col_name, t2_level)
  
  for (var_name in vars_to_test) {
    if (!var_name %in% colnames(data_df)) {
      warning(paste("Variable '", var_name, "' not found in data. Skipping.", sep = ""))
      next
    }
    
    formula_str <- paste0("`", var_name, "` ~ `", original_group_col_name, "`")
    model <- lm(as.formula(formula_str), data = data_df)
    print(summary(model))
    tidy_model <- broom::tidy(model)
    
    # print(paste("For variable:", var_name, "--- R model terms:", paste(tidy_model$term, collapse=", "))) # Keep for debugging
    
    cleaned_var_name <- gsub("pre_", "", var_name) %>% 
      gsub("_", " ", .) %>% 
      tolower() %>%
      tools::toTitleCase()
    
    # Use the simple internal column names for the list
    current_row_values <- list()
    current_row_values[["Variable"]] <- cleaned_var_name
    
    intercept_row <- tidy_model %>% filter(term == "(Intercept)")
    current_row_values[["Control_Est_SE"]] <- if (nrow(intercept_row) > 0) {
      paste0(sprintf("%.3f", intercept_row$estimate), " (", sprintf("%.3f", intercept_row$std.error), ")", add_stars(intercept_row$p.value))
    } else { "Error" }
    
    t1_row <- tidy_model %>% filter(term == expected_term_t1)
    if (nrow(t1_row) == 0) { # Fallback
      fallback_term_t1 <- tidy_model$term[endsWith(tidy_model$term, t1_level) & !endsWith(tidy_model$term, t2_level)]
      if(length(fallback_term_t1) == 1) t1_row <- tidy_model %>% filter(term == fallback_term_t1)
    }
    current_row_values[["T1_Est_SE"]] <- if (nrow(t1_row) > 0) {
      paste0(sprintf("%.3f", t1_row$estimate), " (", sprintf("%.3f", t1_row$std.error), ")", add_stars(t1_row$p.value))
    } else { "Not Found" }
    
    t2_row <- tidy_model %>% filter(term == expected_term_t2)
    if (nrow(t2_row) == 0) { # Fallback
      fallback_term_t2 <- tidy_model$term[endsWith(tidy_model$term, t2_level) & !endsWith(tidy_model$term, t1_level)]
      if(length(fallback_term_t2) == 1) t2_row <- tidy_model %>% filter(term == fallback_term_t2)
    }
    current_row_values[["T2_Est_SE"]] <- if (nrow(t2_row) > 0) {
      paste0(sprintf("%.3f", t2_row$estimate), " (", sprintf("%.3f", t2_row$std.error), ")", add_stars(t2_row$p.value))
    } else { "Not Found" }
    
    results_list[[var_name]] <- current_row_values # Add the list to results_list
  }
  
  # Convert list of lists to a data frame
  if(length(results_list) > 0) {
    final_table_df <- do.call(rbind, lapply(results_list, function(x) data.frame(t(unlist(x)), stringsAsFactors=FALSE)))
    # Ensure column order matches internal_col_names if needed, though direct list to df usually preserves order
    # If names are not exactly matching, it might be from t(unlist(x))
    # A safer way to ensure column names for the dataframe:
    final_table_df <- data.frame(matrix(unlist(results_list), nrow=length(results_list), byrow=TRUE))
    colnames(final_table_df) <- internal_col_names # Assign simple column names
  } else {
    final_table_df <- data.frame(matrix(ncol = length(internal_col_names), nrow = 0))
    colnames(final_table_df) <- internal_col_names
    warning("No variables were processed. Output table will be empty.")
  }
  
  # --- LaTeX Output Generation ---
  latex_output <- capture.output({
    cat("\\begin{table}[htbp]\n")
    cat("\\centering\n")
    cat("\\begin{threeparttable}\n")
    cat(paste0("\\caption{", latex_caption, "}\n"))
    cat(paste0("\\label{", latex_label, "}\n"))
    cat("\\scriptsize\n")
    
    col_format <- "@{}lccc@{}" 
    cat(paste0("\\begin{tabular}{", col_format, "}\n"))
    cat("\\toprule\n")
    
    # Use display_col_headers for LaTeX
    cat(paste(display_col_headers, collapse = " & "), " \\\\\n")
    cat("\\midrule\n")
    
    if(nrow(final_table_df) > 0) {
      for (i in 1:nrow(final_table_df)) {
        # Access columns using the simple internal_col_names
        cat(paste(
          final_table_df[i, "Variable"],
          final_table_df[i, "Control_Est_SE"], 
          final_table_df[i, "T1_Est_SE"],
          final_table_df[i, "T2_Est_SE"],
          sep = " & "
        ), " \\\\\n")
      }
    } else {
      cat(paste(rep(" ", length(display_col_headers)), collapse = " & "), " \\\\ % No data rows\n")
    }
    
    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    
    cat("\\begin{tablenotes}\n")
    cat("  \\item[] \\textit{Notes:} OLS regression for each variable on treatment group indicators. Reference group is Control. Estimates are coefficients (standard errors in parentheses). \n")
    cat("  \\item[] Significance codes: $^{***} p<0.001$, $^{**} p<0.01$, $^{*} p<0.05$, $^{.} p<0.1$.\n")
    cat("\\end{tablenotes}\n")
    
    cat("\\end{threeparttable}\n")
    cat("\\end{table}\n")
  })
  
  latex_string <- paste(latex_output, collapse = "\n")
  
  if (!is.null(save_to_file_path)) {
    dir_name <- dirname(save_to_file_path)
    if (!dir.exists(dir_name) && dir_name != ".") {
      dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
    }
    writeLines(latex_string, save_to_file_path)
    cat(paste("\nLaTeX table saved to:", save_to_file_path, "\n"))
  }
  
  return(latex_string)
}



descriptive_stats_by_group <- function(data_df, # Renamed to avoid conflict with base R data
                                       score_cols, 
                                       group_col, 
                                       latex_caption = "Descriptive Statistics by Group",
                                       latex_label = "tab:desc_stats_by_group",
                                       latex_notes = "Values are Mean (Standard Deviation).",
                                       save_to_file_path = NULL) { # Path for saving .tex file
  
  # Ensure necessary columns exist
  if (!group_col %in% colnames(data_df)) {
    stop(paste("Group column '", group_col, "' not found in data.", sep=""))
  }
  missing_scores <- setdiff(score_cols, colnames(data_df))
  if (length(missing_scores) > 0) {
    stop(paste("Score column(s) not found in data:", paste(missing_scores, collapse=", ")))
  }
  
  # Subset data with selected columns and handle NAs for these specific columns
  data_subset <- data_df[, c(group_col, score_cols)]
  # data_subset <- na.omit(data_subset) # This might remove too many rows if NAs are sparse
  # na.rm=TRUE in summarize is generally better for this.
  
  # Rename group column for internal use
  colnames(data_subset)[1] <- 'group_internal'
  
  # Define group labels - ensure these exactly match the levels in your factor Q1
  # Or, better, derive them from the factor levels if Q1 is already a factor with labels
  group_labels_map <- c(
    "1" = "Control (Traditional Path)", # Shorter for column names
    "2" = "Treatment 1 (Reduced TP + AI)",
    "3" = "Treatment 2 (Tailored AI)"
  )
  
  # Ensure group column is character for mapping, or factor for ordering
  data_subset$group_internal <- as.character(data_subset$group_internal)
  
  # Calculate descriptive statistics
  group_stats <- data_subset %>%
    pivot_longer(cols = -group_internal, names_to = "variable_raw", values_to = "score") %>%
    filter(!is.na(score)) %>% # Handle NAs per variable-group combination
    group_by(group_internal, variable_raw) %>%
    summarize(
      mean_score = mean(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      group_label_for_cols = group_labels_map[group_internal], # Use this for pivoting
      # Clean up variable names for display
      variable_display = gsub("pre_", "", variable_raw), # Remove "pre_"
      variable_display = gsub("_", " ", variable_display), # Replace underscore with space
      variable_display = tools::toTitleCase(tolower(variable_display)) # Title Case
    )
  
  # Ensure the order of group_label_for_cols is correct for pivot_wider
  # This is important for the column order in the final table
  desired_group_order <- group_labels_map[sort(unique(names(group_labels_map)))]
  group_stats$group_label_for_cols <- factor(group_stats$group_label_for_cols, levels = desired_group_order)
  
  # Reshape data so groups are columns
  table_data <- group_stats %>%
    select(variable_display, group_label_for_cols, mean_score, sd_score) %>%
    pivot_wider(names_from = group_label_for_cols, 
                values_from = c(mean_score, sd_score),
                names_vary = "slowest", # Ensures mean_X, sd_X, mean_Y, sd_Y order
                names_glue = "{.value}_{group_label_for_cols}") # Creates mean_score_GroupA, sd_score_GroupA etc.
  
  # Get the unique group labels for the table header in the correct order
  # unique_group_display_labels <- levels(group_stats$group_label_for_cols)
  # Or use the map directly to ensure order
  unique_group_display_labels <- group_labels_map[sort(as.character(unique(group_stats$group_internal)))]
  
  
  # Create LaTeX output using threeparttable structure
  latex_output <- capture.output({
    cat("\\begin{table}[htbp]\n")
    cat("\\centering\n")
    cat("\\begin{threeparttable}\n")
    cat(paste0("\\caption{", latex_caption, "}\n"))
    cat(paste0("\\label{", latex_label, "}\n"))
    cat("\\scriptsize\n") # As in your example table
    
    # Constructing the column format string dynamically
    # l for variable name, then ccc for each group's Mean (SD)
    col_format <- paste0("@{}l", paste(rep("c", length(unique_group_display_labels)), collapse = ""), "@{}")
    cat(paste0("\\begin{tabular}{", col_format, "}\n"))
    cat("\\toprule\n")
    
    # Header Row 1: Variable | Group 1 Name | Group 2 Name | Group 3 Name
    header_row1 <- paste("Variable", paste(unique_group_display_labels, collapse = " & "), sep = " & ")
    cat(paste0(header_row1, " \\\\\n"))
    
    # Header Row 2: (empty) | Mean (SD) | Mean (SD) | Mean (SD)
    mean_sd_header <- paste(rep("Mean (SD)", length(unique_group_display_labels)), collapse = " & ")
    cat(paste("", mean_sd_header, sep = " & "), " \\\\\n")
    cat("\\midrule\n")
    
    # Data Rows
    for (i in 1:nrow(table_data)) {
      row_name <- table_data$variable_display[i]
      row_values_str <- c()
      for (group_lab in unique_group_display_labels) {
        mean_col_name <- paste0("mean_score_", group_lab)
        sd_col_name <- paste0("sd_score_", group_lab)
        
        # Check if columns exist (pivot_wider might not create them if all data for a group/var is NA)
        if (mean_col_name %in% colnames(table_data) && sd_col_name %in% colnames(table_data) &&
            !is.na(table_data[[mean_col_name]][i]) && !is.na(table_data[[sd_col_name]][i])) {
          
          val_str <- paste0(sprintf("%.3f", table_data[[mean_col_name]][i]), 
                            " (", sprintf("%.3f", table_data[[sd_col_name]][i]), ")")
        } else {
          val_str <- "NA (NA)" # Or just "-"
        }
        row_values_str <- c(row_values_str, val_str)
      }
      cat(paste(row_name, paste(row_values_str, collapse = " & "), sep = " & "), " \\\\\n")
    }
    
    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    
    # Table notes
    if (!is.null(latex_notes) && latex_notes != "") {
      cat("\\begin{tablenotes}\n")
      cat(paste0("  \\item[] \\textit{Notes:} ", latex_notes, "\n"))
      # Add more specific notes if needed using \item[] or \item[a]
      cat("\\end{tablenotes}\n")
    }
    
    cat("\\end{threeparttable}\n")
    cat("\\end{table}\n")
  })
  
  latex_string <- paste(latex_output, collapse = "\n")
  
  # Save to file if requested
  if (!is.null(save_to_file_path)) {
    # Ensure directory exists
    dir_name <- dirname(save_to_file_path)
    if (!dir.exists(dir_name) && dir_name != ".") {
      dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
    }
    writeLines(latex_string, save_to_file_path)
    cat(paste("\nLaTeX table saved to:", save_to_file_path, "\n"))
  }
  
  return(latex_string)
}
