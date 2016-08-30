#!/usr/bin/env Rscript

R_path<<- Sys.getenv("QC_R_DIR") # path to the R folder
config.path <- NULL
default.config <- paste0(R_path, "/shiny_plots_config")

args = commandArgs(trailingOnly=TRUE)

matrix_path <- args[1] # path to extended matrix
nargs <- length(args)

if (nargs<1&&nargs>2) {
  cat("usage: <path_to_extended_matrix> [path2config file]\n")
  cat("ERROR: incorrect number of arguments\n");
  q(status=1);
}

if (nargs==2) {
    config_path <- args[1] # path to extended matrix
} else {
    config.path <- default.config
}

library(dtplyr)
library(dplyr)
library(data.table)
library(shiny)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(bit64)
verbose <- TRUE


# these files reside in R folder
source(paste0(R_path, "/create_stack_barplot.R")) # function to create stacked barplots
source(paste0(R_path, "/create_stack_barplot_aggregated.R")) # function to create AGGREGATED stacked barplots
source(paste0(R_path, "/transpose_profile.R")) # function to output single QC profile vertically
source(paste0(R_path, "/check_classes_file.R")) # function to check if "classes" file exits and, if not, create one
source(paste0(R_path, "/determine_profile_class.R")) # function to determine the class of a given profile from the "classes" file mapping
source(paste0(R_path, "/create_stats_matrix.R")) # function to create matrix with means by id's
source(paste0(R_path, "/create_data_for_boxplot.R")) # function to create appropriate datasets for boxplots 



if ( ! file.exists(matrix_path) ) {
  cat("ERROR: file '", matrix_path, "' not found or access denied.\n")
  q(status=1)
}
cat("Loading matrix ",matrix_path,"...")
suppressWarnings(dataset <- fread(matrix_path, na = c("NA", "")))
cat("done.\n")


setkey(dataset, Prefix)

classes_file <<- paste0(matrix_path,"_classes")
check_classes_file(classes_file, dataset, verbose)
cat("Loading classes ", classes_file, "...")
classes <<- fread(classes_file)
cat("done.\n")
setkey(classes, "Prefix")

cat("Loading configuration ",config.path,"...")
config_table <- fread(config.path, na = c("NA", ""))
setkey(config_table, "Name")
cat("done.\n")

# precomputing mapping precentages
cat("Computing mapping percentages...")
dataset$NumberReads <- dataset$"STATS.CSV_ReadsUnmapped" + dataset$"STATS.CSV_ReadsMapped"
cols <- c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads")
for (feature in cols ) {
  newcol <- paste0(feature,"_perc")
  dataset[, newcol] <- round(as.numeric(unlist(dataset[, feature, with = F]))/dataset$NumberReads * 100, 2)
}
cat("Computing mapping percentages...done.")

ui <- navbarPage(
  title = "QC profile explorer",
  # 2nd tab: some fixed plots
  tabPanel("Fixed Plots",
           wellPanel(
             fluidRow(
               column(width = 5,
                      selectInput(inputId = "fix_feat", label = "What group to investigate?", 
                                  choices = c("ORGANISM", "STUDY_ID")
                      )
               ),
               column(width = 5, offset = 1,
                      uiOutput("fix_val"))
             )
           ),
           fluidRow(
             column(width = 5, 
                    plotlyOutput("fix_box_num"),
                    plotlyOutput("fix_box_per")),
             column(width = 5, offset = 1,
                    plotlyOutput("fix_box_num_total"),
                    plotlyOutput("fix_box_per_total"))
           ),
           fluidRow(
             column(width = 5, 
                    plotlyOutput("fix_scat_rs_rmap")),
             column(width = 5, offset = 1,
                    plotlyOutput("fix_scat_rs_rsl"))
           ),
           fluidRow(
             column(width = 5, 
                    plotlyOutput("fix_scat_nreads_mem")),
             column(width = 5, offset = 1,
                    plotlyOutput("fix_scat_rs_mem"))
           ),
           fluidRow(
             column(width = 5, 
                    plotlyOutput("fix_scat_nreads_time")),
             column(width = 5, offset = 1,
                    plotlyOutput("fix_scat_rs_time"))
           )
  ),
  # 1st tab: general plot with flexible axis
  tabPanel("General Overview",
           plotlyOutput("gen_plot"),
           fluidRow(
             column(width = 5, offset = 1,
                    selectInput(inputId = "x", label = "X", choices = colnames(dataset))
             ),
             column(width = 5, offset = 1,
                    selectInput(inputId = "y", label = "Y", choices = colnames(dataset))
             )
           ),
           fluidRow(
             column (width = 5, offset = 1,
                     selectInput(inputId = "type", label = "Plot Type", choices = c("Linear", "Box Plot"))
             )
           )
  ),
  # 3rd tab: information about single profiles 
  tabPanel("Single QC Profile",
           tabsetPanel(
             tabPanel("Custom profile",
                      fluidRow(
                        column(3, 
                               textInput("prof_usr", label = "Enter the prefix")),
                        column(3,
                               uiOutput("class")),
                        column(3,
                               actionButton("update_class", label = "Update Class")),
                        column(3,
                               actionButton("write_class", label = "Write Updated Classes"))),
                      column(width = 4,
                             tableOutput("single_prof_cust")),
                      column(width = 7,
                             plotlyOutput("sprof_cust_time"),
                             plotlyOutput("sprof_cust_mem"))
             ),
             tabPanel("Profile from General overview",
                      tabsetPanel(
                        tabPanel("Boxplots",
                                 column(width = 4, 
                                        tableOutput("single_prof_gen_box")),
                                 column(width = 7,
                                        plotlyOutput("sprof_gen_box_time"),
                                        plotlyOutput("sprof_gen_box_mem"))
                        ),
                        tabPanel("Scatterplots",
                                 column(width = 4, 
                                        tableOutput("single_prof_gen_scat")),
                                 column(width = 7,
                                        plotlyOutput("sprof_gen_scat_time"),
                                        plotlyOutput("sprof_gen_scat_mem"))
                        )
                      )
             ),
             tabPanel("Profile from Fixed Plots",
                      tabsetPanel(
                        tabPanel("Boxplots",
                                 column(width = 4,
                                        tableOutput("single_prof_fix_box")),
                                 column(width = 7,
                                        plotlyOutput("sprof_fix_box_time"),
                                        plotlyOutput("sprof_fix_box_mem"))
                        ),
                        tabPanel("Scatterplots",
                                 column(width = 4,
                                        tableOutput("single_prof_fix_scat")),
                                 column(width = 7,
                                        plotlyOutput("sprof_fix_scat_time"),
                                        plotlyOutput("sprof_fix_scat_mem"))
                        )
                      )
             )
           )
  ),
  
  # 4th tab: table with means by Study ID
  tabPanel("Table with means by Study ID",
           selectInput("means_matrix_type", label = "Choose the type of matrix ids", choices = c("ORGANISM", "STUDY_ID")),
           dataTableOutput("means_table")),
  
  # 5th tab: some general about Time/Memory
  tabPanel("Execution Time/Memory",
           tabsetPanel(
             tabPanel("Time",
                      fluidRow(
                        column(width = 5, 
                               plotlyOutput("total_box_study_time_sum"),
                               plotlyOutput("total_box_spic_time_sum"),
                               plotlyOutput("total_box_rs_time_sum")),
                        column(width = 5, offset = 1,
                               plotlyOutput("total_bar_time_sum_mean"),
                               plotlyOutput("total_bar_time_sum_median"),
                               plotlyOutput("total_bar_time_sum_sd")
                        )
                      )   
             ),
             tabPanel("Memory",
                      fluidRow(
                        column(width = 5, 
                               plotlyOutput("total_box_study_mem"),
                               plotlyOutput("total_box_spic_mem"),
                               plotlyOutput("total_box_rs_mem")),
                        column(width = 5, offset = 1,
                               plotlyOutput("total_bar_mem_max_mean"),
                               plotlyOutput("total_bar_mem_max_median"),
                               plotlyOutput("total_bar_mem_max_sd")
                        )
                      ) 
             )
           )
  )
)

server <- function(input, output) {
  # 1st tab: general overview plot
  output$gen_plot <- renderPlotly({
    plot_df <- data.table(dataset[, input$x, with = F],
                          dataset[, input$y, with = F],
                          dataset[, "Prefix", with = F])
    colnames(plot_df) <- c("x", "y", "Prefix")
    if (input$type == "Linear") {
      plot_ly(plot_df, x = x, y = y,
              type = "scatter", mode = "markers", text = Prefix, 
              source = "point_gen_scat")%>%
        layout (xaxis = list(title = "",
                             tickangle = 45), 
                yaxis = list(title = "",
                             tickangle = 25),
                margin = list(b = 100,
                              l = 80)) 
    } else if (input$type == "Box Plot") {
      plot_ly(data = plot_df, x = x, y = y,
              type = "scatter", mode = "markers", text = Prefix, name = "Points",
              source = "point_gen_box") %>%
        add_trace(data = plot_df, x = x, y = y,
                  type = "box", orientation = "h", name = "Distribution") %>%
        layout (xaxis = list(title = "",
                             tickangle = 45), 
                yaxis = list(title = "",
                             tickangle = 25),
                margin = list(b = 100,
                              l = 80)) 
    }
  })
  
  #------------------------------------------------------------------------------------
  # 2nd tab: some fixed plots
  # selector on the Fixed Plots tab
  output$fix_val <- renderUI({
    selectInput(inputId = "fix_val", label = "What value from this group?", 
                choices = unique(dataset[, get(input$fix_feat)]))
  })
  # preparing data for "numeric" PLOTLY box plot
  plot_data_num <- reactive({
    plot_data <- create_data_for_boxplot(dataset, input$fix_feat, input$fix_val, type = "num", total = F)
  })
  # preparing data for "percentage" box plot
  plot_data_per <- reactive({
    plot_data <- create_data_for_boxplot(dataset, input$fix_feat, input$fix_val, type = "per", total = F)
  })
  plot_data_num_total <- reactive({
    plot_data <- create_data_for_boxplot(dataset, input$fix_feat, NA, type = "num", total = T)
  })
  plot_data_per_total <- reactive({
    plot_data <- create_data_for_boxplot(dataset, input$fix_feat, NA, type = "per", total = T)
  })
  output$fix_box_num <- renderPlotly({
    config <- config_table["fix_box_num", ]
    y_names <- unique(gsub(".*_", "", plot_data_num()$Feature))
    plot_ly(plot_data_num(), x = Value, y = Feature,
            type = "scatter", mode = "markers", text = Prefix, name = "Points",
            source = "point_fix_box") %>%
      add_trace(data = plot_data_num(), x = Value, y = Feature,
                type = "box", orientation = "h", name = "Distribution") %>%
      layout(yaxis = list(title = input$fix_val,
                          tickangle = 25,
                          tickmode = "array",
                          tickvals = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"),
                          ticktext = y_names,
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)),
             xaxis = list(title = "Number of Reads",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             margin = list(l = 150))
  })
  output$fix_box_num_total <- renderPlotly({
    config <- config_table["fix_box_num_total", ]
    y_names <- unique(gsub(".*_", "", plot_data_num_total()$Feature))
    plot_ly(plot_data_num_total(), x = Value, y = Feature,
            type = "scatter", mode = "markers", text = plot_data_num_total()[[input$fix_feat]], name = "Points",
            source = "point_fix_box_total") %>%
      add_trace(data = plot_data_num_total(), x = Value, y = Feature,
                type = "box", orientation = "h", name = "Distribution") %>%
      layout(yaxis = list(title = paste("All", input$fix_feat),
                          tickangle = 25,
                          tickmode = "array",
                          tickvals = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"),
                          ticktext = y_names,
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)),
             xaxis = list(title = "Number of Reads",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             margin = list(l = 150))
  })
  output$fix_box_per <- renderPlotly({
    config <- config_table["fix_box_per", ]
    y_names <- unique(gsub(".*_", "perc", plot_data_per()$Feature))
    plot_ly(plot_data_per(), x = Value, y = Feature,
            type = "scatter", mode = "markers", text = Prefix, name = "Points",
            source = "point_fix_box") %>%
      add_trace(data = plot_data_per(), x = Value, y = Feature,
                type = "box", orientation = "h", name = "Distribution") %>%
      layout(yaxis = list(title = input$fix_val,
                          tickangle = 25,
                          tickmode = "array",
                          tickvals = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"),
                          ticktext = y_names,
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)),
             xaxis = list(title = "Percentage",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             margin = list(l = 150))
  })
  output$fix_box_per_total <- renderPlotly({
    config <- config_table["fix_box_per_total", ]
    y_names <- unique(gsub(".*_", "", plot_data_per_total()$Feature))
    plot_ly(plot_data_per_total(), x = Value, y = Feature,
            type = "scatter", mode = "markers", text = plot_data_per_total()[[input$fix_feat]], name = "Points",
            source = "point_fix_box_total") %>%
      add_trace(data = plot_data_per_total(), x = Value, y = Feature,
                type = "box", orientation = "h", name = "Distribution") %>%
      layout(yaxis = list(title = paste("All", input$fix_feat),
                          tickangle = 25,
                          tickmode = "array",
                          tickvals = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"),
                          ticktext = y_names,
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)),
             xaxis = list(title = "Percentage",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             margin = list(l = 150))
  })
  
  # scatterplots
  # preparing data for scatterplots
  scat_data <- reactive({
    scat_data <- subset(dataset, unlist(dataset[, input$fix_feat, with = F]) %in% input$fix_val)
  })
  output$fix_scat_rs_rmap <- renderPlotly({
    config <- config_table["fix_scat_rs_rmap", ]
    plot_ly(data = scat_data(), x = INFO_rs, y = STATS.CSV_ReadsMapped_perc,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix_scat") %>%
      layout(xaxis = list(title = "Read Size, bases",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             yaxis = list(title = "Mapped Reads, %",
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)))
  })
  output$fix_scat_rs_rsl <- renderPlotly({
    config <- config_table["fix_scat_rs_rsl", ]
    plot_ly(data = scat_data(), x = INFO_rs, y = STATS.CSV_ReadsSpliced_perc,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix_scat") %>%
      layout(xaxis = list(title = "Read Size, bases",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             yaxis = list(title = "Spliced Reads, %",
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)))
  })
  output$fix_scat_nreads_mem <- renderPlotly({
    config <- config_table["fix_scat_nreads_mem", ]
    plot_ly(data = scat_data(), x = INFO_nreads, y = TIME_iRAP_Mapping_memory_last,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix_scat") %>%
      layout(xaxis = list(title = "Number of reads",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             yaxis = list(title = "Mapping Memory, MB",
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)))
  })
  output$fix_scat_rs_mem <- renderPlotly({
    config <- config_table["fix_scat_rs_mem", ]
    plot_ly(data = scat_data(), x = INFO_rs, y = TIME_iRAP_Mapping_memory_last,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix_scat") %>%
      layout(xaxis = list(title = "Read Size, bases",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             yaxis = list(title = "Mapping Memory, MB",
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)))
  })
  output$fix_scat_nreads_time <- renderPlotly({
    config <- config_table["fix_scat_nreads_time", ]
    plot_ly(data = scat_data(), x = INFO_nreads, y = TIME_sum,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix_scat") %>%
      layout(xaxis = list(title = "Number of Reads",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             yaxis = list(title = "Time, min",
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)))
  })
  output$fix_scat_rs_time <- renderPlotly({
    config <- config_table["fix_scat_rs_time", ]
    plot_ly(data = scat_data(), x = INFO_rs, y = TIME_sum,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix_scat") %>%
      layout(xaxis = list(title = "Read Size",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             yaxis = list(title = "Time, min",
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)))
  })
  
  # ------------------------------------------------------------------------------------
  # the 3rd tab (single profile data)
  
  # data and plots of Custom Profile
  output$class <- renderUI({
    selectInput(inputId = "class", label = "Profile Quality", choices = c("Good", "Bad", "Unclear", "Unknown"), 
                selected = determine_profile_class(input$prof_usr, classes))
  })
  observeEvent(input$update_class, {
    if (is.na(classes[input$prof_usr, ]$Class) == F) {
      classes[input$prof_usr, ]$Class <<- input$class
    }
  })
  observeEvent(input$write_class, {
    write.classes(classes,classes_file)
  })
  output$sprof_cust_time <- renderPlotly({
    create_stack_barplot_sprof(input$prof_usr, dataset, "TIME")
  })
  output$sprof_cust_mem <- renderPlotly({
    create_stack_barplot_sprof(input$prof_usr, dataset, "MEMORY")
  })
  profile_cust <- reactive({
    profile <- dataset[input$prof_usr, ]
    return(profile)
  })
  output$single_prof_cust <- renderTable({
    transpose_profile(profile_cust())
  })
  
  
  # data and plots of profile from General Overview tab (boxplots)
  prefix_gen_box <- reactive({
    point_data <- event_data("plotly_click", source = "point_gen_box")
    if(is.null(point_data) == T) 
      return(NULL)
    prefix_gen_box <-  dataset$Prefix[point_data[[2]] + 1]
    return(prefix_gen_box)
  })
  output$sprof_gen_box_time <- renderPlotly({
    create_stack_barplot_sprof(prefix_gen_box(), dataset, "TIME")
  })
  output$sprof_gen_box_mem <- renderPlotly({
    create_stack_barplot_sprof(prefix_gen_box(), dataset, "MEMORY")
  })
  profile_gen_box <- reactive({
    profile <- dataset[prefix_gen_box(), ]
    return(profile)
  })
  output$single_prof_gen_box <- renderTable({
    transpose_profile(profile_gen_box())
  })
  # data and plots of profile from General Overview tab (scatterplots)
  prefix_gen_scat <- reactive({
    point_data <- event_data("plotly_click", source = "point_gen_scat")
    if(is.null(point_data) == T) 
      return(NULL)
    prefix_gen_scat <-  dataset$Prefix[point_data[[2]] + 1]
    return(prefix_gen_scat)
  })
  output$sprof_gen_scat_time <- renderPlotly({
    create_stack_barplot_sprof(prefix_gen_scat(), dataset, "TIME")
  })
  output$sprof_gen_scat_mem <- renderPlotly({
    create_stack_barplot_sprof(prefix_gen_scat(), dataset, "MEMORY")
  })
  profile_gen_scat <- reactive({
    profile <- dataset[prefix_gen_scat(), ]
    return(profile)
  })
  output$single_prof_gen_scat <- renderTable({
    transpose_profile(profile_gen_scat())
  })
  
  
  # data and plots of profile from Fixed Plots tab (boxplots)
  prefix_fix_box <- reactive({
    point_data <- event_data("plotly_click", source = "point_fix_box")
    if(is.null(point_data) == T) 
      return(NULL)
    prefix_fix_box <-  plot_data_num()$Prefix[point_data[[2]] + 1]
    return(prefix_fix_box)
  })
  output$sprof_fix_box_time <- renderPlotly({
    create_stack_barplot_sprof(prefix_fix_box(), dataset, "TIME")
  })
  output$sprof_fix_box_mem <- renderPlotly({
    create_stack_barplot_sprof(prefix_fix_box(), dataset, "MEMORY")
  })
  profile_fix_box <- reactive({
    profile <- dataset[prefix_fix_box(), ]
    return(profile)
  })
  output$single_prof_fix_box <- renderTable({
    transpose_profile(profile_fix_box())
  })
  # data and plots of profile from Fixed Plots tab (scatterplots)
  prefix_fix_scat <- reactive({
    point_data <- event_data("plotly_click", source = "point_fix_scat")
    if(is.null(point_data) == T)
      return(NULL)
    prefix_fix_scat <-  scat_data()$Prefix[point_data[[2]] + 1]
    return(prefix_fix_scat)
  })
  output$sprof_fix_scat_time <- renderPlotly({
    create_stack_barplot_sprof(prefix_fix_scat(), dataset, "TIME")
  })
  output$sprof_fix_scat_mem <- renderPlotly({
    create_stack_barplot_sprof(prefix_fix_scat(), dataset, "MEMORY")
  })
  profile_fix_scat <- reactive({
    profile <- dataset[prefix_fix_scat(), ]
    return(profile)
  })
  output$single_prof_fix_scat <- renderTable({
    transpose_profile(profile_fix_scat())
  })
  
  # ------------------------------------------------------------------------------------
  # the 4th tab: table with means by STUDY ID or ORGANISM
  id_fix_box_total <- reactive({
    point_data <- event_data("plotly_click", source = "point_fix_box_total")
    if(is.null(point_data) == T)
      return(NULL)
    data <- plot_data_num_total()[, input$fix_feat, with = F]
    id <-  unlist(data[point_data[[2]] + 1])
    return(id)
  })
  means_matrix <- reactive({
    if (!is.null(id_fix_box_total())){
      means_matrix <- create_stats_matrix(dataset, input$means_matrix_type, mean)
      pos <- grep(id_fix_box_total(),
                  unlist(means_matrix[, input$means_matrix_type, with = F]))
      means_matrix <- means_matrix[pos]
    } else {
      means_matrix <- create_stats_matrix(dataset, input$means_matrix_type, mean)
    }
  })
  output$means_table <- renderDataTable({
    means_matrix()
  })
  
  # ------------------------------------------------------------------------------------
  # the 5th tab: fixed grouped plots
  # Time subtab
  box_data_time <- reactive({ 
    dataset[, c("Prefix", "INFO_rs", "INFO_nreads", "TIME_sum", "STUDY_ID", "ORGANISM"), with = F]
  })
  output$total_box_study_time_sum <- renderPlotly({
    plot_ly(box_data_time(), x = TIME_sum, y = STUDY_ID,
            color = STUDY_ID, type = "box", orientation = "h",
            showlegend = F) %>%
      layout(xaxis = list(title = "Time (Summary), min"),
             yaxis = list(title = "Study ID"),
             margin = list(l = 250))
  })
  output$total_box_spic_time_sum <- renderPlotly({
    plot_ly(box_data_time(), x = TIME_sum, y = ORGANISM,
            color = ORGANISM, type = "box", orientation = "h") %>%
      layout(xaxis = list(title = "Time (Summary), min"),
             yaxis = list(title = "ORGANISM"),
             margin = list(l = 250))
  })
  output$total_box_rs_time_sum <- renderPlotly({
    plot_ly(box_data_time(), x = TIME_sum, y = INFO_rs,
            color = ORGANISM, type = "box", orientation = "h") %>%
      layout(xaxis = list(title = "Time (Summary), min"),
             yaxis = list(title = "Read Size, bases"),
             margin = list(l = 250))
  })
  output$total_bar_time_sum_mean <- renderPlotly({
    stats_matrix <- create_stats_matrix(dataset, "ORGANISM", mean)
    create_stack_barplot_agg(stats_matrix, "TIME", "sum", "mean")
  })
  output$total_bar_time_sum_median <- renderPlotly({
    stats_matrix <- create_stats_matrix(dataset, "ORGANISM", median)
    create_stack_barplot_agg(stats_matrix, "TIME", "sum", "median")
  })
  output$total_bar_time_sum_sd <- renderPlotly({
    stats_matrix <- create_stats_matrix(dataset, "ORGANISM", sd)
    create_stack_barplot_agg(stats_matrix, "TIME", "sum", "sd")
  })
  
  # Memory subtab
  box_data_mem <- reactive({ 
    dataset[, c("Prefix", "INFO_rs", "INFO_nreads", "TIME_iRAP_Mapping_memory_last", "STUDY_ID", "ORGANISM"), with = F]
  })
  output$total_box_study_mem <- renderPlotly({
    plot_ly(box_data_mem(), x = TIME_iRAP_Mapping_memory_last, y = STUDY_ID,
            color = STUDY_ID, type = "box", orientation = "h",
            showlegend = F) %>%
      layout(xaxis = list(title = "Mapping Memory (last), MB"),
             yaxis = list(title = "Study ID"),
             margin = list(l = 200))
  })
  output$total_box_spic_mem <- renderPlotly({
    plot_ly(box_data_mem(), x = TIME_iRAP_Mapping_memory_last, y = ORGANISM,
            color = ORGANISM, type = "box", orientation = "h") %>%
      layout(xaxis = list(title = "Mapping Memory (last), MB"),
             yaxis = list(title = "ORGANISM"),
             margin = list(l = 250))
  })
  output$total_box_rs_mem <- renderPlotly({
    plot_ly(box_data_mem(), x = TIME_iRAP_Mapping_memory_last, y = INFO_rs,
            color = ORGANISM, type = "box", orientation = "h") %>%
      layout(xaxis = list(title = "Mapping Memory (last), MB"),
             yaxis = list(title = "Read Size, bases"),
             margin = list(l = 200))
  })
  output$total_bar_mem_max_mean<- renderPlotly({
    stats_matrix <- create_stats_matrix(dataset, "ORGANISM", mean)
    create_stack_barplot_agg(stats_matrix, "MEMORY", "max", "mean")
  })
  output$total_bar_mem_max_median <- renderPlotly({
    stats_matrix <- create_stats_matrix(dataset, "ORGANISM", median)
    create_stack_barplot_agg(stats_matrix, "MEMORY", "max", "median")
  })
  output$total_bar_mem_max_sd <- renderPlotly({
    stats_matrix <- create_stats_matrix(dataset, "ORGANISM", sd)
    create_stack_barplot_agg(stats_matrix, "MEMORY", "max", "sd")
  })
}

shinyApp(ui = ui, server = server)
