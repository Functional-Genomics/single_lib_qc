#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

matrix_path <- args[1] # path to extended matrix

if (length(args)!=1) {
  cat("usage: <path_to_extended_matrix> \n")
  cat("ERROR: incorrect number of arguments\n");
  q(status=1);
}

library(data.table)
library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(RColorBrewer)

R_path <<- Sys.getenv("QC_R_DIR") # path to the R folder

source("create_stack_barplot.R") # function to create stacked barplots
source("transpose_profile.R") # function to output single QC profile vertically
source("check_classes_file.R") # function to check if "classes" file exits and, if not, create one
source("determine_profile_class.R") # function to determine the class of a given profile from the "classes" file mapping

suppressWarnings(dataset <- fread(matrix_path, na = c("NA", "")))
setkey(dataset, Prefix)

check_classes_file(R_path, dataset)
classes <<- fread(paste0(R_path, "/classes"))
setkey(classes, "Prefix")

config_table <- fread(paste0(R_path, "/shiny_plots_config"), na = c("NA", ""))
setkey(config_table, "Name")

ui <- navbarPage(
  title = "Work in progress",
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
                    plotlyOutput("fix_box_num")),
             column(width = 5, offset = 1,
                    plotlyOutput("fix_box_per"))
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
  tabPanel("Execution Time/Memory",
           tabsetPanel(
             tabPanel("Time",
                      fluidRow(
                        column(width = 5, 
                               plotlyOutput("box_study_time_sum"),
                               plotlyOutput("box_spic_time_sum"),
                               plotlyOutput("box_rs_time_sum")),
                        column(width = 5, offset = 1
                        )
                      )   
             ),
             tabPanel("Memory",
                      fluidRow(
                        column(width = 5, 
                               plotlyOutput("box_study_mem"),
                               plotlyOutput("box_spic_mem"),
                               plotlyOutput("box_rs_mem")),
                        column(width = 5, offset = 1
                        )
                      ) 
             )
           )
  )
)

server <- function(input, output) {
  # plot on the 1st (General Overview) tab
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
              type = "scatter", mode = "markers", text = Prefix, name = "Dots",
              source = "point_gen_box") %>%
        add_trace(data = plot_df, x = x, y = y,
                  type = "box", orientation = "h", name = "Boxes") %>%
        layout (xaxis = list(title = "",
                             tickangle = 45), 
                yaxis = list(title = "",
                             tickangle = 25),
                margin = list(b = 100,
                              l = 80)) 
    }
  })
  
  #------------------------------------------------------------------------------------
  # selector on the Fixed Plots tab
  output$fix_val <- renderUI({
    selectInput(inputId = "fix_val", label = "What value from this group?", 
                choices = unique(dataset[, get(input$fix_feat)]))
  })
  # preparing data for "numeric" PLOTLY box plot
  plot_data_num <- reactive({
    raw_data <- subset(dataset, unlist(dataset[, input$fix_feat, with = F]) %in% input$fix_val)
    
    plot_data_num <- melt(raw_data, id.vars = "Prefix", 
                          measure.vars = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), 
                          variable.name = "Feature", value.name = "Value")
    setkey(plot_data_num, Prefix)
  })
  # box plots on the Fixed Plots tab
  output$fix_box_num <- renderPlotly({
    config <- config_table["fix_box_num", ]
    y_names <- unique(gsub(".*_", "", plot_data_num()$Feature))
    plot_ly(plot_data_num(), x = Value, y = Feature,
            type = "scatter", mode = "markers", text = Prefix, name = "Dots",
            source = "point_fix_box") %>%
      add_trace(data = plot_data_num(), x = Value, y = Feature,
                type = "box", orientation = "h", name = "Boxes") %>%
      layout(yaxis = list(title = input$fix_val,
                          tickangle = 25,
                          tickmode = "array",
                          tickvals = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"),
                          ticktext = y_names,
                          type = config$Y_type,
                          range = c(config$Y_min, config$Y_max)),
             xaxis = list(title = "Value",
                          type = config$X_type,
                          range = c(config$X_min, config$X_max)),
             margin = list(l = 150))
  })
  # preparing data for "percentage" box plot
  plot_data_per <- reactive ({
    raw_data <- subset(dataset, unlist(dataset[, input$fix_feat, with = F]) %in% input$fix_val)
    raw_data <- raw_data[, c("Prefix",
                             "STATS.CSV_All_entries", "STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), with = F]
    
    plot_data_per <- raw_data[, "Prefix", with = F]
    
    for (feature in colnames(raw_data)[3:length(colnames(raw_data))]) {
      dt <- (raw_data[, feature, with = F] / raw_data[, "STATS.CSV_All_entries", with = F]) * 100
      plot_data_per <- bind_cols(plot_data_per, dt)
    }
    plot_data_per <- melt(plot_data_per, id.vars = "Prefix", 
                          measure.vars = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), 
                          variable.name = "Feature", value.name = "Value")
    setkey(plot_data_per, Prefix)
  })
  output$fix_box_per <- renderPlotly({
    config <- config_table["fix_box_per", ]
    y_names <- unique(gsub(".*_", "perc", plot_data_per()$Feature))
    plot_ly(plot_data_per(), x = Value, y = Feature,
            type = "scatter", mode = "markers", text = Prefix, name = "Dots",
            source = "point_fix_box") %>%
      add_trace(data = plot_data_per(), x = Value, y = Feature,
                type = "box", orientation = "h", name = "Boxes") %>%
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
  
  # scatterplots
  # preparing data for scatterplots
  scat_data <- reactive({
    scat_data <- subset(dataset, unlist(dataset[, input$fix_feat, with = F]) %in% input$fix_val)
  })
  output$fix_scat_rs_rmap <- renderPlotly({
    config <- config_table["fix_scat_rs_rmap", ]
    plot_ly(data = scat_data(), x = INFO_rs, y = (STATS.CSV_ReadsMapped / STATS.CSV_All_entries) * 100,
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
    plot_ly(data = scat_data(), x = INFO_rs, y = (STATS.CSV_ReadsSpliced / STATS.CSV_All_entries) * 100,
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
    write.table(classes, paste0(R_path, "/classes"), 
                append = F, quote = F, row.names = F)
  })
  output$sprof_cust_time <- renderPlotly({
    create_stack_barplot(input$prof_usr, dataset, "TIME")
  })
  output$sprof_cust_mem <- renderPlotly({
    create_stack_barplot(input$prof_usr, dataset, "MEMORY")
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
    create_stack_barplot(prefix_gen_box(), dataset, "TIME")
  })
  output$sprof_gen_box_mem <- renderPlotly({
    create_stack_barplot(prefix_gen_box(), dataset, "MEMORY")
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
    create_stack_barplot(prefix_gen_scat(), dataset, "TIME")
  })
  output$sprof_gen_scat_mem <- renderPlotly({
    create_stack_barplot(prefix_gen_scat(), dataset, "MEMORY")
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
    create_stack_barplot(prefix_fix_box(), dataset, "TIME")
  })
  output$sprof_fix_box_mem <- renderPlotly({
    create_stack_barplot(prefix_fix_box(), dataset, "MEMORY")
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
    create_stack_barplot(prefix_fix_scat(), dataset, "TIME")
  })
  output$sprof_fix_scat_mem <- renderPlotly({
    create_stack_barplot(prefix_fix_scat(), dataset, "MEMORY")
  })
  profile_fix_scat <- reactive({
    profile <- dataset[prefix_fix_scat(), ]
    return(profile)
  })
  output$single_prof_fix_scat <- renderTable({
    transpose_profile(profile_fix_scat())
  })
  
  # ------------------------------------------------------------------------------------
  # the 4th tab: fixed grouped plots
  box_data_time <- reactive({ 
    dataset[, c("Prefix", "INFO_rs", "INFO_nreads", "TIME_sum", "STUDY_ID", "ORGANISM"), with = F]
  })
  output$box_study_time_sum <- renderPlotly({
    plot_ly(box_data_time(), x = TIME_sum, y = STUDY_ID,
            color = STUDY_ID, type = "box", orientation = "h") %>%
      layout(xaxis = list(title = "Time (Summary), min"),
             yaxis = list(title = "Study ID"),
             margin = list(l = 250))
  })
  output$box_spic_time_sum <- renderPlotly({
    plot_ly(box_data_time(), x = TIME_sum, y = ORGANISM,
            color = ORGANISM, type = "box", orientation = "h") %>%
      layout(xaxis = list(title = "Time (Summary), min"),
             yaxis = list(title = "ORGANISM"),
             margin = list(l = 250))
  })
  output$box_rs_time_sum <- renderPlotly({
    plot_ly(box_data_time(), x = TIME_sum, y = INFO_rs,
            color = ORGANISM, type = "box", orientation = "h") %>%
      layout(xaxis = list(title = "Time (Summary), min"),
             yaxis = list(title = "Read Size, bases"),
             margin = list(l = 250))
  })
  box_data_mem <- reactive({ 
    dataset[, c("Prefix", "INFO_rs", "INFO_nreads", "TIME_iRAP_Mapping_memory_last", "STUDY_ID", "ORGANISM"), with = F]
  })
  output$box_study_mem <- renderPlotly({
    plot_ly(box_data_mem(), x = TIME_iRAP_Mapping_memory_last, y = STUDY_ID,
            color = STUDY_ID, type = "box", orientation = "h") %>%
      layout(xaxis = list(title = "Mapping Memory (last), MB"),
             yaxis = list(title = "Study ID"),
             margin = list(l = 200))
  })
  output$box_spic_mem <- renderPlotly({
    plot_ly(box_data_mem(), x = TIME_iRAP_Mapping_memory_last, y = ORGANISM,
            color = ORGANISM, type = "box", orientation = "h") %>%
      layout(xaxis = list(title = "Mapping Memory (last), MB"),
             yaxis = list(title = "ORGANISM"),
             margin = list(l = 250))
  })
  output$box_rs_mem <- renderPlotly({
    plot_ly(box_data_mem(), x = TIME_iRAP_Mapping_memory_last, y = INFO_rs,
            color = ORGANISM, type = "box", orientation = "h") %>%
      layout(xaxis = list(title = "Mapping Memory (last), MB"),
             yaxis = list(title = "Read Size, bases"),
             margin = list(l = 200))
  })
}

shinyApp(ui = ui, server = server)