#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

matrix_path <- args[1] # path to extended matrix

if (length(args)!=1) {
  cat("usage: <path_to_extended_matrix> \n")
  cat("ERROR: incorrect amount of arguments\n");
  q(status=1);
}

library(data.table)
library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)

if ( ! file.exits(matrix_path) ) {
    cat("ERROR: file",matrix_path," not found.\n")
    q(status=1)
}
cat("Loading matrix...")
dataset <- fread(matrix_path, na = c("NA", ""))
#dataset <- extended_profiles_matrix
cat("done.\n")

ui <- navbarPage(
  title = "QC profiler stats",
  
  # the 1st tab (specific fixed plots)
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
                    plotOutput("boxplot_num")),
             column(width = 5, offset = 1,
                    plotOutput("boxplot_per"))
           ),
           fluidRow(
             column(width = 5, 
                    plotOutput("scat_rs_rmap")),
             column(width = 5, offset = 1,
                    plotOutput("scat_rs_rsl"))
           ),
           fluidRow(
             column(width = 5, 
                    plotOutput("scat_nreads_mem")),
             column(width = 5, offset = 1,
                    plotOutput("scat_rs_mem"))
           ),
           fluidRow(
             column(width = 5, 
                    plotOutput("scat_nreads_time")),
             column(width = 5, offset = 1,
                    plotOutput("scat_rs_time"))
           )
           
  ),
  # the 2nd tab (general overview)
  tabPanel("General Overview",
           plotOutput("gen_plot", click = "click"),
           verbatimTextOutput("info"),
           
           hr(),
           
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
  #the 3rd tab (same plots but with Plotly)
  tabPanel("Plotly Fixed Plots",
           wellPanel(
             fluidRow(
               column(width = 5,
                      selectInput(inputId = "fix_feat_ply", label = "What group to investigate?", 
                                  choices = c("ORGANISM", "STUDY_ID")
                      )
               ),
               column(width = 5, offset = 1,
                      uiOutput("fix_val_ply"))
             )
           ),
           fluidRow(
             column(width = 5, 
                    plotlyOutput("boxplot_num_plotly")),
             column(width = 5, offset = 1,
                    plotlyOutput("boxplot_per_plotly"))
           ),
           fluidRow(
             column(width = 5, 
                    plotlyOutput("scat_rs_rmap_ply")),
             column(width = 5, offset = 1,
                    plotlyOutput("scat_rs_rsl_ply"))
           ),
           fluidRow(
             column(width = 5, 
                    plotlyOutput("scat_nreads_mem_ply")),
             column(width = 5, offset = 1,
                    plotlyOutput("scat_rs_mem_ply"))
           ),
           fluidRow(
             column(width = 5, 
                    plotlyOutput("scat_nreads_time_ply")),
             column(width = 5, offset = 1,
                    plotlyOutput("scat_rs_time_ply"))
           )
  ),
  # the fourth tab, information about single profile
  tabPanel("Single QC Profile",
           wellPanel(
             fluidRow(
               column(width = 5,
                      selectInput(inputId = "class", label = "What class do you want to assign?", 
                                  choices = c("Good", "Bad", "Unclear", "Unknown")
                      )
               )
             ),
             dataTableOutput("profiles_matrix")
           )
  )
)

server <- function(input, output) {
  # selector on the 1st (Fixed Plots) tab
  output$fix_val <- renderUI({
    selectInput(inputId = "fix_val", label = "What value from this group?", 
                choices = unique(dataset[, get(input$fix_feat)]))
  })
  # plots on the 1st (Fixed Plots) tab
  # preparing data for "numeric" box plot
  plot_data_num <- reactive({
    raw_data <- subset(dataset, unlist(dataset[, input$fix_feat, with = F]) %in% input$fix_val)
    plot_data_num <- melt(raw_data, id.vars = "Prefix", 
                          measure.vars = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), 
                          variable.name = "Feature", value.name = "Value")
  })
  # box plot with numeric values
  output$boxplot_num <- renderPlot({
    x_names <- unique(gsub(".*_", "", plot_data_num()$Feature))
    ggplot(plot_data_num(), aes(x = Feature, y = Value)) +
      geom_boxplot() +
      scale_x_discrete(input$fix_val, labels = x_names) +
      ggtitle ("Numerical Box Plot") +
      coord_flip()
  })
  # preparing data for "percentage" box plot
  plot_data_per <- reactive ({
    raw_data <- subset(dataset, unlist(dataset[, input$fix_feat, with = F]) %in% input$fix_val)
    raw_data <- raw_data[, c("Prefix",
                            "STATS.CSV_All_entries", 
                            "STATS.CSV_ReadsSpliced", 
                            "STATS.CSV_ReadsUnmapped", 
                            "STATS.CSV_ReadsMapped", 
                            "STATS.CSV_UniquelyMappedReads", 
                            "STATS.CSV_MultimapReads"), with = F]
    
    plot_data_per <- raw_data[, "Prefix", with = F]
    
    for (feature in colnames(raw_data)[3:length(colnames(raw_data))]) {
      dt <- (raw_data[, feature, with = F] / raw_data[, "STATS.CSV_All_entries", with = F]) * 100
      plot_data_per <- bind_cols(plot_data_per, dt)
    }
    plot_data_per <- melt(plot_data_per, id.vars = "Prefix", 
                          measure.vars = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), 
                          variable.name = "Feature", value.name = "Value")
  })
  # box plot with percentage values
  output$boxplot_per <- renderPlot({
    x_names <- unique(gsub(".*_", "perc", plot_data_per()$Feature))
    ggplot(plot_data_per(), aes(x = Feature, y = Value)) +
      geom_boxplot() + 
      scale_x_discrete(input$fix_val, labels = x_names) +
      labs(y = "Percentage") +
      ggtitle ("Percentage Box Plot") +
      coord_flip()
  })
  # preparing data for scatterplots
  scat_data <- reactive({
    scat_data <- subset(dataset, unlist(dataset[, input$fix_feat, with = F]) %in% input$fix_val)
  })
  # various scatterplots
  output$scat_rs_rmap <- renderPlot ({
    ggplot(scat_data(), aes(x = INFO_rs, y = (STATS.CSV_ReadsMapped / STATS.CSV_All_entries) * 100 )) +
      geom_point() + 
      labs(x = "Read Size, bases", y = "Mapped Reads Percentage")
  })
  output$scat_rs_rsl <- renderPlot ({
    ggplot(scat_data(), aes(x = INFO_rs, y = (STATS.CSV_ReadsSpliced / STATS.CSV_All_entries) * 100 )) +
      geom_point() + 
      labs(x = "Read Size, bases", y = "Sliced Reads Percentage")
  })
  output$scat_nreads_mem <- renderPlot ({
    ggplot(scat_data(), aes(x = INFO_nreads, y = TIME_iRAP_Mapping_memory_sum)) +
      geom_point() +
      labs(x = "Number of reads", y = "Mapping Memory, MB")
  })
  output$scat_rs_mem <- renderPlot ({
    ggplot(scat_data(), aes(x = INFO_rs, y = TIME_iRAP_Mapping_memory_sum)) +
      geom_point() +
      labs(x = "Read size, bases", y = "Mapping Memory, MB")
  })
  output$scat_nreads_time <- renderPlot ({
    ggplot(scat_data(), aes(x = INFO_nreads, y = TIME_iRAP_Mapping_sum)) +
      geom_point() +
      labs(x = "Number of reads", y = "Time, min")
  })
  output$scat_rs_time <- renderPlot ({
    ggplot(scat_data(), aes(x = INFO_rs, y = TIME_iRAP_Mapping_sum)) +
      geom_point() +
      labs(x = "Read size, bases", y = "Time, min")
  })
  #------------------------------------------------------------------------------------
  #plot on the 2nd (General Overview) tab
  output$gen_plot <- renderPlot({
    if (input$type == "Linear") {
      ggplot(dataset, aes_string(x = input$x, 
                                 y = input$y)) +
        geom_point() +
        labs(x = input$x, y = input$y) +
        ggtitle(paste(input$y, "from", input$x)) +
        scale_x_discrete()
      
    } else if (input$type == "Box Plot") {
      ggplot(dataset, aes_string(x = input$x, 
                                 y = input$y)) + 
        geom_boxplot(fill = "#4271AE", color = "#1F3552", alpha = 0.7) + 
        labs(x = input$x, y = input$y) +
        ggtitle(paste(input$y, "from", input$x)) + 
        coord_flip()
    }
  })
  # function to output cursor coordinates
  output$info <- renderText ({
    paste0("x = ", input$click$x, "\ny = ", input$click$y)
  })
  #------------------------------------------------------------------------------------
  # selector on the 3rd (Plotly) tab
  output$fix_val_ply <- renderUI({
    selectInput(inputId = "fix_val_ply", label = "What value from this group?", 
                choices = unique(dataset[, get(input$fix_feat_ply)]))
  })
  # preparing data for "numeric" PLOTLY box plot
  plot_data_num_ply <- reactive({
    raw_data <- subset(dataset, unlist(dataset[, input$fix_feat_ply, with = F]) %in% input$fix_val_ply)
    plot_data_num_ply <- melt(raw_data, id.vars = "Prefix", 
                          measure.vars = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), 
                          variable.name = "Feature", value.name = "Value")
  })
  #plotly plots on the 3rd (Plotly) tab
  output$boxplot_num_plotly <- renderPlotly({
    x_names <- unique(gsub(".*_", "", plot_data_num_ply()$Feature))
    bp <- ggplot(plot_data_num_ply(), aes(x = Feature, y = Value)) +
      geom_boxplot() +
      scale_x_discrete(input$fix_val_ply, labels = x_names) +
      coord_flip()
    (bp_plotly <- ggplotly(bp))
  })
  # preparing data for "percentage" PLOTLY box plot
  plot_data_per_ply <- reactive ({
    raw_data <- subset(dataset, unlist(dataset[, input$fix_feat_ply, with = F]) %in% input$fix_val_ply)
    raw_data <- raw_data[, c("Prefix",
                             "STATS.CSV_All_entries", 
                             "STATS.CSV_ReadsSpliced", 
                             "STATS.CSV_ReadsUnmapped", 
                             "STATS.CSV_ReadsMapped", 
                             "STATS.CSV_UniquelyMappedReads", 
                             "STATS.CSV_MultimapReads"), with = F]
    
    plot_data_per_ply <- raw_data[, "Prefix", with = F]
    
    for (feature in colnames(raw_data)[3:length(colnames(raw_data))]) {
      dt <- (raw_data[, feature, with = F] / raw_data[, "STATS.CSV_All_entries", with = F]) * 100
      plot_data_per_ply <- bind_cols(plot_data_per_ply, dt)
    }
    plot_data_per_ply <- melt(plot_data_per_ply, id.vars = "Prefix", 
                          measure.vars = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), 
                          variable.name = "Feature", value.name = "Value")
  })
  # PLOTLY box plot with percentage values
  output$boxplot_per_plotly <- renderPlotly({
    x_names <- unique(gsub(".*_", "perc", plot_data_per_ply()$Feature))
    bp <- ggplot(plot_data_per_ply(), aes(x = Feature, y = Value)) +
      geom_boxplot() + 
      scale_x_discrete(input$fix_val_ply, labels = x_names) +
      labs(y = "Percentage") +
      coord_flip()
    (bp_plotly <- ggplotly(bp))
  })
  # preparing data for PLOTLY scatterplots
  scat_data_ply <- reactive({
    scat_data_ply <- subset(dataset, unlist(dataset[, input$fix_feat_ply, with = F]) %in% input$fix_val_ply)
  })
  # various PLOTLY scatterplots
  output$scat_rs_rmap_ply <- renderPlotly({
    sp <- ggplot(scat_data_ply(), aes(x = INFO_rs, y = (STATS.CSV_ReadsMapped / STATS.CSV_All_entries) * 100 )) +
      geom_point() + 
      labs(x = "Read Size, bases", y = "Mapped Reads Percentage")
    (sp_ply <- ggplotly (sp))
  })
  output$scat_rs_rsl_ply <- renderPlotly({
    ggplot(scat_data_ply(), aes(x = INFO_rs, y = (STATS.CSV_ReadsSpliced / STATS.CSV_All_entries) * 100 )) +
      geom_point() + 
      labs(x = "Read Size, bases", y = "Sliced Reads Percentage")
  })
  output$scat_nreads_mem_ply <- renderPlotly({
    ggplot(scat_data_ply(), aes(x = INFO_nreads, y = TIME_iRAP_Mapping_memory_sum)) +
      geom_point() +
      labs(x = "Number of reads", y = "Mapping Memory, MB")
  })
  output$scat_rs_mem_ply <- renderPlotly({
    ggplot(scat_data_ply(), aes(x = INFO_rs, y = TIME_iRAP_Mapping_memory_sum)) +
      geom_point() +
      labs(x = "Read size, bases", y = "Mapping Memory, MB")
  })
  output$scat_nreads_time_ply <- renderPlotly({
    ggplot(scat_data_ply(), aes(x = INFO_nreads, y = TIME_iRAP_Mapping_sum)) +
      geom_point() +
      labs(x = "Number of reads", y = "Time, min")
  })
  output$scat_rs_time_ply <- renderPlotly({
    ggplot(scat_data_ply(), aes(x = INFO_rs, y = TIME_iRAP_Mapping_sum)) +
      geom_point() +
      labs(x = "Read size, bases", y = "Time, min")
  })
  #------------------------------------------------------------------------------------
  #the 4th tab (profiles)
  output$profiles_matrix <- renderDataTable({
    dataset
  }, options = list(orderClasses = T))
  
}

shinyApp(ui = ui, server = server)
