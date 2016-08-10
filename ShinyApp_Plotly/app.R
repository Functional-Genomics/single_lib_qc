library(data.table)
library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(gridExtra)

suppressWarnings(dataset <- fread(matrix_path, na = c("NA", "")))
setkey(dataset, Prefix)

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
  # 3rd tab: information about single profiles 
  tabPanel("Single QC Profile",
           tabsetPanel(
             tabPanel("Custom profile",
                      textInput("prof_usr", label = "Enter the prefix"),
                      column(width = 4,
                             tableOutput("single_prof_usr")),
                      column(width = 5, offset = 1,
                             plotlyOutput("bar_prof_usr_time"),
                             plotlyOutput("bar_prof_usr_mem"))
             ),
             tabPanel("Profile from General overview",
                      tabsetPanel(
                        tabPanel("Boxplots",
                                 column(width = 4, 
                                        tableOutput("single_prof_gen_box")),
                                 column(width = 5, offset = 1,
                                        plotlyOutput("bar_prof_gen_time_box"),
                                        plotlyOutput("bar_prof_gen_mem_box"))
                        ),
                        tabPanel("Scatterplots",
                                 column(width = 4, 
                                        tableOutput("single_prof_gen")),
                                 column(width = 5, offset = 1,
                                        plotlyOutput("bar_prof_gen_time"),
                                        plotlyOutput("bar_prof_gen_mem"))
                        )
                      )
             ),
             tabPanel("Profile from Fixed Plots",
                      tabsetPanel(
                        tabPanel("Boxplots",
                                 column(width = 4,
                                        tableOutput("single_prof_fix_box")),
                                 column(width = 5, offset = 1,
                                        plotlyOutput("bar_prof_fix_time_box"),
                                        plotlyOutput("bar_prof_fix_mem_box"))
                        ),
                        tabPanel("Scatterplots",
                                 column(width = 4,
                                        tableOutput("single_prof_fix")),
                                 column(width = 5, offset = 1,
                                        plotlyOutput("bar_prof_fix_time"),
                                        plotlyOutput("bar_prof_fix_mem"))
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
              type = "scatter", mode = "markers", text = Prefix, source = "point_gen")%>%
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
  # selector on the 2nd (Plotly plots) tab
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
    setkey(plot_data_num_ply, Prefix)
  })
  # box plots on the 2nd (Plotly) tab
  output$boxplot_num_plotly <- renderPlotly({
    y_names <- unique(gsub(".*_", "", plot_data_num_ply()$Feature))
    plot_ly(plot_data_num_ply(), x = Value, y = Feature,
            type = "scatter", mode = "markers", text = Prefix, name = "Dots",
            source = "point_fix_box") %>%
      add_trace(data = plot_data_num_ply(), x = Value, y = Feature,
                type = "box", orientation = "h", name = "Boxes") %>%
      layout(yaxis = list(title = input$fix_val_ply,
                          tickangle = 25,
                          tickmode = "array",
                          tickvals = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"),
                          ticktext = y_names),
             xaxis = list(title = "Value"),
             margin = list(l = 150))
  })
  # preparing data for "percentage" PLOTLY box plot
  plot_data_per_ply <- reactive ({
    raw_data <- subset(dataset, unlist(dataset[, input$fix_feat_ply, with = F]) %in% input$fix_val_ply)
    raw_data <- raw_data[, c("Prefix",
                             "STATS.CSV_All_entries", "STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), with = F]
    
    plot_data_per_ply <- raw_data[, "Prefix", with = F]
    
    for (feature in colnames(raw_data)[3:length(colnames(raw_data))]) {
      dt <- (raw_data[, feature, with = F] / raw_data[, "STATS.CSV_All_entries", with = F]) * 100
      plot_data_per_ply <- bind_cols(plot_data_per_ply, dt)
    }
    plot_data_per_ply <- melt(plot_data_per_ply, id.vars = "Prefix", 
                              measure.vars = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"), 
                              variable.name = "Feature", value.name = "Value")
    setkey(plot_data_per_ply, Prefix)
  })
  output$boxplot_per_plotly <- renderPlotly({
    y_names <- unique(gsub(".*_", "perc", plot_data_per_ply()$Feature))
    plot_ly(plot_data_per_ply(), x = Value, y = Feature,
            type = "scatter", mode = "markers", text = Prefix, name = "Dots",
            source = "point_fix_box") %>%
      add_trace(data = plot_data_per_ply(), x = Value, y = Feature,
                type = "box", orientation = "h", name = "Boxes") %>%
      layout(yaxis = list(title = input$fix_val_ply,
                          tickangle = 25,
                          tickmode = "array",
                          tickvals = c("STATS.CSV_ReadsSpliced", "STATS.CSV_ReadsUnmapped", "STATS.CSV_ReadsMapped", "STATS.CSV_UniquelyMappedReads", "STATS.CSV_MultimapReads"),
                          ticktext = y_names),
             xaxis = list(title = "Percentage"),
             margin = list(l = 150))
  })
  
  # scatterplots
  # preparing data for PLOTLY scatterplots
  scat_data_ply <- reactive({
    scat_data_ply <- subset(dataset, unlist(dataset[, input$fix_feat_ply, with = F]) %in% input$fix_val_ply)
  })
  output$scat_rs_rmap_ply <- renderPlotly({
    plot_ly(data = scat_data_ply(), x = INFO_rs, y = (STATS.CSV_ReadsMapped / STATS.CSV_All_entries) * 100,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix") %>%
      layout(xaxis = list(title = "Read Size, bases"),
             yaxis = list(title = "Mapped Reads, %"))
  })
  output$scat_rs_rsl_ply <- renderPlotly({
    plot_ly(data = scat_data_ply(), x = INFO_rs, y = (STATS.CSV_ReadsSpliced / STATS.CSV_All_entries) * 100,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix") %>%
      layout(xaxis = list(title = "Read Size, bases"),
             yaxis = list(title = "Spliced Reads, %"))
  })
  output$scat_nreads_mem_ply <- renderPlotly({
    plot_ly(data = scat_data_ply(), x = INFO_nreads, y = TIME_iRAP_Mapping_memory_last,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix") %>%
      layout(xaxis = list(title = "Number of reads"),
             yaxis = list(title = "Mapping Memory, MB"))
  })
  output$scat_rs_mem_ply <- renderPlotly({
    plot_ly(data = scat_data_ply(), x = INFO_rs, y = TIME_iRAP_Mapping_memory_last,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix") %>%
      layout(xaxis = list(title = "Read Size, bases"),
             yaxis = list(title = "Mapping Memory, MB"))
  })
  output$scat_nreads_time_ply <- renderPlotly({
    plot_ly(data = scat_data_ply(), x = INFO_nreads, y = TIME_sum,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix") %>%
      layout(xaxis = list(title = "Number of Reads"),
             yaxis = list(title = "Time, min"))
  })
  output$scat_rs_time_ply <- renderPlotly({
    plot_ly(data = scat_data_ply(), x = INFO_rs, y = TIME_sum,
            type = "scatter", mode = "markers", text = Prefix, source = "point_fix") %>%
      layout(xaxis = list(title = "Read Size"),
             yaxis = list(title = "Time, min"))
  })
  
  # ------------------------------------------------------------------------------------
  # the 3rd tab (single profile data)
  
  # data and plots of custom profile
  profile_usr <- reactive({
    if (is.null(input$prof_usr) == T) return(NULL)
    profile <- dataset[input$prof_usr, ]
    return(profile)
  })
  output$single_prof_usr <- renderTable({
    if (is.null(profile_usr()) == T) return(NULL)
    vector <- transpose(profile_usr())
    rownames(vector) <- colnames(profile_usr())
    colnames(vector) <- "Value"
    vector
  })
  output$bar_prof_usr_time <- renderPlotly({
    if(is.null(profile_usr()) == T) return(NULL) 
    
    profile <- profile_usr()
    profile <- profile[, grep("TIME_.*_last", colnames(profile), value = T), with = F]
    profile <- profile[, -grep("TIME_.*_memory_last", colnames(profile), value = T), with = F]
    
    plot_ly(data = profile, x = as.numeric(profile), y = colnames(profile),
            type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Time, min"),
             yaxis = list(title = ""),
             margin = list(l = 300))
  })
  output$bar_prof_usr_mem <- renderPlotly({
    if(is.null(profile_usr()) == T) return(NULL) 
    
    profile <- profile_usr()
    profile <- profile[, grep("TIME_.*_last", colnames(profile), value = T), with = F]
    profile <- profile[, grep("TIME_.*_memory_last", colnames(profile), value = T), with = F]
    
    plot_ly(data = profile, x = as.numeric(profile), y = colnames(profile),
            type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Memory, MB"),
             yaxis = list(title = ""),
             margin = list(l = 300))
  })
  
  # data and plots of profile from General Overview tab (boxplots)
  profile_gen_box <- reactive({
    point_data <- event_data("plotly_click", source = "point_gen_box")
    if(is.null(point_data) == T) 
      return(NULL)
    prefix <-  dataset$Prefix[point_data[[2]] + 1]
    profile <- dataset[prefix, ]
    return(profile)
  })
  output$single_prof_gen_box <- renderTable({
    if(is.null(profile_gen_box()) == T) return(NULL) 
    
    vector <- transpose(profile_gen_box())
    rownames(vector) <- colnames(profile_gen_box())
    colnames(vector) <- "Value"
    vector
  })
  output$bar_prof_gen_time_box <- renderPlotly({
    if(is.null(profile_gen_box()) == T) return(NULL) 
    
    profile <- profile_gen_box()
    profile <- profile[, grep("TIME_.*_last", colnames(profile), value = T), with = F]
    profile <- profile[, -grep("TIME_.*_memory_last", colnames(profile), value = T), with = F]
    
    plot_ly(data = profile, x = as.numeric(profile), y = colnames(profile),
            type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Time, min"),
             yaxis = list(title = ""),
             margin = list(l = 300))
  })
  output$bar_prof_gen_mem_box <- renderPlotly({
    if(is.null(profile_gen_box()) == T) return(NULL) 
    
    profile <- profile_gen_box()
    profile <- profile[, grep("TIME_.*_last", colnames(profile), value = T), with = F]
    profile <- profile[, grep("TIME_.*_memory_last", colnames(profile), value = T), with = F]
    
    plot_ly(data = profile, x = as.numeric(profile), y = colnames(profile),
            type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Memory, MB"),
             yaxis = list(title = ""),
             margin = list(l = 300))
  })
  # data and plots of profile from General Overview tab (scatterplots)
  profile_gen <- reactive({
    point_data <- event_data("plotly_click", source = "point_gen")
    if(is.null(point_data) == T) 
      return(NULL)
    prefix <-  dataset$Prefix[point_data[[2]] + 1]
    profile <- dataset[prefix, ]
    return(profile)
  })
  output$single_prof_gen <- renderTable({
    if(is.null(profile_gen()) == T) return(NULL) 
    
    vector <- transpose(profile_gen())
    rownames(vector) <- colnames(profile_gen())
    colnames(vector) <- "Value"
    vector
  })
  output$bar_prof_gen_time <- renderPlotly({
    if(is.null(profile_gen()) == T) return(NULL) 
    
    profile <- profile_gen()
    profile <- profile[, grep("TIME_.*_last", colnames(profile), value = T), with = F]
    profile <- profile[, -grep("TIME_.*_memory_last", colnames(profile), value = T), with = F]
    
    plot_ly(data = profile, x = as.numeric(profile), y = colnames(profile),
            type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Time, min"),
             yaxis = list(title = ""),
             margin = list(l = 300))
  })
  output$bar_prof_gen_mem <- renderPlotly({
    if(is.null(profile_gen()) == T) return(NULL) 
    
    profile <- profile_gen()
    profile <- profile[, grep("TIME_.*_last", colnames(profile), value = T), with = F]
    profile <- profile[, grep("TIME_.*_memory_last", colnames(profile), value = T), with = F]
    
    plot_ly(data = profile, x = as.numeric(profile), y = colnames(profile),
            type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Memory, MB"),
             yaxis = list(title = ""),
             margin = list(l = 300))
  })
  
  # data and plots of profile from Fixed Plots tab (boxplots)
  profile_fix_box <- reactive({
    point_data <- event_data("plotly_click", source = "point_fix_box")
    if(is.null(point_data) == T) 
      return(NULL)
    prefix <-  plot_data_num_ply()$Prefix[point_data[[2]] + 1]
    profile <- dataset[prefix, ]
    return(profile)
  })
  output$single_prof_fix_box <- renderTable({
    if(is.null(profile_fix_box()) == T) return(NULL) 
    
    vector <- transpose(profile_fix_box())
    rownames(vector) <- colnames(profile_fix_box())
    colnames(vector) <- "Value"
    vector
  })
  output$bar_prof_fix_time_box <- renderPlotly({
    if(is.null(profile_fix_box()) == T) return(NULL) 
    
    profile <- profile_fix_box()
    profile <- profile[, grep("TIME_.*_last", colnames(profile), value = T), with = F]
    profile <- profile[, -grep("TIME_.*_memory_last", colnames(profile), value = T), with = F]
    
    plot_ly(data = profile, x = as.numeric(profile), y = colnames(profile),
            type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Time, min"),
             yaxis = list(title = ""),
             margin = list(l = 300))
  })
  output$bar_prof_fix_mem_box <- renderPlotly({
    if(is.null(profile_fix_box()) == T) return(NULL) 
    
    profile <- profile_fix_box()
    profile <- profile[, grep("TIME_.*_last", colnames(profile), value = T), with = F]
    profile <- profile[, grep("TIME_.*_memory_last", colnames(profile), value = T), with = F]
    
    plot_ly(data = profile, x = as.numeric(profile), y = colnames(profile),
            type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Memory, MB"),
             yaxis = list(title = ""),
             margin = list(l = 300))
  })
  
  # data and plots of profile from Fixed Plots tab (scatterplots)
  profile_fix <- reactive({
    point_data <- event_data("plotly_click", source = "point_fix")
    if(is.null(point_data) == T) 
      return(NULL)
    prefix <-  scat_data_ply()$Prefix[point_data[[2]] + 1]
    profile <- dataset[prefix, ]
    return(profile)
  })
  output$single_prof_fix <- renderTable({
    if(is.null(profile_fix()) == T) return(NULL) 
    
    vector <- transpose(profile_fix())
    rownames(vector) <- colnames(profile_fix())
    colnames(vector) <- "Value"
    vector
  })
  output$bar_prof_fix_time <- renderPlotly({
    if(is.null(profile_fix()) == T) return(NULL) 
    
    profile <- profile_fix()
    profile <- profile[, grep("TIME_.*_last", colnames(profile), value = T), with = F]
    profile <- profile[, -grep("TIME_.*_memory_last", colnames(profile), value = T), with = F]
    
    plot_ly(data = profile, x = as.numeric(profile), y = colnames(profile),
            type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Time, min"),
             yaxis = list(title = ""),
             margin = list(l = 300))
  })
  output$bar_prof_fix_mem <- renderPlotly({
    if(is.null(profile_fix()) == T) return(NULL) 
    
    profile <- profile_fix()
    profile <- profile[, grep("TIME_.*_last", colnames(profile), value = T), with = F]
    profile <- profile[, grep("TIME_.*_memory_last", colnames(profile), value = T), with = F]
    
    plot_ly(data = profile, x = as.numeric(profile), y = colnames(profile),
            type = "bar", orientation = "h") %>%
      layout(xaxis = list(title = "Memory, MB"),
             yaxis = list(title = ""),
             margin = list(l = 300))
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
