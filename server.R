library(shiny)
library(readxl)
library(ggplot2)
library(extrafont)
library(showtext)
library(patchwork)
library(here)

# Function to generate a theme with dynamic font size and legend position
get_custom_theme <- function(font_size, legend_position, aspect_ratio) {
  theme_bw(base_size = font_size) + 
    theme(
      aspect.ratio = aspect_ratio,
      axis.text = element_text(size = font_size, color = "black"),
      axis.title = element_text(size = font_size),
      plot.title = element_text(size = font_size, face = "bold"),
      panel.grid = element_blank(),
      legend.text = element_text(size = font_size),
      legend.title = element_text(size = font_size),
      strip.text = element_text(size = font_size),
      legend.position = match.arg(legend_position, choices = c("none", "right", "left", "bottom", "inside", "top")),
      strip.background = element_rect(fill = NA, colour = NA),
      legend.background = element_blank(),
      text = element_text(family = "Helvetica", size = font_size)
    )
}

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the Excel file
  df <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  # Dynamically populate the choices for the yvar input based on the uploaded dataset
  observe({
    req(df())
    updateSelectInput(session, "yvar", choices = names(df()))
    updateSelectInput(session, "colorby", choices = names(df()))
    
  })
  
  # Render UI for selecting facet variable(s) if facet_wrap is selected
  output$facet_var_ui <- renderUI({
    req(input$facet_wrap)
    data <- df()
    
    selectInput("facet_var", "Select Facet Variable",
                choices = names(data), multiple = TRUE)
  })
  
  # Function to generate the plot
  create_plot <- function(data) {
    p <- ggplot(data, aes_string(x = "log2FoldChange", y = input$yvar, color = input$colorby)) +
      geom_vline(xintercept = 0, linewidth = .3, linetype = 2, color = "gray35") +
      geom_errorbar(aes(xmin = log2FoldChange - lfcSE, xmax = log2FoldChange + lfcSE), width = .1, color = "gray35") +
      geom_point(size = 2) +
      scale_y_discrete(limits = rev) +
      coord_fixed(xlim = c(input$xlim_min, input$xlim_max)) +
      labs(y = "", x = "Log2 fold change") +
      get_custom_theme(input$font_size, input$legend_position, input$aspect_ratio)
    
    if (input$show_pvalue) {
      p <- p + geom_text(aes(x = log2FoldChange + lfcSE * 1.1, label = paste0("P = ", padj)), size = 3, colour = "black", hjust = -0.5)
    }
    
    if (input$facet_wrap) {
      p <- p + facet_wrap(as.formula(paste("~", paste(input$facet_var, collapse = " + "))))
    } 
    
    p
  }
  
  # Generate the forest plot
  output$forestPlot <- renderPlot({
    req(df())
    data <- df()
    p <- create_plot(data)
    print(p)
  })
  
  # Save the plot as a PDF
  output$savePlot <- downloadHandler(
    filename = function() {
      paste("forest_plot.pdf")
    },
    content = function(file) {
      pdf(file, width = 8, height = 6)
      p <- create_plot(df())
      print(p)
      dev.off()
    }
  )
}
