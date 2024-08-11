# server.R
library(shiny)
library(readxl)
library(ggplot2)
library(extrafont)
library(sysfonts)
library(showtext)
library(patchwork)
library(here)

font_add_google("Montserrat")

# Function to generate a theme with dynamic font size
get_custom_theme <- function(font_size) {
  theme_bw(base_size = font_size) + 
    theme(
    aspect.ratio = 1/2,
    axis.text = element_text(size = font_size, color = "black"),
    axis.title = element_text(size = font_size),
    plot.title = element_text(size = font_size, face = "bold"),
    panel.grid = element_blank(),
    legend.text = element_text(size = font_size),
    legend.title = element_text(size = font_size),
    strip.text = element_text(size = font_size),
    
    strip.background = element_rect(fill = NA, colour = NA),
    legend.background = element_blank(),
    
    text = element_text(family = "Montserrat", size = font_size)
  )
}

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the Excel file
  df <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  # Render UI for selecting facet variable(s) if facet_wrap is selected
  output$facet_var_ui <- renderUI({
    req(input$facet_wrap)
    data <- df()
    
    selectInput("facet_var", "Select Facet Variable",
                choices = names(data), multiple = TRUE)
  })
  
  # Generate the forest plot
  output$forestPlot <- renderPlot({
    req(df())
    data <- df()
    
    font_size <- input$font_size
    print(paste("Font size:", font_size))  # Debug: print font size
    
    p <- ggplot(data, aes(x = log2FoldChange, y = Comparison, color = Comparison)) +
      geom_vline(xintercept = 0, linewidth = .3, linetype = 2, color = "gray35") +
      geom_errorbar(aes(xmin = log2FoldChange - lfcSE, xmax = log2FoldChange + lfcSE), width = .1, color = "gray35") +
      geom_point(size = 2) +
      geom_text(aes(x = 3, y = Comparison, label = paste0("P = ", padj)), size = 3, colour = "black", hjust = -0.5) +
      scale_y_discrete(limits = rev) +
      coord_fixed(xlim = c(input$xlim_min, input$xlim_max)) +
      labs(y = "", x = "Log2 fold change") +
      get_custom_theme(font_size)
    
    if (input$facet_wrap) {
      p <- p + facet_wrap(as.formula(paste("~", paste(input$facet_var, collapse = " + "))))
    } else {
      p <- p + facet_grid(Symbol ~ .)
    }
    
    print(p)
  })
  
  # Save the plot as a PDF
  output$savePlot <- downloadHandler(
    filename = function() {
      paste("forest_plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 8, height = 6)
      print(ggplot(df(), aes(x = log2FoldChange, y = Comparison, color = Comparison)) +
              geom_vline(xintercept = 0, linewidth = .3, linetype = 2, color = "gray35") +
              geom_errorbar(aes(xmin = log2FoldChange - lfcSE, xmax = log2FoldChange + lfcSE), width = .1, color = "gray35") +
              geom_point(size = 2) +
              geom_text(aes(x = 3, y = Comparison, label = paste0("P = ", padj)), size = 3, colour = "black", hjust = -0.5) +
              scale_y_discrete(limits = rev) +
              coord_fixed(xlim = c(input$xlim_min, input$xlim_max)) +
              labs(y = "", x = "Log2 fold change") +
              get_custom_theme(input$font_size) +
              (if (input$facet_wrap) {
                facet_wrap(as.formula(paste("~", paste(input$facet_var, collapse = " + "))))
              } else {
                facet_grid(Symbol ~ .)
              }))
      dev.off()
    }
  )
}
