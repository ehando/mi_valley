library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(shiny)
library(readxl)
rm(list = ls())

#setwd("C:/Users/Eirik/OneDrive/College/Senior/Data 332/mi_valley")

# Read in excel file
df <- read_excel("data/master_meta.xlsx") %>%
  
  # Selecting the variables I want to use  
  select(Watershed, Site, FBI, pH, Temp, DO_per, DO_ppm, SPC, TDS,
         PO4, NO3, NH3, BOD, TSS, Discharge, Chloride, Arsenic, Mercury, Lead,
                Wetland, Forest, Grass, Agricultural, Imper_Catch) %>%
  # Duck and Rock River Watersheds are just NA values so filter them out
  filter(!Watershed %in% c("Duck", "Rock River")) %>%
  #replace NA values with the column mean
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

## EDA

# Quick summary of data
summary(df)

# Correlation matrix
df_num <- df[, sapply(df, is.numeric)]
cor_matrix <- cor(df_num)
print(cor_matrix)

## Analysis and Shiny

# Define UI
ui <- fluidPage(
  titlePanel("Mississippi Valley Project"),
  sidebarLayout(
    sidebarPanel(
      # Select plot type
      selectInput("plotType", "Select Plot Type",
                  choices = c("Arsenic by Site", "Lead by Site",
                              "Scatter Plot with Phosphate and Catchment Levels",
                              "Phosphate by Site"),
                  selected = "Scatter Plot with Phosphate and Catchment Levels"),
      # Text output
      textOutput("text")
    ),
    mainPanel(
      # Place output plots here
      plotOutput("plot", width = '800px', height = '650px')
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load data
  selected_plot <- reactive({
    input$plotType
  })
  
  # Render selected plot
  output$plot <- renderPlot({
    plotType <- selected_plot()
    if (plotType == "Arsenic by Site") {
      ggplot(df, aes(x = Site, y = Arsenic, fill = Watershed)) +
        geom_col() +
        scale_fill_viridis(discrete = TRUE) +
        theme(axis.text.x =element_text(size=11, angle=90, vjust= 0.5, hjust=1),
              axis.text.y = element_text(size = 11),
              axis.title = element_text(size = 16),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              plot.title = element_text(size = 18, face = "bold"),
              plot.subtitle = element_text(size = 16, face = "italic"),
              plot.caption = element_text(size = 12),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        labs(title = "Arsenic Levels by Site and Watershed",
             x = "Site",
             y = "Arsenic Levels",
             fill = "Watershed")
      
    } else if (plotType == "Lead by Site") {
      ggplot(df, aes(x = Site, y = Lead, fill = Watershed)) +
        geom_col() +
        scale_fill_viridis(discrete = TRUE) +
        theme_minimal() +
        theme(axis.text.x =element_text(size=11, angle=90, vjust= 0.5, hjust=1),
              axis.text.y = element_text(size = 11),
              axis.title = element_text(size = 16),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              plot.title = element_text(size = 18, face = "bold"),
              plot.subtitle = element_text(size = 16, face = "italic"),
              plot.caption = element_text(size = 12),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        labs(title = "Lead Levels by Site and Watershed",
             x = "Site",
             y = "Lead Levels",
             fill = "Watershed")
      
    } else if (plotType == "Scatter Plot with Phosphate and Catchment Levels") {
      ggplot(df, aes(x = Imper_Catch, y = PO4)) +
        scale_fill_viridis(discrete = TRUE) +
        geom_point(aes(col = Watershed)) +
        geom_smooth(method = "loess", se = FALSE) +
        theme(axis.text = element_text(size=11),
              axis.title = element_text(size = 16),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              plot.title = element_text(size = 18, face = "bold"),
              plot.subtitle = element_text(size = 16, face = "italic"),
              plot.caption = element_text(size = 12),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        labs(title = "Scatter Plot with Phosphate and Catchment Levels",
             x = "Catchment Levels",
             y = "Phosphate Levels")
        
    } else if (plotType == "Phosphate by Site") {
      ggplot(df, aes(x = Site, y = PO4, label = sprintf("%.2f", PO4), color = Watershed)) +
        scale_fill_viridis(discrete = TRUE) +
        geom_point(stat = 'identity', size = 9) +
        geom_segment(aes(y = 0, x = Site, yend = PO4, xend = Site)) +
        geom_text(color = "white", size = 3.8) +
        theme(axis.text = element_text(size=11),
              axis.title = element_text(size = 16),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              plot.title = element_text(size = 18, face = "bold"),
              plot.subtitle = element_text(size = 16, face = "italic"),
              plot.caption = element_text(size = 12),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        labs(title = "Phosphate Levels by Site and Watershed",
             x = "Site",
             y = "Phosphate Levels (mg/L)",
             fill = "Watershed") +
        coord_flip()
    }
  })
  output$text <- renderText({
    plotType <- selected_plot()
    if (plotType == "Arsenic by Site") {
      "This chart shows the Arsenic levels at different sites in the area.
      The colors show which watershed the site belongs to. The sites in the Rock 
      Island watershed have much higher Arsenic values than the others, and is 
      something to explore in further detail."
    } else if (plotType == "Lead by Site") {
      "This chart shows the Lead levels at different sites in the area.
      The colors show which watershed the site belongs to. The sites in the Rock 
      Island watershed have much higher Lead values than the others, and is 
      something to explore in further detail."
    } else if (plotType == "Scatter Plot with Phosphate and Catchment Levels") {
      "This scatter plot shows the correlation between Phosphate and Catchment 
      Levels. There is a strong correlation between the two as the line moves up 
      and to the right."
    } else if (plotType == "Phosphate by Site") {
      "This lollipop chart shows the Phosphate values at different sites in the 
      area. The lollipops are colorcoded by Watershed to see patterns. The 
      Phosphate values at Rock Island sites stand out compared to the rest."
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

