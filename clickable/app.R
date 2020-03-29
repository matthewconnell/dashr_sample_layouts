# Layout example for DashR
# This layout includes two basic plots next to each other, two dropdown boxes, a title bar, and a sidebar
# Author: Matthew Connell
# Date: February 2020

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)

# Set an external stylesheet for CSS
app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# Use built-in mtcars dataset
df <- mtcars

# Set plot height and width
options(repr.plot.width = 10, repr.plot.height = 10)

# Define function that makes a basic histogram
# When making a dashboard, it's a good idea to set a default for your plot functions as the 
# plots can break easily if the user doesn't select a value for one of the axes, for example

histplot <- function(xaxis="cyl", num_bins=3) { 
  
  # Function creates a histogram showing count of cars with different numbers of cylinders
  #
  # --------
  # @param : 
  #        xaxis: the variable that will be the x-axis of the histogram
  #
  # --------
  # @Return :
  #        a histogram showing the count of cars in terms of the chosen x-axis
  #
  # --------
  # @Example : 
  #       histplot("cyl")
  #       histplot("mpg")
  # --------
  
  # Make the histogram object, assign it to 'hist'
  hist <- ggplot(data = df, aes(x=cyl)) +
    geom_histogram(bins = as.integer(num_bins)) +
    labs(y = "Number of cars", x = xaxis) +
    theme_bw(20)
  
  # Return the ggplot object
  ggplotly(hist)
}

# Scatterplot
scatterplot <- function(xaxis="qsec", 
                        yaxis="drat", 
                        selection=4) {
  
  # Function creates a scatterplot 
  #
  # --------
  # @param : 
  #
  # --------
  # @Return :
  #
  # --------
  # @Example : 
  #
  # --------
  
  # Selection starts as NULL, so set it to some value.
  if (is.null(selection)) {
    selection <- 4
  }

  #Return the ggplot
  df_filtered <- filter(df, cyl == selection)
  
  scatter_plot <- df_filtered %>% 
    ggplot(aes(x= !!sym(xaxis), y = !!sym(yaxis))) +
    geom_point() + 
    theme_bw(20) +
    labs(y=yaxis, x=xaxis)
  
  ggplotly(scatter_plot,
           width=500)
  
}

# Create instance of the histplot function and assign it to 'histogram'
histogram <- histplot()

# 
graph_hist <- dccGraph(id='histogram',
                       figure=histogram,
                       config = list('displaylogo' = FALSE))

scatter <- scatterplot()

graph_scatter <- dccGraph(id='scatter_plot',
                          figure=scatter,
                          config = list('displaylogo' = FALSE))


# Create a dropdown box for the xaxis
xaxis <- dccDropdown(
  id = "xaxis",
  
  # Set the options for the dropdown (all the columns of the df)
  options = map(
    names(df), function(x){
      list(label=x, value=x)
    }),
  
  # Assign a default value for the dropdown
  value = 'mpg'
)

# Do the same for the y-axis
yaxis <- dccDropdown(
  id = "yaxis",
  options = map(
    names(df), function(x){
      list(label=x, value=x)
    }),
  value = 'mpg'
)

# Create a slider for number of bins
num_bins <- dccSlider(
  id="num_bins",
  min=1,
  max=30,
  value=3,
  step=1,
  marks = as.list(
    setNames(
      seq(2, 30, 3),
      seq(2, 30, 3)
    )
  )
)

tmp <- 'click-data'

## Attribution and more good slider stuff: https://github.com/plotly/dash-sample-apps/blob/639ebbb57df5d261ff28d92ad2edc9dc092aa7c7/apps/dashr-svm/app.R#L96


# Start the layout
app$layout(
  
  # Title bar
  htmlDiv(
    list(
      htmlH1("MT Cars")
    ), style = list('columnCount'=1, 
                    'background-color'= 'black', 
                    'color'='white',
                    'text-align'='center')
  ),
  
  # SIDEBAR
  htmlDiv(
    list(
      htmlDiv(
        list(
          htmlDiv(
            list(
              # Dropdowns
              htmlP("Select a variable for the x-axis:"),
              xaxis,
              
              # Use htmlBr() for line breaks
              htmlBr(),
              
              htmlP("Select a variable for the y-axis:"),
              yaxis,
              
              htmlP("Choose the number of bins for your histogram:"),
              num_bins,
              
              htmlBr(),
              
              htmlPre(id='click-data', style=styles$pre),
              
              htmlBr(),
              
              # Some placeholder text
              htmlP("Motor Trend Car Road Tests
Description

The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models). Source: https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html")
              
            ), style = list('background-color'='lightgrey', 
                            'columnCount'=1, 
                            'white-space'='pre-line')
          ),
          htmlDiv(
            list(
              htmlDiv(
                list(
                  htmlDiv(
                    list(
                      
                      # Histogram here
                      graph_hist
                    ), style=list('width'='100%')
                  ),
                  htmlDiv(
                    list(
                      # Scatterplot here
                      graph_scatter
                    ), style=list('width'='100%')
                  )
                ), style = list('display'='flex')
              )
              
              
            )
          )
        ), style=list('display'='flex')
      )
    ), style = list('display'='flex')
  )
)

# app$callback is what allows the graphs to update after the user changes the slider or dropdown
app$callback(
  
  # Update the 'figure' property of the object with id 'histogram' 
  output=list(id = 'histogram', property = 'figure'),
  
  # with the 'value' property of the object with id 'num_bins' (the num_bins slider)
  params=list(input(id='num_bins', property = 'value')),
  
  # Update the histplot
  function(xaxis, num_bins) {
    histplot(xaxis, num_bins)
  }
)

# Print the clickData
app$callback(output = list(id = 'click-data', property = 'children'),
             
             params = list(input(id = 'histogram', property = 'clickData')),
             function(clickData) {
               return(clickData$points[[1]]$x)
             })

print(cdata)

# Update the 'figure' property of the object with id 'scatter' 
app$callback(
  
  output=list(id = 'scatter_plot', property = 'figure'),
  
  params=list(input(id='xaxis', property = 'value'), 
              input(id='yaxis', property = 'value'),
              input(id = 'histogram', property = 'clickData')),
  
  # Update the scatterplot
  function(xaxis, yaxis, selection) {
    scatterplot(xaxis, yaxis, selection$points[[1]]$x)
  }
)

app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))