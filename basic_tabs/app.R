# Layout example for DashR
# This layout includes two basic plots on separate tabs, with two dropdown boxes and a title bar
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

histplot <- function(xaxis="mpg") { 
  
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
  hist <- ggplot(data = df, aes(x=df[[xaxis]])) +
    geom_histogram(bins = 30) +
    labs(y = "Number of cars", x = xaxis) +
    theme_bw(20)
  
  # Return the ggplot object
  ggplotly(hist)
}

scatterplot <- function(xaxis="mpg", yaxis="mpg") {
  
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
  
  
  #Return the ggplot
  scatter_plot <- df %>% 
    ggplot(aes(x= df[[xaxis]], y = df[[yaxis]])) +
    geom_point() + 
    geom_smooth(se=FALSE) +
    theme_bw(20) +
    labs(y=yaxis, x=xaxis)
  
  ggplotly(scatter_plot)
  
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


# Start the layout
app$layout(
  
  # TITLE BAR
  htmlDiv(
    list(
      htmlH1("MT Cars")
    ), style = list('columnCount'=1, 
                    'background-color'= 'black', 
                    'color'='white',
                    'text-align'='center')
  ),
  
  # MAIN AREA
  htmlDiv(
    list(
          # TABS
          dccTabs(id="tabs", value='tab-1', children=list(
            dccTab(label='Histogram', value='tab-1'),
            dccTab(label='Scatterplot', value='tab-2')
            )),
          htmlDiv(id='tabs-content')
          )
      ),
  htmlDiv(
    list(
      htmlH2("Motor Trend Car Road Tests"),
      htmlH3("Description:"),
      htmlP("The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models). Source: https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html")
    ), style = list('background-color'='black',
                    'color'='white',
                    'font-family'='Courier',
                    'text-align'='center')
  )
    )

app$callback(
  
  output = list(id = 'tabs-content', property = 'children'),
  
  params = list(input(id='tabs', 'value')),

  render_content <- function(tab) {
    if (tab == 'tab-1') {
      htmlDiv(
        list(
          # DROPDOWNS
          htmlDiv(
            list(
              htmlDiv(
                list(
                  htmlP("Select a variable for the x-axis:"),
                  xaxis
                )
              )
          
        ), style = list('display'='flex',
                        'white-space'='pre-line')
      ),
    htmlDiv(
      list(
        # Histogram here
        graph_hist
            )
          )
        )
      )
      }
  
    else if (tab == 'tab-2') {
      htmlDiv(
        list(
          
          # DROPDOWNS
          htmlDiv(
            list(
              htmlDiv(
                list(
                  htmlP("Select a variable for the x-axis:"),
                  xaxis,
                  htmlP("Select a variable for the y-axis:"),
                  yaxis
            )
          )
        ), style = list('display'='flex',
                        'justify-content'='flex-start',
                        'white-space'='pre-line')
      ),
      htmlDiv(
            list(
              # Scatterplot here
              graph_scatter
              )
          )
        )
      )
    }
    }
  )

# app$callback is what allows the graphs to update after the user changes the slider or dropdown
app$callback(
  
  # Update the 'figure' property of the object with id 'histogram' 
  output=list(id = 'histogram', property = 'figure'),
  
  # with the 'value' property of the object with id 'xaxis' (the x-axis dropdown)
  params=list(input(id = 'xaxis', property = 'value')),
  
  # Update the histplot
  function(xaxis) {
    histplot(xaxis)
  }
)


app$callback(
  
  # Update the 'figure' property of the object with id 'scatter' 
  output=list(id = 'scatter_plot', property = 'figure'),
  
  params=list(input(id = 'xaxis', property = 'value'), input(id='yaxis', property = 'value')),
  
  # Update the histplot
  function(xaxis, yaxis) {
    scatterplot(xaxis, yaxis)
  }
)


app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))