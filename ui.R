library(shiny)
require(dplyr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("HomebrewR"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(width = 3,

      
      radioButtons('rdb_search',label = 'Analyse Beers by:',
                   choices = c('Style','Character','Ingredients')),
      
      hr(),
      
      conditionalPanel("input.rdb_search == 'Ingredients'",
                       
                       selectInput('slt_malts',
                                   label = 'Malt/Grain/Extract',
                                   choices = malts,multiple = T),
                       selectInput('slt_hops',
                                   label = 'Hops',
                                   choices = hops,multiple = T)

                       ),      


      conditionalPanel("input.rdb_search == 'Style'",
                       
                       selectInput('slt_styles',
                                   label = 'Choose beer styles to compare',
                                   choices = styles,
                                   selected=c('American Pale Ale','Belgian Golden Strong Ale',
                                              'Weizen/Weissbier','Robust Porter'),
                                   multiple = T)
                       
                       
      ),
      conditionalPanel("input.rdb_search == 'Character'",
                       
                       sliderInput(inputId = "sld_ABV",min = 2,
                                   step = 0.1,max = 20,value = 5,
                                   label = '%Alcohol by Volume'),
                       sliderInput(inputId = "sld_SRM",min = 0,
                                   step = 0.5,max = 110,value = 10,
                                   label = 'Colour (SRM units)'),
                       sliderInput(inputId = "sld_IBU",min = 0,
                                   step = 0.5,max = 280,value = 5,
                                   label = 'IBU')
                       
      ),
      hr(),
      wellPanel(
        checkboxGroupInput('chk_type',label = 'Brew Type',
                           selected = 'all-grain',
                           choices = c('all-grain',
                                       'extract',
                                       'other',
                                       'partial mash'))
        ),
      hr(),
      p('Recipes by',a('beerrecipes.org',href='http://beerrecipes.org')),
      p('Developed by',a('@davesteps',href='https://twitter.com/davesteps'))

      
      
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel("input.rdb_search == 'Style'",
        tabsetPanel(type = 'pills',
                    tabPanel('Bitterness~Colour',
                             h5('IBU Bitterness vs SRM colour by beer style'),
                             plotOutput('IBUSRMplot',height = 600)),

                    tabPanel('Gravity',
                             h5('Original and final Specific Gravity'),
                             plotOutput('GravityPlot',height = 600)),

                    tabPanel('%ABV',
                             h5('Distribution of %abv values'),
                             plotOutput('ABVplot')),
                    
          tabPanel('Grains',
                   h5('Fraction of recipes containing each grain type'),
                   plotOutput('maltPlot',height = 700)),
          tabPanel('Hops',
                   h5('Fraction of recipes containing each hop type'),
                   plotOutput('hopsPlot',height = 700)),

          tabPanel('%ABV~OG',
                   h5('Orignial Gravity vs %abv '),
                   plotOutput('OGplot',height = 500)),
          tabPanel('Bitterness',
                   h5('IBU Bitterness values by style'),
                   plotOutput('IBUplot')),
          tabPanel('Colour',
                   h5('SRM colour values by beer style'),
                   plotOutput('SRMplot',height = 600)),
          
          tabPanel('Summary',
                   tableOutput('summaryTble'))
          #tabPanel('SRM~IBU',
           #        h5('IBU values by beer style'),
            #       plotOutput('SRMIBUplot',height=700)),

          
          #tabPanel('Data',dataTableOutput("df1"))
        )
      ),
       conditionalPanel("input.rdb_search == 'Ingredients'",
                        #tabsetPanel(type = 'pills',
                        #  
                        #  tabPanel('Filter',
                                   h5('Number of recipes containing all specified ingredients'),
                                   plotOutput("ingPlot")#)#,  
                         # tabPanel('Data',dataTableOutput("df2"))
                        #)
      ),
      conditionalPanel("input.rdb_search == 'Character'",
                       #tabsetPanel(type = 'pills',
                        # tabPanel('Summary',
                                  plotOutput('ABVdensplot',height = 300),
                                  h5('Top 10 nearest recipes based on alcohol, colour and bitterness'),
                                  
                                  tableOutput('summaryTble2')#)#,
                         #tabPanel('Data',dataTableOutput("df3"))
                         #)
      ) 
    )
  )
))