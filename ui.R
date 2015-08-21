library(shiny)
require(dplyr)
require(shinyjs)
require(shinydashboard)






shinyUI(bootstrapPage(useShinyjs(),
                      
                      #                       # Hidden input boxes to save the variable to 
                      #                       HTML(' <input type="text" id="username" name="username" style="display: none;"> '), 
                      #                       includeScript("www/getusername.js"), 
                      #                       # include the js code 
                      #                       
                      #                       
                      
                      # Add custom CSS & Javascript;
                      tagList(
                        tags$head(
                          tags$link(rel="stylesheet", type="text/css",href="style.css"),
                          tags$script(type="text/javascript", src = "busy.js"),
                          tags$script(src = "message-handler.js")
                        )
                      ),
                      dashboardPage(skin = 'yellow', 
                                    title = "HomebrewR",
                                    dashboardHeader(title = 'HomebrewR'),
                                    dashboardSidebar(
                                      
                                      sidebarMenu(
                                        # Setting id makes input$tabs give the tabName of currently-selected tab
                                        id = "tabs",
                                        menuItem("Style", tabName = "Style", icon = icon("angle-right")),
                                        menuItem("Character", tabName = "Character", icon = icon("angle-right")),
                                        menuItem("Ingredients", icon = icon("angle-right"), tabName = "Ingredients")
                                        
                                      ),
                                      # hr(),
                                      # fluidRow(
                                      checkboxGroupInput('chk_type',label = 'Brew Type',
                                                         selected = 'all-grain',
                                                         choices = c('all-grain',
                                                                     'extract',
                                                                     'other',
                                                                     'partial mash')),
                                      # fluidRow(hr()),
                                      # fluidRow(column(width=12,
                                      hr(),
                                     div(class='info',
                                      p('Recipes by',a('beerrecipes.org',href='http://beerrecipes.org')),
                                      # p('Developed by',a('@davesteps',href='https://twitter.com/davesteps')),
                                      a(icon('github fa-2x'),href='https://github.com/davesteps/homebrewR'),
                                      a(icon('twitter fa-2x'),href='https://twitter.com/davesteps')
                                       )
                                      
                                      #                                                          
                                      #                                                                                                # fluidRow(
                                      #                                                                                                div(class='user',
                                      #                                                                                                    hr(),
                                      #                                                                                                    textOutput('username')
                                      #                                                                                                )
                                      #                                       # )
                                      
                                    ),
                                    dashboardBody(#div(tags$head(includeCSS("styles.css")),
                                      tabItems(
                                        tabItem("Style",
                                                box(width=3,
                                                    selectInput('slt_styles',
                                                                label = 'Choose beer styles to compare',
                                                                choices = styles,
                                                                selected=c('American Pale Ale','Belgian Golden Strong Ale',
                                                                           'Weizen/Weissbier','Robust Porter'),
                                                                multiple = T)
                                                ),
                                                tabBox(width=9,
                                                       # tabsetPanel(type = 'pills',
                                                       tabPanel('Bitterness&Colour',
                                                                h5('IBU Bitterness vs SRM colour by beer style'),
                                                                plotOutput('IBUSRMplot',height = 600)),

                                                       tabPanel('Grains',
                                                                h5('Fraction of recipes containing each grain type'),
                                                                plotOutput('maltPlot',height = 700)),
                                                       tabPanel('Hops',
                                                                h5('Fraction of recipes containing each hop type'),
                                                                plotOutput('hopsPlot',height = 700)),
                                                       
                                                       tabPanel('%ABV',
                                                                h5('Distribution of %abv values'),
                                                                plotOutput('ABVplot')),
                                                       
                                                       tabPanel('Gravity',
                                                                h5('Original and final Specific Gravity'),
                                                                plotOutput('GravityPlot',height = 600)),
                                                       
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
                                                       
                                                )
                                                
                                        ),
                                        tabItem("Character",
                                                box(width=3,
                                                    sliderInput(inputId = "sld_ABV",min = 2,
                                                                step = 0.1,max = 20,value = 5,
                                                                label = '%Alcohol by Volume'),
                                                    sliderInput(inputId = "sld_SRM",min = 0,
                                                                step = 0.5,max = 110,value = 10,
                                                                label = 'Colour (SRM)'),
                                                    sliderInput(inputId = "sld_IBU",min = 0,
                                                                step = 0.5,max = 280,value = 5,
                                                                label = 'Bitterness (IBU)')),
                                                box(width=9,
                                                    plotOutput('ABVdensplot',height = 300),
                                                    uiOutput('nearest')#,
                                                    # tableOutput('summaryTble2')
                                                )
                                        ),
                                        tabItem("Ingredients",
                                                box(
                                                  selectInput('slt_malts',
                                                              label = 'Malt/Grain/Extract',
                                                              choices = malts,multiple = T),
                                                  selectInput('slt_hops',
                                                              label = 'Hops',
                                                              choices = hops,multiple = T)
                                                  
                                                  
                                                ),
                                                box(h5('Number of recipes containing all specified ingredients'),
                                                    plotOutput("ingPlot")#)#,   
                                                    
                                                )
                                        )
                                      )
                                      
                                    )
                                    
                      ),
                      div(class = "busy", 
                          h4("working..."),
                          h2(HTML('<i class="fa fa-cog fa-spin"></i>'))#,
                          #       singleton(
                          #         tags$head(tags$script(src = "message-handler.js"))
                          #       )
                      )
                      
                      
)
)
