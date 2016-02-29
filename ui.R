




shinyUI(bootstrapPage(useShinyjs(),
                      tagList(
                        tags$head(
                          tags$link(rel="stylesheet", type="text/css",href="style.css"),
                          tags$script(type="text/javascript", src = "busy.js"),
                          tags$script(src = "message-handler.js"),
                          includeScript("google-analytics.js")
                        )
                      ),
                      dashboardPage(skin = 'yellow', 
                                    title = "HomebrewR",
                                    dashboardHeader(title = HTML(paste(icon('beer'),'HomebrewR'))),
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
                                      # a(icon('twitter fa-2x'),href='https://twitter.com/davesteps'),
                                      p(
                                      HTML("<div style='float:center'>
                  <a href='https://twitter.com/share' 
                                           class='twitter-share-button' 
                                           align='middle' 
                                           data-url='www.davesteps.com/homebrewR/' 
                                           data-text='created by @davesteps using #rstats and #shiny: davesteps.com/homebrewR/' 
                                           data-size='large'>Tweet
                                           </a>
                                           <script>!function(d,s,id){
                                           var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
                                           if(!d.getElementById(id)){
                                           js=d.createElement(s);
                                           js.id=id;
                                           js.src=p+'://platform.twitter.com/widgets.js';
                                           fjs.parentNode.insertBefore(js,fjs);
                                           }
                                           }(document, 'script', 'twitter-wjs');
                                           </script>
                                           </div>")),
                                      p(a(icon('github fa-2x'),href='https://github.com/davesteps/homebrewR'))
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
                                                box(width=3,status = 'primary',
                                                    selectInput('slt_styles',
                                                                label = h3('Choose beer styles to compare:'),
                                                                choices = styles,
                                                                selected=c('American Pale Ale','Imperial IPA','Weizenbock',
                                                                           'Weizen/Weissbier','Baltic Porter'),
                                                                multiple = T),
                                                    
                                                    uiOutput('style_config'),
                                                    uiOutput('points')
                                                    
                                                ),
                                                tabBox(width=9,id = 'style_main',
                                                       # tabsetPanel(type = 'pills',
                                                       tabPanel('Bitterness&Colour',
                                                                h5("This plot shows the bitterness and colour 'signature' of each beer style"),
                                                               
                                                                plotOutput('IBUSRMplot',height = 600,click = "plot1_click"
                                                                           
                                                                           )
                                                               
                                                                ),

                                                       tabPanel('Grains',
                                                                h5('% of recipes containing each grain type'),
                                                                plotOutput('maltPlot',height = 650)),
                                                       tabPanel('Hops',
                                                                h5('% of recipes containing each hop type'),
                                                                plotOutput('hopsPlot',height = 650)),
                                                       
                                                       tabPanel('%ABV',
                                                                h5('Distribution of %abv values'),
                                                                plotOutput('ABVplot')),
                                                       
                                                       tabPanel('Gravity',
                                                                h5('Original and final Specific Gravity'),
                                                                plotOutput('GravityPlot',height = 600)),
                                                       
                                                       tabPanel('%ABV~OG',
                                                                h5('ABV ~ Orignial Gravity'),
                                                                plotOutput('OGplot',height = 600)),
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
                                                                step = 0.1,max = 20,value = 4,
                                                                label = '%Alcohol by Volume'),
                                                    sliderInput(inputId = "sld_SRM",min = 0,
                                                                step = 0.5,max = 110,value = 10,
                                                                label = 'Colour (SRM)'),
                                                    sliderInput(inputId = "sld_IBU",min = 0,
                                                                step = 0.5,max = 280,value = 100,
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
