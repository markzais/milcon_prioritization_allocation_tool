fluidPage(theme = "mystyle.css",
          tags$style("
                     body {
                     -moz-transform: scale(0.8, 0.8); /* Moz-browser */
                     zoom: 0.8; /* Other non-webkit browsers */
                     zoom: 80%; /* Webkit browsers */
                     }"),
          
          header <- dashboardHeader(
            title = "MILCON Prioritization and Allocation Tool (MPAT)",
            titleWidth = 500
          ),
          
          sidebar <- dashboardSidebar(
            width = 300, 
            useShinyjs(),
            # extendShinyjs(text = showModal),
            
            sidebarMenu(
              menuItem("Instructions", tabName = "instructions", icon = icon("info-circle", lib = "font-awesome")),
              menuItem("Optimization", tabName = "optimize", icon = icon("cog", lib = "font-awesome")),
              menuItem("Summary", tabName = "summary", icon = icon("bar-chart", lib = "font-awesome")),
              menuItem("Glossary of Input Data Fields", tabName = "glossary", icon = icon("question-circle", lib = "font-awesome"))
            ),

            sidebarMenu(id = "foot", class = "sidefooter", includeMarkdown(paste(path, "/", "www/footer.md",  sep = ""))
            ) #End sidebarPanel for Footer
          ),#End dashboardSidebar
          
          
          
          body <- dashboardBody(
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
            ),
            useShinyjs(),

            
            tabItems(
              tabItem(tabName = "instructions",
                      box(width = 12,
                          includeMarkdown("www/instructions.md")
                      ),
                      
                      box(width = 12,
                          HTML('<img src="construction2.png" height="400" width=100%>')
                      ),
                      
                      bsModal("instructionsModal", h3(" "), "", size = "large",
                              includeMarkdown("www/instructions.md")
                      )
              ),
              
              tabItem(tabName = "optimize",
                      
                      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                      
                      
                      fluidRow(
                        column(width = 5,
                               box(width = NULL, 
                                   title = "Budget Constraints", status = "primary", solidHeader = TRUE,
                                   sliderInput("budget1", label = "Budget - Year 1", min = 0, max = 1000000, value = 441100,
                                               step = 500, pre = "$", sep = ",", animate =TRUE),
                                   sliderInput("budget2", label = "Budget - Year 2", min = 0, max = 1000000, value = 344348,
                                               step = 500, pre = "$", sep = ",", animate =TRUE),
                                   sliderInput("budget3", label = "Budget - Year 3", min = 0, max = 1000000, value = 395012,
                                               step = 500, pre = "$", sep = ",", animate =TRUE),
                                   sliderInput("budget4", label = "Budget - Year 4", min = 0, max = 1000000, value = 375564,
                                               step = 500, pre = "$", sep = ",", animate =TRUE),
                                   sliderInput("budget5", label = "Budget - Year 5", min = 0, max = 1000000, value = 400036, # JGD 20190301 reduced from 442036
                                               step = 500, pre = "$", sep = ",", animate =TRUE)
                               )
                        ),
                        
                        column(width = 6,
                               box(width = NULL,
                                   title = tagList(shiny::icon("keyboard-o")," Parameters"), status = "primary", solidHeader = TRUE,
                                   splitLayout(
                                     numericInput("CBPLweight", label = "CBPL Weight",
                                                  value = 0.1, min = 0, max = 1, step = 0.1),
                                     numericInput("Rankweight", label = "Rank Weight",
                                                  value = 0.9, min = 0, max = 1, step = 0.1),
                                     numericInput("runtime", label = "Run Time (sec)", value = 60)
                                   )
                               ),
                               
                               
                               box(width = NULL,
                                   title = tagList(shiny::icon("cloud-upload")," Load Data and Parameters"), status = "primary", solidHeader = TRUE,
                                   
                                   # This style tag is used to group the import text and checkmark together
                                   tags$head(tags$style("#container * { display: inline;vertical-align:top}")),
                                   
                                   splitLayout(
                                     # Select a CSV file to upload
                                     fileInput("import", "Upload CSV File",
                                               multiple = FALSE,
                                               accept = c("text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv")),
                                     # Download a template CSV file for import format 
                                     div(downloadButton("downloadTemplate", "Download Template"),style =list("padding: 25px;"))
                                   ),
                                   splitLayout(
                                     cellWidths = c("54%", "15%","25%"),
                                     actionButton("load", 
                                                  # width = "150px", 
                                                  label = "Solve",  icon("ellipsis-h")
                                                  # ,  class = "btn btn-primary btn-md"
                                                  )
                                     # , shinyjs::hidden(actionButton("solve", 'Solve'))
                                     
                                     
                                     # ,
                                     # div(id="container",imageOutput("loadCheckmark"), textOutput("presolveMSG")),
                                     # div(id="container",imageOutput("solvedCheckmark"), textOutput("solvedMSG"))
                                   )
                                   ,br()
                                   # ,div(id="container",imageOutput("loadCheckmark"), textOutput("presolveMSG"))
                                   ,div(id="container",imageOutput("solvedCheckmark"), textOutput("solvedMSG"))
                                   ,br()
                                   , shinyjs::hidden(textInput('solution_name', 'Solution Name', value = '', width = '200px'))
                                   , shinyjs::hidden(actionButton("save", 'Save Solution'))
                               ),
                               
                               
                               tags$style(HTML('#reset{background-color:#DE5A52; border-color:#DE5A52}')),# Change color of reset button
                               tags$style(HTML('#reset:hover{background-color:#9D544F; border-color:#9D544F}')),
                               tags$style(type='text/css',"#download {width=100%; margin-top: 25px;}"),
                               
                               # # This creates a box for the File Output and Reset buttons
                               # box(width = NULL,
                               #     title = tagList(shiny::icon("cloud-download")," File Output"), status = "primary", solidHeader = TRUE,
                               #     splitLayout(
                               #       textInput("outputFile", label = "Name your exported file:", value = "MPAT_Solution"),
                               #       downloadButton("download", "Download"),
                               #       div(actionButton("reset", width = "120px", label = "Reset Model", class = "btn-primary"), style =list("padding: 25px;"))
                               #     )
                               # ),
                               
                               # This creates a box for the Status Reports of the optimization
                               box(width=NULL, 
                                   title = "Status Report", status = "warning", solidHeader = TRUE,
                                   splitLayout(
                                     valueBoxOutput("projectBox", width = NULL),
                                     valueBoxOutput("fundedBox", width = NULL)
                                   ))
                        )
                        
                      )
                      
              ),
              
              tabItem(tabName = 'summary',
                      fluidRow(
                      box(title = 'Controls', solidHeader = TRUE, collapsible = TRUE, width = 6, status = 'primary',
                      column(width = 5,
                        hidden(selectInput('choice', '' ,choices = NULL, width = '100%')))
                      ,column(width = 3,
                        box(solidHeader = TRUE
                        ,hidden(downloadButton('export','Export Selected'))
                        ,br()
                        ,hidden(downloadButton('exportAll','Export All'))))
                      ,column(width = 3
                        ,box(solidHeader = TRUE
                        ,hidden(actionButton('remove','Remove Selected'))
                        ,br()
                        ,hidden(actionButton('removeAll','Remove All'))))

                      ))
                      
                      ,fluidRow(
                        box(width = 12, status = 'primary',
                        tabsetPanel(id = 'tab', 
                        tabPanel("Project Statistics",
                                fluidRow(
                                column(width = 5
                                , hidden(plotOutput("descriptive1"))), 
                                column(width = 5
                                , hidden(plotOutput("descriptive2")))),
                                br(), ## JGD 20190219 added a space between the plots on the first and second rows.
                                br(),
                                fluidRow(
                                column(width = 5
                                , plotlyOutput("descriptive3")), 
                                column(width = 5
                                , plotlyOutput("descriptive4")))) ## JGD 20190219 changed from plotOutput to plotlyOutput.)
                        ,tabPanel("Table: MPAT Solution",
                                div(dataTableOutput("fullSolution"), style = list("font-size:90%")))
                        ,tabPanel("Table: Capability Sponsor Costs",
                                div(dataTableOutput("table2"), style = "font-size:100%"),
                                div(dataTableOutput("table3"), style = "font-size:100%"))
                        ,tabPanel("Chart: Capability Sponsors",
                                hidden(plotOutput('plot1', height = "600px")))
                        ,tabPanel("Chart: MIS Programs",
                                hidden(plotOutput('plot2', height = "600px")))
                        ,tabPanel("Chart: Project Counts",
                                plotlyOutput('plot3', height = "600px"))
                        ,tabPanel("Chart: Cost-Value",
                                plotlyOutput("costValuePlot", height = "800px")))
                      )
                      )),
                      
              tabItem(tabName = "glossary",
                      box(width = 12,
                          includeMarkdown("www/glossary.md")
                      ))
            ) # End tabItems
          ) # End dashboardBody
          
)

ui <- dashboardPage(header, sidebar, body, skin = "green")