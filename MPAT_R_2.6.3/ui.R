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
            # useShinyjs(),
            # extendShinyjs(text = showModal),
            
            sidebarMenu(
              menuItem("Instructions", tabName = "instructions", icon = icon("info-circle", lib = "font-awesome")),
              menuItem("Optimization", tabName = "optimize", icon = icon("cog", lib = "font-awesome")),
              menuItem("Project Statistics", tabName = "projects", icon = icon("bar-chart", lib = "font-awesome")),
              menuItem("Table: MPAT Solution", tabName = "datatable", icon = icon("table", lib = "font-awesome")),
              menuItem("Table: Capability Sponsor Costs", tabName = "pom_sponsor_costs", icon = icon("table", lib = "font-awesome")),
              menuItem("Chart: Capability Sponsors", tabName = "sponsor_chart", icon = icon("bar-chart", lib = "font-awesome")),
              menuItem("Chart: MIS Programs", tabName = "mis_programs", icon = icon("bar-chart", lib = "font-awesome")),
              menuItem("Chart: Project Counts", tabName = "project_counts", icon = icon("bar-chart", lib = "font-awesome")),
              menuItem("Chart: Cost-Value", tabName = "cost_value_plot", icon = icon("bar-chart", lib = "font-awesome")),
              menuItem("Summary Statistics", tabName = "statistics", icon = icon("file-text", lib = "font-awesome")),
              menuItem("Glossary of Input Data Fields", tabName = "glossary", icon = icon("question-circle", lib = "font-awesome"))
            ),

            sidebarMenu(id = "foot", class = "sidefooter", includeMarkdown(paste(path, "/", "www/footer.md",  sep = ""))
            ) #End sidebarPanel for Footer
            
          ),#End dashboardSidebar
          
          
          
          body <- dashboardBody(
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
            ),
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
                                   splitLayout(cellWidths = c("35%", "25%", "40%"),
                                     actionButton("load", width = "150px", label = "Load Parameters",  icon("ellipsis-h"),  class = "btn btn-primary btn-md"),
                                     div(id="container",imageOutput("loadCheckmark"), textOutput("presolveMSG")),
                                     div(id="container",imageOutput("solvedCheckmark"), textOutput("solvedMSG"))
                                   )
                               ),
                               
                               
                               tags$style(HTML('#reset{background-color:#DE5A52; border-color:#DE5A52}')),# Change color of reset button
                               tags$style(HTML('#reset:hover{background-color:#9D544F; border-color:#9D544F}')),
                               tags$style(type='text/css',"#download {width=100%; margin-top: 25px;}"),
                               
                               # This creates a box for the File Output and Reset buttons
                               box(width = NULL,
                                   title = tagList(shiny::icon("cloud-download")," File Output"), status = "primary", solidHeader = TRUE,
                                   splitLayout(
                                     textInput("outputFile", label = "Name your exported file:", value = "MPAT_Solution"),
                                     downloadButton("download", "Download"),
                                     div(actionButton("reset", width = "120px", label = "Reset Model", class = "btn-primary"), style =list("padding: 25px;"))
                                   )
                               ),
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
              
              tabItem(tabName = "projects", fluidRow( column(width = 5,
              plotOutput("descriptive1")), column(width = 5,
              plotOutput("descriptive2") )), 
              br(), ## JGD 20190219 added a space between the plots on the first and second rows.
              fluidRow( column(width = 5,
              plotlyOutput("descriptive3")), column(width = 5,
              plotlyOutput("descriptive4")) ## JGD 20190219 changed from plotOutput to plotlyOutput.
              )),
              
              tabItem(tabName = "datatable",
                      div(dataTableOutput("fullSolution"), style = list("font-size:90%"))
              ),
              
              tabItem(tabName = "pom_sponsor_costs",
                      div(dataTableOutput("table2"), style = "font-size:100%"),
                      div(dataTableOutput("table3"), style = "font-size:100%")
              ),
              tabItem(tabName = "sponsor_chart",
                      plotOutput("plot1", height = "600px")
              ),
              tabItem(tabName = "mis_programs",
                      plotOutput("plot2", height = "600px")
              ),
              tabItem(tabName = "project_counts",
                      plotOutput("plot3", height = "600px")
              ),
              tabItem(tabName = "cost_value_plot",
                      plotlyOutput("costValuePlot", height = "800px")#,
                      # JGD 20190301 trying to create an output for cost_value_plot
                      # box(width = NULL,
                      #     title = tagList(shiny::icon("cloud-download")," File Output"), status = "primary", solidHeader = TRUE,
                      #     splitLayout(
                      #       textInput("outputFile2", label = "Name your exported file:", value = "Cost_Value_Plot"),
                      #       downloadButton("download", "Download")
                      #     )
                      # )
                      
                      ),
              tabItem(tabName = "statistics",
                      fluidRow(
                        column(6, style =list("padding: 10px;"),
                               div(style = list("height: 40px;"),
                                   verbatimTextOutput("iterations")))
                      ),
                      
                      fluidRow(
                        column(6, style =list("padding: 10px;"),
                               div(style = list("height: 40px;"),
                                   verbatimTextOutput("summaryProjectCount")))
                      ),
                      fluidRow(
                        column(6, style =list("padding: 10px;"),
                               div(style = list("height: 40px;"),
                                   verbatimTextOutput("summaryProjectsAdded")))
                      ),
                      fluidRow(
                        column(6, style =list("padding: 10px;"),
                               div(style = list("height: 40px;"),
                                   verbatimTextOutput("summaryProjectsDropped")))
                      ),
                      fluidRow(
                        column(6, style =list("padding: 10px;"),
                               div(style = list("height: 40px;"),
                                   verbatimTextOutput("summaryProjectsMoved")))
                      ),
                      fluidRow(
                        column(6, style =list("padding: 10px;"),
                               div(style = list("height: 40px;"),
                                   verbatimTextOutput("objectiveValue")))
                      )
              ),
              tabItem(tabName = "glossary",
                      box(width = 12,
                          includeMarkdown("www/glossary.md")
                      )
              )
              
              
            ) # End tabItems
          ) # End dashboardBody
          
)

ui <- dashboardPage(header, sidebar, body, skin = "green")