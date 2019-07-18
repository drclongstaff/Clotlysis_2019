library(shiny)
library(xlsx)
library(plotly)

fluidPage(
  includeCSS("styles2.css"),
  titlePanel (h2("Clot Lysis",  align = "center")),
 
 
                
  sidebarLayout(
    
    sidebarPanel(
      conditionalPanel(condition="input.tabselected==1",

      fluidRow(
      column(7,  fileInput("data", label = "Select dataset:")),
      
      column(3, tags$h4("File type")),
      
      column(2,  radioButtons(inputId = "fil", label = NULL, 
             choices = c( "csv", "csv2", "txt", "xlsx"), selected = "csv")) #
      
      ),
      
      fluidRow(
        
        column(5, tags$h4("Arrange data and clot/lysis point")),
        
        column(3, numericInput("numrows",
                               label = h5("How many rows?"), value = 3)),
        
        column(4,  offset= 0, numericInput("num", 
                                           label = h5("% change for lysis (0.5 is 50%)"),step = 0.1,
                                           value = 0.5))
        
        
      ),
      
      fluidRow(
        
        column(6, tags$h4("Answer at nearest point or interpolate between points")),
        
        column(4, offset = 2,  radioButtons(inputId="interpolate", label = "Find points", choices = c( "Nearest point", "Interpolate"),
                                            selected = "Interpolate"))
        
      ),
      
      
      fluidRow(
        
        column(6, tags$h4("Use raw data or fit additional points")),
        
        column(4, offset =  2, radioButtons(inputId="spline", label= "Data source", choices = c("Raw data", "Fitted points")))
        
      ),
      
      
      
      tags$h4("Settings for fitting additional points"),
      
    fluidRow(
      column(4, numericInput("splt0", label = h5("fitting start"), value = 0, step = 1)),
      
      column(4, numericInput("Spltend", label = h5("truncate time by"), value = 0, step = 100)),
      
      column(4, numericInput("spln", label = h5("n points"), value = 1000, step = 500))
      
    ),
        
    
    tags$h4("Adjust graphs"),
    
      sliderInput("xrange", 
                  label =h5( "x axis range"),
                  min = 0, max = 22500, value = c(0, 2500)),
                  #min = 0, max = 250, value = c(0, 75)),
      
      sliderInput("yrange", 
                  label =h5( "y axis range"),
                    min = -.10, max = 2, value = c(0, 1.00)),
                  # min = -10, max = 1000, value = c(0, 500)),
      
      
     
    tags$h4("Baseline options"),
      
      fluidRow(
        
        column(5, radioButtons(inputId="abini", label = NULL, 
                               choices = c("global zero", "nth absorbance", 
                                           "min abs + offset below"))),
        column(4, numericInput("minabs",
                               label = h5("global zero"), step = 0.05, value = 0.05)),
        
        column(3, numericInput("first",
                               label = h5("nth point"), value = 1))
        
        ),  
      
      
      
      sliderInput("offset", 
                  label =h6( ""),
                  min = -0.1, max = .5, value = 0)
   
      ),
      
      
      conditionalPanel(condition="input.tabselected==2",
                       
                       uiOutput("what"),
                        
                        radioButtons(inputId="curveRes", label = "Select Results", 
                                             choices = c("All", "Generation","Decay", "1st Derivative (magenta)"), selected = "All")
                        
                         
                       
                       
      ),
      
      
      
      conditionalPanel(condition="input.tabselected==3", 
                       helpText(h4("Data file")),
                       uiOutput("which")
      ),
    
    
    conditionalPanel(condition="input.tabselected==4", 
                     helpText(h4("Raw data")),
                     uiOutput("whichraw")
    ),
    
    conditionalPanel(condition="input.tabselected==5", 
                     helpText(h3("Heat map or scatter plot")),
                     
                     #helpText(h4("heat map of selection on plots page")),
                     radioButtons(inputId = "heat", label = "Choose plot", 
                                  choices = c("heat", "scatter"), selected = "heat"),
                     
                     helpText(h4("Select heat plot output")),
                     uiOutput("whatheat"),
                     
                     helpText(h4("")),
                     
                     helpText(h4("Select scatter plot output")),
                     uiOutput("whatx"), 
                     
                     uiOutput("whaty")
    ),
      
      
    conditionalPanel(condition="input.tabselected==6", 
                     helpText(h4("Settings")),
                     helpText(h5("This table records analysis settings for quality control and reproducibility "))
                    
    ),
    
    
    conditionalPanel(condition="input.tabselected==7",
                       helpText(h5("Please cite this reference if you find the app useful, Longstaff C, Development of a Shiny app tool to simplify and standardize the analysis 
                                   of hemostasis assay data: 
                                   communication from the SSC of the ISTH, J Thromb Haemost, 15: 1044-6, 2017.  DOI 10.1111/jth.13656")),
                       tags$br(),
                       helpText(h5("Version 1.1, last accessed", Sys.Date()))
                       
      )
     
   
    
    
  ),
  mainPanel( 
    tabsetPanel(#type="tab",
                tabPanel("Plots", value = 1,
                         
                         
                         plotOutput(outputId = "myplotAll"),
                         radioButtons(inputId="tabRes", label = h4("Select a parameter", align = "center"), choices = c("Column names"=1, "chosen zero"=2, "Time to % clotting     "=3, "Reading at % clotting"=4, 
                                                                                                                   "Reading at peak      "=5, "Reading peak-zero"=6,
                                                                                                                   "Time to peak from zero"=7, "Time clotting to peak"=8, "Time to % lysis from zero"=9, 
                                                                                                                   "Reading at % lysis"=10, "Time clotting to % lysis"=11,"Time peak to % lysis"=12,
                                                                                                                   "Time to full lysis "=13,"Time from clotting to lysis"=14,  "Area under the curve"=15,
                                                                                                                   "Time at max rate increase"=16, "Time at max rate decrease"=17,  "Time at sign change"=18 
                                                                                                                   
                         ), selected = 9, inline = TRUE, width = "100%"),
                         
                         h5(textOutput("text3")),
                         
                        tableOutput("resultsTable"), align = "center"),
                
                 
                
                   
                
                tabPanel("Curve", value = 2,
                         
                          
                         
                         plotOutput(outputId = "myplot"),
                         
                       
                         
                         fluidRow(
                          column(7,
                                 
                                 h4("Results table"), tableOutput("curveTable"), align = "centre")
                         
                         )),
                
                
                
              
                tabPanel("All Results", value = 3, dataTableOutput("contents")),
                
                tabPanel("Raw Data", value = 4, tableOutput("raw")),
                
                tabPanel("Explore", value = 5,
                         
                         #plotOutput(outputId = "exploreplot")
                         plotlyOutput(outputId = "exploreplot")
                        
                         
                        
                ),
                
                tabPanel("Settings", value = 6,  
                         #helpText(h4("Settings")),
                         h4(uiOutput("setfile")),
                         tableOutput("settings")),
                
                tabPanel("Help", value = 7,
                         tags$h4("Load data"),
                         tags$blockquote(h5("►Load a data file from your computer and select the data format. An example set of data is provided",
                                            tags$br(),
                                            "►The csv2 format is used where the decimal marker is a comma rather than a point",
                                            tags$br(),
                                            "►Data should be supplied as one column of time and any number of columns of absorbances. Include header text to label columns of time and well names",
                                            tags$br(),
                                            "►Note: Data files should not contain any gaps or spaces between characters and avoid unusual characters",
                                            tags$br(),
                                            "►Input the number of rows to display your results in the correct arrangement (e.g. 8 for a full plate)",
                                            tags$br(),
                                            "►Type in a value for percentage clotting and lysis (default 0.5 = 50% clotting and lysis)"
                         )),
                         tags$h4("Nearset point or interpolate  /  Raw or fitted data"),                 
                         tags$blockquote(h5 ("►You can find the nearest point to the parameter you select or interpolate to approximate the answer between points",
                                             tags$br(),
                                              "►There is also an option to use raw data or fit more points between the data points supplied to improve precision",
                                             tags$br(),
                                             "►Raw data (-o-) or fitted points (.) are shown on the plots on the Curve tab",
                                             tags$br(),
                                             "►If you have some wells with noisy or flat data (empty wells), iterpolating between points may not work, so select Nearest point",
                                             tags$br(),
                                             "►If you have good data with reasonable density of points, select  'Interpolate' and 'Raw data'",
                                             tags$br(),
                                             "►Settings can be adjusted to restrict the parts of the curve for investigation and controlling the number of points to add",
                                             tags$br(),
                                             "►Input data (raw or with extra fitted points, as selected), is also shown in the Raw Data tab")),
                         tags$h4("Graph options"),                 
                         tags$blockquote(h5 ("►Displayed graphs are adjusted using the sliders",
                                             tags$br(),
                                             "►All the plots are shown or individual curves can be scrutinised on the next tab (Curve)",
                                             tags$br(),
                                             "►These options do not affect data or calculated results"
                         )),
                         
                         tags$h4("Baseline options"),                 
                         tags$blockquote(h5 ("►These selections adjust the baseline for complete lysis",
                                             tags$br(),
                                             "►Global zero sets zero absorbance for all the curves to the same selected value",
                                             tags$br(),
                                             "►nth point uses the absorbance value from a selected point on all the curves as zero (e.g first point or a point after lysis)",
                                             tags$br(),
                                             "►Min abs finds the lowest absorbance for each curve and an offset can be added using the slider"
                         )),
                         tags$h4("Select a parameter"),                 
                         tags$blockquote(h5 ("►Select what should be shown in the results table below and indicated on the graphs above",
                                             tags$br(),
                                             "►The results table is organised like the graphical output above",
                                             tags$br(),
                                             "►Values can be copied and pasted for further analysis",
                                             tags$br()
                                             
                                             
                         )),
                         tags$h4("Other tabs"),                 
                         tags$blockquote(h5 ("►'Curve' focuses on an individual well",
                                             tags$br(),
                                             "►'All Results' is a table of all fitted values for every well",
                                             tags$br(),
                                             "►'Raw Data' is the time and absorbance data set.  Time interval changes if fitted curves are used",
                                             tags$br(),
                                             "►'Explore' presents graphical outputs of fitted results.  There is also a table of seetings to help reproducible analysis",
                                             tags$br(),
                                             "►'Settings' generates a table of seetings to help reproducibility",
                                             tags$br(),
                                             "►Code files and detailed help notes are available in a github repository", 
                                             tags$a (href="https://github.com/drclongstaff/Clotlysis_2019/blob/master/Clotlysis_CL_help_notes_2019.pdf", "Here")
                         )),
                  
                  
                  tags$img(src="GraphTable.png", width=600, height=700)
                  #tags$img(src="screenCapS6.png", width=600, height=700)
                  
                ),
                
               id = "tabselected"
                
    )
  )
  
)   
)
