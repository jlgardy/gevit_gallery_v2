library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)

source("extras/utilityFunctions.R")


#-------------------------------------------------------------------------
# BODY
#-------------------------------------------------------------------------
body<-dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$link(rel="stylesheet",href="https://use.fontawesome.com/releases/v5.0.9/css/all.css", integrity="sha384-5SOiIsAziJl6AWe0HWRKTXlfcSHKmYV4RBF18PPJ173Kzn7jzMyFuTtk8JA7QQG1", crossorigin="anonymous"),
    tags$head(tags$script(src = "http://www.elevateweb.co.uk/wp-content/themes/radial/jquery.elevatezoom.min.js")),
    tags$script(src="js/app.js")
  ),
  useShinyjs(),
  tabsetPanel(
    tabPanel("Catalogue",
             div(style="display: inline-block;vertical-align:middle; width: 400px;",HTML("<strong>Only show images with the following tags (select to activate):</strong>")),
             div(style="display: inline-block;vertical-align:middle; width: 450px;",uiOutput("tagUI")),
             #uiOutput("tagUI"),
             br(),
             br(),
             tableOutput("imageGrid")
    ),
    tabPanel("Figure",
             hr(),
             uiOutput("imageInfo"),
             actionButton("paperChoice","Look-up other figures from this paper"),
             br(),
             hr(),
             fluidRow(
               column(width=7,
                      h3("Data Visualization"),
                      fluidRow(column(width = 1,tags$div(id = "popup",
                                                         helpPopup(strong("Image Zooming Actions"),
                                                                   includeMarkdown("explainerMarkDown/testExplainer.md"),
                                                                   placement = "right", trigger = "click"))),
                               column(width = 11, 
                                      materialSwitch(inputId = "enableZoom", label = "Enable Image Zoom", status = "danger"))
                               ),
                      uiOutput("figImage_only")
                      ),
               column(width=5,
                      h3("GEViT Breakdown"),
                      tableOutput("summaryImageTable")
                    )
             )
    ),
    id="opsPanel"
  )
)

#-------------------------------------------------------------------------
# SIDEBAR
#-------------------------------------------------------------------------
sideDash<-dashboardSidebar(
  width="300px",
  br(),
  HTML("<p style='margin-left: 10px;margin-right:5px;'><em>Use the different filters below to navigate the GEViT Gallery. To get more ifnromation about each filter click the <i class='fas fa-info-circle'></i> icon</em></p> "),
  hr(),
  uiOutput("paperLookupUI"),
  hr(),
  fluidRow(column(width = 10, h4("Visualization Context")),
  column(width = 2,tags$div(id = "popup",
           helpPopup(strong("Visualization Context Info"),
                     includeMarkdown("explainerMarkDown/visContextExplainer.md"),
                     placement = "right", trigger = "click")))),
  uiOutput("pathogenUI"),
  uiOutput("conceptUI"),
  uiOutput("captionLookUp"),
  hr(),
  fluidRow(column(width = 10, h4("Visualization Graphical Properties")),
           column(width = 2,tags$div(id = "popup",
                                     helpPopup(strong("Visualization Graphical Properties"),
                                               includeMarkdown("explainerMarkDown/visPropertiesExplainer.md"),
                                               placement = "right", trigger = "click")))),
  uiOutput("chartTypeUI"),
  uiOutput("specialChartTypeUI"),
  checkboxGroupInput("chartCombo",
                     label = "Chart Combinations",
                     choices = c("Simple","Composite","Small Multiples","Multiple Linked","Multiple General"),
                     selected = c("Simple","Composite","Small Multiples","Multiple Linked","Multiple General")),
  HTML("<p style='margin-left:10px';>Chart Enhancements</p>"),
  materialSwitch("addMarksSelect",label = "Must have added marks",value = FALSE,right=TRUE,status="danger"),
  br(),
  materialSwitch("rencodeMarksSelect",label = "Must have re-encoded marks",value = FALSE,right=TRUE)
)

#-------------------------------------------------------------------------
# ALL TOGETHER
#-------------------------------------------------------------------------

dashboardPage(
  dashboardHeader(title="GEViT Gallery",titleWidth = "300px"),
  sideDash,
  body,
  skin="black"
)
