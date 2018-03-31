library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

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
             tableOutput("imageGrid")
    ),
    tabPanel("Figure",
             
             materialSwitch(inputId = "enableZoom", label = "Enable Image Zoom", status = "danger"),
             imageOutput("figImage_only")
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
  fluidRow(column(width = 10, h4("Visualization Context")),
  column(width = 2,tags$div(id = "popup",
           helpPopup(strong("Visualization Context Info"),
                     includeMarkdown("explainerMarkDown/testExplainer.md"),
                     placement = "right", trigger = "click")))),
  uiOutput("pathogenUI"),
  uiOutput("conceptUI"),
  uiOutput("captionLookUp"),
  hr(),
  fluidRow(column(width = 10, h4("Visualization Graphical Properties")),
           column(width = 2,tags$div(id = "popup",
                                     helpPopup(strong("Visualization Graphical Properties"),
                                               includeMarkdown("explainerMarkDown/testExplainer.md"),
                                               placement = "right", trigger = "click")))),
  uiOutput("chartTypeUI"),
  uiOutput("specialChartTypeUI"),
  checkboxGroupInput("chartCombo",
                     label = "Chart Combinations",
                     choices = c("Simple","Composite","Small Multiples","Multiple Linked","Multiple General"),
                     selected = c("Simple","Composite","Small Multiples","Multiple Linked","Multiple General")),
  hr(),
  h4("Paper Look-up")
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
