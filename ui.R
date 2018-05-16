library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

source("extras/utilityFunctions.R")


## Function from Joe Cheng
## https://gist.github.com/jcheng5/5913297
helpPopup <- function(title, content,
                      placement = c('right', 'top', 'left', 'bottom'),
                      trigger = c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover()})"),
        tags$style(type = "text/css", ".popover{max-width:500px; position: fixed;color:black;}")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-link",
      `data-toggle` = "popover", `data-html` = "true",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok = TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok = TRUE)[1],
      icon("info-circle", class = NULL, lib = "font-awesome")
    )
  )
}
#-------------------------------------------------------------------------
# BODY
#-------------------------------------------------------------------------
body<-dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$link(rel="stylesheet",href="https://use.fontawesome.com/releases/v5.0.9/css/all.css", integrity="sha384-5SOiIsAziJl6AWe0HWRKTXlfcSHKmYV4RBF18PPJ173Kzn7jzMyFuTtk8JA7QQG1", crossorigin="anonymous"),
    tags$script(src="js/app.js"),
    tags$script(src="js/elevatezoom.min.js"),
    includeScript("https://cdnjs.cloudflare.com/ajax/libs/vanilla-lazyload/8.7.1/lazyload.min.js")
  ),
  #br(),
  tabsetPanel(
    tabPanel("Catalogue",
             div(id = "grad",
                 uiOutput("numFigBox")
             ),
             div(style="display: inline-block;vertical-align:middle; width: 400px;",HTML("<strong>Only show images with the following tags (select to activate):</strong>")),
             div(style="display: inline-block;vertical-align:middle; width: 450px;",uiOutput("tagUI")),
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
                      uiOutput("summaryStatement"),
                      br(),
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
  actionButton("disclaimerButtonToggle",label="Disclaimer (click to hide text)",class="btn-menu",width = "275px"),
  #br(),
  div(id = "disclaimer-text",style="padding-left:10px;padding-right:5px;",
      HTML('<p style="color:white;font-size:14px;font-weight:300;"> The images in the GEViT gallery are presented solely for research purposes and under copyright Fair Use terms. Clicking on an image provides a link back to the original source publication. Beyond the images themselves no other materials relating to the published articles (such as PDFs of the full text) have been made available. If you are an author of a publication contained with this gallery and you would like your work to be removed please <a style="color:white;font-size:14px;font-weight:400;" href = "https://github.com/amcrisan/gevit_gallery_v2/issues">notify us.</a></p>'),
      hr()
  ),
  HTML("<p style='margin-left: 10px;margin-right:5px;'><em>Click the 'Show' buttons to reveal the different filters you can use to navigate the GEViT Gallery.</em></p> "),
  hr(),
  #uiOutput("numFigBox"),
  #hr(),
  uiOutput("paperLookupUI"),
  hr(),
  div(style="display: inline-block;vertical-align:middle; width: 165px;",h4("Visualization Context")),
  div(id = "popup",style="display: inline-block;vertical-align:middle; width: 60px;align:left;",
      helpPopup(strong("Visualization Context Info"),includeMarkdown("explainerMarkDown/visContextExplainer.md"),placement = "right", trigger = "click")),
  div(style="display: inline-block;vertical-align:middle; width: 55px;",actionButton("buttonToggle",label=" Show",class="btn-menu",width="50px",icon=icon("plus-square"))),
  
  
  hidden(
    div(id = "visContext",
        HTML("<p style='margin-left: 10px;margin-right:5px;'><em>Filter by pathogen, derived paper topics, or data terms within figure captions</em></p>"),
      uiOutput("pathogenUI"),
      uiOutput("conceptUI"),
      uiOutput("captionLookUp")
    )
  ),
  hr(),
  div(style="display: inline-block;vertical-align:top; width: 164px;",h4("Visualization Graphical Properties")),
  div(id = "popup",style="display: inline-block;vertical-align:top; width: 60px;align:left;",
      helpPopup(strong("Visualization Context Info"),includeMarkdown("explainerMarkDown/visPropertiesExplainer.md"),placement = "right", trigger = "click")),
  div(style="display: inline-block;vertical-align:middle; top: 55px;",actionButton("buttonToggleTwo",label=" Show",class="btn-menu",width="50px",icon=icon("plus-square"))),
  
  hidden(
    div(id = "visProperties",
      HTML("<p style='margin-left: 10px;margin-right:5px;'><em>Filter by chart types, chart combinations, and whether visualizations have chart elements enhanced or re-encoded</em></p>"),
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
    ))
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
