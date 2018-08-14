library(shiny)
library(markdown)
library(dplyr)
library(tidyr)
library(stringr)

source("extras/utilityFunctions.R")


#Load Necessary data files
load("data/analysisData.RData")
load("data/widgetPopulatingData.RData")

#-----------------------------------------------
# Server Functions

#function to filter data are return ID
filtData<-function(dat=NULL,colFilt=NULL,filtOpts=NULL){
  if(!is.null(filtOpts)){
    if(colFilt %in% c("addedMarks","reencodedMarks")){
      if(filtOpts){
        return(dat[!is.na(dat[,colFilt]),"figID_initial"])
      }else{
        return(dat[,"figID_initial"])
      }
      
    }else{
    tmp<-sapply(filtOpts,function(x){
      sapply(dat[,colFilt],function(y){grepl(x,y)})
    })
    foundItems<-rowSums(tmp)
    
    return(unique(dat[foundItems>0,"figID_initial"]))
    }
  }else{
    return(unique(dat[,"figID_initial"]))
  }
}


#update the lazy load instance
updateFlushAction<-function(session=NULL){
  session = getDefaultReactiveDomain() #getting the session info
  session$sendCustomMessage("lazyLoadUpdate",message=list()) 
}

#------------------
# SERVER CODE
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp)
  session$onFlushed(updateFlushAction,once=FALSE) #update the lazy load instance after session flushed
  
  #-----------------------------------------------
  # Reactive Data
  #-----------------------------------------------
  #reactive dataset
  datasetInput <- reactive({
    #browser()
    #filter initially on figure caption content (if any is supplied)
    tmpDat<-datSub
    if(!is.null(input$captionSelect)){
      idKeep<-filtData(commonWords,"bigram",input$captionSelect)
      tmpDat<-inner_join(datSub,idKeep)
    }
    
    if(nrow(tmpDat)==0){
      empty_figs <- "There are no visualizations that match your filtering criteria."
      return(matrix(empty_figs, nrow = 1, ncol = 1)) # return empty matrix to avoid console warning
    }
    #browser()
    
    #filter dataset - images MUST have these items to be included
    filtID<-c(filtData(tmpDat,"chartCombinations",input$chartCombo)$figID_initial,
              filtData(tmpDat,"chartType",input$chartType)$figID_initial,
              filtData(tmpDat,"specialChartType",input$specialChartType)$figID_initial,
              filtData(tmpDat,"Pathogen",input$pathogenSelect)$figID_initial,
              filtData(tmpDat,"concepts",input$conceptSelect)$figID_initial,
              filtData(tmpDat,"addedMarks",input$addMarksSelect)$figID_initial,
              filtData(tmpDat,"reencodedMarks",input$rencodeMarksSelect)$figID_initial,
              filtData(tmpDat,"classExample",input$tagSelect)$figID_initial,
              filtData(tmpDat,"PMID",values$paperSelect)$figID_initial)
    
    
    tmp<-table(filtID)
    tmp<-tmp[tmp==9]
    
    if(length(tmp)==0){
      empty_figs <- "There are no visualizations that match your filtering criteria."
      return(matrix(empty_figs, nrow = 1, ncol = 1)) # return empty matrix to avoid console warning
    }
    
    tmp<-datSub %>% filter(figID_initial %in% names(tmp))
    
    #Banner above image to indicate if it is good practice or not
    #currently just a random indicator
    supportingText<-apply(tmp[,c("figID_initial","classExample")],1,function(val){
      y<-val[1]
      x<-val[2]
      
      if(!is.na(x)){
        if(x=="Good Practice"){
          return('<p class="special-content-good">Good Practice</p>')
        }else if (x=="Missed Opportunity"){
          return('<p class="special-content-missed">Missed Opportunity</p>')
        }else{
          return('<p class="regular-content">No Status Assigned</p>')
        }
      }else{
        return(paste0('<p class="regular-content"','" id="',y,'">No Status Assigned</p>'))
      }
    })
    
    #Note to self: using data-src instead of src because of the lazy load plugin
  imgTxt<-paste0(supportingText,'<img class="chart" data-src="https://s3.ca-central-1.amazonaws.com/gevit-proj/imagesSmaller/',tmp$figID_initial, '" alt="',tmp$figID_initial,'" data-code="', tmp$figID_initial,'" data-zoom-image="', tmp$figID_initial, ',"></img>')
    
    imgTxtPadded<-pad.Vector(imgTxt)
    
    #adding some labels
    if(length(imgTxtPadded)/3 > 0) {
      imgTxtPadded<-matrix(imgTxtPadded, ncol = 3, nrow = (length(imgTxtPadded)/3), byrow = TRUE)
    } else {
      empty_figs <- "There are no graphs that match the selected criteria."
      imgTxtPadded<-matrix(empty_figs, nrow = 1, ncol = 1) # return empty matrix to avoid console warning
    }
    
    imgTxtPadded
    
  })
  
  
  #reactive values
  values <- reactiveValues(
    clicked = FALSE,
    code = NULL,
    scrollPos = NULL,
    sumTable = NULL,
    paperSelect = NULL
  )
  
  
  #-----------------------------------------------
  # Main Gallery Elements
  #-----------------------------------------------
  
  #Outputting Gallery of Images
  output$imageGrid <- renderTable({
    datasetInput()
  }, sanitize.text.function = function(x) x,
  include.colnames = FALSE, 
  include.rownames = FALSE,
  bordered=TRUE)

  output$figImage_only = renderUI({
    tags$img(src = paste0('https://s3.ca-central-1.amazonaws.com/gevit-proj/images/',values$code))  
  })

  #-----------------------------------------------
  # Reactive Functions
  #-----------------------------------------------
  
  # When the app loads, if there is a hash value then load that figure
  observe({
    urlHash <- session$clientData$url_hash_initial
    if (!is.null(urlHash) && substring(urlHash, 1, 1) == "#") {
      values$code <- substring(urlHash, 2)
    }
  })
  
  # When a figure is clicked, load that figure
  observe({
    if (!is.null(input$clicked) && input$clicked == TRUE) {
  
      values$code <- input$code
      values$scrollPos<-input$scrollPos
      
      # Add figure name to URL so it can be retrieved later
      session$sendCustomMessage("figClick", values$code)
      
      updateTabsetPanel(session, "opsPanel", selected = "Figure")
      session$sendCustomMessage("scrollCallback",0)
      updateMaterialSwitch(session,"enableZoom",FALSE)
    }
  })
  
  observe({
    # Automatically toggle image zooming on and off
    if(input$enableZoom){
      session$sendCustomMessage(type = 'createZoom',message=list())
    }else{
      session$sendCustomMessage(type = 'removeZoom',message=list())
    }

  })
  
  observeEvent(input$opsPanel == "catalogue",{
    if(!(is.null(values$scrollPos))){
      session$sendCustomMessage("scrollCallback",values$scrollPos)
    }
  })
  
  
  observeEvent(input$paperChoice,{
    if(!is.null(values$code)){
      tmp<-filter(datSub,figID_initial == values$code)
      values$paperSelect<-tmp$PMID
      #updateSelectInput(session,"paperSelect",selected=unique(tmp$PMID))
      updateTabsetPanel(session,"opsPanel",selected = "Catalogue")
    }
  })
  
  observeEvent(input$showAllPapers,{
    values$paperSelect<-NULL
  })
  
  #-----------------------------------------------
  # WIDGETS
  #-----------------------------------------------
  
  # Widget : Dropdown menu for Pathogen
  output$pathogenUI<-renderUI({
    # pathogen<-datSub %>%
    #   ungroup()%>%
    #   mutate(Pathogen=strsplit(Pathogen,";"))%>%
    #   tidyr::unnest()%>%
    #   group_by(Pathogen)%>%
    #   count()
    
    selectInput(inputId = "pathogenSelect",
                label = "Pathogen:",
                choices=c("Type to select a pathogen" = '',pathogen$Pathogen),
                selected=NULL,
                multiple=TRUE,
                selectize = TRUE)
  })
  
  # Widget : Dropdown menu for Concepts
  output$conceptUI<-renderUI({
    # concept<-datSub %>%
    #   ungroup()%>%
    #   mutate(concepts=strsplit(concepts,";"))%>%
    #   tidyr::unnest()%>%
    #   group_by(concepts)%>%
    #   count()
    
    selectInput(inputId = "conceptSelect",
                label = "A priori concept:",
                choices=c("Type to select paper topics" = '',concept$concepts),
                selected=NULL,
                multiple=TRUE,
                selectize = TRUE)
  })
  
  
  # Widget: Dropdown menu to choose chart type
  output$chartTypeUI<-renderUI({
    # chartTypes<-datSub %>%
    #   ungroup()%>%
    #   mutate(chartType=strsplit(chartType,";"))%>%
    #   tidyr::unnest()%>%
    #   group_by(chartType)%>%
    #   count()
    
    selectInput(inputId = "chartType",
                label = "Chart Type",
                choices=c("Type to select Chart Types" = '',chartTypes$chartType),
                selected=NULL,
                multiple=TRUE,
                selectize = TRUE,
                width =)
  })
  
  # Widget: Dropdown menu for special chart types
  output$specialChartTypeUI<-renderUI({
    if(is.null(input$chartType)){
      choiceVals<-as.character(chartRln$specialChartType)
    }else{
      tmp<-chartRln %>%
        filter(chartType %in% input$chartType)
      
      choiceVals<-as.character(tmp$specialChartType)
    }
    
    selectInput(inputId = "specialChartType",
                label = "Special Chart Type",
                choices=c("Type to select Special Chart Types" = '',choiceVals),
                selected=NULL,
                multiple =TRUE,
                selectize = TRUE)
  })
  
  #Widget : dropdown menu for data caption
  output$captionLookUp<-renderUI({
    selectInput("captionSelect",
                label="Figure caption text:",
                choices = c("Type to select data" = '',usefulBigrams$bigram,usefulSingles$word),
                selected=NULL,
                multiple=TRUE)
  })
  
  
  #Widget : Show only
  output$tagUI<-renderUI({
    #classTags<-unique(datSub$classExample)
    
    checkboxGroupButtons(
      inputId = "tagSelect", 
      label = NULL, 
      choices = setdiff(classTags,NA), 
      justified = FALSE, 
      individual = TRUE,
      status = "primary",
      checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = NULL)
    )
  })
  
  # output$paperLookupUI<-renderUI({
  #   #PMIDUnique<-unique((datSub$PMID))
  #   if(is.null(input$paperChoice)){
  #     return(NULL)
  #   }else{
  #     selectInput(
  #       "paperSelect",
  #       label="Paper Lookup (PMID):",
  #       choices = PMIDUnique,
  #       selected=NULL,
  #       multiple = TRUE
  #     )
  #   }
  # })
  
  #widget: info box showing number of figures
  output$numFigBox<-renderUI({
    totalNum<-nrow(datSub) 
    totalShown<-sum(as.vector(!is.na(datasetInput())) & grepl("<img",datasetInput()))
    
    HTML(paste0('<p style="color:black;">',
                paste0('<strong style="font-size:20px;">',round((totalShown/totalNum)*100),"% </strong> of figures shown"),
                paste0(" <strong>(",totalShown,"</strong> out of ",totalNum," figures)"),
                '</p>'))
  })
  
  #-----------------------------------------------
  # SUMMARIES
  #-----------------------------------------------
  #output figure metadata
  output$imageInfo<-renderUI({
    outText<-NULL
    if(length(values$code) > 0) {
      tmp<-filter(datSub,figID_initial == values$code)
      outText<-HTML(sprintf("<b>ID:</b>%s<br><em>%s</em>(%d) <a href='https://www.ncbi.nlm.nih.gov/pubmed/%s' target='_blank'>doi</a>",tmp$figID_initial,tmp$Title,tmp$YearPub,tmp$PMID))
    }
    outText
  })
  

  observe(
    if(length(values$code) == 0) {
      NULL
    } else {
      tmp<-filter(datSub,figID_initial == values$code)
      
      #chart types
      chartType<-unlist(strsplit(tmp$chartType,";"))
      specialChartType<-unlist(strsplit(tmp$specialChartType,";"))
      
      if(length(chartType) == length(specialChartType)){
        chartTypes<-data.frame(item = rep("Chart Type",length(chartType)),
                               chartType = chartType,
                               specialChartType = specialChartType,
                               stringsAsFactors = FALSE)
      }else{
        chartTypes<-cbind(tmp$chartType,tmp$specialChartType)
      }
      
      
      #added marks
      if(!is.na(tmp$addedMarks)){
        addedMarks<-unname(parseMarkText(tmp$addedMarks))
        addedMarks<-data.frame(item=rep("Added Marks",nrow(addedMarks)),
                               chartType = addedMarks[,1],
                               specialChartType = addedMarks[,2],
                               stringsAsFactors = FALSE)
        #colnames(addedMarks)<-c("item","chartType","specialChartType")
      }else{
        addedMarks<-data.frame(item="Added Marks",chartType=NA,specialChartType=NA)
      }
      
      #rencoded marks
      if(!is.na(tmp$reencodedMarks)){
        rencodedMarks<-unname(parseMarkText(tmp$reencodedMarks))
        rencodedMarks<-data.frame(item=rep("Re-encoded Marks",nrow(rencodedMarks)),
                                  chartType = rencodedMarks[,1],
                                  specialChartType = rencodedMarks[,2],
                                  stringsAsFactors = FALSE)
        #colnames(addedMarks)<-c("item","chartType","specialChartType")
      }else{
        rencodedMarks<-data.frame(item="Re-encoded Marks",chartType=NA,specialChartType=NA)
      }
      
      #annotations
      #rencoded marks
      if(!is.na(tmp$annotations)){
        annotationsMarks<-unname(parseMarkText(tmp$annotations))
        annotationsMarks<-data.frame(item=rep("Annotations",nrow(annotationsMarks)),
                                     chartType = annotationsMarks[,1],
                                     specialChartType = annotationsMarks[,2],
                                     stringsAsFactors = FALSE)
        #colnames(addedMarks)<-c("item","chartType","specialChartType")
      }else{
        annotationsMarks<-data.frame(item="Annotations",chartType=NA,specialChartType=NA)
      }
      
      #putting it together
      outfig<-rbind(chartTypes,
                    c("Chart Combinations",tmp$chartCombinations,NA),
                    addedMarks,
                    rencodedMarks,
                    annotationsMarks)
      
      values$sumTable<-outfig %>% filter(!is.na(chartType))
    }
  )
  
  
  #output table for figure metadata
  output$summaryImageTable<-renderTable({
    values$sumTable
  },
  colnames=FALSE,
  rownames=FALSE)
  
  output$summaryStatement<-renderUI({gc()
    
    df<-values$sumTable
    
    summaryString<-""
    
    if(is.null(values$code)){
      return(summaryString)
    }
    
    #Report Basic Chart Types
    tmp<-filter(df,item == "Chart Type")
    summaryString<-c(summaryString,
                     paste0("This visualization was constructed using <strong>",
                            nrow(tmp),
                            ifelse(nrow(tmp)==1," type of chart, a "," types of charts, a "),
                            paste0(tmp$chartType,collapse=", "),
                            "</strong>.")
    )
    #Combination
    tmp<-filter(df,item == "Chart Combinations")
    if(!("Simple" %in% tmp$chartType)){
      summaryString<-c(summaryString,
                       paste0("These charts were combined in a <strong> ",
                              paste0(tmp$chartType,collapse=", "),
                              "</strong> pattern.")
      )
    }
    
    #Added Marks
    tmp<-filter(df,item == "Added Marks")
    if(nrow(tmp)>0){
      summaryString<-c(summaryString,
                       paste0("New marks were added to the base chart types to encode additional data. These marks took the form of a ",
                              "<strong>",
                              paste0(tmp$chartType,collapse=", "),
                              "</strong>."))
    }
    
    #Reconded Marks
    tmp<-filter(df,item == "Re-encoded Marks")
    if(nrow(tmp)>0){
      summaryString<-c(summaryString,
                       paste0("Existing elements of the base chart type were modified (re-encoded) to include additional datal. Re-enconded marks included ",
                              "<strong>",
                              paste0(tmp$chartType,collapse=", "),
                              "</strong>."))
    }
    
    #Annotatations
    tmp<-filter(df,item == "Annotations")
    if(nrow(tmp)>0){
      summaryString<-c(summaryString,
                       paste0("Specific elements within the basic chart types were annotated, for example highlighted or individual emphasized, using ",
                              "<strong>",
                              paste0(tmp$chartType,collapse=", "),
                              " marks.</strong>"))
    }
    
    #concluding statement
    summaryString<-c(summaryString,"<br><br><small><em> The table below summarizes the chart types, chart combinations, added and re-encoded marks (as well as the aesthetic properties of the marks used to encode data), and finally annotations.</em></small>")
    #output
    HTML(paste0(summaryString,collapse=" "))
  })
  
  
  #-----------------------------------------------
  # Hiding/Showing DIVS
  #-----------------------------------------------
  # Shiny JS test elements
  onclick("paperChoice",toggle(id="showPapers"))
  onclick("showAllPapers",toggle(id="showPapers"))
  
})
