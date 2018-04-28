library(shiny)
library(readxl)
library(markdown)
#library(tesseract)
#library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)


source("extras/utilityFunctions.R")

#------------------
# PREP

#Prep Images
#imgs<-list.files(path="www/images/")

#Prep Data
dat<-read_excel(path="data/figure_classification_final.xlsx",na=c("","NA"))
#allIMG<-readRDS(file="data/allIMG.RDS") #temporary till I track down some missing files

datSub<-dat %>%
  #filter(figID_initial %in% allIMG)%>%
  filter(!(chartCombinations) %in% c("BREAK THIS UP","MISSING","EXCLUDE - LEGIT TABLE"))%>%
  mutate(figID_initial = gsub("\\s+","_",figID_initial)) #to match AWS
  #filter(!(specialChartType %in% c("INCOMPLETE","MISSING"))) %>%
  #filter(!(chartCombinations) == "Simple - BREAK THIS UP")%>%
  #filter(!grepl("CONSIDER REMOVING",specialChartType))

chartRln<-getChartRln(datSub)
paperData<-read_excel("data/MasterDocumentList.xlsx") %>% filter(Include == "Y")

datSub<-inner_join(datSub,paperData,by="PMID")

load("data/captionItems.RData")
commonWords<- inner_join(commonWords,select(datSub,figID_initial), by="figID_initial")

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

#------------------
# SERVER CODE
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp)
  
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
              filtData(tmpDat,"Pathogen",input$pathogenSelect)$figID_initial,
              filtData(tmpDat,"concepts",input$conceptSelect)$figID_initial,
              filtData(tmpDat,"addedMarks",input$addMarksSelect)$figID_initial,
              filtData(tmpDat,"reencodedMarks",input$rencodeMarksSelect)$figID_initial,
              filtData(tmpDat,"classExample",input$tagSelect)$figID_initial,
              filtData(tmpDat,"PMID",input$paperSelect)$figID_initial)
    
    
    
    tmp<-table(filtID)
    tmp<-tmp[tmp==8]
    
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
          return('<p class="special-content-missed">Missed Oppertunity</p>')
        }else{
          return('<p class="regular-content">No Status Assigned</p>')
        }
      }else{
        return(paste0('<p class="regular-content"','" id="',y,'">No Status Assigned</p>'))
      }
    })
    
    #imgTxt<-paste0(supportingText,'<img class="chart" src="../gevitTextmining/figureAnalysisShiny/www/figures/"',tmp$figID_initial, '"data-code="', tmp$figID_initial,'"data-zoom-image="', tmp$figID_initial, ',"></img>')
    
    imgTxt<-paste0(supportingText,'<img class="chart" src="https://s3.ca-central-1.amazonaws.com/gevit-proj/imagesSmaller/',tmp$figID_initial, '" alt="',tmp$figID_initial,'" data-code="', tmp$figID_initial,'" data-zoom-image="', tmp$figID_initial, ',"></img>')
    
    imgTxtPadded<-pad.Vector(imgTxt)
    
    #adding some labels
    if(length(imgTxtPadded)/3 > 0) {
      matrix(imgTxtPadded, ncol = 3, nrow = (length(imgTxtPadded)/3), byrow = TRUE)
    } else {
      empty_figs <- "There are no graphs that match the selected criteria."
      matrix(empty_figs, nrow = 1, ncol = 1) # return empty matrix to avoid console warning
    }
    
  })
  
  #Outputting Gallery of Images
  output$imageGrid <- renderTable({
    datasetInput()
  }, sanitize.text.function = function(x) x,
  include.colnames = FALSE, 
  include.rownames = FALSE,
  bordered=TRUE)
  
  #reactive values
  values <- reactiveValues(
    clicked = FALSE,
    code = NULL,
    scrollPos = NULL
  )
  
  
  #-----------------------------------------------
  # Reactive Functions
  #-----------------------------------------------
  #output$figImage_only <- renderImage({

  #  filename <- paste0('https://s3.ca-central-1.amazonaws.com/gevit-proj/images/',values$code)
    
  #  list(src = filename,
  #       width = "auto",
  #       height = 500)
  #})
  
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
      #updateMaterialSwitch(session,"enableZoom",FALSE)
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
      updateSelectInput(session,"paperSelect",selected=unique(tmp$PMID))
      updateTabsetPanel(session,"opsPanel",selected = "Catalogue")
    }
  })
  
  #-----------------------------------------------
  # WIDGETS
  #-----------------------------------------------
  
  # Widget : Dropdown menu for Pathogen
  output$pathogenUI<-renderUI({
    pathogen<-datSub %>%
      ungroup()%>%
      mutate(Pathogen=strsplit(Pathogen,";"))%>%
      tidyr::unnest()%>%
      group_by(Pathogen)%>%
      count()
    
    selectInput(inputId = "pathogenSelect",
                label = "Pathogen:",
                choices=pathogen$Pathogen,
                selected=NULL,
                multiple=TRUE)
  })
  
  # Widget : Dropdown menu for Concepts
  output$conceptUI<-renderUI({
    concept<-datSub %>%
      ungroup()%>%
      mutate(concepts=strsplit(concepts,";"))%>%
      tidyr::unnest()%>%
      group_by(concepts)%>%
      count()
    
    selectInput(inputId = "conceptSelect",
                label = "Topic:",
                choices=concept$concepts,
                selected=NULL,
                multiple=TRUE)
  })
  
  
  # Widget: Dropdown menu to choose chart type
  output$chartTypeUI<-renderUI({
    chartTypes<-datSub %>%
      ungroup()%>%
      mutate(chartType=strsplit(chartType,";"))%>%
      tidyr::unnest()%>%
      group_by(chartType)%>%
      count()
    
    selectInput(inputId = "chartType",
                label = "Chart Type",
                choices=chartTypes$chartType,
                selected=NULL,
                multiple=TRUE)
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
                choices=choiceVals,
                selected=NULL,
                multiple =TRUE)
  })
  
  #Widget : dropdown menu for data caption
  output$captionLookUp<-renderUI({
    selectInput("captionSelect",
                label="Data (from figure captions):",
                choices = c(usefulBigrams$bigram,usefulSingles$word),
                selected=NULL,
                multiple=TRUE)
  })
  
  
  #Widget : Show only
  output$tagUI<-renderUI({
    classTags<-unique(datSub$classExample)
    
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
  
  output$paperLookupUI<-renderUI({
    PMIDUnique<-unique((datSub$PMID))
    selectInput(
      "paperSelect",
      label="Paper Lookup (PMID):",
      choices =PMIDUnique,
      selected=NULL,
      multiple = TRUE
    )
  })
  
  #widget: info box showing number of figures
  output$numFigBox<-renderUI({
    
    #browser()
    totalNum<-nrow(datSub) 
    totalShown<-nrow(datasetInput()) * ncol(datasetInput())
    
    valueBox(value = paste0(round((totalShown/totalNum)*100),"%"),
            subtitle  = paste(totalShown, "out of",totalNum,"figures"),
            color = "light-blue",
            width = 12)
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
  
  
  #output table for figure metadata
  
  output$summaryImageTable<-renderTable({
    if(length(values$code) == 0) {
      NULL
    } else {
      tmp<-filter(datSub,figID_initial == values$code)
      #browser()
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
      
      outfig %>% filter(!is.na(chartType))
    }
    
  },
  colnames=FALSE,
  rownames=FALSE)

})
