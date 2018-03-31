pad.Vector <- function(x) {
  # Pad vector so its length is multiple of 3
  # (the # of cols of display matrix)
  #
  # Args:
  #   x: vector
  #
  # Returns:
  #   Vector of length multiple of 3
  
  # determine length of padding needed to fill matrix of ncol = 3
  n <- 3 * ceiling(length(x) / 3) - length(x)
  
  x_padded <- c(x, rep(NA, n))
  x_padded
  
}

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




#Parse the marks (re-ecndoed and added)
parseMarkText<-function(A=NULL){
  #extract whole substrates
  marksWithChannel<-unlist(str_extract_all(A,"[a-z]+\\[[a-z;\\s]+\\]"))
  marksWithNoChannel<-gsub("[a-z]+\\[[a-z;\\s]+\\]","",A)
  
  #deal with channeless marks first
  marksWithNoChannel<-setdiff(unlist(strsplit(marksWithNoChannel,";")),"")
  marksWithNoChannel<-cbind(marksWithNoChannel,rep(NA,length(marksWithNoChannel)))
  
  channelInfo<-gsub("\\[|\\]","",unlist(str_extract_all(marksWithChannel,"\\[[a-z;]+\\]")))
  
  #Extracting 
  tmp<-gsub("\\[([a-z;]+)\\]","",marksWithChannel,perl=T)
  tmp<-gsub("\\([a-z]+\\)","",tmp,perl=T)
  tmp<-gsub("\\[pie chart\\[[a-z]+\\]\\]","",tmp,perl=T) #the one edge case of this
  tmp<-gsub("\\[pie chart\\]","",tmp,perl=T) #the one edge case of this
  
  markInfo<-unlist(strsplit(tmp,";"))
  marksWithChannel<-cbind(markInfo,channelInfo)
  
  
  return(rbind(marksWithNoChannel,marksWithChannel))
}

# making a data frame that indicates the relationship between charts and special chart types
# essentially, reduces calculating item multiple times

## Chart Types

getChartRln<-function(datSub = NULL){
  tmp<-apply(datSub[,c("chartType","specialChartType")],1,function(x){
    
    chartType<-unlist(strsplit(x[1],";"))
    specialChartType<-unlist(strsplit(x[2],";"))
    
    if(length(chartType) == length(specialChartType)){
      return(cbind(chartType,specialChartType))
    }else{
      if(!is.na(x[2])){
        return(cbind(x[1],x[2]))
      }
    }
  }) %>% do.call(rbind,.) %>% 
    data.frame()
  
  chartRln<-tmp%>%  na.omit() %>%
    filter(specialChartType  != "NA")%>%
    group_by(chartType,specialChartType)%>%
    count()
  
  return(chartRln)
}