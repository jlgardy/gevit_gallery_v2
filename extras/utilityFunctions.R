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



#Parse the marks (re-ecndoed and added)
parseMarkText<-function(A=NULL){
  #extract whole substrates
  marksWithChannel<-unlist(str_extract_all(A,"[a-z]+\\[[a-z;\\s]+\\]"))
  marksWithNoChannel<-gsub("[a-z]+\\[[a-z\\s]+\\]","",A,perl = T)
  
  #deal with channeless marks first
  marksWithNoChannel<-setdiff(unlist(strsplit(marksWithNoChannel,";")),"")
  marksWithNoChannel<-cbind(marksWithNoChannel,rep(NA,length(marksWithNoChannel)))
  
  channelInfo<-gsub("\\[|\\]","",unlist(str_extract_all(marksWithChannel,"\\[[a-z;\\s]+\\]")),perl=T)
  
  #Extracting 
  tmp2<-gsub("\\[([a-z;]+)\\]","",marksWithChannel,perl=T)
  tmp2<-gsub("\\([a-z]+\\)","",tmp2,perl=T)
  tmp2<-gsub("\\[pie chart\\[[a-z]+\\]\\]","",tmp2,perl=T) #the one edge case of this
  tmp2<-gsub("\\[pie chart\\]","",tmp2,perl=T) #the one edge case of this
  
  markInfo<-unlist(strsplit(tmp2,";"))
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


##Find and removing white space
findWS<-function(data=NULL){
  tmp<-sapply(data,function(x){strsplit(x,";")}) %>%
    lapply(function(x){
      trailing<-grepl("\\s+$",x) %>% as.numeric() %>% sum()
      leading<-grepl("^\\s+",x) %>% as.numeric() %>% sum()
      trailing + leading
    }) %>%
    unlist()
  
  return(tmp)
}
