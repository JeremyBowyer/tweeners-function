tweeners_func <- function(df,category,date,values,tweeners) {
  tempDF <- df
  
  tempDF[,category] <- as.character(tempDF[,category])
  tempDF <- tempDF[order(tempDF[,category],tempDF[,date]),]
  
  original_dates <- unique(tempDF[,date])
  original_dates <- original_dates[!is.na(original_dates)]
  
  extra_cols <- names(tempDF)[!names(tempDF) %in% c(category,date,values)]
  
  tempDF[,date] <- paste0(tempDF[,date]," (0)")
  
  categories <- unique(tempDF[,category])
  categories <- categories[!is.na(categories)]
  
  dates <- unique(tempDF[,date])
  dates <- dates[dates != "NA (0)"]
    
  for (c in 1:length(categories)) {
    for (y in 1:(length(dates)-1)) {
      for (t in 1:tweeners) {
        tempDF[nrow(tempDF)+1,category] <- categories[c]
        tempDF[nrow(tempDF),date] <- paste0(original_dates[y]," (",t,")")
        for (v in 1:length(values)) {
        tempDF[nrow(tempDF),values[v]] <- (((tempDF[tempDF[,category] == categories[c] & tempDF[,date] == dates[y+1],values[v]] -
                                         	 tempDF[tempDF[,category] == categories[c] & tempDF[,date] == dates[y],values[v]])/(tweeners+1)) * t +
                                         	 tempDF[tempDF[,category] == categories[c] & tempDF[,date] == dates[y],values[v]])
        }
        tempDF[nrow(tempDF),extra_cols] <- tempDF[tempDF[,category] == categories[c] & tempDF[,date] == dates[y],extra_cols]
      }
    }
  }
  tempDF[order(tempDF[,category],tempDF[,date]),]
}