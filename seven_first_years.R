library(tidyverse)
library(data.table)

seven_first_years <- function(n){
  
  draft50 <- fread("F:/NBA_20191705/Excel/Draft_analyze/List_drafted_50.csv", index = "Year")
  draftall <- fread("F:/NBA_20191705/Excel/Draft_analyze/List_drafted_corr1.csv", index = "Year")
  undraft <- fread("F:/NBA_20191705/Excel/Draft_analyze/List_undrafted_corr.csv", index = "Year")
  ws <- fread("F:/NBA_20191705/Excel/Draft_analyze/List_ws_vorp.csv", index = "Year")
  
  draft <- draft50[.(n), on = "Year"]
  
  #First year of career
  season1 <- ws[.(n), on = "Year"][,":=" (Year = NULL, Tm = NULL)]
  draft1 <- copy(draft)
  draft1 <- draft1[,":=" (Year = NULL, Tm = NULL)]
  united1 <- merge(draft1, season1, all.x = TRUE)
  united1$Year <- n
  
  #Second year of career
  season2 <- ws[.(n+1), on = "Year"][,":=" (Year = NULL, Tm = NULL)]
  draft2 <- copy(draft)
  draft2 <- draft2[,":=" (Year = NULL, Tm = NULL)]
  united2 <- merge(draft2, season2, all.x = TRUE)
  united2$Year <- n+1
  
  #Third year of career
  season3 <- ws[.(n+2), on = "Year"][,":=" (Year = NULL, Tm = NULL)]
  draft3 <- copy(draft)
  draft3 <- draft3[,":=" (Year = NULL, Tm = NULL)]
  united3 <- merge(draft3, season3, all.x = TRUE)
  united3$Year <- n+2
  
  #Fourth year of career
  season4 <- ws[.(n+3), on = "Year"][,":=" (Year = NULL, Tm = NULL)]
  draft4 <- copy(draft)
  draft4 <- draft4[,":=" (Year = NULL, Tm = NULL)]
  united4 <- merge(draft4, season4, all.x = TRUE)
  united4$Year <- n+3
  
  #Fifth year of career
  season5 <- ws[.(n+4), on = "Year"][,":=" (Year = NULL, Tm = NULL)]
  draft5 <- copy(draft)
  draft5 <- draft5[,":=" (Year = NULL, Tm = NULL)]
  united5 <- merge(draft5, season5, all.x = TRUE)
  united5$Year <- n+4
  
  #Sixth year of career
  season6 <- ws[.(n+5), on = "Year"][,":=" (Year = NULL, Tm = NULL)]
  draft6 <- copy(draft)
  draft6 <- draft6[,":=" (Year = NULL, Tm = NULL)]
  united6 <- merge(draft6, season6, all.x = TRUE)
  united6$Year <- n+5
  
  #Seventh year of career
  season7 <- ws[.(n+6), on = "Year"][,":=" (Year = NULL, Tm = NULL)]
  draft7 <- copy(draft)
  draft7 <- draft7[,":=" (Year = NULL, Tm = NULL)]
  united7 <- merge(draft7, season7, all.x = TRUE)
  united7$Year <- n+6
  
  #Merging tables
  l <- list(united1, united2, united3, united4, united5, united6, united7)
  general <- rbindlist(l)
  general <- na.omit(general)
  
  #Amount of WS & VORP in seven seasons by top-50 draft picks
  general <- general[, .(WS =sum(WS), VORP = sum(VORP)), by = Player]
  
  #Return of round and pick numbers
  general <- merge(draft, general, all.x = TRUE)
  
  #Function to replace NA values on zeros
  f_dowle2 <- function(DT) {
    for (i in names(DT))
      DT[is.na(get(i)), (i):=0]
    return(DT)
  }
  general <- f_dowle2(general)
  general <- general[, ":=" (VORP = round(VORP, digits =1),
                             WS = round(WS, digits =1))]
  return(general)
}
