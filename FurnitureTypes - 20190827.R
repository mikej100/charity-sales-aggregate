#  AS a marketing mananager
# I want to plot averagle selling price against median time to sel
# So that I can diferentiate between types of goods which sell
# quickly and are valuabe from slow to sell and low value types.

getwd(  )
library(tidyverse)
library(stringr)
library(plotly)
df <- read.csv("Sales details.csv", header = TRUE)
# names(df)
#levels(df$ArticleDescription)

# aggregate by article description, assigning a name to the vector and
# leaving function and name open for definiton.
aggByDesc <- function (x, FUN, na.rm, name) {
  setNames (
    aggregate (x=x,
               by=list(df$ArticleDescription),
               FUN = FUN ,
               na.rm = na.rm ),
    c("description", name=name))
}
aggByDescMean <-  function(x, name) aggByDesc(x=x,
                                              FUN=mean, 
                                              na.rm=TRUE,
                                              name=name
)


aggByDescMedian <- function(x, name) aggByDesc(x=x,
                                               FUN=median, 
                                               na.rm=TRUE,
                                               name=name
)


aggByDesc.Count <- {
  setNames (
    aggregate(df$PriceSold,
                           by=list(df$ArticleDescription),
                             FUN=length
                             ),
        c("description", "count"))
      }
    
aggByDescMea.Price <- aggByDescMean(x = df$PriceSold, 
                                     name="meanPrice")

aggByDescMedian.Time <- aggByDescMedian(x= df$TimetoSell, 
                                        name="medianTime")


aggByDescMean.Price[ 1:3,] 
aggByDescMedian.Time[ 1:3,] 
aggByDesc.Count[ 1:3,] 

timeVsPrice <- Reduce (
    function(x, y) merge (x=x, y=y, by = "description"), 
    list(aggByDescMean.Price,
         aggByDescMedian.Time,
         aggByDesc.Count) )
timeVsPrice[1:3,]

    pl1 <- function (df)   ggplot( data = df) +
        geom_point(mapping = aes(x = medianTime, 
                             y = meanPrice,
                           size = count
      ))

  pl1(df) <- function(df) ggplot( data = df) +
    geom_point(mapping = aes(x = medianTime, 
                             y = meanPrice,
                             size = count,
                             text = paste(description,
                                          "<br>£", round(meanPrice),
                                          ", ", round(medianTime), " days",
                                          "<br> ", count, " items")
    )) +
    scale_x_log10() +
    ggtitle("Articles by time to sell and price sold")
  
  pl2(df) <-  ggplotly(pl1(df), tooltip="text")
  
  pl2(tv)
  knames(df )

  
FtypeByADesc <- df[unique(df$ArticleDescription),
                     c("FurnitureType", "ArticleDescription")]
  
unique(df$ArticleDescription)
  

        FtypeByADesc[1:3,]
            priceThreshold <- c(60,25,10)
timeThresshold <- c(10,50)
aggByDescMean.Price

tv <- timeVsPrice
tv[1:3,]
tp1 <- tv[tv$meanPrice >= priceThreshold[1],]

pl(tp1)
  
  tp1t1 <- tv[tv$meanPrice >priceThreshold[1] & tv$medianTime > timeThresshold[1],]

tplist <- lapply(priceThreshold, tv[tv$meanPrice > priceThreshold]) 
  
    itemsHH <- timeVsPrice[]
        k