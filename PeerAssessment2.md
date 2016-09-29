---
title: "Peer Assessment 2"
author: "SiddharthIITG"
date: "21 October 2015"
output: html_document
---




```r
library("plyr")
library("dplyr")
library("tm")

data = read.csv("repdata-data-StormData.csv.bz2")
pdf <- readPDF(control = list(c(text = "-layout")))
pdf <- pdf(elem=list(uri="nws_i10_1605.pdf"),language="en")
keep = c(pdf$content[seq(397, 420)], pdf$content[seq(425, 448)])
keep
```

```
##  [1] "Astronomical Low Tide"    "Avalanche"               
##  [3] "Blizzard"                 "Coastal Flood"           
##  [5] "Cold/Wind Chill"          "Debris Flow"             
##  [7] "Dense Fog"                "Dense Smoke"             
##  [9] "Drought"                  "Dust Devil"              
## [11] "Dust Storm"               "Excessive Heat"          
## [13] "Extreme Cold/Wind Chill"  "Flash Flood"             
## [15] "Flood"                    "Frost/Freeze"            
## [17] "Funnel Cloud"             "Freezing Fog"            
## [19] "Hail"                     "Heat"                    
## [21] "Heavy Rain"               "Heavy Snow"              
## [23] "High Surf"                "High Wind"               
## [25] "Hurricane (Typhoon)"      "Ice Storm"               
## [27] "Lake-Effect Snow"         "Lakeshore Flood"         
## [29] "Lightning"                "Marine Hail"             
## [31] "Marine High Wind"         "Marine Strong Wind"      
## [33] "Marine Thunderstorm Wind" "Rip Current"             
## [35] "Seiche"                   "Sleet"                   
## [37] "Storm Surge/Tide"         "Strong Wind"             
## [39] "Thunderstorm Wind"        "Tornado"                 
## [41] "Tropical Depression"      "Tropical Storm"          
## [43] "Tsunami"                  "Volcanic Ash"            
## [45] "Waterspout"               "Wildfire"                
## [47] "Winter Storm"             "Winter Weather"
```


```r
freq  = count(data, 'EVTYPE')
freq = arrange(freq, desc(freq))
```

```
## Error in arrange_impl(.data, dots): Value of SET_STRING_ELT() must be a 'CHARSXP' not a 'expression'
```

```r
freq = freq[1:50,]
df = merge(data, freq, by = "EVTYPE")
```

```
## Error in fix.by(by.y, y): 'by' must specify a uniquely valid column
```

```r
df = filter(df, FATALITIES != 0 | PROPDMG != 0 | CROPDMG != 0)
```

```
## Error in UseMethod("filter_"): no applicable method for 'filter_' applied to an object of class "function"
```

```r
grouping <- group_by(df, EVTYPE)
```

```
## Error in UseMethod("group_by_"): no applicable method for 'group_by_' applied to an object of class "function"
```

```r
sumHealth = summarize(grouping,sumFat = sum(FATALITIES),
                      sumInj =  sum(INJURIES))
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'grouping' not found
```

```r
arrange(sumHealth, desc(sumFat))
```

```
## Error in arrange_(.data, .dots = lazyeval::lazy_dots(...)): object 'sumHealth' not found
```


```r
head(sumHealth)
```

```
## Error in head(sumHealth): object 'sumHealth' not found
```
