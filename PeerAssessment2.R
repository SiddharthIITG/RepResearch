library("plyr")
library("tm")
library("knitr")
library("gridExtra")

data = read.csv("repdata-data-StormData.csv.bz2")
pdf <- readPDF(control = list(c(text = "-layout")))
pdf <- pdf(elem=list(uri="nws_i10_1605.pdf"),language="en")
keep = c(pdf$content[seq(397, 420)], pdf$content[seq(425, 448)])


freq  = count(data, 'EVTYPE')
freq = arrange(freq, desc(freq))
freq = freq[1:50,]
detach("package:plyr", unload=TRUE)
library("dplyr")
df = merge(data, freq, by = "EVTYPE")
df = filter(df, FATALITIES != 0 | PROPDMG != 0 | CROPDMG != 0)
grouping <- group_by(df, EVTYPE)
sumHealth = summarize(grouping,sumFat = sum(FATALITIES),
                      sumInj =  sum(INJURIES))
sumHealth = arrange(sumHealth, desc(sumFat))

sumHealth = sumHealth[1:5,]
plot1 = ggplot(sumHealth, aes(reorder(factor(EVTYPE), sumFat), 
                       y = sumFat, fill = sumFat)) +
  geom_bar(stat = "identity") +  
  labs(title = "Total fatalities for types of storms.", 
       x = "Storm Type", y = "Total Fatalities") + 
        theme(axis.text.x = element_text(angle=45))

sumHealth = arrange(sumHealth, desc(sumInj))

sumHealth = sumHealth[1:5,]
plot2 = ggplot(sumHealth, aes(reorder(factor(EVTYPE), sumInj), 
                               y = sumInj, fill = sumInj)) +
  geom_bar(stat = "identity") +
  labs(title = "Total injuries for types of storms.", 
       x = "Storm Type", y = "Total Injuries") + 
  theme(axis.text.x = element_text(angle=45))

grid.arrange(plot1, plot2, ncol=2)

df$PROPMUL = 0
df$CROPMUL = 0

df[as.character(df$CROPDMGEXP) == "K",]$CROPMUL = 1000
df[as.character(df$CROPDMGEXP) == "M",]$CROPMUL = 1000000
df[as.character(df$CROPDMGEXP) == "B",]$CROPMUL = 1000000000
df[as.character(df$CROPDMGEXP) == "m",]$CROPMUL = 1000000
df[as.character(df$CROPDMGEXP) == "2",]$CROPMUL = 100
df[as.character(df$CROPDMGEXP) == "k",]$CROPMUL = 1000
df[as.character(df$CROPDMGEXP) == "0",]$CROPMUL = 1


df[as.character(df$PROPDMGEXP) == "B",]$PROPMUL = 1000000000
df[as.character(df$PROPDMGEXP) == "M",]$PROPMUL = 1000000
df[as.character(df$PROPDMGEXP) == "K",]$PROPMUL = 1000
df[as.character(df$PROPDMGEXP) == "0",]$PROPMUL = 1
df[as.character(df$PROPDMGEXP) == "1",]$PROPMUL = 10
df[as.character(df$PROPDMGEXP) == "2",]$PROPMUL = 100
df[as.character(df$PROPDMGEXP) == "3",]$PROPMUL = 1000
df[as.character(df$PROPDMGEXP) == "4",]$PROPMUL = 10000
df[as.character(df$PROPDMGEXP) == "5",]$PROPMUL = 100000
df[as.character(df$PROPDMGEXP) == "6",]$PROPMUL = 1000000
df[as.character(df$PROPDMGEXP) == "7",]$PROPMUL = 10000000
df[as.character(df$PROPDMGEXP) == "8",]$PROPMUL = 100000000
df[as.character(df$PROPDMGEXP) == "m",]$PROPMUL = 1000000
df[as.character(df$PROPDMGEXP) == "H",]$PROPMUL = 100
df[as.character(df$PROPDMGEXP) == "h",]$PROPMUL = 100
df[as.character(df$PROPDMGEXP) == "",]$PROPMUL = 1

df$PROPDMGTOT = df$PROPDMG*df$PROPMUL
df$CROPDMGTOT = df$CROPDMG*df$CROPMUL

grouping <- group_by(df, EVTYPE)
sumDMG = summarize(grouping,sumPROPDMG = sum(PROPDMGTOT),
                      sumCROPDMG =  sum(CROPDMGTOT))
sumDMG
sumDMG$sumDMGTOT = sumDMG$sumPROPDMG + sumDMG$sumCROPDMG
sumDMG = arrange(sumDMG, desc(sumDMGTOT))
sumDMGhead = sumDMG[1:10,]
ggplot(sumDMGhead, aes(reorder(factor(EVTYPE), sumDMGTOT), 
                       y = sumDMGTOT, fill = sumDMGTOT)) +
    geom_bar(stat = "identity") + coord_flip() + 
  labs(title = "Total damage for types of storms.", 
       x = "Storm Type", y = "Total Damage")

sumDMG = arrange(sumDMG, desc(sumPROPDMG))
sumDMGhead = sumDMG[1:5,]
plot1 = ggplot(sumDMGhead, aes(reorder(factor(EVTYPE), sumPROPDMG), 
                       y = sumPROPDMG, fill = sumPROPDMG)) +
  geom_bar(stat = "identity")  + 
  labs(title = "Total property damage for types of storms.", 
       x = "Storm Type", y = "Total Property Damage") + 
          theme(axis.text.x = element_text(angle=45))

sumDMG = arrange(sumDMG, desc(sumCROPDMG))
sumDMGhead = sumDMG[1:5,]
plot2 = ggplot(sumDMGhead, aes(reorder(factor(EVTYPE), sumCROPDMG), 
                       y = sumCROPDMG, fill = sumCROPDMG)) +
  geom_bar(stat = "identity")  + 
  labs(title = "Total crop damage for types of storms.", 
       x = "Storm Type", y = "Total Crop Damage") + 
        theme(axis.text.x = element_text(angle=45))

grid.arrange(plot1, plot2, ncol=2)