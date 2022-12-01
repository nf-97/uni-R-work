# create a dataset for fisheries
Year <- rep(c("2015","2016", "2017", "2018", "2019", "2020"),5)
Stage <-  c(rep("Juvenile" , 6) , rep("Immature 1" , 6) , rep("Immature 2" , 6) , rep("Immature 3" , 6), rep("Adult", 6))
value <- c(0, 1923.916288, 1429.469802, 1062.096063, 789.1373747, 586.3290694,
           0, 0, 659.9032868, 490.3081421, 364.2989496, 270.6741195,
           0, 0, 0, 490.3081421, 364.2989496, 270.6741195,
           0, 0, 0, 0, 364.2989496, 270.6741195,
           8512, 6324.416, 4699.041088, 3491.387528, 2594.100934, 2198.091113)
total <- 2*(value)
fishery <- data.frame(Year,Stage,value)

# stacked bar chart for fisheries dataset
library(ggplot2)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
scale_fill_manual(values=cbPalette)
ggplot(fishery, aes(fill=Stage, y=value, x=Year)) + 
 geom_bar(position="stack", stat="identity") +
scale_fill_manual(values=cbPalette) +
theme_linedraw() +
labs(x = "Year", y = "Total Population Number") +
theme(axis.title = element_text(size=15), 
legend.title = element_text(size=15), 
legend.text = element_text(size=13), 
axis.text = element_text(size=13)) +
scale_y_continuous(breaks=seq(0, 20000, 2500),
expand = c(0,0), 
limits = c(0,20000)) 

# dataset for no fisheries
Year_no <- rep(c("2015","2016", "2017", "2018", "2019", "2020"),5)
Stage_no <-  c(rep("Juvenile" , 6) , rep("Immature 1" , 6) , rep("Immature 2" , 6) , rep("Immature 3" , 6), rep("Adult", 6))
value_no <- c(0, 2693.477696, 2001.253928, 1486.931669, 1104.79023, 820.8591407,
           0, 0, 923.8628497, 686.4300973, 510.0175623, 378.9430488, 
           0, 0, 0, 686.4300973, 510.0175623, 378.9430488,
           0, 0, 0, 0, 510.0175623, 378.9430488,
           8512, 6324.416, 4699.041088, 3491.387528, 2594.100934, 2306.360042)
total_no <- 2*(value_no)
no_fishery <- data.frame(Year_no,Stage_no,value_no)

# dataset for both
Year <- rep(c("2015","2016", "2017", "2018", "2019", "2020"),10)
Stage <-  c(rep("Juvenile" , 6) , rep("Immature 1" , 6) , rep("Immature 2" , 6) , rep("Immature 3" , 6), rep("Adult", 6), rep("Juvenile" , 6) , rep("Immature 1" , 6) , rep("Immature 2" , 6) , rep("Immature 3" , 6), rep("Adult", 6))
Data <- c(rep("Fishery Present", 30), rep("Fishery Absent", 30))
value_comb <- c(0,1923.916288, 1429.469802, 1062.096063, 789.1373747, 586.3290694,
           0, 0, 659.9032868, 490.3081421, 364.2989496, 270.6741195,
           0, 0, 0, 490.3081421, 364.2989496, 270.6741195,
           0, 0, 0, 0, 364.2989496, 270.6741195,
           8512, 6324.416, 4699.041088, 3491.387528, 2594.100934, 2198.091113, 
           0, 2693.477696, 2001.253928, 1486.931669, 1104.79023, 820.8591407,
           0, 0, 923.8628497, 686.4300973, 510.0175623, 378.9430488, 
           0, 0, 0, 686.4300973, 510.0175623, 378.9430488,
           0, 0, 0, 0, 510.0175623, 378.9430488,
           8512, 6324.416, 4699.041088, 3491.387528, 2594.100934, 2306.360042)
total <- 2*(value_comb)
combined <- data.frame(Year,Stage,Data, value_comb)

#stacked barchart with both sets of data
library(ggplot2)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
graph_combined <- ggplot(combined, aes(fill=Stage, y=value_comb, x=Year)) + 
geom_bar(position="stack", stat="identity") + 
facet_wrap( ~ Data, ncol=2) +
scale_fill_manual(values=cbPalette) +
theme_bw() +
labs(x = "Year", y = "Number of Females") +
theme(text=element_text(size=17, colour="black"), 
axis.title = element_text(size=15, colour="black"), 
legend.title = element_text(size=15, colour="black"), 
legend.text = element_text(size=13, colour="black"), 
axis.text = element_text(size=13, colour="black")) +
scale_y_continuous(breaks=seq(0, 20000, 2500),expand = c(0,0), limits = c(0,10000))                 

# see graph
graph_combined

# stats - comparing predicted stable age distribution 
Predicted <- c("62.50%", "23.66%", "11.19%" , "2.65%", "0.00%")
Projected <- c("62.10%", "24.40%", "10.60%", "2.90%", "0.00%")
prop <- data.frame(Predicted, Projected)

# statistical test
fisher.test(prop$Predicted, prop$Projected) 