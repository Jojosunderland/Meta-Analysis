## Paper screen of SS 168 Rabbits/Hare focused ##

install.packages("devtools")
devtools::install_github("EIvimeyCook/metRscreen")
library(metRscreen)


# String search results 
metRscreen(screen.file = "~/Documents/WorkingD/Meta-Analysis/Coursework/SS_168_RabHar.csv")

# Screen the graphs
install.packages("devtools")
devtools::install_github("daniel1noble/metaDigitise")
library(metaDigitise)

data <- metaDigitise(dir = "~/example_figures/")

