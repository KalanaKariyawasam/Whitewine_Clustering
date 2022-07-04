install.packages("readxl")
library(readxl)

# reading white wine dataset
whitewine_v2 <- read_excel("Whitewine_v2.xlsx")
View(whitewine_v2)