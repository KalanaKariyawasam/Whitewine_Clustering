library(readxl)
library(zoo)
library(neuralnet)
library(ggpubr)

uow_load <- read_excel("UoW_load.xlsx")

View(uow_load)

summary(uow_load)

boxplot(uow_load[, -1])


normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

uow_load_normalized <- as.data.frame(lapply(uow_load[, -1], normalize))

ggdensity(uow_load_normalized$X11.00, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")

summary(uow_load_normalized)

boxplot(uow_load_normalized)

uow_load_train = uow_load_normalized[1:430,]

View(uow_load_train)

uow_load_test = uow_load_normalized[431:500,]

View(uow_load_test)

#r = zoo(uow_load_train$`11:00`, uow_load_train$Dates)

#s = as.data.frame(uow_load_train[, c(4)])

#uow_load_ts <- ts(r)

#print(uow_load_ts)

X_train = matrix(, nrow = 0, ncol = 4)
y_train = c()

for (x in 1:length(uow_load_train$X11.00)) {
  # find the end of this pattern
  end_ix <- x + 4
  
  # check if we are beyond the sequence
  if (end_ix > length(uow_load_train$X11.00)) {
    break
  }
  # gather input and output parts of the pattern
  seq_x <- uow_load_train$X11.00[x:end_ix] 
  seq_y <- uow_load_train$X11.00[end_ix]
  
  X_train <- rbind(X_train, c(seq_x))
  y_train <- append(y_train, seq_y)
}

# convert the matrix into data frame
dataframe_data = cbind(as.data.frame(X_train), y_train)

# print data frame data
print(dataframe_data)

View(dataframe_data)

net.sqrt <- neuralnet(y_train ~ V1 + V2 + V3 + V4, data = dataframe_data)
print(net.sqrt)

plot(net.sqrt)

X_test = matrix(, nrow = 0, ncol = 4)
y_test = c()

for (x in 1:length(uow_load_test$X11.00)) {
  # find the end of this pattern
  end_ix <- x + 4
  
  # check if we are beyond the sequence
  if (end_ix > length(uow_load_test$X11.00)) {
    break
  }
  # gather input and output parts of the pattern
  seq_x <- uow_load_test$X11.00[x:end_ix] 
  seq_y <- uow_load_test$X11.00[end_ix]
  
  X_test <- rbind(X_test, c(seq_x))
  y_test <- append(y_test, seq_y)
}

print(X_test)
print(y_test)

testdata <- as.data.frame(X_test)

View(testdata)

net.results <- compute(net.sqrt, testdata)

predicted_strength <- net.results$net.result

cor(predicted_strength, y_test)

strength_min <- min(uow_load$`11:00`)
strength_max <- max(uow_load$`11:00`)

unnormalize <- function(x, min, max) {
  return( (max - min)*x + min )
}

strength_pred <- unnormalize(predicted_strength, strength_min, strength_max)

#define RMSE function
rmse <- function(error) {
  sqrt(mean(error^2))
}

error <- (uow_load[435:500,]$`11:00` - strength_pred)

pred_RMSE <- rmse(error)

final_result <- as.data.frame(cbind(uow_load[435:500,]$`11:00`, strength_pred))
colnames(final_result) <- c("Expected Output", "Neural Net Output")