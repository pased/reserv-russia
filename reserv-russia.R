library(ggvis)

# Sample data from 2014 till 2015-02
data2014 <- read.csv("data2014.csv")
data2014$Date <- as.Date(data2014$Date)

# Linear model with prediction for 2017-04-23
data2014 %>%
  ggvis(~Date, ~Value) %>%
  layer_points() %>%
  layer_lines() %>%
  layer_model_predictions(model="lm", stroke:="red",
                          domain=as.Date(c("2014-01-03","2017-04-23")))

# Broken ggvis code. Trying to do interactive input for prediction period.
data %>%
  ggvis(~Date, ~Value) %>%
  layer_points() %>%
  layer_lines() %>%
  layer_model_predictions(model="lm", stroke:="red",
                          domain=input_radiobuttons(c(o1,o2,o3,o4),
                                  selected=as.Date(c("2014-01-03","2015-12-23")), map=as.Date))

# Nothing important in this chunk.
model <- glm(Value ~ Date, data=data)
model <- lm(Date ~ Value, data=data)
new.df <- data.frame(Date=as.Date("2017-04-23"))
predict(model, new.df)

# Scrapping all availiable data from http://www.cbr.ru/hd_base/Default.aspx?Prtid=mrrf_7d
# Table is saved to cbr.html
library(rvest)

reservru <- html('K:/валютные резервы ЦБ РФ/cbr.html')

testdata <- reservru %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

# Tidying up the data
names(testdata) <- c("Date", "Value")

testdata$Date <- strptime(testdata$Date, "%d.%m.%Y")
testdata$Date <- as.Date(testdata$Date)
testdata$Value <- as.numeric(testdata$Value)

write.csv2(testdata, "data.csv") #Now the data is in a proder data file.

# Working with the whole dataset from 1998-05-29 till 2015-02-20
data <- read.csv2("data.csv")
data$Date <- as.Date(data$Date)

data %>%
  ggvis(~Date, ~Value) %>%
  layer_points()

summary(data)
plot(data$Date, data$Value, ylab="Резервы",xlab="дата",
     main="Валютные резервы ЦБ РФ в млрд. долл. США")
abline(v=as.Date("2008-09-01"), col="blue")
text(x=as.Date("2004-01-01"), y=350, "поднимается с колен")
text(x=as.Date("2009-06-01"), y=550, "бля")
abline(v=as.Date("2014-01-01"), col="red")
text(x=as.Date("2014-06-01"), y=300, "введи войска")
