# Visualizing data to gather insights.

################################################################################

# Determining my working directory.
getwd()

# Install the tidyverse library.
install.packages('tidyverse')
# Import the tidyverse library.
library(tidyverse)

# Import a CSV file.
turtle_sales <- read.csv(file.choose(), header=T)

# Viewing the DataFrame.
View(turtle_sales)

################################################################################
# Loading and exploring the data.
################################################################################

# Removing redundant columns (Ranking, Year, Genre, Publisher) by creating a 
# subset of the DataFrame.
sales_df = subset(turtle_sales, select = -c(Ranking, Year, Genre, Publisher))

# Viewing new, reduced DataFrame.
View(sales_df)

# Creating a summary of the new DataFrame.
# Viewing first 5 rows of sales_df.
head(sales_df, n=5)

# Viewing last 5 rows of sales_df.
tail(sales_df, n=5)

# Dimensions of sales_df.
dim(sales_df)

# Internal structure of sales_df.
str(sales_df)

# Importing psych library.
install.packages("psych")
library("psych")

# Getting summary table for the continuous datasets (all the sales data).
describe(sales_df[ , c('NA_Sales', 'EU_Sales', 'Global_Sales')],fast=TRUE)

# Importing DescTools library.
install.packages("DescTools")
library ("DescTools")

# Getting mode of Product column.
mode_product <- Mode(sales_df[,1])
mode_product

# Summary of the sales data.
summary(sales_df)
################################################################################
# Create plots to review and determine insights into sales data set.
################################################################################

# Scatterplot

# Importing ggplot2.
installed.packages('ggplot2')
library(cowplot)

# Scatterplot of NA_Sales vs EU_Sales.
qplot(NA_Sales, EU_Sales, data=sales_df)

# Scatterplot for NA_Sales vs Global_Sales.
qplot(NA_Sales, Global_Sales, data=sales_df)

# Scatterplot for EU_Sales vs Global_Sales.
qplot(EU_Sales, Global_Sales, data=sales_df)

# Scatterplot for Product.
qplot(y=Product, data=sales_df)

# Scatterplot for product vs NA_Sales.
qplot(Product, NA_Sales, data=sales_df) + ggtitle("Product vs NA_Sales")

# Scatterplot for product vs EU_Sales.
qplot(Product, EU_Sales, data=sales_df) + ggtitle("Product vs EU_Sales")

# Scatterplot for product vs Global_Sales.
qplot(Product, Global_Sales, data=sales_df) + ggtitle("Product vs Global_Sales")


# Histogram

# Histogram for product.
qplot(Product, bins=13, data=sales_df) + ggtitle("Histogram for product")

# Barchart

# Barchart for Platform.
qplot(Platform, data=sales_df) + ggtitle("Platform count")

# Boxplot
# Produxt BoxPlot(BP).
Product_BP<- ggplot(sales_df, aes(x=Product))  + 
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
  ggtitle("Product BoxPlot")

Product_BP

# NA_Sales BoxPlot(BP).
NA_Sales_BP<- ggplot(sales_df, aes(x=NA_Sales))  + 
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
  ggtitle("NA_Sales BoxPlot")

NA_Sales_BP

# EU_Sales BoxPlot.
EU_Sales_BP <- ggplot(sales_df, aes(x=EU_Sales)) +
  stat_boxplot(geom='errorbar') +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 11))+
  ggtitle("EU_Sales Box Plot")

EU_Sales_BP

# Global_Sales BoxPlot.
Global_Sales_BP <- ggplot(sales_df, aes(x=Global_Sales), xlim=40) +
  stat_boxplot(geom='errorbar', width=0.5) +
  geom_boxplot() +
  # Eradicated one extreme outlier>40.
  coord_cartesian(xlim = c(0, 40)) +
  ggtitle("Global_Sales Box Plot")

Global_Sales_BP

# Global_Sales split by Platform.
Global_Sales_Platform_BP <- 
  ggplot(sales_df, aes(x=Global_Sales, y=Platform), xlim=40) +
  stat_boxplot(geom='errorbar', width=0.5) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 40)) +
  ggtitle("Global Sales split by platform Box Plot")

Global_Sales_Platform_BP

# EU_Sales split by Platform.
EU_Sales_Platform_BP <- ggplot(sales_df, aes(x=EU_Sales, y=Platform), xlim=40) +
  stat_boxplot(geom='errorbar', width=0.5) +
  geom_boxplot() +
  # Eradicated one extreme outlier>30.
  coord_cartesian(xlim = c(0, 13))  +
  ggtitle("EU_Sales split by platform Box Plot")

EU_Sales_Platform_BP

# NA_Sales split by Platform.
NA_Sales_Platform_BP <- ggplot(sales_df, aes(x=NA_Sales, y=Platform), xlim=40) +
  stat_boxplot(geom='errorbar', width=0.5) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 40)) +
  ggtitle("NA_Sales split by Platform")

NA_Sales_Platform_BP
################################################################################
# Noting some main insights for the business:

# The main aim of the work above was to identify the impact that each product 
# has on sales. Therefore, the analysis done on the Sales per Platform is not
# necessary and will not be included in my final analysis. 

# I will be including the histogram I did for Product. This histogram indicates
# the spread of the product, and how many of each product exist. The histogram 
# shows that the number of products beyond 3750 (approximately) aren't as many
# productions of the product as below that amount. This could give insight into
# whether this may be a factor in the dispersion of the sales.

# The next plots I will be including are the scatter plots for Product vs 
# NA_Sales, Product vs EU_Sales, and Product vs Global_Sales.

# The scatter plots for Product vs NA_Sales show that there are more products 
# sold below Product 2500 than there are beyond 2500. Is this because the games
# above Product 2500 simply aren't as popular, or is this due to a lack in 
# production of the product?

# The scatter plot for Product vs EU Sales show a similar trend to NA_Sales in 
# that EU_Sales decrease as Product number increases. However, more NA_Sales are
# made than EU_Sales. The population in EU is higher (748 million) compared to
# North America (597 million). So, it's worth exploring why this may be the case?
# Are there more gaming enthusiasts in America versus EU? If so, Turtle Games 
# could invest more in North America, and capitalise on this finding. Or is 
# there less exposure to Turtle Games in other regions of the world?

# The scatter plot for Product vs Global Sales looks similar to the other graphs.
# However the products sold beyond Product 5000 are still quite low, meaning 
# globally the products that sold low in NA and EU also sold low in other places
# too. 

# I will also include the boxplots as they indicate skewness of the sales and 
# products.

# The Product BoxPlot is skewed to the right (positively skewed). It shows that 
# more products are produced that have product ID close to Q1 (1945) and Q2 
# (3340) than above.

# NA_Sales is skewed to the right.This means that there aren't as many more
# expensive sales made as there are sales made in the lower end of the scale. 
# This is true for EU_Sales and Global_Sales too.

################################################################################

# CLEANING, MANIPULATING, AND VISUALISING THE DATA

################################################################################
# Loading an exploring the data.
################################################################################

# Viewing the first 5 rows of sales_df DataFrame.
head(sales_df, n=5)

# Checking dimensions.
dim(sales_df)

# Structure of sales_df.
str(sales_df)

# Determining min, max, and mean values of all the sales data.
# NA_Sales: min, max, and mean.
min(sales_df$NA_Sales)
max(sales_df$NA_Sales)
mean(sales_df$NA_Sales)

# EU_Sales: min, max, and mean.
min(sales_df$EU_Sales)
max(sales_df$EU_Sales)
mean(sales_df$EU_Sales)

# Global_Sales: min, max, and mean.
min(sales_df$Global_Sales)
max(sales_df$Global_Sales)
mean(sales_df$Global_Sales)

# Summary of sales_df.
summary(sales_df)

################################################################################
# Determining impact on sales per product_id
################################################################################

# Creating new Data Frame without the Platform column.
sales_product_df <- subset(sales_df, select = -c(Platform) )

# Viewing new Data Frame.
head(sales_product_df, n=2)

# Summing the sales value grouped by product_id
# and saving this into a new Data Frame.
sales_product_sum <- aggregate(. ~ Product, sales_product_df, sum)

# Viewing first two rows of Data Frame.
head(sales_product_sum, n=2)

# Summary of sales_product_sum Data Frame.
summary(sales_product_sum)

################################################################################
# Create plots to gain insights into the sales data.
################################################################################

# Creating scatter plots, histograms, and box plots to gain insight into the 
# sales data.

# Histogram for frequency of product.
Product_NA_Histogram <- hist(sales_product_sum$Product)

# Barplot for product vs sum of NA_Sales.
barplot(sales_product_sum$NA_Sales ~ sales_product_sum$Product, 
        main="Product vs sum of NA_Sales",
        xlab="Product",
        ylab="NA_Sales sum",
        density=10)


# Bar plot for product vs EU_Sales.
barplot(sales_product_sum$EU_Sales ~ sales_product_sum$Product, 
        main="Product vs sum of EU_Sales",
        xlab="Product",
        ylab="EU_Sales sum",
        density=10) 

# Bar plot for product vs Global_Sales.
barplot(sales_product_sum$Global_Sales ~ sales_product_sum$Product, 
        main="Product vs sum of Global_Sales",
        xlab="Product",
        ylab="NA_Sales sum",
        density=10)

# Box plot for sum of NA_Sales.
NA_Sales_Sum_boxplot <- ggplot(sales_product_sum, aes(x=NA_Sales), xlim=40) +
  stat_boxplot(geom='errorbar', width=0.5) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 35)) 

NA_Sales_Sum_boxplot

# Box plot for sum of EU_Sales.
EU_Sales_Sum_boxplot <- ggplot(sales_product_sum, aes(x=EU_Sales), xlim=40) +
  stat_boxplot(geom='errorbar', width=0.5) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 25)) 

EU_Sales_Sum_boxplot

# Box plot for sum of Global_Sales.
Global_Sales_Sum_boxplot <- ggplot(sales_product_sum, aes(x=Global_Sales), 
                                   xlim=40) +
  stat_boxplot(geom='errorbar', width=0.5) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 65)) 

Global_Sales_Sum_boxplot

# Creating a line graph comparing NA_Sales, EU_Sales, and Global_Sales.
plot(x=sales_product_sum$Product, y=sales_product_sum$NA_Sales, pch = 19,
     main='Comparison of sum of sales per product',
     xlab = "Product",
     ylab = "Sum of Sales",
     ylim=c(0, 68),
     type="l")

# Combining EU_Sales to the above plot, with the line being a blue colour. 
lines(x=sales_product_sum$Product, y=sales_product_sum$EU_Sales, pch = 19, 
      col='blue')

# Adding line graph containing Global_Sales in the colour red.
lines(x=sales_product_sum$Product, y=sales_product_sum$Global_Sales, pch=19,
      col='red')

# Adding a legend for ease of understanding the graph.
legend("topright", legend=c("NA_Sales", "EU_Sales", "Global_Sales"), 
       cex=0.6, inset=0.01, pch=NULL, 
       lty=1:1,
       col=c("black", "blue", "red"))

################################################################################

# DETERMINING THE NORMALITY OF THE DATASETS

################################################################################
# Creating and exploring Q-Q plots for all sales data. 
################################################################################

# QQ plot for NA_Sales.
qqnorm(sales_product_sum$NA_Sales, main="NA_Sales Normal Q-Q Plot")
qqline(sales_product_sum$NA_Sales)

# QQ plot for EU_Sales.
qqnorm(sales_product_sum$EU_Sales, main="EU_Sales Normal Q-Q Plot")
qqline(sales_product_sum$EU_Sales)

# QQ plot for Global_Sales.
qqnorm(sales_product_sum$Global_Sales, main="Global_Sales Normal Q-Q Plot")
qqline(sales_product_sum$Global_Sales)

################################################################################
# Performing a Shapiro-Wilk test on all the sales data.
################################################################################

# Shapiro-Wilk test for NA_Sales.
shapiro.test(sales_product_sum$NA_Sales)

# Shapiro-Wilk test for EU_Sales.
shapiro.test(sales_product_sum$EU_Sales)

# Shapiro-Wilk test for Global_Sales.
shapiro.test(sales_product_sum$Global_Sales)

# A p-value <= 0.05 indicates that the test rejects the hypothesis of normality.
# In the case of these sales data, all the p-values are less than 0.05, and thus
# show that the datasets aren't normally distributed.

################################################################################
# Determining the Skewness and Kurtosis of the sales data.
################################################################################

install.packages("moments")
library(moments)

# Skewness and Kurtosis for NA_Sales.
skewness(sales_product_sum$NA_Sales) # = 3.048198>1, so highly skewed data.
kurtosis(sales_product_sum$NA_Sales) # = 15.6026>2, so not normally distributed.

# Skewness and Kurtosis for EU_Sales.
skewness(sales_product_sum$EU_Sales) # =  2.886029>1, highly skewed data.
kurtosis(sales_product_sum$EU_Sales) # = 16.22554>2, not normally distributed.

# Skewness and Kurtosis for Global_Sales.
skewness(sales_product_sum$Global_Sales) # = 3.066769>1, highly skewed data.
kurtosis(sales_product_sum$Global_Sales) # = 17.79072>2, not normally distributed.

# Kurtosis value shows the curves to be too peaked. 
# (https://www.smartpls.com/documentation/functionalities/excess-kurtosis-and-skewness/)

################################################################################
# Determining if there is any correlation between the sales data columns.
################################################################################

# Between NA_Sales and EU_Sales.
# Pearson correlation test.
cor(sales_product_sum$NA_Sales, sales_product_sum$EU_Sales, 
    method=c("pearson"))

# Kendall rank correlation test.
cor(sales_product_sum$NA_Sales, sales_product_sum$EU_Sales,
    method=c("kendall"))

# Spearman rank correlation coefficient.
cor(sales_product_sum$NA_Sales, sales_product_sum$EU_Sales,
    method = c("spearman"))


# Between NA_Sales and Global_Sales.
# Pearson correlation test.
cor(sales_product_sum$NA_Sales, sales_product_sum$Global_Sales, 
    method=c("pearson"))

# Kendall rank correlation test.
cor(sales_product_sum$NA_Sales, sales_product_sum$Global_Sales,
    method=c("kendall"))

# Spearman rank correlation coefficient.
cor(sales_product_sum$NA_Sales, sales_product_sum$Global_Sales,
    method = c("spearman"))


# Between EU_Sales and Global_Sales.
# Pearson correlation test.
cor(sales_product_sum$EU_Sales, sales_product_sum$Global_Sales, 
    method=c("pearson"))

# Kendall rank correlation test.
cor(sales_product_sum$EU_Sales, sales_product_sum$Global_Sales,
    method=c("kendall"))

# Spearman rank correlation coefficient.
cor(sales_product_sum$EU_Sales, sales_product_sum$Global_Sales,
    method = c("spearman"))

################################################################################

# CREATE PLOTS TO GAIN INSIGHTS INTO THE SALES DATA

################################################################################
# Choosing the type of plot that would best suit the data set and what I want to 
# investigate.
################################################################################

# MAKING RECOMMENDATIONS TO THE BUSINESS

################################################################################

# Viewing sales_product_sum data set.
head(sales_product_sum, n=5)
tail(sales_product_sum, n=5)


# Creating a simple linear regression model.

# Importing necessary library.
install.packages("readxl")
library("readxl")

# Between Product and EU_Sales.
# Linear regression.
Product_EU = lm(Product~NA_Sales, data = sales_product_sum) 
# Viewing the results.
summary(Product_EU) 

# Between Product and NA_Sales.
# Linear regression.
Product_NA = lm(Product~EU_Sales, data = sales_product_sum) 
# Viewing the results.
summary(Product_NA)

# Between Product and Global_Sales.
# Linear regression.
Product_GL = lm(Product~Global_Sales, data = sales_product_sum) 
# Viewing the results.
summary(Product_GL)

# Linear regression between NA_Sales and EU_Sales.
NA_EU = lm(NA_Sales~EU_Sales, data = sales_product_sum)
# Viewing the results.
summary(NA_EU)

# Plotting linear regression.
NA_EU_plot = plot(NA_Sales~EU_Sales, data = sales_product_sum) +
  title(main="Linear Regression")
abline(NA_EU)

# Linear regression between NA_Sales and Global_Sales.
NA_GL = lm(NA_Sales~Global_Sales, data = sales_product_sum)
# Viewing the results.
summary(NA_GL)

# Plotting linear regression.
NA_GL_plot = plot(NA_Sales~Global_Sales, data = sales_product_sum) +
  title(main="Linear Regression")
abline(NA_GL)

# Linear regression between NA_Sales and Global_Sales.
EU_GL = lm(EU_Sales~Global_Sales, data = sales_product_sum)
# Viewing the results.
summary(EU_GL)

# Plotting linear regression.
EU_GL_plot = plot(EU_Sales~Global_Sales, data = sales_product_sum) +
  title(main="Linear Regression")
abline(EU_GL)

################################################################################
# CREATING A MULTIPLE LINEAR REGRESSION MODEL
################################################################################

# Installing necessary libraries
install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")
install.packages("plotrix")
library(plotrix)
install.packages("predict3d")
require(predict3d)
require(rgl)

# NA_Sales vs EU_Sales + Global_Sales
NA_EU_GL <- lm(NA_Sales ~ EU_Sales + Global_Sales, data=sales_product_sum)
# View results.
summary(NA_EU_GL) 

# Plotting multiple linear regression.
equation1=function(x){coef(NA_EU_GL)[2]*x+coef(NA_EU_GL)[1]}
equation2=function(x){coef(NA_EU_GL)[2]*x+coef(NA_EU_GL)[1]+coef(NA_EU_GL)[3]}

ggplot(sales_product_sum,aes(y=NA_Sales,x=EU_Sales, color=Global_Sales))+
  geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])

# EU_Sales vs NA_Sales + Global_Sales
EU_NA_GL <- lm(EU_Sales ~ NA_Sales + Global_Sales, data=sales_product_sum)
# View results.
summary(EU_NA_GL) 

# Global_Sales vs EU_Sales + NA_Sales
GL_EU_NA <- lm(Global_Sales ~ EU_Sales + NA_Sales, data=sales_product_sum)
# View results.
summary(GL_EU_NA) 

################################################################################
# PREDICT GLOBAL SALES BASED ON PROVIDED VALUES.
# COMPARE PREDICTED TO OBSERVED VALUES
################################################################################

# Function to calculate predicted Global_Sales_Sum.
Global_predicted <- function(NA_Sales_sum, EU_Sales_sum) {
  Global_predicted_sum <- coef(GL_EU_NA)[3]*NA_Sales_sum + 
    coef(GL_EU_NA)[2]*EU_Sales_sum + 
    coef(GL_EU_NA)[1]
  return(Global_predicted_sum)
}

# a) NA_Sales_sum = 34.02 and EU_Sales_sum = 23.80
Global_predicted(34.02, 23.80) # Observed value =67.85, predicted value = 68.06

# b) NA_Sales_sum = 3.93 and EU_Sales_sum = 1.56
Global_predicted(3.93, 1.56) # observed = 6.04, predicted = 7.36

# c) NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65
Global_predicted(2.73, 0.65) # Observed = 4.32, predicted = 4.908

# d) NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97
Global_predicted(2.26, 0.97) # Observed = 3.53, predicted = 4.76

# e) NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52
Global_predicted(22.08, 0.52) # Observed = 23.21, predicted = 26.626

