# About Data checking and quality control 

# funModeling package -----------------------------------------------------

install.packages("funModeling") 
library(funModeling)

status(heart_disease)

describe(heart_disease)

di=data_integrity(heart_disease)

# returns a summary
summary(di)

# print all the metadata information
print(di)


# Plotting distributions for numerical variables
plot_num(heart_disease)

# Retrieves several statistics for numerical variables.
profiling_num(heart_disease)

# Note:
# plot_num and profiling_num automatically exclude non-numeric variables

# Getting frequency distributions for categoric variables
# Frequency distribution
freq(heart_disease)

# Retrieves R metric (or Pearson coefficient) for all numeric variables, 
# skipping the categoric ones.
correlation_table(heart_disease) # wrong usage

correlation_table(heart_disease, "has_heart_disease")

# 1. For further functions see the Vignette of funModeling package 

# Further reading 
# 2. https://livebook.datascienceheroes.com/


# dlookr package ----------------------------------------------------------

install.packages("dlookr")
library(dlookr)


# diagnose() provides basic diagnostic information for variables.
# diagnose_category() provides detailed diagnostic information for categorical variables.
# diagnose_numeric() provides detailed diagnostic information for numerical variables.
# diagnose_outlier() and plot_outlier() provide information and visualization of outliers.

diagnose(iris)

# Select columns by name
diagnose(iris, Sepal.Length)

# Select all columns between 
diagnose(iris, Sepal.Length:Petal.Width)

# By using with dplyr, variables including missing values 
# can be sorted by the weight of missing values.
library(tidyverse)
library(nycflights13)


flights %>%
  diagnose() %>%
  select(-unique_count, -unique_rate) %>% 
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count))

# diagnose_numeric() diagnoses numeric(continuous and discrete) variables 
# in a data frame. Usage is the same as diagnose() but returns more diagnostic 
# information

diagnose_numeric(iris)
diagnose_category(iris)


diagnose_numeric(flights)

# diagnose_outlier() diagnoses the outliers of the numeric (continuous and discrete) 
# variables of the data frame.

diagnose_outlier(iris)

diagnose_outlier(flights)

flights %>%
  plot_outlier(arr_delay) 

# Note that:
# Looking at the results of the visualization, arr_delay shows that the 
# observed values without outliers are similar to the normal distribution. 
# In the case of a linear model, we might consider removing or imputing outliers.

# For further reading:

# https://cran.r-project.org/web/packages/dlookr/vignettes/diagonosis.html


# skimr package -----------------------------------------------------------

install.packages("skimr")
library(skimr)

get_default_skimmer_names()


skim(iris)

# Has a useful summary function
skim(iris) %>%
  summary()

# Individual columns can be selected

skim(iris, Sepal.Length, Petal.Length)

# skim() can handle data that has been grouped using dplyr::group_by()

iris %>%
  dplyr::group_by(Species) %>%
  skim()


# For further reading:

# https://github.com/ropensci/skimr

# https://www.business-science.io/code-tools/2021/03/09/data-quality-with-skimr.html

# DataExplorer package ----------------------------------------------------

# There are 3 main goals for DataExplorer:
#   
# 1.Exploratory Data Analysis (EDA)
# 2.Feature Engineering
# 3. Data Reporting

install.packages("DataExplorer")
library(DataExplorer)

plot_str(flights)
plot_str(chol)

introduce(flights) %>% 
  rmarkdown::paged_table()

plot_intro(flights)

# More details about missing 

plot_missing(flights)
plot_missing(starwars)

plot_bar(chol)

plot_bar(chol, by = "MORT")

plot_boxplot(chol, by = "SMOKE")
plot_bar_category(chol, by = "SMOKE")
plot_scatterplot(chol, by = "HEIGHT")
plot_scatterplot(CMN168data, by = "OVERALL")

plot_histogram(chol)
plot_box_numeric(chol)

plot_correlation(na.omit(chol), maxcat = 5L)

data_list <- list(airlines, airports, flights, planes, weather)
plot_str(data_list)

# For further reading 

# https://boxuancui.github.io/DataExplorer/articles/dataexplorer-intro.html

# Others

# https://www.littlemissdata.com/blog/simple-eda

# https://cran.r-project.org/web/packages/validate/vignettes/cookbook.html#2_Variable_checks
