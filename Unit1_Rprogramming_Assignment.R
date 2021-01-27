# 1.a
  #With Loop
  for (a in 5:100) {
    result = (a^4) + (5*a^3)
    print(result)
  }
  #Without Loop
  a <- 5:100
  x = (a^4) + (5*a^3)
  x
  
#1.b - Calculate U(40)
  Cal_U40 = function(n) {
    x = numeric(n)
    x[1] = 1
    x[2] = 2
    for(i in 3:n) 
      x[i] = x[i-2] + x[i-1]
    return(x)
  }
  print(Cal_U40(40))
  
#1.c - Calculate cos x, sin x (Taylor series)
  library(pracma)
  x <- as.numeric(readline(prompt="Enter x: "))
  taylor(cos, x, n=45)

#1.d - Create vector of the values of e^xcos(x)
  ex <- seq(3, 6, by=0.1)
  exp(ex)*cos(ex)
  
#1.e - x<-1:300. How many numbers in x are divisible by 2?
  for (i in 1:300) {
    num==0
    if(i%%2==0){
      num = num+1
    }
  }
  print(num)
  
  ####################################
  
  #2.  Solve the following system of linear equations using Gaussian elimination (Ax=y)
  library(matlib)
  A <- matrix(c(1, 2, 3,
                2, 1, 2,
                3,  3, 1), 3, 3, byrow=TRUE)
  b <- c(9, -3, 5)
  result <- gaussianElimination(A, b)
  result
  paste("Result is x =",result[10],' y = ', result[11], ' z =',result[12])
  
  ####################################
  
  #3.Use outer function to create the following matrix
  
  row <- 0:4
  col <- 0:4
  outer(col, row, "+")
  
  ####################################
  
  #4. Get the COVID-19 Dataset from the data sources. Number of observations should be more than 100.Then, report the following information
  
  # a. Data Source detail(Ex: Link) 
  # Dataset on Novel Corona Virus Disease 2019 in India
  # Author: sudalairajkumar
  # URL : https://www.kaggle.com/sudalairajkumar/covid19-in-india
  library(tidyverse)
  dataset <- read_csv("../input/covid19-in-usa/us_counties_covid19_daily.csv")
  covidDataSet <- dataset
  covidDataSet
  
  
  # b. Explain the Unit & Necessity of each variable 
  # This dataset have 6 variables:
  # date: reporting date
  # Country: US countries that has cases of Covid19
  # state: State in US that has cases of Covid19
  # fips: FIPS code in US
  # cases: Number of covid-19 cases in US
  # deaths: Number of fatalities
  
  names(covidDataSet)
  
  
  # c. Find the missing values(rows & columns) and replace them with mean(Tidy Dataset) 
  covidDataSet[] <- lapply(covidDataSet, function(x) { 
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  })
  covidDataSet
  
  
  # d. Generate the two new variables(Var1:Mean, Var2: Median from available variable) 
  covidDataSet %>% mutate(Mean = mean(cases),Median = median(cases))
  
  
  # e. Rename the two existing variables 
  covidDataSet %>% rename(C = cases,D = deaths)
  
  # f. Create a plot using following instructions (using 7 layers of Grammar of Graphics)
  # i. Choose x and y axis(aes) 
  # Choose Date as X and Positive case of covid as Y
  data <- covidDataSet %>% count(date, wt = cases)
  date <- data[['date']]
  cases <- data[['n']]
  data
  
  # ii. geom_point() - specify the parameters, size : 5, color: red, alpha: ??? 
  library(ggplot2)
  gp = geom_point(alpha = 1/5, color = "red", size = 5)
  gp
  p <- ggplot(x = data, mapping = aes(x = date, y = cases)) + gp
  p
  
  # iii. Use Facet grid, cartesian coordinates & geom_smooth() 
  
  # facet_grid
  p + facet_grid(cases)
  # coord_cartesian
  p + coord_cartesian(expand = FALSE)
  #geom_smooth
  p + geom_smooth()
  
  
  
  # iv. Assign the title to x, y and graph 
  p <- p + ggtitle("Number of cases of Covid19 in USA") +
    xlab("Date") + ylab("Number of Cases")
  p
  
  
  # v. Export the graph to your working directory with the title called "covid_19_ dataset.png"
  ggsave(filename = "/covid_19_dataset.png", units = "cm", width = 25, height = 25)
  
  
  
  
  
  
  