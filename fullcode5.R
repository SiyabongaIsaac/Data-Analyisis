install.packages("nlme")

install.packages("tidyverse")

install.packages("sjPlot")

install.packages("gridExtra")

install.packages("ggplot2")

library(nlme)

library(tidyverse)

library(sjPlot)

library(ggplot2)

# Set random seed

set.seed(123)

# Generate the data

num_students <- 60

num_classes_per_school <- 3

num_schools <- 2

teaching_methods <- c("Method 1", "Method 2", "Method 3")


nestdata <- data.frame(
  
  Student = paste("S", 1:num_students, sep=""),
  
  Class = rep(paste("C", 1:num_classes_per_school, sep=""),
              
             each=num_students/(num_classes_per_school*num_schools)),
  
  School = rep(rep(paste("School", c(" A", " B"), sep=""),
                   
                   each=num_students/(num_classes_per_school*num_schools))),
  
  Teaching_Method = rep(rep(teaching_methods, 
                            
                            each=num_students/(num_classes_per_school*num_schools)),
                        
                        times=num_classes_per_school*num_schools),
  
  Test_Score = round(rnorm(num_students, mean=75, sd=10))
)

nestdata$Class <- with(nestdata, paste(School, Class, sep="_"))

# Saving the data set

write.csv(nestdata, "Modified_dataset.csv", row.names = FALSE)

cat("Dataset saved as 'modified_dataset.csv'\n")


data.nest <- read.csv("C:/Users/siyab/OneDrive/Documents/modified_dataset.csv")

# Create a box plot

ggplot(data.nest, aes(x = Teaching_Method, y = Test_Score, fill = Class)) +
  
  geom_boxplot() +
  
  labs(x = "Teaching Method", y = "Test Score", fill = "") +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

# Fit a mixed-effects model

data.nest.lme <- lme(Test_Score ~ Teaching_Method, random = ~ 1 | School/Class,
                     data = data.nest)

summary(data.nest.lme)


interaction.plot(data.nest$School, data.nest$Teaching_Method, data.nest$Test_Score, 
                 
                 xlab = "School", ylab = "Test Score", legend = FALSE)

unique_methods <- unique(data.nest$Teaching_Method)

# Define line types and colors for the legend

line_types <- c(3, 2, 1)  # 3 = solid, 2 = dashed, 1 = dotted

line_colors <- c("black", "black", "black")

# Create a legend

legend("topright", legend = unique_methods, col = line_colors, 
       
       lty = line_types, cex = 0.8)

plot_grid(plot_model(data.nest.lme, type = "diag"))


# Missing values 

missing.data <- 
  read.csv("C:/Users/siyab/OneDrive/Documents/FIXmodified_dataset_with_missing.csv")

summary(missing.data)

nes <-missing.data

nes1 <- nes$Test_Score # accessing the test results 

summary(nes1)

p <- data.frame(na.omit(nes1)) #removing the missing observation 

p1 <- summary(p) #summary of the dataset without missingness

data <- nes

# Remove missing values 

data_cleaned <- data[complete.cases(data), ] 

summary(data_cleaned$Test_Score)

data.cleaned.lme <- lme(Test_Score ~ Teaching_Method,
                        random = ~ 1 | School/Class, 
                        data = data_cleaned)

summary(data.cleaned.lme )

interaction.plot(data_cleaned$School, data_cleaned$Teaching_Method,
                 data_cleaned$Test_Score, 
                 
                 xlab = "School", ylab = "Test Score", legend = FALSE)

unique_methods <- unique(data_cleaned$Teaching_Method)

line_types <- c(3, 2, 1)  # 3 = solid, 2 = dashed, 1 = dotted

line_colors <- c("black", "black", "black")

# Create a custom legend with line types and colors

legend("topright", legend = unique_methods, col = line_colors, 
       
       lty = line_types, cex = 0.8)


# Single imputation method

imputed <- mean(nes$Test_Score,na.rm=TRUE) # calculating the mean 

nes[is.na(nes$Test_Score),"Test_Score"] <- 
  imputed #replacing missing values with the mean

imputednest <- nes

imputed.data.lme <- lme(Test_Score ~ Teaching_Method, random = ~ 1 | School/Class, 
                         data = imputednest)

summary(imputed.data.lme)

interaction.plot(imputednest$School, imputednest$Teaching_Method, imputednest$Test_Score, 
                 
                 xlab = "School", ylab = "Test Score", legend = FALSE)

unique_methods <- unique(imputednest$Teaching_Method)

line_types <- c(3, 2, 1)  # 3 = solid, 2 = dashed, 1 = dotted

line_colors <- c("black", "black", "black")

# Create a custom legend with line types and colors
legend("topright", legend = unique_methods, col = line_colors, 
       lty = line_types, cex = 0.8)

# Multiple imputation method with mice package

micedata <- 
  read.csv("C:/Users/siyab/OneDrive/Documents/complete_dataset_with_imputed.csv")


micedata.lme <- lme(Test_Score ~ Teaching_Method, random = ~ 1 | School/Class, 
                        data = micedata)

summary(micedata.lme)

interaction.plot(micedata$School, micedata$Teaching_Method, micedata$Test_Score, 
                 
                 xlab = "School", ylab = "Test Score", legend = FALSE)

unique_methods <- unique(micedata$Teaching_Method)


line_types <- c(3, 2, 1)  # 3 = solid, 2 = dashed, 1 = dotted

line_colors <- c("black", "black", "black")

legend("topright", legend = unique_methods, col = line_colors, 
       
       lty = line_types, cex = 1)

