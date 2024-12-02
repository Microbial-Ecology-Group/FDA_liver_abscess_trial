# Start Here --------------------------------------------------------------
#Project Name: FDA Liver Abscess
#Data: This data is the simulation model output from metainsight
#Goal: Calculate the upper 95% interval for non-inferiority comparisons
#Start: 9Nov24 


# Library -----------------------------------------------------------------
library(readxl)
library(dplyr)
library(xlsx)


# A+ ONLY!!! --------------------------------------------------------------
#load the data
chain1 <- read.csv("C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Meta Analysis Stuff/Final Manuscript files/Chains and R code/A+/data_for_chain_1.csv")
chain2 <- read.csv("C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Meta Analysis Stuff/Final Manuscript files/Chains and R code/A+/data_for_chain_2.csv")
chain3 <- read.csv("C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Meta Analysis Stuff/Final Manuscript files/Chains and R code/A+/data_for_chain_3.csv")
chain4 <- read.csv("C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Meta Analysis Stuff/Final Manuscript files/Chains and R code/A+/data_for_chain_4.csv")

#Combine the datasets by stacking them vertically
combined_data <- bind_rows(chain1, chain2, chain3, chain4)

#Save the combined dataset to a new Excel file
write.xlsx(combined_data, "C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Meta Analysis Stuff/Final Manuscript files/Chains and R code/A+/combined_data.xlsx")

#Create the new column as the difference between the specified columns
combined_data <- combined_data %>%
  mutate(difference = `d.1Placebo.3Earlywithdrawal` - `d.1Placebo.2Continuous`)

#Calculate the upper 95% quantile of the new column
upper_95_quantile <- quantile(combined_data$difference, 0.95)
upper_97.5_quantile <- quantile(combined_data$difference, 0.975)
lower_2.5_quantile <- quantile(combined_data$difference, 0.025)
lower_mean_quantile <- quantile(combined_data$difference, 0.5)

# Print the result
print(upper_95_quantile)
print(lower_2.5_quantile)
print(upper_97.5_quantile)
print(lower_mean_quantile)
# checking
exp(lower_mean_quantile)
exp(lower_2.5_quantile)
exp(upper_97.5_quantile)
exp(upper_95_quantile)

PEWupper_95_quantile <- quantile(combined_data$d.1Placebo.3Earlywithdrawal, 0.95)
PEWupper_97.5_quantile <- quantile(combined_data$d.1Placebo.3Earlywithdrawal, 0.975)
PEWlower_2.5_quantile <- quantile(combined_data$d.1Placebo.3Earlywithdrawal, 0.025)

print(PEWlower_2.5_quantile)
print(PEWupper_95_quantile)
print(PEWupper_97.5_quantile)

exp(PEWlower_2.5_quantile)
exp(PEWupper_95_quantile)
exp(PEWupper_97.5_quantile)


#more checks
PCupper_95_quantile <- quantile(combined_data$d.1Placebo.2Continuous, 0.95)
PCupper_97.5_quantile <- quantile(combined_data$d.1Placebo.2Continuous, 0.975)
PClower_2.5_quantile <- quantile(combined_data$d.1Placebo.2Continuous, 0.025)
PC_mean_quantile <- quantile(combined_data$d.1Placebo.2Continuous, 0.5)



1/exp(PClower_2.5_quantile)
1/exp(PC_mean_quantile)
1/exp(PCupper_97.5_quantile)


20/18

# SEVERE!!!!! -------------------------------------------------------------
#load the data
Schain1 <- read.csv("C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Meta Analysis Stuff/Final Manuscript files/Chains and R code/Severe/data_for_chain_1.csv")
Schain2 <- read.csv("C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Meta Analysis Stuff/Final Manuscript files/Chains and R code/Severe/data_for_chain_2.csv")
Schain3 <- read.csv("C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Meta Analysis Stuff/Final Manuscript files/Chains and R code/Severe/data_for_chain_3.csv")
Schain4 <- read.csv("C:/Users/danie/OneDrive - West Texas A and M University/Research/FDA Study/Meta Analysis Stuff/Final Manuscript files/Chains and R code/Severe/data_for_chain_4.csv")

#Combine the datasets by stacking them vertically
Scombined_data <- bind_rows(Schain1, Schain2, Schain3, Schain4)

#Create the new column as the difference between the specified columns
Scombined_data <- Scombined_data %>%
  mutate(S_difference = `d.1Placebo.3Earlywithdrawal` - `d.1Placebo.2Continuous`)

# Calculate the upper 95% quantile of the new column
S_upper_95_quantile <- quantile(Scombined_data$S_difference, 0.95)
S_upper_97.5_quantile <- quantile(Scombined_data$S_difference, 0.975)
S_lower_2.5_quantile <- quantile(Scombined_data$S_difference, 0.025)

# Print the result
print(S_upper_95_quantile)
print(S_lower_2.5_quantile)
print(S_upper_97.5_quantile)

S_PEWupper_95_quantile <- quantile(combined_data$d.1Placebo.3Earlywithdrawal, 0.95)
S_PEWupper_97.5_quantile <- quantile(combined_data$d.1Placebo.3Earlywithdrawal, 0.975)
S_PEWlower_2.5_quantile <- quantile(combined_data$d.1Placebo.3Earlywithdrawal, 0.025)

print(S_PEWlower_2.5_quantile)
print(S_PEWupper_95_quantile)
print(S_PEWupper_97.5_quantile)


