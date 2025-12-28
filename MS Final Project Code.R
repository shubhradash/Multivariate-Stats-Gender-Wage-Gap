
#############################Linear Discriminant Analysis 
install.packages("MASS")  
install.packages("heplots")

library(MASS)	
library(biotools)	
library(klaR)	
library(car)	
library(dplyr)	
library(lubridate)	
library(ggplot2)	
library(ggExtra)
library(heplots)	

ggdata <- read.csv(file.choose())
names(ggdata)
head(ggdata)

#Checking for Multivariate Normality
cqplot(ggdata[ggdata$Gender == 'Female', c("Age", "PerfEval", "Seniority", "BasePay", "Bonus")], label = "Female")	
cqplot(ggdata[ggdata$Gender == 'Male', c("Age", "PerfEval", "Seniority", "BasePay", "Bonus")], label = "Male")
ggdata$logBasePay <- log(ggdata$BasePay)
ggdata$logBonus <- log(ggdata$Bonus)
cqplot(ggdata[ggdata$Gender == 'Female', c("Age", "PerfEval", "Seniority", "logBasePay", "logBonus")], label = "Female")	
cqplot(ggdata[ggdata$Gender == 'Male', c("Age", "PerfEval", "Seniority", "logBasePay", "logBonus")], label = "Male")

#Check for Similarity of Covarinace Matrices 
covariances <- by(ggdata[c("Age", "PerfEval", "Seniority", "logBasePay", "logBonus")], ggdata$Gender, cov)
print(covariances)

box_m <- boxM(ggdata[c("Age", "PerfEval", "Seniority", "logBasePay", "logBonus")], ggdata$Gender)
summary(box_m)
#P-Value of 0.01 indicates we reject the null that covarinace matrices are similar and proceed with QDA

#Matrice plot 
ggdata$colors <- ifelse(ggdata$Gender == "Male", "blue", "green")
plot(ggdata[c("Age", "PerfEval", "Seniority", "logBasePay", "logBonus")], col = ggdata$colors, pch = 19, cex = .7, main = "Matrix Plot of Gender Pay Gap Data")

ldamodel <- lda(ggdata[c("Age", "PerfEval", "Seniority", "logBasePay", "logBonus")], grouping = ggdata$Gender)	
summary(ldamodel)
coefficients <- ldamodel$scaling
coefficients
raw <- table(ggdata$Gender, predict(ldamodel)$class)
raw
qdamodel <- qda(ggdata[c("Age", "PerfEval", "Seniority", "logBasePay", "logBonus")], grouping = ggdata$Gender)
summaryqda <- summary(qdamodel)
raw1 <- table(ggdata$Gender, predict(qdamodel)$class)	
raw1

ggdata.manova <- manova(as.matrix(ggdata[c("Age", "PerfEval", "Seniority", "logBasePay", "logBonus")]) ~ ggdata$Gender)	
summary.manova(ggdata.manova, test = "Wilks")
summary.aov(ggdata.manova)
#statistically significant differences in mean Performance Evaluations between genders.
#very strong evidence that mean log-transformed Base Pay differs significantly between genders.
#log-transformed Bonus between genders are not statistically significant 

#The significant results for Performance Evaluation and logBasePay suggest that these factors may be influenced by gender biases or systematic differences in how genders are treated or paid within the organization.
#The non-significant results for Age, Seniority, and logBonus suggest that these factors do not differ substantially between genders, indicating either a balanced treatment across these dimensions or that these variables do not capture gender disparities effectively.

#Number of significant discriminating functions 
names(ldamodel)
# Extract eigenvalues
eigenvalues <- ldamodel$svd^2
# Compute proportion of trace explained by each eigenvalue
prop_trace_explained <- eigenvalues / sum(eigenvalues)
num_significant <- sum(eigenvalues > 0)
# Display results
cat("Number of significant discriminant functions:", num_significant, "\n")
cat("Relative discriminating power of each function (proportion of trace explained):\n")
print(prop_trace_explained)

# raw results - use the 'predict' function	
ctraw <- table(ggdata$Gender, predict(qdamodel)$class)	
ctraw	
# total percent correct	
round(sum(diag(prop.table(ctraw))), 2)

#cross-validated results	
ctCV <- qda(ggdata$Gender ~ ggdata$Age + ggdata$PerfEval + ggdata$Seniority + ggdata$logBasePay + ggdata$logBonus, CV = TRUE)	
tableCV <- table(ggdata$Gender, ctCV$class)	
tableCV	
# total percent correct	
round(sum(diag(prop.table(tableCV))), 2)

print("Standardized Coefficients")	
round(qda(scale(ggdata[c("Age", "PerfEval", "Seniority", "logBasePay", "logBonus")]), grouping = ggdata$Gender)$scaling, 2)

ggdatalda <- lda(scale(ggdata[, c("Age", "PerfEval", "Seniority", "logBasePay", "logBonus")]), grouping = ggdata$Gender)
scores <- predict(ggdatalda)$x
scores_df <- data.frame(scores, Gender = ggdata$Gender)

# Creating a density plot with ggplot2 for better visualization
ggplot(scores_df, aes(x = LD1, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of LD1 Scores", x = "LD1 Scores") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "green"))

#PLOT Predicted Regions based on two variables 	
#LDA
ggdata$Gender <- as.factor(ggdata$Gender)
partimat(Gender ~ Age + logBasePay, data = ggdata, method = "lda")


################################MANOVA###########

#Continous Response Variables - PerfEval and logBasePay 
#Categorical Predictor Variable - Gender, Education, and Job Title 
#Continus Predictor Variable - Age 

library(MASS)	
library(biotools)	
library(klaR)	
library(car)
install.packages("remotes")
library(remotes)
library(heplots)
remotes::install_github('gastonstat/DiscriMiner', force = TRUE)

str(ggdata)
ggdata$Education <- factor(ggdata$Education, levels = c("High School", "College", "Masters", "PhD"))
interaction.plot(ggdata$Education,ggdata$Gender, ggdata$logBasePay,	
                 lwd = 3, col = c("green", "blue"),trace.label="ggdata$Gender",
                 xlab = "Education", main = "Interaction Plot logBasePay")

interaction.plot(ggdata$Education,ggdata$Gender, ggdata$PerfEval,	
                 lwd = 3, col = c("green", "blue"),trace.label="ggdata$Gender",
                 xlab = "Education", main = "Interaction Plot PerfEval")

#Two-Way MANOVA
ggdataMAOV <- lm(cbind(`logBasePay`,`PerfEval`) ~ `Gender`*`Education`,  	
                    data = ggdata)	
summary(Anova(ggdataMAOV, type = 3), univariate = T)


#Performing Multivariate and Univariate contrasts 

options(contrasts = c("contr.treatment", "contr.poly"))	
ggdata$TRTCOMB <- interaction(ggdata$Gender, ggdata$Education)
ggdata$TRTCOMB <- as.factor(ggdata$TRTCOMB)
#one-way MANOVA model
ggdataMAOV2 <- lm(cbind(`logBasePay`,`PerfEval`) ~ `TRTCOMB`,  	
                  data = ggdata)
#one-way ANOVA model
ggdatapay <- lm(`logBasePay` ~ `TRTCOMB`, data = ggdata)
contrasts(ggdata$TRTCOMB)	
levels(ggdata$TRTCOMB)	

#multivariate contrast of females vs males 
linearHypothesis(ggdataMAOV2, "TRTCOMBMale.High School + TRTCOMBMale.College + TRTCOMBMale.Masters + TRTCOMBMale.PhD - 
     TRTCOMBFemale.College - TRTCOMBFemale.Masters - TRTCOMBFemale.PhD = 0")

#univariate contrast of of females vs males for base pay
linearHypothesis(ggdatapay, "TRTCOMBMale.High School + TRTCOMBMale.College + TRTCOMBMale.Masters + TRTCOMBMale.PhD - 
     TRTCOMBFemale.College - TRTCOMBFemale.Masters - TRTCOMBFemale.PhD = 0")

#Adding a continuous predictor variable 
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
ggsubset <- ggdata[, c("logBasePay","PerfEval","Age")]
correlation_matrix <- cor(ggsubset)
plot(ggsubset)
abline(pch=19, lwd=3,col="red")
chart.Correlation(correlation_matrix, histogram=TRUE, pch=19)

options(contrasts = c("contr.sum", "contr.poly"))
ggdataMAOV3 <- lm(cbind(`logBasePay`,`PerfEval`) ~ `Gender`*`Education`+ `Age`,  	
                                data = ggdata)
summary(Anova(ggdataMAOV3, type = 3), univariate = T)
cqplot(ggdataMAOV3$residuals, label = "Residuals from GG Data")


################################CLUSTER ANALYSIS###########
library(aplpack)
library(fpc)
library(cluster)
library(ape)
library(amap)

ggdata$AgeCat <- cut(ggdata$Age, breaks=c(18, 30, 65, Inf), include.lowest=TRUE, labels=c("Young Adult", "Adult", "Senior"))
ggdata$PerfEvalCat <- cut(ggdata$PerfEval, breaks=3, labels=c("Low", "Medium", "High"))
ggdata$SeniorityCat <- factor(ggdata$Seniority,
                              levels = 1:5,
                              labels = c("Entry-Level", "Junior", "Mid-Level", "Senior", "Executive"))
ggdata$BasePaycat <- cut(ggdata$PerfEval, breaks=3, labels=c("Low", "Medium", "High"))
ggdata$Bonuscat <- cut(ggdata$PerfEval, breaks=3, labels=c("Low", "Medium", "High"))

library(dplyr)
library(tidyr)
library(forcats)

# Convert 'JobTitle' and 'Dept' to factors
ggdata$JobTitle <- as.factor(ggdata$JobTitle)
ggdata$Dept <- as.factor(ggdata$Dept)

# Apply one-hot encoding using model.matrix
job_title_dummies <- data.frame(model.matrix(~ JobTitle - 1, data=ggdata))
dept_dummies <- data.frame(model.matrix(~ Dept - 1, data=ggdata))

# Bind the dummy variables back to the original data, dropping original categorical columns
ggdata <- bind_cols(ggdata, job_title_dummies, dept_dummies) %>%
  select(-JobTitle, -Dept)

# Binary Encoding for 'Gender'
ggdata$Gender <- as.numeric(as.factor(ggdata$Gender)) - 1  # Convert to factor then to numeric

# Ordinal Encoding for 'Education'
# Assuming education levels are 'College', 'Bachelor', 'Masters', 'PhD'
education_levels <- c('College', 'Bachelor', 'Masters', 'PhD')
ggdata$Education <- factor(ggdata$Education, levels = education_levels, ordered = TRUE)
ggdata$Education <- as.numeric(ggdata$Education) - 1  # Convert to numeric

# Output the first few rows to verify the transformations
head(data)
ggclusterdata <- ggdata[, c("Gender","PerfEval","Age","Education","Seniority","JobTitleData.Scientist", "JobTitleDriver", "JobTitleFinancial.Analyst", "JobTitleGraphic.Designer",
                            "JobTitleIT", "JobTitleManager", "JobTitleMarketing.Associate", "JobTitleSales.Associate",  "JobTitleSoftware.Engineer", "JobTitleWarehouse.Associate", "DeptAdministration", "DeptEngineering","DeptManagement", "DeptOperations", "DeptSales")]

library(cluster)

# Assuming 'clustering_data' includes age and ordinal data in their original forms
d <- daisy(ggclusterdata, metric = "gower")

# Perform hierarchical clustering using an appropriate method
hc <- hclust(d, method = "average")  # You can choose 'complete' or 'ward.D2' if appropriate

# Plot the dendrogram to inspect cluster formation
plot(hc,labels = FALSE)
rect.hclust(hc, k = 6)

plot(hc$height, type = 'b', xlab = "Number of Merges", ylab = "Height", main = "Agglomeration Schedule")
r_squared <- rev(cumsum(rev(hc$height^2))) / sum(hc$height^2)
max_clusters <- 20 
num_clusters <- 2:max_clusters
plot(num_clusters, r_squared[num_clusters - 1], type = 'b', xlab = "Number of Clusters", ylab = "R-squared", main = "R-squared Plot")

# Assuming 'hc' is your hierarchical clustering object
clusters <- cutree(hc, k = 6)
ggdata$Cluster <- as.factor(clusters)  # Add the cluster assignments to your dataframe

# Summary statistics for BasePay by cluster
basepay_summary <- ggdata %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    MeanBasePay = mean(BasePay, na.rm = TRUE),
    MedianBasePay = median(BasePay, na.rm = TRUE),
    SD_BasePay = sd(BasePay, na.rm = TRUE)
  )

print(basepay_summary)

# Summary statistics for Gender by cluster
gender_summary <- ggdata %>%
  group_by(Cluster) %>%
  summarise(
    Male = sum(Gender == 1, na.rm = TRUE),
    Female = sum(Gender == 0, na.rm = TRUE),
    Proportion_Female = mean(Gender == 0, na.rm = TRUE)
  )

print(gender_summary)

# Merge the summaries
cluster_summary <- merge(basepay_summary, gender_summary, by = "Cluster")

# View the merged summary
print(cluster_summary)

library(ggplot2)

# Boxplot of BasePay by cluster
ggdata$Educationfactor <- as.factor(ggdata$Education)
ggdata$Proportion_Female = mean(1 - ggdata$Gender, na.rm = TRUE)
ggplot(ggdata, aes(x = Cluster, y = BasePay, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Base Pay Distribution by Cluster", x = "Cluster", y = "Base Pay")

#Could try another distance or agglomoration method "manhattan" or K-means clustering 


