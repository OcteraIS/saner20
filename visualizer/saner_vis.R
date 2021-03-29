library(tidyverse)
library(reshape2)
library(ggplot2)
setwd('/Users/nernst/Documents/projects/design-detect/EmbeddedDesign/emse-new/')

# newbest = matrix(c(0.84,0.656,0.588,.559,.555,
#                    .768,NA,.592,.552,.500,
#                    .706,.626,NA,.555,	.547,
#                    .681,.644,.644, NA,.552,
#                    0.632,	0.513,	0.483,	0.496,NA), nrow=5, ncol=5)
newbest = matrix(c(0.84,0.656,0.588,.559,.555,
                   .768,0.7,.592,.552,.500,
                   .706,.626,.714,.555,	.547,
                   .681,.644,.644,.646 ,.552,
                   0.632,	0.513,	0.483,	0.496,.882), nrow=5, ncol=5)
# saner 20 single run
# data located in Google Sheet
#ulmfit = matrix(c(0.932,0.481,0.597,0.548, NA,
                  # 0.502,0.502,0.516,0.501,0.576,
                  # 0.729,0.624,0.918,0.552,0.837,
                  # 0.547,0.559,0.523,0.500,0.799,
                  # 0.555,0.513,0.508,0.498,0.790), nrow=5, ncol=5)

#EMSE extension - Alvi average over 3 runs
ulmfit = matrix(c(0.922,0.482,0.599,0.562, 0.821,
                  0.494,0.510,0.518,0.503,0.569,
                  0.739,0.608,0.929,0.537,0.848,
                  0.555,0.565,0.515,0.497,0.813,
                  0.563,0.521,0.513,0.505,0.773), nrow=5, ncol=5)
rownames(newbest) = colnames(newbest) = c("Brunet 2014","Shakiba 2016","Viviani 2018","SATD","Stack Overflow")
rownames(ulmfit) = colnames(ulmfit) = c("Brunet 2014","Shakiba 2016","Viviani 2018","SATD","Stack Overflow")
#c("Brunet 2014","Shakiba 2016","Viviani 2018","SATD","Stack Overflow")
longNew<-melt(newbest)
longUlm = melt(ulmfit)
longNew = longNew %>% na.omit()
longUlm = longUlm %>% na.omit()

# plot
ggplot(longNew, aes(x = Var2, y = Var1, fill=value)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Train", y="Test", title="") +
  theme_bw() + 
  theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
        axis.text.y=element_text(size=9),
        plot.title=element_text(size=11)) + 
  geom_text(aes(label=value))
ggsave('heatmap_newbest.tiff')
# plot
ggplot(longUlm, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Train", y="Test", title="") +
  theme_bw() + 
  theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
        axis.text.y=element_text(size=9),
        plot.title=element_text(size=11)) + 
  geom_text(aes(label=value))
ggsave('heatmap_ulmfit.tiff')

# Is one set of results different than the other?  
library("fitdistrplus")
plotdist(as.vector(newbest))
plotdist(as.vector(ulmfit))
# distributions similar, MW valid
wilcox.test(newbest,ulmfit)

# violin plots
data = read.csv("boxplot_data.csv")
data$data_size <- as.factor(data$data_size)
data$data_size <- as.factor(data$data_size)
p <- ggplot(data, aes(x=data_size, y=auc)) + 
  geom_violin(trim=FALSE) + 
  labs(x = "Train data chunk size", y = "AUC") +
  stat_summary(fun = "median", geom="point", shape=23, size=2) +
  geom_boxplot(width=0.1)
print(p)

# bar charts
install.packages("googlesheets4")
library(googlesheets4)
library(googledrive)
data_sheet = drive_get("EMSE results")
# this sets a common color scheme for the "ours"/"theirs" comparison
group.colors <- c("With cross domain similar word injection" = "#2b8cbe",
                  "With similar word injected"  = "#2b8cbe",
                  "With Domain vectorizer" = "#2b8cbe",
                  "With GloVe vectorizer" = "#a6bddb",
                  "Without cross domain similar word injection" = "#a6bddb",
                  "Without similar word injected" = "#a6bddb")

# D5:N7 is the word embedding test result
embed_test = read_sheet(data_sheet, sheet="cross data", range="D5:N7", col_names=TRUE)
# data.emse = data.emse_all %>% select(c(2,3,5,24,25,26,27,28,29,30,31))
colnames(embed_test) = c("Treatment","Nearest Neighbors","Decision Tree","Random Forest","Logistic Regression",
                         "Naive Bayes","Neural Net","AdaBoost","QDA","Linear SVM","RBF SVM")
# rownames(data.emse_all) <- c("Augmentation+Word Vector","No treatment")
# embed_test = tibble::rownames_to_column(embed_test, "Treatment")
embed_long = embed_test %>% gather(Algorithm, AUC, "Nearest Neighbors":"RBF SVM")

p = ggplot(embed_long, aes(Algorithm, AUC,group = Treatment)) + 
  geom_col(position = "dodge2", aes(fill=Treatment))+#colour="black")+
  labs(x = "", y = "AUC") +
  geom_text(aes(label = sprintf("%0.3f", round(AUC, digits = 4))), vjust = -0.5, size = 3, position = position_dodge(0.9)) +
  theme(axis.text.x = element_text(size = 10, angle = 90,vjust = 0.5,hjust=1)) + 
  scale_fill_manual(values=group.colors)

print(p)
ggsave('fig/embed_long_auc.pdf')

# D10:N12 is the word injection approach
inject_test = read_sheet(data_sheet, sheet="cross data", range="D10:N12", col_names=TRUE)
colnames(inject_test) = c("Treatment","Nearest Neighbors","Decision Tree","Random Forest","Logistic Regression",
                         "Naive Bayes","Neural Net","AdaBoost","QDA","Linear SVM","RBF SVM")
inject_long = inject_test %>% gather(Algorithm, AUC, "Nearest Neighbors":"RBF SVM")
p = ggplot(inject_long, aes(Algorithm, AUC,group = Treatment)) + 
  geom_col(position = "dodge2", aes(fill=Treatment))+
  labs(x = "", y = "AUC") +
  geom_text(aes(label = sprintf("%0.3f", round(AUC, digits = 4))), vjust = -0.5, size = 3, position = position_dodge(0.9)) +
  theme(axis.text.x = element_text(size = 10, angle = 90,vjust = 0.5,hjust=1)) +
  scale_fill_manual(values=group.colors)

print(p)
ggsave('fig/inject_auc.pdf')

# D15:N17 is the cross domain approach
cross_test = read_sheet(data_sheet, sheet="cross data", range="D15:N17", col_names=TRUE)
colnames(cross_test) = c("Treatment","Nearest Neighbors","Decision Tree","Random Forest","Logistic Regression",
                          "Naive Bayes","Neural Net","AdaBoost","QDA","Linear SVM","RBF SVM")
cross_long = cross_test %>% gather(Algorithm, AUC, "Nearest Neighbors":"RBF SVM")
p = ggplot(cross_long, aes(Algorithm, AUC,group = Treatment)) + 
  geom_col(position = "dodge2", aes(fill=Treatment))+
  labs(x = "", y = "AUC") +
  geom_text(aes(label = sprintf("%0.3f", round(AUC, digits = 4))), vjust = -0.5, size = 3, position = position_dodge(0.9)) +
  theme(axis.text.x = element_text(size = 10, angle = 90,vjust = 0.5,hjust=1))+
  scale_fill_manual(values=group.colors)
print(p)
ggsave('fig/cross_auc.pdf')

# C1:L3 is the SOTA result: all augmentation and vectorizer
final_test = read_sheet(data_sheet, sheet="test data", range="C1:L3", col_names=TRUE)
colnames(final_test) = c("Nearest Neighbors","Decision Tree","Random Forest","Logistic Regression",
                            "Naive Bayes","Neural Net","AdaBoost","QDA","Linear SVM","RBF SVM")
rownames(data.emse_all) <- c("Augmentation+Word Vector","No treatment")
final_test = tibble::rownames_to_column(final_test, "treatment")
final_long = final_test %>% gather(similar_word, AUC, "Nearest Neighbors":"RBF SVM")
p = ggplot(final_long, aes(similar_word, AUC,group = treatment)) + 
  geom_col(position = "dodge2", aes(fill=treatment))+
  labs(x = "", y = "AUC") +
  geom_text(aes(label = sprintf("%0.3f", round(AUC, digits = 4))), vjust = -0.5, size = 3, position = position_dodge(0.9)) +
  theme(axis.text.x = element_text(size = 10, angle = 90,vjust = 0.5,hjust=1))
print(p)
ggsave('fig/final_auc.pdf')
