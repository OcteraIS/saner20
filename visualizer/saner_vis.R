library(tidyverse)
library(reshape2)
library(ggplot2)
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