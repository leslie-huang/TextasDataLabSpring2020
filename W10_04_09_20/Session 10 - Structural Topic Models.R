# TA: Leslie Huang
# Course: Text as Data
# Date: 04/09/2020

# Lab adapted from: Patrick Chester, Leslie Huang and Pedro L. Rodríguez

# Supervised vs. Unsupervised
# topic-models: excellent for exploration
# semi-supervised approaches: https://github.com/gregversteeg/corex_topic
# see: https://medium.com/pew-research-center-decoded/overcoming-the-limitations-of-topic-models-with-a-semi-supervised-approach-b947374e0455

# -----------------------------------------------
# Structural Topic Models                       ---
# -----------------------------------------------
rm(list = ls())
libraries <- c("topicmodels", "dplyr", "stm", "quanteda")
lapply(libraries, require, character.only = T)
setwd("/Users/lesliehuang/TextasDataLabSpring2020/W10_04_09_20")

# Loading data: Political blogs from the 2008 election on a conservative-liberal dimension
data(poliblog5k)
head(poliblog5k.meta)
head(poliblog5k.voc)

# Fits an STM model with 3 topics
system.time(
blog_stm <- stm(poliblog5k.docs, poliblog5k.voc, 3, prevalence = ~rating + s(day), data = poliblog5k.meta))

# A plot that summarizes the topics by what words occur most commonly in them
plot(blog_stm, type = "labels")

# A summary plot of the topics that ranks them by their average proportion in the corpus
plot(blog_stm, type = "summary")

# A visualization of what words are shared and distinctive to two topics
plot(blog_stm, type="perspectives", topics = c(1,2))

# Estimates a regression with topics as the dependent variable and metadata as the independent variables
# s() is a wrapper for bs() from the splines package
# A spline of degree D is a function formed by connecting polynomial segments of degree D
prep <- estimateEffect(1:3 ~ rating + s(day) , blog_stm, meta = poliblog5k.meta)

# Plots the distribution of topics over time
plot(prep, "day", blog_stm, topics = c(1,2), 
     method = "continuous", xaxt = "n", xlab = "Date")

# Plots the Difference in coverage of the topics according to liberal or conservative ideology
plot(prep, "rating", model = blog_stm,
     method = "difference", cov.value1 = "Conservative", cov.value2 = "Liberal")
