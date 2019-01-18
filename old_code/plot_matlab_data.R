library(R.matlab)
library(ggplot2)

mat.filename <- "cedric-h429b-contrast6_07-11-18_analyze_main.mat"
mat.data <- readMat(mat.filename)
accuracy.matrix <- matrix(c(mat.data["soa.list"], mat.data["valid.hits"], mat.data["invalid.hits"]), nrow=4, ncol=3, dimnames = list(c(), c("soa", "valid.hits", "invalid.hits")))
accuracy <- data.frame(mat.data["soa.list"], mat.data["valid.hits"], mat.data["invalid.hits"])
ggplot(accuracy) + geom_point(mapping = aes(x = soa.list, y = valid.hits))
