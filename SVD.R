library(plyr)
library(lattice)
library(calibrate)

Admission <- read.csv("./data/graduate-admissions/Admission_Predict.csv", row.names = "Serial.No.")

Admission_new <- scale(Admission[-8])
AdmissionSVD <- svd(Admission_new)

u = AdmissionSVD$u
v = AdmissionSVD$v
d = AdmissionSVD$d

singular_value = d / sum(d)

variation.explained.plot <- barchart(singular_value~c(1:length(d)), 
                                     horizontal = F, xlab = "Component",
                                     ylab="Percentage of variation explained")
variation.explained.plot


plot(u[,1], u[,2], xlab = "First Component", ylab = "Second Component")
title("First Component VS Second Component")
textxy(u[,1],u[,2],rownames(Admission_new))


plot(v[,1], v[,2], xlab = "First Component", ylab = "Second Component")
title("First Component VS Second Component")
textxy(v[,1],v[,2],colnames(Admission_new[, 1:7]))
