source('FileTypeCounts.R')

#Using ExtensionCounts from FileTypeCounts.R

#Create Binary variable
ExtensionCounts$Real[which(ExtensionCounts$Type=='Legit')] <- 1 
ExtensionCounts$Real[which(ExtensionCounts$Type=='Fake')] <- 0
ExtensionCounts[is.na(ExtensionCounts)] <- 0

ExtforLogit <- ExtensionCounts[,-1:-2]


colnames(ExtforLogit)[-ncol(ExtforLogit)]

ExtLogit <- glm(Real ~ jpg + gif + html,
                  data=ExtforLogit, family='binomial')

summary(ExtLogit)

confint(ExtLogit)


exp(coef(ExtLogit))
