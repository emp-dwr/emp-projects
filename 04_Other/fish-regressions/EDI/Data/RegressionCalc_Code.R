#L:W Regressions
#determine L:W regression parameters using data from published studies
#questions: sarah.perry@water.ca.gov

#import packages
library(ggplot2)
library(tidyverse)
library(ggpmisc)
library(grid)

#read in data
fishData <- read_csv('C:/R/FishRegressions/fish_biomass.csv')

#determine unique species within dataset
fishSpecies <- unique(fishData$SpeciesCode)

#initialize empty tibble
fishTib = NULL

#compute the linear regression and create a graph for each species/locality pair
for (species in fishSpecies) { 
  speciesData <- fishData %>% filter(SpeciesCode == species) #filter by species
  speciesLoc <- unique(speciesData$Locality) #unique localities within the species subset

    for (loc in speciesLoc) {
      locData <- speciesData %>% filter(Locality == loc) #filter by loc (& species)
      
      #compute linear regression
      linReg <- lm(log10(locData$Weight_g) ~ log10(locData$Length_cm)) #y = weight, x = length
      b <- 10^(signif(coef(linReg)[1])) #intercept (raised to 10; don't want in log form)
      m <- signif(coef(linReg)[2]) #slope
      bPval <- summary(linReg)$coefficients[, 4][[1]] #p-val for b
      mPval <- summary(linReg)$coefficients[, 4][[2]] #p-val for m
      rSq <- summary(linReg)$r.squared #r-squared value
      
      #create text to go on graph
      gEq <- paste('y = ',round(b,2),' + ',round(m,2),'x',sep = '')
      gRSq <- paste('r2 = ',round(rSq,2), sep = '')
      grobEq <-
        grobTree(
          textGrob(
            gEq,
            x = 0.05, 
            y = 0.95,
            hjust = 0,
            gp = gpar(fontsize = 14, fontface = 'bold')
            )
          )
      
      grobRSq <-
        grobTree(
          textGrob(
            gRSq,
            x = 0.05,
            y = 0.91,
            hjust = 0,
            gp = gpar(fontsize = 14, fontface = 'bold')
            )
          )
      
      #create graph
      fishForm <- log10(locData$Weight_g) ~ log10(locData$Length_cm)
      plot <-
        ggplot(linReg, aes(x=log10(locData$Length_cm), y=log10(locData$Weight_g))) +
        geom_point(data = linReg, size = 3, aes(x=log10(locData$Length_cm), y=log10(locData$Weight_g))) +
        geom_smooth(method = lm) +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.text.x = element_text(size = 13, color = 'black'),
          axis.text.y = element_text(size = 13, color = 'black'),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15)
          ) +
        #ggtitle(paste(species, 'data for', loc)) +
        xlab('log(Length) (cm)') +
        ylab('log(Weight) (g)') +
        annotation_custom(grobEq) +
        annotation_custom(grobRSq)
      
      ggsave(paste('C:/R/FishRegressions/',species,loc,'.png',sep = ''), width = 8, height = 5)
      print(plot)
      
      #append everything to tibble
      newRow <- list(species = species,loc = loc, slope = m[[1]],intercept = b[[1]], r2 = rSq, slope_pVal = mPval, int_pVal = bPval)
      fishTib <- fishTib %>% bind_rows(newRow)
    }
  
  #export tibble
  write.table(
    fishTib,
    'C:/R/fishLW.csv',
    quote = FALSE,
    row.names = FALSE,
    sep = ','
    )
}
