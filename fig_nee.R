
##### Country-level figures for Blanchard, J.L., Watson, R.A., Fulton, E.A. et al. Linked sustainability challenges and trade-offs among fisheries, aquaculture and agriculture. Nat Ecol Evol 1, 1240–1249 (2017). https://doi.org/10.1038/s41559-017-0258-8

##### Please contact julia.blanchard@utas.edu.au if you have questions or wish to use these data

rm(list=ls())

require(ggrepel)

require(ggplot2)

 # library for multiple ggplots

library(gridExtra)


###### Figures 1,2 and 4 are maps orginally produced using ArcGIS. Will update to R soon. 
###### For updates check: https://github.com/juliablanchard/Linked_Sustainability_Challenges


###### This script reproduces Figures 3 and 5 country-level data


#### READ IN COUNTRY-LEVEL DATA  
 
 
fcdat<-read.csv("country_level_nee_combined.csv")


#### colours

palette1 <- colorRampPalette(c("grey99", "black"))
palette2 <- colorRampPalette(c("yellow", "orange", "red", "brown"))
palette3 <- colorRampPalette(c("Dark Green", "White", "Blue"))


#### FIGURE 3: "Marine biodiversity threat and adaptive capacity in fisheries-dependent countries"

### note: colours and layout appear differently in typeset publication

 ggplot(fcdat,aes(x = Balance_Dep, y = log10(WeightedThreat))) +

 #labs(x="Fisheries Dependence/Combined Dependence", y = bquote(paste("log",*10,"(Weighted Threat Sharks and Rays)"))) +

 geom_point(aes(x =Balance_Dep, y = log10(WeightedThreat),fill=HDIcat,color=HDIcat,size=(Fish_Dep)), shape=21,alpha=0.6) +
 
 # geom_point(aes(x = Balance_Dep, y = WeightedThreat),size=0.8,alpha=0.8,pch=5,color="dark grey") +
         
 scale_fill_discrete(name="HDI category")   +  
 
 scale_size(name="Fisheries\nDependence",range=c(0.001,10)) +
         # facet_wrap(~HDIcat,ncol=2) +
 
 scale_color_discrete(guide=FALSE) +

 # add lines to show global averages
 
 geom_hline(aes(yintercept=mean(log10(fcdat$WeightedThreat),na.rm=T)), colour="grey",lty=2) +
 geom_vline(aes(xintercept=mean(fcdat$Balance_Dep,na.rm=T)), colour="grey",lty=2)  +
 geom_text_repel(aes(x = Balance_Dep, y = log10(WeightedThreat),label=C_Name), size=1.8,point.padding = unit(0.001, 'lines'),
 # Color of the line segments.
     segment.color = "dark grey",    # # Width of the line segments.
     segment.size = 0.2,
     min.segment.length = unit(0.4, "lines")) +
 
 # theme(legend.position = c(0.8, 0.7),legend.key.size(1))
  
 # theme_hc()+ scale_colour_hc() +

 # theme_calc() +
  theme_bw() +
    
 ylab(expression(paste(log[10],"(Weighted Threat Sharks and Rays)"))) +

 xlab("Fisheries Dependence/Combined Dependence") +

 theme(legend.key = element_blank()) 


  
######### FIGURE 5: "Climate change impacts and adaptive capacity by continent across land and sea." 

  fcdat$extremes <- NA 
  fcdat$extremes <- ifelse(fcdat$Continent=="Africa" && fcdat$log10cropc  < -0.5 ,as.character(fcdat$C_Name), fcdat$extremes)
   # fcdat$extremes <- ifelse(fcdat$Balance_Threat_prp > 0.97,as.character(fcdat$C_Name), fcdat$extremes)
 fcdat$extremes <- ifelse(fcdat$ RCP60L10EEZ < -0.2,as.character(fcdat$C_Name), fcdat$extremes)
 fcdat$extremes <- ifelse(fcdat$ RCP60L10EEZ >0,as.character(fcdat$C_Name), fcdat$extremes)
  fcdat$extremes <- ifelse(fcdat$log10cropc >0,as.character(fcdat$C_Name), fcdat$extremes)
 fcdat$extremes <- ifelse(fcdat$HDIcat =="low",as.character(fcdat$C_Name), fcdat$extremes)

 library(RColorBrewer)
 palette3 <- colorRampPalette(c("Dark Green", "Turquoise", "Blue"))


  ggplot(fcdat, aes(x = log10cropc,y= RCP60L10EEZ)) +
   facet_wrap(~HDIcat,ncol=2) +
  ylab("Change in marine production") +
  xlab("Change in terrestrial production") +
  geom_hline(aes(yintercept=0), colour="dark grey") +
  geom_vline(aes(xintercept=0), colour="dark grey")  + 
  geom_abline(aes(slope=1,intercept=0), colour="dark grey",lty=2) +
   
  geom_point(aes(x = log10cropc,y= RCP60L10EEZ),size=0.8,alpha=0.8,pch=5,color="dark grey") +

  geom_point(aes(x = log10cropc,y= RCP60L10EEZ,size = Total_Dep,color=Balance_Dep)) + 
  # scale_fill_gradient2(high = "blue",low="dark green",mid="white", 
                       # name="Relative\nDependence",midpoint=0.5) +
  scale_colour_gradientn(colours= palette3(3), limits=c(0, 1), breaks=c(0, 0.5 ,1), labels=c("Land", 0.5, "Sea"), name="Relative\nDependence", guide = guide_colorbar(barwidth = 1, barheight = 5, title.position="top", title.hjust = 0, ticks=F, order=2)) +
    
    
  scale_size_area(max_size=6,name="Combined\nDependence") +
  geom_text_repel(aes(x = log10cropc,y= RCP60L10EEZ,label=C_Name), size=2,point.padding = unit(0.01, 'lines'),
   # Color of the line segments.
     segment.color = "grey",
   ## Width of the line segments.
     segment.size = 0.3,
     min.segment.length = unit(0.5, "lines")) +
    
 theme(legend.key = element_blank()) +

 theme_bw () 


####### EXPLORATION: Interactive scatter for data exploration purposes, here showing counry-level RCP8.5 climate projections for crops and fish and dependence on both sectors

library(plotly)

p<-plot_ly(fcdat, x = ~RCP85L10EEZ, y = ~rcp85, type="scatter", text = paste("Country: ", fcdat$C_Name,"HDI: ", fcdat$HDI,"FD: ", fcdat$Fish_Dep,"AD: ", fcdat$Agri_Dep),mode = "markers", color = ~Agri_Dep, size = ~log(Fish_Dep+1))
  
p <- p %>% layout(xaxis = list(title = "Mean Relative Change Fish"), 
           yaxis = list(title = "Mean Relative Change Crops"))
           #note: log10 change 
p

