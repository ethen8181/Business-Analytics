# Avoiding Pie Charts
library(scales)
library(ggplot2)
library(ggthemes)
library(data.table)
setwd("/Users/ethen/Business-Analytics/ggplot2/avoid_pie_charts")
d <- fread("pie.txt")
d <- melt( d, id.vars = "Task", 
		   variable.name = "Hours", value.name = "Percentage" )

# faceted bar plot, x label 90 degrees rotation
p1 <- ggplot( d, aes( x = Hours, y = Percentage ) ) + 
	  geom_bar( stat = "identity" ) + 
	  facet_wrap( ~ Task ) + 
	  xlab("Hours spent per week") + 
	  geom_text( aes( label = paste0( Percentage, "%" ), y = Percentage ),
			     vjust = 1.4, size = 3, color = "white" )
p1 + theme_bw() + 
theme( axis.text.x = element_text( angle = 90,  hjust = 1 ) )


# remove chart junks using the theme tufte
p1 + theme_tufte()


# stacked bar plot 
ggplot( d, aes( x = Task, y = Percentage, fill = Hours ) ) + 
geom_bar( stat = "identity", position = "stack" ) +
coord_flip() +
scale_fill_brewer( palette = "YlGnBu" ) +
theme_minimal() + theme( legend.position = "bottom" )

 
# refined version of bar plot
# 1. geom_bar's size controls the bar's border size
# 2. expand:
# 	 A numeric vector of length two giving multiplicative and 
# 	 additive expansion constants. These constants ensure that the data is placed 
# 	 some distance away from the axes
# 3. strip .background / .text: controls the title section for each facet
# 4. panel.grid .minor / .major: controls the grid lines in the plot
# 5. panel.margin: contols the margin between facets

# http://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels
x_labels <- c( "<1 hr/\nweek", "1-4 hrs/\nweek", "1-3 hrs/\nday", "4+ hrs/\nday" )
label_names <- c("Basic exploratory data analysis" = "Basic Exploratory\nData Analysis", 
				"Data cleaning" = "Data\nCleaning", 
				"Machine learning/statistics" = "Machine Learning,\nStatistics", 
				"Creating visualizations" = "Creating\nVisualizations", 
				"Presenting analysis" = "Presenting\nAnalysis", 
				"Extract/transform/load" = "Extract,\nTransform, Load" )

# The amount of time spent on various tasks by surveyed non-managers in data-science positions.
ggplot( d, aes( x = Hours, y = Percentage / 100, fill = Hours ) ) +
geom_bar( stat = "identity", width = 0.75, color = "#2b2b2b", size = 0.05 ) + 
scale_y_continuous( labels = percent, limits = c( 0, 0.5 ) ) + 
scale_x_discrete( expand = c( 0, 1 ), labels = x_labels ) + 
scale_fill_manual( values = c( "#a6cdd9", "#d2e4ee", "#b7b079", "#efc750" ) ) +
facet_wrap( ~ Task, labeller = as_labeller(label_names) ) + 
labs( x = NULL, y = NULL, title = "Where Does the Time Go?") +
theme( strip.text = element_text( size = 12, color = "white", hjust = 0.5 ),
	   strip.background = element_rect( fill = "#858585", color = NA ),	   
	   panel.background = element_rect( fill = "#efefef", color = NA ),
	   panel.grid.major.x = element_blank(),
	   panel.grid.minor.x = element_blank(),
	   panel.grid.minor.y = element_blank(),
	   panel.grid.major.y = element_line( color = "#b2b2b2" ),
	   panel.margin.x = unit( 1, "cm" ),
	   panel.margin.y = unit( 0.5, "cm" ),
	   legend.position = "none" ) 
 
