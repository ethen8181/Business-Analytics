library(dplyr)
library(scales)
library(viridis)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(lubridate)
library(data.table)
setwd('/Users/ethen/Business-Analytics/articles/calendar_heatmaps')
attacks <- fread("data/eventlog.csv")


# preprocessing --------------------------------------------------------------
make_hr_wkday <- function(ts, sc, tz) {
	# convert each time with the appropriate timezone,
	# the timezone parameter, tz, only takes a single value,
	# then extract its weekdays and hour
	real_times <- ymd_hms( ts, tz = tz[1], quiet = TRUE )
	dt <- data.table( source_country = sc,
					  wkday = weekdays(real_times),
					  hour = hour(real_times) )
	return(dt)
}

# convert weekday and hour into factor so they'll be ordered when plotting
wkday_levels <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 
				  'Thursday', 'Friday', 'Saturday')
attacks <- attacks %>% 
		   group_by(tz) %>%
		   do( make_hr_wkday( .$timestamp, .$source_country, .$tz ) ) %>% 
		   ungroup() %>% 
		   mutate( wkday = factor(wkday, levels = wkday_levels),
		   		   hour  = factor(hour, levels = 0:23) )
head(attacks)

# attacks --------------------------------------------------------------------

# then we can simply group the count by hour and wkday
# since we know that we have values for every combination
# there's no need to further preprocess the data
grouped <- attacks %>% count(wkday, hour) %>% ungroup()

ggplot( grouped, aes(hour, wkday, fill = n) ) + 
geom_tile(color = "white", size = 0.1) + 
theme_tufte(base_family = "Helvetica") + 
coord_equal() + 
scale_fill_viridis(name = "# of Events", label = comma) + 
labs(x = NULL, y = NULL, title = "Events per weekday & time of day") +
theme( axis.ticks = element_blank(),
	   plot.title = element_text(hjust = 0.5),
	   legend.title = element_text(size = 8),
	   legend.text = element_text(size = 6) )


# attacks per country ---------------------------------------------------------------

# extract the top 4 countries (sorted by the number of attacks) 
# for visualization
events_by_country <- count( attacks, source_country ) %>% 
					 mutate( percent = percent( n / sum(n) ) ) %>%
					 arrange( desc(n) )
top_country <- events_by_country$source_country[1:4]
head(events_by_country)


top_country_attacks <- attacks %>%
					   filter( source_country %in% top_country ) %>% 
					   count( source_country, wkday, hour ) %>% 
					   ungroup() %>% 
					   mutate( source_country = factor( source_country, levels = top_country ) )


gg <- ggplot( top_country_attacks, aes(x = hour, y = wkday, fill = n) ) + 
	  geom_tile(color = "white", size = 0.1) +
	  scale_fill_viridis(name = "# Events") + 
	  coord_equal() + 
	  facet_wrap( ~source_country, ncol = 2 ) +
	  labs(x = NULL, y = NULL, title = "Events per weekday & time of day by country\n") + 
	  theme_tufte(base_family = "Helvetica") + 
	  theme( axis.ticks = element_blank(),
			 axis.text = element_text(size = 8),
			 panel.border = element_blank(),
			 plot.title = element_text(hjust = 0.5),
			 strip.text = element_text(hjust = 0.5),
			 panel.margin = unit(0.1, "cm"),
	  		 legend.position = "bottom",
	  		 legend.title = element_text(size = 8),
	  		 legend.text = element_text(size = 6),
	  		 legend.key.size = unit(0.4, "cm"),
	  		 legend.key.width = unit(1, "cm") )
gg


plots <- lapply(top_country, function(x) {

	subset_data <- top_country_attacks %>% filter(source_country == x)
	gg <- ggplot( subset_data, aes(x = hour, y = wkday, fill = n) ) + 
		  geom_tile(color = "white", size = 0.1) +
		  scale_fill_viridis(name = "# Events") + 
		  scale_y_discrete( expand = c(0, 0) ) +
		  coord_equal() + 
		  labs(x = NULL, y = NULL, title = x) + 
		  theme_tufte(base_family = "Helvetica") + 
		  theme( axis.ticks = element_blank(),
				 axis.text = element_text(size = 7),
				 panel.border = element_blank(),
				 plot.title = element_text(hjust = 0.5),
				 strip.text = element_text(hjust = 0.5),
				 panel.margin = unit(0.1, "cm"),
		  		 legend.position = "bottom",
		  		 legend.title = element_text(size = 6),
		  		 legend.text = element_text(size = 6),
		  		 legend.key.size = unit(0.4, "cm") )
	return(gg)
})

# specify the additional arguments to grid.arrange
# by adding it to the list that's going to be do.called
plots[['ncol']] = 2
do.call( grid.arrange, plots )

