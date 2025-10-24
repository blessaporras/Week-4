disasters.data.frame <- read.csv("EMDAT-W4.csv")

str(disasters.data.frame)

#load packages 
#install as well
install.packages("gridExtra")

library(ggplot2)
library(dplyr)
library(purrr)
library(glue)
library(gridExtra)


#ask for a summary
filtered.disasters.df <- disasters.data.frame %>% select(Entity, Year, deaths_all_disasters, injured_all_disasters, homeless_all_disasters) %>%
  rename(deaths = deaths_all_disasters, injuries = injured_all_disasters, 
         homelessness = homeless_all_disasters, country = Entity)

filtered.disasters.df %>%
  select(-country, -Year) %>%  # Remove non numerical columns 
  map_dbl(mean, na.rm = TRUE) #map function #mean function, if mean of each col in new filtered data frame called filtered.disasters.df

#filter data before automation - otherwise will be long

filteredfr.disasters.df <- filtered.disasters.df %>% filter(country %in% c("Brazil", "Turkey", "Argentina", "Afghanistan", "Belgium"))

create_point_plot <- function(i) {
  filteredfr.disasters.df %>%
    ggplot(aes_string(x = names(filteredfr.disasters.df)[2], y = names(filteredfr.disasters.df)[i])) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add trend lin
    labs(
      title = glue("The Trend of {names(df)[i]}"),
      y = glue("{names(df)[i]}")
    )
}

plots_list <- map(3:ncol(plots_list <- map(3:ncol(filtered_df), create_point_plot)
                         plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2) # Adjust ncol as needed), create_point_plot)
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2) # Adjust ncol as needed

# Create a list of plots
plots_list <- map(3:ncol(filteredfr.disasters.df), create_point_plot)

# Arrange plots in a grid
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2)
