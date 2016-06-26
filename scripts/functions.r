
# Promedio de encuestas
# Funciones auxiliares

# Kiko Llaneras (2016)

# load libaries

library(reshape2)
library(dplyr)
library(tidyr)
library(broom)
library(readr)
# library(purrr)
# library(ggplot2)

# Build the matrix to asign seats
dhondt <- function( votes, n_seats, minBarrier){ 
  
  votes[votes<minBarrier] <- NA
  
  # get matrix and assign seats for top values
  matrix <- sapply( votes, function(x) x / 1:n_seats )
  party  <- rep( letters[1:length(votes)], each = n_seats )
  seat   <- party[order( - matrix )[1:n_seats]]
  
  # return seat count per party
  as.vector( table( factor(seat, levels=unique(party) ) ) )
}

# basic style for charts 
# style <-list(theme_minimal(), 
#              theme(text = element_text(size=12),
#                    panel.grid.minor = element_blank(),
#                    panel.grid.major = element_line(colour="#eeeeee", size=0.25),
#                    axis.text.x = element_text(colour = "#666666"),
#                    axis.text.y = element_text(colour = "#666666"),
#                    axis.title.x = element_text(colour = "#222222", size=12, margin=margin(8,0,0,0) ),
#                    axis.title.y = element_text(colour = "#222222", size=12, margin=margin(0,8,0,0) ))
# )

# The palette with black:
colParties <- c("#5D78FF", "#FF302E", "#E657C4", "#F18640", "#CCCCCC","#5FDD9D")
