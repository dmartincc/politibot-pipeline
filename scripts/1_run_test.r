
# Promedio de encuestas
# Kiko Llaneras (2016)

# libraries
source("scripts/functions.r")
 
# load things ---------------------

# seats per district
provs <- c("madrid", "tarragona", "teruel")
n_seats <- c(36, 6, 3)

# load new recount
df_in <- read_csv2("scripts/test.csv")  %>% 
      mutate(PROV = factor(PROV) ) %>% 
      mutate(PROV = factor(PROV, levels = rev(levels(PROV)), ordered = T ) ) %>%
      mutate(SEATS = n_seats[match( PROV, provs )] )  %>%
      select(-CCAA,-ERC:-BLANCO)  %>%
      select(PROV, PERCENT, TIME, SEATS, everything() )

# load model of polls 
polls <- read_csv2("scripts/model.csv")  %>% 
  mutate(PROV = factor(PROV) ) %>% 
  mutate(PROV = factor(PROV, levels = rev(levels(PROV)), ordered = T ) ) %>% 
  select(-CCAA,-ERC:-BLANCO) 


# run the model 1 ---------------------

# parameters
alpha_tau <- 0
gamma <- 1

# fusion of  votes
model_1 <- inner_join( df_in, polls, by = "PROV" ) %>%  
       group_by(TIME) %>% 
       mutate( PP.bias   = weighted.mean( PP.x/PP.y,     PERCENT ),
               PSOE.bias = weighted.mean( PSOE.x/PSOE.y, PERCENT ),
               PIU.bias  = weighted.mean( PIU.x/PIU.y,   PERCENT ),
               CS.bias   = weighted.mean( CS.x/CS.y,     PERCENT )) %>%
       ungroup() %>% 
       mutate( alpha  = 1 -exp( -PERCENT/(alpha_tau) ),
               PP.v   = alpha*PP.x   + (1-alpha)*PP.y  *(1-gamma+gamma*PP.bias),
               PSOE.v = alpha*PSOE.x + (1-alpha)*PSOE.y*(1-gamma+gamma*PSOE.bias),
               PIU.v  = alpha*PIU.x  + (1-alpha)*PIU.y *(1-gamma+gamma*PIU.bias),
               CS.v   = alpha*CS.x   + (1-alpha)*CS.y  *(1-gamma+gamma*CS.bias))  %>%
       select(PROV, TIME, PERCENT, SEATS, PP.v, PSOE.v, PIU.v, CS.v)

# clean seats data
model_1$PP <- NA
model_1$PSOE <- NA
model_1$PIU <- NA
model_1$CS <- NA

# compute seats
for (i in 1:nrow(model_1) ) {
  seats <- model_1$SEATS[i]
  votes <- model_1[i,5:8]
  model_1[i, 9:12] <- dhondt( votes, seats, 3 )
}

# run the model 2 ---------------------

# parameters
alpha_tau <- 50/3
gamma <- 1

# fusion of  votes
model_2 <- inner_join( df_in, polls, by = "PROV" ) %>%  
  group_by(TIME) %>% 
  mutate( PP.bias   = weighted.mean( PP.x/PP.y,     PERCENT ),
          PSOE.bias = weighted.mean( PSOE.x/PSOE.y, PERCENT ),
          PIU.bias  = weighted.mean( PIU.x/PIU.y,   PERCENT ),
          CS.bias   = weighted.mean( CS.x/CS.y,     PERCENT )) %>%
  ungroup() %>% 
  mutate( alpha  = 1 -exp( -PERCENT/(alpha_tau) ),
          PP.v   = alpha*PP.x   + (1-alpha)*PP.y  *(1-gamma+gamma*PP.bias),
          PSOE.v = alpha*PSOE.x + (1-alpha)*PSOE.y*(1-gamma+gamma*PSOE.bias),
          PIU.v  = alpha*PIU.x  + (1-alpha)*PIU.y *(1-gamma+gamma*PIU.bias),
          CS.v   = alpha*CS.x   + (1-alpha)*CS.y  *(1-gamma+gamma*CS.bias))  %>%
  select(PROV, TIME, PERCENT, SEATS, PP.v, PSOE.v, PIU.v, CS.v)

# clean seats data
model_2$PP <- NA
model_2$PSOE <- NA
model_2$PIU <- NA
model_2$CS <- NA

# compute seats
for (i in 1:nrow(model_2) ) {
  seats <- model_2$SEATS[i]
  votes <- model_2[i,5:8]
  model_2[i, 9:12] <- dhondt( votes, seats, 3 )
}

# merge...  ---------------------
model_1$MODEL <- 1
model_2$MODEL <- 2
output <- full_join( model_1, model_2 )
output <- select(output, PROV, TIME, PERCENT, SEATS, MODEL, PP, PSOE, PIU, CS)

write_csv(output,"output.csv")

# 3. compute rows with totals
# # get totals
# totals <- model_seats %>%  
#        group_by(TIME) %>% 
#        mutate(     PROV = "total",
#                    PERCENT = NA,
#                    PP   = sum(PP), 
#                    PSOE = sum(PSOE), 
#                    PIU  = sum(PIU), 
#                    CS   = sum(CS)) %>%
#        ungroup() %>% 
#        distinct(TIME)
# 
# # put together
# model_seats <- model_seats %>% 
#                full_join(totals) %>%
#                mutate(PROV = factor(PROV) ) %>% 
#                mutate(PROV = factor(PROV, levels = rev(levels(PROV)), ordered = T ) ) %>% 
#                select(PROV, TIME, PERCENT, SEATS, PP, PSOE, PIU, CS)

# random plotting ---------------------
# ggplot(data.frame(x = c(0, 100)), aes(x)) +
#   stat_function(fun = function(x) {1-exp(-x/(50/4))}, geom = "line") + 
#   stat_function(fun = function(x) {1-exp(-x/(50/3))}, geom = "line")

# PLOT as table ---------------------
# data_gg <- output %>%
#   gather(party, seats, -PROV, -PERCENT, -TIME, -SEATS, -MODEL) %>%
#   mutate(party = factor(party, levels = c("PP","PSOE","PIU","CS" ) ) )

# ggplot(filter(data_gg, PERCENT==max(PERCENT), MODEL==1 ),
#        aes(party, PROV, label = round(seats,1) )) +
#   geom_tile(aes(fill = seats), colour = "white") +
#   scale_fill_distiller(palette = "Blues", direction = 1) +
#   geom_text(size=3, color="white") +
#   ylab("") + xlab("") +
#   style + theme(legend.position = "none")


# # PLOT as series ---------------------
# data_gg <- model_seats %>%
#   gather(party, seats, -PROV, -PERCENT, -TIME, -SEATS, -MODEL) %>%
#   mutate(party = factor(party, levels = c("PP","PSOE","PIU","CS" ) ) )

# ggplot(filter(data_gg, PROV=="madrid", MODEL==1),
#        aes(PERCENT, seats, group=party, color=party)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual(values=colParties) +
#   scale_y_continuous(limits = c(0, 20)) +
#   style + theme(legend.position="none") + ylab("") + xlab("")



