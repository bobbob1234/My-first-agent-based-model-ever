library(data.table)
library(magrittr)
library(stats)
library(sm)
library(dplyr)
setwd("C:/Users/White/OneDrive/Data Products/AGM/My-first-agent-based-model-ever/F1 Real World Analysis/F1 1970 - 2017 Data")
driver_frame <- fread("drivers.csv")
lap_times <- fread("lapTimes.csv") 
lap_times_filtered <- subset(lap_times,lap_times$position == 1 | lap_times$position == 1)
lap_agg <- aggregate(lap_times_filtered$milliseconds,by = list(lap_times_filtered$raceId),mean)
lap_times$custom_split <- paste(lap_times$driverId,lap_times$raceId)
positions_list <- split(lap_times,c(lap_times$custom_split))
for(i in 1:length(positions_list))
{
  positions_list[[i]] <- positions_list[[i]][order(raceId,lap),]
  positions_list[[i]] <- mutate(positions_list[[i]], G_delta = -1*(position - lag(position)))
}
position_frame <- do.call(rbind.data.frame,positions_list) %>% subset(.,.$G_delta >= 0)
position_agg <- aggregate(position_frame$G_delta,by = list(position_frame$raceId),sum)
colnames(position_agg) <- c("race_id","No of Overtakes")
colnames(lap_agg) <- c("race_id","time lag")
f1_comp_frame <- dplyr::inner_join(position_agg,lap_agg)
status_frame <- fread("status.csv")