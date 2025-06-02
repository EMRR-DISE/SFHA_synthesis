#what's going on with flow at NSL?

library(cder)
library(tidyverse)

nsl = cdec_query("NSL", c(1,20), start.date = ymd("2022-01-01"), end.date = today())

bdl = cdec_query("BDL", c(1,20), start.date = ymd("2022-01-01"), end.date = today())

flows = bind_rows(nsl, bdl) %>%
  mutate(Date = date(ObsDate))

ggplot(flows, aes(x = ObsDate, y = Value, color = StationID))+
  facet_wrap(~SensorType, nrow =2, scales = "free")+
  geom_line()

aveflow = group_by(flows, StationID, Date, SensorType) %>%
  summarize(Value = mean(Value, na.rm =T))


ggplot(aveflow, aes(x = Date, y = Value, color = StationID))+
  facet_wrap(~SensorType, nrow =2, scales = "free")+
  geom_line()

ggplot(aveflow, aes(x = Date, y = Value, color = StationID))+
  facet_wrap(~SensorType, nrow =2, scales = "free")+
  geom_line()
