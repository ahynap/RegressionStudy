library(tidyverse)
library(dplyr)
library(ggplot2)

daily_demand_forecast <- read.csv("L8-demand.csv", sep=";")
ls(daily_demand_forecast)
summary(daily_demand_forecast)

#===== single =====#
model_1 <- lm(Target..Total.orders. ~ Non.urgent.order ,daily_demand_forecast)
summary(model_1)$r.squared #max single value

model_2 <- lm(Target..Total.orders. ~ Urgent.order ,daily_demand_forecast)
summary(model_2)$r.squared

model_3 <- lm(Target..Total.orders. ~ Order.type.A ,daily_demand_forecast)
summary(model_3)$r.squared

model_4 <- lm(Target..Total.orders. ~ Order.type.B ,daily_demand_forecast)
summary(model_4)$r.squared

model_5 <- lm(Target..Total.orders. ~ Order.type.C ,daily_demand_forecast)
summary(model_5)$r.squared

model_6 <- lm(Target..Total.orders. ~ Fiscal.sector.orders ,daily_demand_forecast)
summary(model_6)$r.squared

model_7 <- lm(Target..Total.orders. ~ Orders.from.the.traffic.controller.sector ,daily_demand_forecast)
summary(model_7)$r.squared

model_8 <- lm(Target..Total.orders. ~ Banking.orders..1. ,daily_demand_forecast)
summary(model_8)$r.squared

model_9 <- lm(Target..Total.orders. ~ Banking.orders..2. ,daily_demand_forecast)
summary(model_9)$r.squared

model_10 <- lm(Target..Total.orders. ~ Banking.orders..3. ,daily_demand_forecast)
summary(model_10)$r.squared


#===== double =====#
model_1_2 <- lm(Target..Total.orders. ~ Non.urgent.order + Urgent.order ,daily_demand_forecast)
summary(model_1_2)$r.squared

model_1_3 <- lm(Target..Total.orders. ~ Non.urgent.order + Order.type.A ,daily_demand_forecast)
summary(model_1_3)$r.squared

model_1_4 <- lm(Target..Total.orders. ~ Non.urgent.order + Order.type.B ,daily_demand_forecast)
summary(model_1_4)$r.squared

model_1_5 <- lm(Target..Total.orders. ~ Non.urgent.order + Order.type.C ,daily_demand_forecast)
summary(model_1_5)$r.squared

model_1_6 <- lm(Target..Total.orders. ~ Non.urgent.order + Fiscal.sector.orders ,daily_demand_forecast)
summary(model_1_6)$r.squared

model_1_7 <- lm(Target..Total.orders. ~ Non.urgent.order + Orders.from.the.traffic.controller.sector ,daily_demand_forecast)
summary(model_1_7)$r.squared

model_1_8 <- lm(Target..Total.orders. ~ Non.urgent.order + Banking.orders..1. ,daily_demand_forecast)
summary(model_1_8)$r.squared

model_1_9 <- lm(Target..Total.orders. ~ Non.urgent.order + Banking.orders..2. ,daily_demand_forecast)
summary(model_1_9)$r.squared

model_1_10 <- lm(Target..Total.orders. ~ Non.urgent.order + Banking.orders..3. ,daily_demand_forecast)
summary(model_1_10)$r.squared

model_2_3 <- lm(Target..Total.orders. ~ Urgent.order + Order.type.A ,daily_demand_forecast)
summary(model_2_3)$r.squared

model_2_4 <- lm(Target..Total.orders. ~ Urgent.order + Order.type.B ,daily_demand_forecast)
summary(model_2_4)$r.squared

model_2_5 <- lm(Target..Total.orders. ~ Urgent.order + Order.type.C ,daily_demand_forecast)
summary(model_2_5)$r.squared

model_2_6 <- lm(Target..Total.orders. ~ Urgent.order + Fiscal.sector.orders ,daily_demand_forecast)
summary(model_2_6)$r.squared

model_2_7 <- lm(Target..Total.orders. ~ Urgent.order + Orders.from.the.traffic.controller.sector ,daily_demand_forecast)
summary(model_2_7)$r.squared

model_2_8 <- lm(Target..Total.orders. ~ Urgent.order + Banking.orders..1. ,daily_demand_forecast)
summary(model_2_8)$r.squared

model_2_9 <- lm(Target..Total.orders. ~ Urgent.order + Banking.orders..2. ,daily_demand_forecast)
summary(model_2_9)$r.squared

model_2_10 <- lm(Target..Total.orders. ~ Urgent.order + Banking.orders..3. ,daily_demand_forecast)
summary(model_2_10)$r.squared

model_3_4 <- lm(Target..Total.orders. ~ Order.type.A + Order.type.B ,daily_demand_forecast)
summary(model_3_4)$r.squared

model_3_5 <- lm(Target..Total.orders. ~ Order.type.A + Order.type.C ,daily_demand_forecast)
summary(model_3_5)$r.squared

model_3_6 <- lm(Target..Total.orders. ~ Order.type.A + Fiscal.sector.orders ,daily_demand_forecast)
summary(model_3_6)$r.squared

model_3_7 <- lm(Target..Total.orders. ~ Order.type.A + Orders.from.the.traffic.controller.sector ,daily_demand_forecast)
summary(model_3_7)$r.squared

model_3_8 <- lm(Target..Total.orders. ~ Order.type.A + Banking.orders..1. ,daily_demand_forecast)
summary(model_3_8)$r.squared

model_3_9 <- lm(Target..Total.orders. ~ Order.type.A + Banking.orders..2. ,daily_demand_forecast)
summary(model_3_9)$r.squared

model_3_10 <- lm(Target..Total.orders. ~ Order.type.A + Banking.orders..3. ,daily_demand_forecast)
summary(model_3_10)$r.squared

model_4_5 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C ,daily_demand_forecast)
summary(model_4_5)$r.squared #max multiple value

model_4_6 <- lm(Target..Total.orders. ~ Order.type.B + Fiscal.sector.orders ,daily_demand_forecast)
summary(model_4_6)$r.squared

model_4_7 <- lm(Target..Total.orders. ~ Order.type.B + Orders.from.the.traffic.controller.sector ,daily_demand_forecast)
summary(model_4_7)$r.squared

model_4_8 <- lm(Target..Total.orders. ~ Order.type.B + Banking.orders..1. ,daily_demand_forecast)
summary(model_4_8)$r.squared

model_4_9 <- lm(Target..Total.orders. ~ Order.type.B + Banking.orders..2. ,daily_demand_forecast)
summary(model_4_9)$r.squared

model_4_10 <- lm(Target..Total.orders. ~ Order.type.B + Banking.orders..3. ,daily_demand_forecast)
summary(model_4_10)$r.squared

model_5_6 <- lm(Target..Total.orders. ~ Order.type.C + Fiscal.sector.orders ,daily_demand_forecast)
summary(model_5_6)$r.squared

model_5_7 <- lm(Target..Total.orders. ~ Order.type.C + Orders.from.the.traffic.controller.sector ,daily_demand_forecast)
summary(model_5_7)$r.squared

model_5_8 <- lm(Target..Total.orders. ~ Order.type.C + Banking.orders..1. ,daily_demand_forecast)
summary(model_5_8)$r.squared

model_5_9 <- lm(Target..Total.orders. ~ Order.type.C + Banking.orders..2. ,daily_demand_forecast)
summary(model_5_9)$r.squared

model_5_10 <- lm(Target..Total.orders. ~ Order.type.C + Banking.orders..3. ,daily_demand_forecast)
summary(model_5_10)$r.squared

model_6_7 <- lm(Target..Total.orders. ~ Fiscal.sector.orders + Orders.from.the.traffic.controller.sector ,daily_demand_forecast)
summary(model_6_7)$r.squared

model_6_8 <- lm(Target..Total.orders. ~ Fiscal.sector.orders + Banking.orders..1. ,daily_demand_forecast)
summary(model_6_8)$r.squared

model_6_9 <- lm(Target..Total.orders. ~ Fiscal.sector.orders + Banking.orders..2. ,daily_demand_forecast)
summary(model_6_9)$r.squared

model_6_10 <- lm(Target..Total.orders. ~ Fiscal.sector.orders + Banking.orders..3. ,daily_demand_forecast)
summary(model_6_10)$r.squared

model_7_8 <- lm(Target..Total.orders. ~ Orders.from.the.traffic.controller.sector + Banking.orders..1. ,daily_demand_forecast)
summary(model_7_8)$r.squared

model_7_9 <- lm(Target..Total.orders. ~ Orders.from.the.traffic.controller.sector + Banking.orders..2. ,daily_demand_forecast)
summary(model_7_9)$r.squared

model_7_10 <- lm(Target..Total.orders. ~ Orders.from.the.traffic.controller.sector + Banking.orders..3. ,daily_demand_forecast)
summary(model_7_10)$r.squared

model_8_9 <- lm(Target..Total.orders. ~ Banking.orders..1. + Banking.orders..2. ,daily_demand_forecast)
summary(model_8_9)$r.squared

model_8_10 <- lm(Target..Total.orders. ~ Banking.orders..1. + Banking.orders..3. ,daily_demand_forecast)
summary(model_8_10)$r.squared

model_9_10 <- lm(Target..Total.orders. ~ Banking.orders..2. + Banking.orders..3. ,daily_demand_forecast)
summary(model_9_10)$r.squared

#===== tripple of 4 + 5 + X =====#
model_4_5_1 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C + Non.urgent.order ,daily_demand_forecast)
summary(model_4_5_1)$r.squared 

model_4_5_2 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C + Urgent.order ,daily_demand_forecast)
summary(model_4_5_2)$r.squared 

model_4_5_3 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C + Order.type.A ,daily_demand_forecast)
summary(model_4_5_3)$r.squared

model_4_5_6 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C + Fiscal.sector.orders ,daily_demand_forecast)
summary(model_4_5_6)$r.squared

model_4_5_7 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C + Orders.from.the.traffic.controller.sector ,daily_demand_forecast)
summary(model_4_5_7)$r.squared 

model_4_5_8 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C + Banking.orders..1. ,daily_demand_forecast)
summary(model_4_5_8)$r.squared

model_4_5_9 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C + Banking.orders..2. ,daily_demand_forecast)
summary(model_4_5_9)$r.squared 

model_4_5_10 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C + Banking.orders..3. ,daily_demand_forecast)
summary(model_4_5_10)$r.squared





#================ MAX ===============#
model_1 <- lm(Target..Total.orders. ~ Non.urgent.order ,daily_demand_forecast)
summary(model_1) #max simple linear regression 0.8733236
plot(Target..Total.orders. ~ Non.urgent.order ,daily_demand_forecast)
abline(model_1)

#max double multiple regression additive
model_4_5 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C ,daily_demand_forecast)
summary(model_4_5)
#max double multiple regression multiple
model_4_5mul <- lm(Target..Total.orders. ~ Order.type.B * Order.type.C ,daily_demand_forecast)
summary(model_4_5mul)$r.squared 

#max 3 multiple regression additive 
model_4_5_8 <- lm(Target..Total.orders. ~ Order.type.B + Order.type.C + Banking.orders..1. ,daily_demand_forecast)
summary(model_4_5_8)
#max 3 multiple regression multiple 0.979365
model_4_5_8mul <- lm(Target..Total.orders. ~ Order.type.B * Order.type.C * Banking.orders..1. ,daily_demand_forecast)
summary(model_4_5_8mul)$r.squared 

