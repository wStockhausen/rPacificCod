#--calculate growth trajectories at different temperatures
initSL = 0;#--initial embryo size (mm)
vT = 1:10;
tbl = NULL;
for (T in vT){
  tbl = rbind(tbl,calcSLatAgeAtConstantTemp(100,initSL,T,egg_StageDuration,egg_GrowthRateSL));
}
tbl$T = factor(tbl$T);
ggplot(tbl,mapping=aes_string(x="age",y="SL",colour="T"))+
  geom_line()+
  labs(x="age-in-stage (days)",y="embryo size (mm SL)",colour="Temperature (deg C)");
rm(initSL,vT,tbl);

initDW = 0.01;#--initial embryo weight (mg)
vT = 1:10;
tbl = NULL;
for (T in vT){
  tbl = rbind(tbl,calcDWatAgeAtConstantTemp(100,initDW,T,egg_StageDuration,egg_GrowthRateDW));
}
tbl$T = factor(tbl$T);
ggplot(tbl,mapping=aes_string(x="age",y="DW",colour="T"))+
  geom_line()+
  labs(x="age-in-stage (days)",y="embryo weight (mg DW)",colour="Temperature (deg C)");
rm(initDW,vT,tbl);

