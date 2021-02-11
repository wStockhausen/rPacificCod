#--compare YSA and PNR over a range of temperatures
require(ggplot2);
require(magrittr);


T = seq(0,15,0.1);
ysa = rPacificCod::ysl_YolkSacAbsorption(T);
pnr = rPacificCod::ysl_PNR(T);
tbl = tibble::tibble(T=T,YSA=ysa,PNR=pnr) %>%
      reshape2::melt(id.vars="T");

ggplot(tbl,mapping=aes_string(x="T",y="value",colour="variable"))+
  geom_line()+
  labs(x="Temperature (deg C)",y="Time (days)");


#--calculate growth trajectories at different temperatures
initSL = 3.8;#--initial YSL SL (mm)
vT = 1:10;
tbl = NULL;
for (T in vT){
  tbl = rbind(tbl,calcSLatAgeAtConstantTemp(100,initSL,T,ysl_YolkSacAbsorption,ysl_GrowthRateSL));
}
tbl$T = factor(tbl$T);
ggplot(tbl,mapping=aes_string(x="age",y="SL",colour="T"))+
  geom_line()+
  labs(x="age-in-stage (days)",y="YSL size (mm SL)",colour="Temperature (deg C)");
rm(initSL,vT,tbl);

initDW = 0.08;#--initial YSL weight (mg)
vT = 1:10;
tbl = NULL;
for (T in vT){
  tbl = rbind(tbl,calcDWatAgeAtConstantTemp(100,initDW,T,ysl_YolkSacAbsorption,ysl_GrowthRateDW));
}
tbl$T = factor(tbl$T);
ggplot(tbl,mapping=aes_string(x="age",y="DW",colour="T"))+
  geom_line()+
  labs(x="age-in-stage (days)",y="YSL weight (mg DW)",colour="Temperature (deg C)");
rm(initDW,vT,tbl);
