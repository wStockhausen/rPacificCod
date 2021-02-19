#'
#'@title Get a list that defines life stage info for the DisMELS Pacific cod IBM.
#'
#'@description Function to get a list that defines life stage info for the DisMELS Pacific cod IBM.
#'
#'@param resType - results file type (i.e., 'NEW2.0SC')
#'
#'@return a list with elements
#'\itemize{
#'  \item{resType - resType used to get standard attributes}
#'  \item{classInfo - list by class name, with each element a list with elements 'info' (a tibble) and 'typeNames' (a character vector)}
#'  \item{lifeStageTypes - tibble with columns typeName, class, and name}
#'}
#'
#'@details Uses \code{rDisMELS::getStandardAttributes}.resType is passed on to this function.
#'
#'@import tibble
#'
#'@export
#'
getLifeStageInfo.PCod<-function(resType='NEW2.0SC'){

    #get standard attributes dataframe
    dfrStdAtts <- rDisMELS::getStandardAttributes(resType);

    #java LHS class names
    classNames<-c('sh.pcod.EggStage.EggStage',
                  'sh.pcod.YSLStage.YSLStage',
                  'sh.pcod.FDLStage.FDLStage',
                  'sh.pcod.FDLpfStage.FDLpfStage',
                  'sh.pcod.EpijuvStage.EpijuvStage',
                  'sh.pcod.BenthicJuvStage.BenthicJuvStage');

    #information on 'additional attributes' for each life stage class
    EggClassInfo<-rbind(tibble(short_name="attached", data_type="character",name="attached?"),
                        tibble(short_name="stgProg",  data_type="numeric",  name="egg stage progression"),
                        tibble(short_name="SL",       data_type="numeric",  name="embryo SL (mm)"),
                        tibble(short_name="DW",       data_type="numeric",  name="embryo DW (mg)"),
                        tibble(short_name="grSL",     data_type="numeric",  name="growth rate for SL (mm/d)"),
                        tibble(short_name="grDW",     data_type="numeric",  name="growth rate for DW (1/d)"),
                        tibble(short_name="density",  data_type="numeric",  name="egg density"),
                        tibble(short_name="temp",     data_type="numeric",  name="temperature deg C"),
                        tibble(short_name="sal",      data_type="numeric",  name="salinity"),
                        tibble(short_name="rho",      data_type="numeric",  name="in situ density")
                        );
    EggClassInfo<-rbind(dfrStdAtts,EggClassInfo);

    YSLClassInfo<-rbind(tibble(short_name="attached",  data_type="character",name="attached?"),
                        tibble(short_name="SL",        data_type="numeric",  name="standard length (mm)"),
                        tibble(short_name="DW",        data_type="numeric",  name="dry weight (mg)"),
                        tibble(short_name="grSL",      data_type="numeric",  name="growth rate for SL (mm/d)"),
                        tibble(short_name="grDW",      data_type="numeric",  name="growth rate for DW (1/d)"),
                        tibble(short_name="temp",      data_type="numeric",  name="temperature deg C"),
                        tibble(short_name="sal",       data_type="numeric",  name="salinity"),
                        tibble(short_name="rho",       data_type="numeric",  name="in situ density"),
                        tibble(short_name="copepod",   data_type="numeric",  name="Small copepods mg/m^3 dry wt C"),
                        tibble(short_name="euphausiid",data_type="numeric",  name="Euphausiids mg/m^3 dry wt C"),
                        tibble(short_name="neocalanus",data_type="numeric",  name="Neocalanoids mg/m^3 dry wt"),
                        tibble(short_name="progYSA",   data_type="numeric",  name="indicator for yolk-sac absorption"),
                        tibble(short_name="progPNR",   data_type="numeric",  name="indicator for point-of-no return"),
                        tibble(short_name="prNotFed",  data_type="numeric",  name="probability of not having fed")
                        );
    YSLClassInfo<-rbind(dfrStdAtts,YSLClassInfo);

    FDLClassInfo<-rbind(tibble(short_name="attached",  data_type="character",name="attached?"),
                        tibble(short_name="SL",        data_type="numeric",  name="standard length (mm)"),
                        tibble(short_name="DW",        data_type="numeric",  name="dry weight (mg)"),
                        tibble(short_name="grSL",      data_type="numeric",  name="growth rate for SL (mm/d)"),
                        tibble(short_name="grDW",      data_type="numeric",  name="growth rate for DW (1/d)"),
                        tibble(short_name="temp",      data_type="numeric",  name="temperature deg C"),
                        tibble(short_name="sal",       data_type="numeric",  name="salinity"),
                        tibble(short_name="rho",       data_type="numeric",  name="in situ density"),
                        tibble(short_name="copepod",   data_type="numeric",  name="Small copepods mg/m^3 dry wt C"),
                        tibble(short_name="euphausiid",data_type="numeric",  name="Euphausiids mg/m^3 dry wt C"),
                        tibble(short_name="neocalanus",data_type="numeric",  name="Neocalanoids mg/m^3 dry wt")
                        );
    FDLClassInfo<-rbind(dfrStdAtts,FDLClassInfo);


    FDLpfClassInfo<-FDLClassInfo;

    EpiJuvClassInfo<-rbind(tibble(short_name="attached",  data_type="character",name="attached?"),
                           tibble(short_name="SL",        data_type="cnumeric", name="standard length (mm)"),
                           tibble(short_name="DW",        data_type="numeric",  name="dry weight (mg)"),
                           tibble(short_name="grSL",      data_type="cnumeric", name="growth rate for SL (mm/d)"),
                           tibble(short_name="grDW",      data_type="numeric",  name="growth rate for DW (1/d)"),
                           tibble(short_name="temp",      data_type="numeric",  name="temperature deg C"),
                           tibble(short_name="sal",       data_type="numeric",  name="salinity"),
                           tibble(short_name="rho",       data_type="numeric",  name="in situ density"),
                           tibble(short_name="copepod",   data_type="numeric",  name="Small copepods mg/m^3 dry wt C"),
                           tibble(short_name="euphausiid",data_type="numeric",  name="Euphausiids mg/m^3 dry wt C"),
                           tibble(short_name="neocalanus",data_type="numeric",  name="Neocalanoids mg/m^3 dry wt"),
                           tibble(short_name="hsi",       data_type="numeric",  name="Habitat suitability index"),
                           tibble(short_name="TL",        data_type="numeric",  name="total length (mm)"),
                           tibble(short_name="WW",        data_type="numeric",  name="wet weight (mg)"),
                           tibble(short_name="grTL",      data_type="numeric",  name="growth rate for TL (mm/d)"),
                           tibble(short_name="grWW",      data_type="numeric",  name="growth rate for WW (1/d)")
                           );
    EpiJuvClassInfo  <-rbind(dfrStdAtts,EpiJuvClassInfo);

    BenthicJuvClassInfo <-EpiJuvClassInfo;

    #class info, by class
    classInfo<-list();
    classInfo[['sh.pcod.EggStage.EggStage']]              <-list(info=EggClassInfo,       typeNames=c("Eggs"));
    classInfo[['sh.pcod.YSLStage.YSLStage']]              <-list(info=YSLClassInfo,       typeNames=c("YSL"));
    classInfo[['sh.pcod.FDLStage.FDLStage']]              <-list(info=FDLClassInfo,       typeNames=c("FDL"));
    classInfo[['sh.pcod.FDLpfStage.FDLpfStage']]          <-list(info=FDLpfClassInfo,     typeNames=c("FDLpfL"));
    classInfo[['sh.pcod.EpijuvStage.EpijuvStage']]        <-list(info=EpiJuvClassInfo,    typeNames=c("Epijuv"));
    classInfo[['sh.pcod.BenthicJuvStage.BenthicJuvStage']]<-list(info=BenthicJuvClassInfo,typeNames=c("BenJuv"));

    #map of defined life stage type names to class names
    lifeStageTypes<-rbind(tibble(typeName="Eggs",      class='sh.pcod.EggStage.EggStage',              name="benthic egg",              nextType="YSL"),
                          tibble(typeName="YSL",       class='sh.pcod.YSLStage.YSLStage',              name="yolksac larva",            nextType="FDL"),
                          tibble(typeName="FDL",       class='sh.pcod.FDLStage.FDLStage',              name="feeding preflexion larva", nextType="FDLpf"),
                          tibble(typeName="FDLpf",     class='sh.pcod.FDLpfStage.FDLpfStage',          name="postflexion larva",        nextType="Epijuv"),
                          tibble(typeName="Epijuv",    class='sh.pcod.EpijuvStage.EpijuvStage',        name="epipelagic juvenile",      nextType="BenthicJuv"),
                          tibble(typeName="BenthicJuv",class='sh.pcod.BenthicJuvStage.BenthicJuvStage',name="benthic juvenile",         nextType=""));

    return(invisible(list(resType=resType,classInfo=classInfo,lifeStageTypes=lifeStageTypes)));
}
