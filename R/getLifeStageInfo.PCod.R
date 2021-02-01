#'
#'@title Get a list that defines life stage info for the DisMELS Pacific cod IBM.
#'
#'@description Function to get a list that defines life stage info for the DisMELS Pacific cod IBM.
#'
#'@param resType - results file type (i.e., 'NEW2.0SC')
#'@return a list
#'
#'@details Uses \code{rDisMELS::getStandardAttributes}.
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
    EggClassInfo<-rbind(data.frame(short_name="attached",  data_type="character",name="attached?",                   stringsAsFactors=FALSE),
                        data.frame(short_name="devStage",  data_type="numeric",name="development stage",             stringsAsFactors=FALSE),
                        data.frame(short_name="diam",      data_type="cnumeric",name="egg diameter",                 stringsAsFactors=FALSE),
                        data.frame(short_name="density",   data_type="numeric",name="egg density",                   stringsAsFactors=FALSE),
                        data.frame(short_name="temp",      data_type="numeric",name="temperature deg C",             stringsAsFactors=FALSE),
                        data.frame(short_name="copepod",   data_type="numeric",name="Small copepods mg/m^3 dry wt C",stringsAsFactors=FALSE),
                        data.frame(short_name="euphausiid",data_type="character",name="Euphausiids mg/m^3 dry wt C", stringsAsFactors=FALSE),
                        data.frame(short_name="neocalanus",data_type="numeric",name="Neocalanoids mg/m^3 dry wt",    stringsAsFactors=FALSE),
                        data.frame(short_name="sal",       data_type="numeric",name="salinity",                      stringsAsFactors=FALSE),
                        data.frame(short_name="rho",       data_type="numeric",name="in situ density",               stringsAsFactors=FALSE));
    EggClassInfo  <-rbind(dfrStdAtts,as.data.frame(EggClassInfo,stringsAsFactors=FALSE));

    YSLClassInfo<-rbind(data.frame(short_name="attached",  data_type="character",name="attached?",                     stringsAsFactors=FALSE),
                        data.frame(short_name="length",    data_type="numeric",  name="length",                        stringsAsFactors=FALSE),
                        data.frame(short_name="temp",      data_type="numeric",  name="temperature deg C",             stringsAsFactors=FALSE),
                        data.frame(short_name="sal",       data_type="numeric",  name="salinity",                      stringsAsFactors=FALSE),
                        data.frame(short_name="rho",       data_type="numeric",  name="in situ density",               stringsAsFactors=FALSE),
                        data.frame(short_name="copepod",   data_type="numeric",  name="Small copepods mg/m^3 dry wt C",stringsAsFactors=FALSE),
                        data.frame(short_name="euphausiid",data_type="numeric",  name="Euphausiids mg/m^3 dry wt C",   stringsAsFactors=FALSE),
                        data.frame(short_name="neocalanus",data_type="numeric",  name="Neocalanoids mg/m^3 dry wt",    stringsAsFactors=FALSE));
    YSLClassInfo  <-rbind(dfrStdAtts,as.data.frame(YSLClassInfo,stringsAsFactors=FALSE));

    FDLClassInfo        <-YSLClassInfo;
    FDLpfClassInfo      <-YSLClassInfo;

    EpiJuvClassInfo<-rbind(data.frame(short_name="attached",  data_type="character",name="attached?",                     stringsAsFactors=FALSE),
                           data.frame(short_name="length",    data_type="numeric",  name="length",             stringsAsFactors=FALSE),
                           data.frame(short_name="temp",      data_type="numeric",  name="temperature deg C",             stringsAsFactors=FALSE),
                           data.frame(short_name="sal",       data_type="numeric",  name="salinity",                      stringsAsFactors=FALSE),
                           data.frame(short_name="rho",       data_type="numeric",  name="in situ density",               stringsAsFactors=FALSE),
                           data.frame(short_name="copepod",   data_type="numeric",  name="Small copepods mg/m^3 dry wt C",stringsAsFactors=FALSE),
                           data.frame(short_name="euphausiid",data_type="numeric",  name="Euphausiids mg/m^3 dry wt C",   stringsAsFactors=FALSE),
                           data.frame(short_name="neocalanus",data_type="numeric",  name="Neocalanoids mg/m^3 dry wt",    stringsAsFactors=FALSE),
                           data.frame(short_name="hsi",       data_type="numeric",  name="Habitat suitability index",     stringsAsFactors=FALSE));
    EpiJuvClassInfo  <-rbind(dfrStdAtts,as.data.frame(EpiJuvClassInfo,stringsAsFactors=FALSE));

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
    lifeStageTypes<-rbind(data.frame(typeName="Eggs",      class='sh.pcod.EggStage.EggStage',
                                     name="benthic egg",   nextType="YSL",              stringsAsFactors=FALSE),
                          data.frame(typeName="YSL",       class='sh.pcod.YSLStage.YSLStage',
                                     name="yolksac larva", nextType="FDL",              stringsAsFactors=FALSE),
                          data.frame(typeName="FDL",                  class='sh.pcod.FDLStage.FDLStage',
                                     name="feeding preflexion larva", nextType="FDLpf", stringsAsFactors=FALSE),
                          data.frame(typeName="FDLpf",         class='sh.pcod.FDLpfStage.FDLpfStage',
                                     name="postflexion larva", nextType="Epijuv",       stringsAsFactors=FALSE),
                          data.frame(typeName="Epijuv",          class='sh.pcod.EpijuvStage.EpijuvStage',
                                     name="epipelagic juvenile", nextType="BenthicJuv", stringsAsFactors=FALSE),
                          data.frame(typeName="BenthicJuv",   class='sh.pcod.BenthicJuvStage.BenthicJuvStage',
                                     name="benthic juvenile", nextType="",            stringsAsFactors=FALSE));

    return(invisible(list(resType=resType,classInfo=classInfo,lifeStageTypes=lifeStageTypes)));
}
