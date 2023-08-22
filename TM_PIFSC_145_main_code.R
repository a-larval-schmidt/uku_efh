<<<<<<< HEAD
#libraries#####
library(tidyverse)
library(sp) 
library(raster)
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
library(lattice)
library(suncalc)
library(ggResidpanel)
library(mgcv)

#restarted again####
#ideally this is very reproducible, includes all the changes to any datasets downloaded form inport
#each new csv explains what scripts something else comes from 
#changes to pa in google sheets made right here
#csvs#####
#oes11_06, oes12_06, tc32####
matchy<-read.csv("inputs/matched_all.csv") 
#^from Matching_CTD_Locations_toSamples.R. (ask Justin if okay to post or if he has a link to this)
#SE12_06 and SE11_06 means used in Matching_CTD_Locations_toSamples.R. were pulled via sette2011_2012_CTD_means_function.R
#which consisted of data from CTD log sheets obtained via PIFSC data request PICDR-113137 

#tc8504, tc8604, tc8605#####
#the "all_tcs.csv" pulls only the top 40m but also loses the depths (nets only which vary between tow)
#so, Andrea pulled the cruise level csvs (which have been QC'd and converted from .MOC files)
#and pulled all surface temps only from these cruises L
tc8504_vars<-read.csv("inputs/TC8504_moc_data_allQCd.csv")
#mocness data with salinity values typed by hand by Andrea Schmidt off PDFs obtained via PIFC data request PICDR-113141
#these csvs were pulled together and quality control checked in the script: new_sal_tc8504_june2022.R
tc8504_vars<- tc8504_vars %>% rename("depth_m"=depth, "salinity"=sal, "temperature"=temp)
tc8504_vars<- tc8504_vars %>% unite("tow_id",c(cruise_number, station), sep="_", remove=F)

mini_8504<-tibble("mocid"=tc8504_vars$moc_id,"tow_id"=tc8504_vars$tow_id, "cruise_number"=tc8504_vars$cruise_number, "station"=tc8504_vars$station, "net_number"=tc8504_vars$net_number,"depth_m"=tc8504_vars$depth_m, "temperature"=tc8504_vars$temperature, "salinity"=tc8504_vars$salinity)


tc8604_vars<-read.csv("inputs/8604_moc_data_all_feb23.csv")
#temperature and salinity values came straight from the mocness_function.R processed .MOC files obtained from Bruce Mundy in late 2021.
#The csvs of these .MOC files will be posted to NCEI in the near future
tc8604_summary<-read.csv("inputs/tc8604_summary_stats_aug22.csv")

mini_8604<-tibble("mocid"=tc8604_vars$moc_id,"tow_id"=tc8604_vars$tow_id, "cruise_number"=tc8604_vars$cruise_number, "station"=tc8604_vars$station, "net_number"=tc8604_vars$net_number,"depth_m"=tc8604_vars$depth_m, "temperature"=tc8604_vars$temp_DegC, "salinity"=tc8604_vars$sal)

tc8602_vars<-read.csv("inputs/TC8602_moc_data_all.csv")
#tc8602 files were all bound together from the mocness_function.R processed .MOC files obtained from Bruce Mundy in late 2021
tc8602_summary<-read.csv("inputs/TC8602_tow_summaries.csv")
tc8602_vars<- tc8602_vars %>% rename("cruise_number"=cruise_id, "station"=station_id,"temperature"=temp_DegC, "salinity"=sal)
mini_8602<-tibble("mocid"=tc8602_vars$moc_id,"tow_id"=tc8602_vars$tow_id, "cruise_number"=tc8602_vars$cruise_number, "station"=tc8602_vars$station, "net_number"=tc8602_vars$net_number,"depth_m"=tc8602_vars$depth_m, "temperature"=tc8602_vars$temperature, "salinity"=tc8602_vars$salinity)
all_tcs<-rbind(mini_8602, mini_8504, mini_8604)

##in order to collapse mocness by depth one needs to create depth bins (top 20m)
all_tcs<-all_tcs %>% mutate(depth_bins=ifelse(depth_m<20, "0-20m", "20-200m"))
#filter to only this depth range (nets for offshore trawls sampled )
tcs_20<-all_tcs %>% filter(depth_bins=="0-20m")
#filter again to get rid of really errant data
tcs_20<-tcs_20 %>% filter(depth_m>0)
#filter again to get smaller depth range (0-5m)
c<-tcs_20 %>% filter(depth_m<5)
#impossible values check
c<-filter(c, (12)<temperature & temperature<=30) #min and max values from HOTS CTD potential temperature 0-1000m, 1989-2019
c<-filter(c, 33<salinity& salinity<37) 

#take means per tow_id
tcsummary<-c %>%
  group_by(tow_id) %>% #in order to mesh appropraitely with pa
  summarise_at(vars("salinity", "temperature"),c(min,max,mean))
all_tcs <- tcsummary %>% rename("min_sal"=salinity_fn1,"min_temp"= temperature_fn1, "max_sal"=salinity_fn2,"max_temp"=temperature_fn2, "mean_sal"=salinity_fn3, "mean_temp"=temperature_fn3)
#final QC
all_tcs<-all_tcs%>% mutate(mean_sal=(ifelse(mean_sal>39,mean_sal-6.323265288,mean_sal))) 
#applied correction factor to all to deal with massive salinity errors from tc8504. 
#See techincal memo Appendix 1, section A6 on page 38 for how this value was calculated.
all_tcs<-all_tcs%>% mutate(min_sal=(ifelse(min_sal>39,min_sal-6.323265288,min_sal)))
all_tcs<-all_tcs%>% mutate(max_sal=(ifelse(max_sal>39,max_sal-6.323265288,max_sal)))

#most upto date presence-absence sheet from GoogleSheets#####
pa<-read.csv("inputs/Relevant_Aprion_virescens_literature - presence_absence_sept22.csv")

#Boehlert+Mundy 1996 data####
mb<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/cast_data_with_coord_updated_DD.csv", stringsAsFactors=FALSE, fileEncoding="latin1") 
#^this is equivalent to the MB data from inport with the coordinates that I added based on the station number from the PDFs in the data parcel from Inport
#source: GUID: gov.noaa.nmfs.inport:56552 
mb$cruise_id<-gsub("_","",mb$cruise_id)
mb$tow_id<-gsub("TC_8","TC8",mb$tow_id)
mb<-mb %>% unite(col="moc_id",cruise_id, STATION_NUMBER,net_number, sep="_", remove=F) #this creates a consistent moc and cruise id which can be used across sheets
mb<- mb %>%unite(col="tow_id",cruise_id, STATION_NUMBER, sep="_", remove=F)#this creates a consistent tow id which can be used to meld with pa
mb$X<-NULL
mb<-mb %>% filter(CRUISE_NUMBER != 8505) #removed because no included in presence absence dataset
##in order to collapse mocness by depth
only_volume<-mb %>% group_by(tow_id) %>% summarise_at(vars(WATER_VOLUME_FILTERED),sum)
only_volume<-only_volume %>% rename("sum_vol"="WATER_VOLUME_FILTERED")
#merging####
#meld will be a new dataframe consisting of only things one really actually needs
#first combine pa and matchy
meld<-full_join(pa,matchy) # presence-absence data combined with matched temperature/salinity data from OES12-06, 11-06, TC32 via Matching_CTD_Locations_toSamples.R
meld<-filter(meld, site!="Kahe Point")
#^removes Kahe point Larvae because they are such an outlier in regards to sampling. Will be treated as a case study.
#please see Tech Memo Appendix A, section on Westree et al. 1972 for further details.

mb<-dplyr::distinct(mb, tow_id, .keep_all=TRUE)
bo_mu<-full_join(mb,all_tcs)
#lines 111 to 114 change Bohlert and Mundy 1996 data outputs to be consistent with presence-absence datasheet
bo_mu<-bo_mu %>%mutate(LOCATION_CODE_1="Oahu")
bo_mu<-bo_mu %>%mutate(LOCATION_CODE_2=ifelse(3,"windward","leeward")) # used to say "windward/leeward, pelagic"
bo_mu<-bo_mu %>%mutate(LOCATION_CODE_3=ifelse(1, 1.5, (ifelse(LOCATION_CODE_3==2,5,15))))
bo_mu<-bo_mu %>%mutate(LOCATION_CODE_4=ifelse(1, "day","night"))
bo_mu<-add_column(bo_mu, MOCNESS="MOCNESS")
bm<-left_join(bo_mu, only_volume)

#finally, combine bm and pa data frames
meld2<-full_join(meld, bm)
#lines 121 to 141 are final quality control and consistency checks across the various data sources amalgamated in meld2
meld2<-meld2 %>% filter(cruise_number != "TC8505") #most MOCNESS data from this cruise leg are incomplete, hence its removal
meld2<-meld2 %>% mutate(this_sal= coalesce(Nearest_Sal_Val,mean_sal), .keep="unused")
meld2<-meld2 %>% mutate(this_sal= (ifelse(this_sal>39,this_sal-6.323265288,this_sal)))
meld2<-meld2 %>% mutate(this_temp= coalesce(max_temp, Nearest_Temp_Val))
meld2<-meld2 %>% mutate(mean_temp= coalesce(mean_temp, Nearest_Temp_Val))
meld2<-meld2 %>% mutate(min_temp= coalesce(min_possible_temp, this_temp, min_temp))
meld2<-meld2 %>% mutate(sampling_method= coalesce(sampling_method, MOCNESS), .keep="unused")
meld2<-meld2 %>% mutate(isle_region=ifelse(str_detect(site, "lee")==T, "leeward", site))
meld2<-meld2 %>% mutate(isle_region=ifelse(str_detect(isle_region, "L")==T, "leeward", isle_region))
meld2<-meld2 %>% mutate(isle_region=ifelse(str_detect(isle_region, "17")==T, "leeward", isle_region))
meld2<-meld2 %>% mutate(isle_region=ifelse(str_detect(isle_region, "30")==T, "leeward", isle_region))
meld2<-meld2 %>% mutate(isle_region=ifelse(str_detect(isle_region, "W")==T, "windward", isle_region))
meld2<-meld2  %>% mutate(min_depth= coalesce(MINIMUM_DEPTH, collection_depth_m_min), .keep="unused")
meld2<-meld2 %>% mutate(collection_day= coalesce(collection_day,TRAWL_DAY), .keep="unused")
meld2<-meld2 %>% mutate(collection_month= coalesce(collection_month,TRAWL_MONTH), .keep="unused")
meld2<-meld2 %>% mutate(collection_year= coalesce(collection_year,TRAWL_YEAR), .keep="unused")
meld2<-meld2  %>% filter(max_cast_depth.m. != "failed")
meld2<-meld2  %>% mutate(max_depth= coalesce(MAXIMUM_DEPTH, as.integer(collection_depth_m_max),as.integer(max_cast_depth.m.)), .keep="unused") #as integer related error is okay. its turning one of those 
meld2<-meld2  %>% mutate(day_or_night= coalesce(LOCATION_CODE_4, day_night), .keep="unused")
meld2<-meld2 %>% mutate(volume= as.numeric(coalesce(V_in_m.3, sum_vol)))#WATER_VOLUME_FILTERED, was included but it masked the sum_vol and was thus left out. Reinclude this if returning to depth integrate data, .keep="unused") #still missing some values which were not in the original BM document
meld2<-meld2 %>%  mutate(volume=ifelse(sampling_method=="MOCNESS", ifelse(volume<sum_vol, sum_vol, volume), volume))#targets MOCNESS only, replaces volumes for each mocness net with depth integrated total volume for all nets in a given tow

#add moon date
larvah<-unite(meld2, "collection_date", c(collection_day, collection_month, collection_year), sep="/", remove=F)
larvah$collection_date<-lubridate::as_date(larvah$collection_date, format="%d/%m/%Y")
larvah<-mutate(larvah, "moon_date"=suncalc::getMoonIllumination(date = larvah$collection_date,keep = "phase"))

#add in gear/fishing style effect
larvah<-larvah %>% mutate(sampling_style=ifelse(sampling_method=="MOCNESS", 1, ifelse(sampling_method=="COBB TRAWL",2,3)))
larvah<-larvah %>% filter(cruise_number != "TC8505") #ensuring that tc8505 data are removed since no environmental data are available (see line 121 above)

#plots of volume filtered asit relates to by distance from shore#####
ggplot(larvah, aes(x=temp_at_colllection_depth, y=Depth_m_v21_50m))+geom_point()
ggplot(larvah, aes(x=temp_at_colllection_depth, y=(-1*max_depth)))+geom_point()
ggplot(larvah, aes(x=this_temp, y=(-1*max_depth)))+geom_point()

ggplot(larvah, aes(x=this_temp, y=Depth_m_v21_50m))+geom_point()
ggplot(larvah, aes(x=volume,fill=uku_present1_absent0))+geom_histogram()
ggplot(larvah, aes(y=log(volume), fill=uku_present1_absent0))+geom_bar()
ggplot(larvah, aes(y=log(Dist2Shore_m), x=log(volume), color=cruise_number))+geom_point()
ggplot(larvah, aes(y=log(Dist2Shore_m), x=log(volume), color=cruise_number,shape=as.factor(uku_present1_absent0)))+
  geom_point(size=4)
ggplot(larvah, aes(fill=as.factor(uku_present1_absent0),x=(Dist2Shore_m/1000), y=(log10(volume))))+
  geom_col(width=1, alpha=0.5)+scale_fill_manual(values=c("bisque4","darkgreen"))+
  labs(fill="Larvae present or absent", y=bquote("Log 10 volume of water filtered m"^"3"),x="Distance to shore (km)" )+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "horizontal", legend.box = "horizontal", legend.position = "bottom")


#statistcal modelling#####
#plots to test for correlation, generated with help of J.S. and J.W.
ggplot(larvah, aes(x=log(Dist2Shore_m), y=this_temp, color=cruise_number))+geom_point()
ggplot(larvah, aes(x=log(Dist2Shore_m), y=this_sal, color=cruise_number))+geom_point()
ggplot(larvah, aes(y=log(Dist2Shore_m), x=Near_Island, color=cruise_number))+geom_point()
ggplot(larvah, aes(x=this_temp, y=isle_region, color=cruise_number))+geom_point()
ggplot(larvah, aes(x=this_temp, y=this_sal, color=cruise_number))+geom_point(size=5)+ylim(34,36)+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=15),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical")+
  labs(x="surface temperature values (?C)", y="surface salinities", color="cruise identifier")


ggplot(larvah, aes(x=collection_month, y=this_sal, color=cruise_number))+geom_point()+ylim(34,36)
ggplot(larvah, aes(x=collection_month, y=this_temp, color=cruise_number))+geom_point()
ggplot(larvah, aes(y=min_depth, x=this_temp, color=cruise_number))+geom_point()
ggplot(larvah, aes(y=max_depth, x=this_temp, color=cruise_number))+geom_point()

ggplot(larvah, aes(y=log(Dist2Shore_m), x=log(volume), color=cruise_number))+geom_point()


#Generalized Additive Models###########
#these models were compared and informed the presentation of certain environmental factors as informative in larval uku distributions in the techincal memo
model1<-mgcv::gam(data=larvah, uku_present1_absent0~sampling_style+cruise_number+
                    s(this_sal, k=4)+
                    s(log(Dist2Shore_m),k=4)+
                    offset(log10(volume)),family=binomial) #for month could add bs="cc" creates cyclic spline to connect dots at the end; might need
mod1_param_no<-5
model2<-mgcv::gam(data=larvah, uku_present1_absent0~(sampling_style)+
                    cruise_number+
                    s(this_temp, k=4)+
                    #te(latitude_start_dd,longitude_start_dd)+
                    s((log(Dist2Shore_m)), k=4)+
                    offset(log10(volume)),family = binomial) #can temp explain the same or better than month and sal together
mod2_param_no<-5
model3<-mgcv::gam(data=larvah, uku_present1_absent0~sampling_style+
                    cruise_number+s(collection_month, k=4)+s((log(Dist2Shore_m)), k=4)+
                    offset(log(volume)),family = binomial) #can temp explain the same or better than month and sal together
mod3_param_no<-5
model4<-mgcv::gam(data=larvah, uku_present1_absent0~(sampling_style)+cruise_number+
                    s(this_temp, k=4)+
                    s(moon_date$phase, k=4)+
                    te(latitude_start_dd,longitude_start_dd)+
                    s((log(Dist2Shore_m)), k=4)+
                    offset(log10(volume)),family = binomial)
mod4_param_no<-7
model5<-mgcv::gam(data=larvah, uku_present1_absent0~(sampling_style)+
                    cruise_number+s(this_sal, k=4)+s(moon_date$phase, k=4)+
                    te(latitude_start_dd,longitude_start_dd)+
                    s((log(Dist2Shore_m)), k=4)+offset(log10(volume)),family = binomial) #can temp explain the same or better than month and sal together
mod5_param_no<-6
model6<-mgcv::gam(data=larvah, uku_present1_absent0~(sampling_style)+
                    cruise_number+
                    s(this_sal, k=4)+
                    te(latitude_start_dd,longitude_start_dd)+
                    s((log(Dist2Shore_m)), k=4)+
                    offset(log10(volume)),family = binomial) 
mod6_param_no<-6
model7<-mgcv::gam(data=larvah, uku_present1_absent0~(sampling_style)+
                    cruise_number+
                    s(this_temp, k=4)+
                    te(latitude_start_dd,longitude_start_dd)+
                    s((log(Dist2Shore_m)), k=4)+
                    offset(log10(volume)),family = binomial) 
mod7_param_no<-6
#compare all models with AIC
AIC(model1, model2, model3, model4, model5, model6, model7) 
#top two candidate models (assess with AICc)
AIC(model4, model7)
library(MuMIn)
AICc(model1, model2, model3, model4, model5, model6, model7)
m4<-vcov(model4)
m7<-vcov(model7)
#to also calculate AICc by hand as a sanity check, formula from Wikipedia
mc4<-86.98395+((2*(40^2)+(2*40))/(205-40-1))
mc7<-84.91197+((2*(37^2)+(2*37))/(205-37-1))

#plot that is in the Tech memo, pg13a#
plot.gam(model7,select=1, ylab="Conditional effect of temperature\n on uku occurrence", 
         xlab="Observed temperature (°C)",cex.axis=1.5, cex.lab=2, shade=F, trans=function(x)exp(x)/(1+exp(x)))

#model diagnostics
model<-model7
plot(model)
lerg=function(x)exp(x)/(1+exp(x))
par(mar=c(5,9,4,1)+1)
#for aic formula k= number of parameters

#further model assessments between models 4 and 7
library(visreg)
visreg(model, "this_temp", scale = "response", ylab = "", xlab = "this_temp (deg.C)", alpha=0.01, rug=1)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(mgcv)
library(tidymv)
plot(log(model$linear.predictors)~model$model$this_temp)
plot.gam(model7,select=1, ylab="Conditional effect of temperature\n on uku occurrence",
         xlab="Observed temperature (°C)",cex.axis=2.5, cex.lab=3, trans=plogis)


plot.gam(model4,select=1, ylab="Conditional effect of temperature on uku occurrence",
         xlab="Observed temperature (°C)",cex.axis=1, cex.lab=1, trans=plogis)
#get_smooths_difference(model)
model<-model4
summary(model) 
anova(model) 
qqnorm(resid(model))
gam.check(model)
hist(resid(model))
#model 1,2 suggestive of normal, need more binss?
plot(resid(model)~fitted(model))#model1 nice
plot(cooks.distance(model),type="h") 
#one concern is spatial autocorrelation of residuals
#simply plotted on a map to test
#map libraries:
library(sp) #already in raster
library(raster)# error
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
#map codes
#for all main hawaiian islands:
oahu_raster <- raster(file.path("inputs/all_hi_bathy.tiff"))
oahu_df <- fortify(as.bathy(oahu_raster))
str(oahu_df)
world<-ne_countries(scale="medium", returnclass = "sf")

oahu_map <- ggplot(data=world) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,0))+new_scale_fill()+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-154.9,-161.2), ylim=c(19, 22.2))

df2<-tibble(resid(model7))
larvahh<-filter(larvah,is.na(larvah$volume)==F)
larvahh<-filter(larvahh,is.na(larvahh$this_temp)==F)
larvahhh<-larvahh %>% mutate(model2_resids=add_column(df2))
larvahhh$latitude_start_dd <- unlist(larvahhh$latitude_start_dd)
larvahhh$longitude_start_dd <- unlist(larvahhh$longitude_start_dd)
larvahhh$model2_resids<-unlist(larvahhh$model2_resids)
model.assesment.colors <- colorRampPalette(c("blue", "white", "red"))
c<-model.assesment.colors(10)
oahu_map+geom_point(data=larvahhh, mapping=aes(y=latitude_start_dd,x=longitude_start_dd,
                                               color=model2_resids, size=3))+
  scale_color_gradientn(colours = c)+labs(color = "Model residuals")+
  theme(legend.direction = 'horizontal', legend.position = "bottom")

#oahu only:
str(oahu_df)
oahu_map <- ggplot() +
  coord_sf(crs = oahu_raster@crs, xlim=c(-157.5,-158.4),ylim=c(21.2,21.75))+
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,1000))+new_scale_fill()+
  theme(panel.background=element_blank(), 
        panel.grid.major.x=element_line())
oahualua<-filter(larvahhh, Near_Island=="Oahu")
oahalua<-filter(oahualua, longitude_start_dd!=(-157.91333333333))

oahu_map+geom_point(data=larvahhh, mapping=aes(y=latitude_start_dd,x=longitude_start_dd, color=model2_resids))+
  scale_color_gradientn(colours = c("blue","red"))
meld2oahu<-filter(meld2, Near_Island=="Oahu")

# to test univariate regressions*####
model1<-gam(larvah$uku_present1_absent0~s(larvah$this_temp),family=binomial)
model2<-gam(larvah$uku_present1_absent0~s(sqrt(larvah$this_temp)),family=binomial)
model3<-gam(data=larvah, uku_present1_absent0~(collection_month),family=binomial)
model4<-gam(larvah$uku_present1_absent0~s(larvah$Dist2Shore_m),family=binomial) 
model5<-gam(larvah$uku_present1_absent0~s(log(larvah$Dist2Shore_m)),family=binomial) 
model6<-gam(larvah$uku_present1_absent0~s(larvah$this_sal),family=binomial)
model7<-gam(larvah$uku_present1_absent0~larvah$sampling_style,family=binomial)
larvah<-larvah %>% mutate(cruise_factor=ifelse(cruise_number=="11_06", 11, ifelse(sampling_method=="12_06",12,ifelse(sampling_method=="32",13,ifelse(sampling_method=="TC8504",14,ifelse(sampling_method=="TC8602",15,16))))))
model8<-gam(larvah$uku_present1_absent0~larvah$cruise_factor,family=binomial)
model9<-gam(larvah$uku_present1_absent0~log(larvah$volume),family=binomial)
larvah<-larvah %>% mutate(dn=ifelse(day_or_night=="day",5,6))
model10<-gam(larvah$uku_present1_absent0~larvah$dn,family=binomial)
model11<-gam(larvah$uku_present1_absent0~s(larvah$moon_date$phase),family=binomial)
model12<-gam(larvah$uku_present1_absent0~te(larvah$latitude_start_dd,larvah$longitude_start_dd),family=binomial)

AIC(model1, model2, model3,  model4,  model5, model6, model7, model8, model9, model10, model11,model12) #model9
model<-model5
plot(model)
qqnorm(resid(model)) 
plot(resid(model)~fitted(model))
hist(resid(model))
plot(cooks.distance(model),type="h")
summary(model) 
anova(model)


#to test multivariate regressions##
#ultimately fruitless and not included in report, included here as a sanity check
model12<-glm(data=larvah, uku_present1_absent0~this_temp*moon_date$phase+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model13<-glm(data=larvah, uku_present1_absent0~dn*moon_date$phase+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model14<-glm(data=larvah, uku_present1_absent0~collection_month*moon_date$phase+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model15<-glm(data=larvah, uku_present1_absent0~log(Dist2Shore_m)*moon_date$phase+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model16<-glm(data=larvah, uku_present1_absent0~this_sal*moon_date$phase+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model17<-glm(data=larvah, uku_present1_absent0~this_sal*this_temp+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model18<-glm(data=larvah, uku_present1_absent0~log(Dist2Shore_m)*this_temp+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model19<-glm(data=larvah, uku_present1_absent0~log(Dist2Shore_m)*collection_month+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
AIC(model12, model13, model14, model15, model16, model17, model18, model19)
library(wiqid)
wiqid::AICc(model12, model13, model14, model15, model16, model17, model18, model19)
model<-model19
summary(model) 
anova(model)
qqnorm(resid(model)) 
plot(resid(model)~fitted(model))
hist(resid(model))
plot(cooks.distance(model),type="h")
plot(model)



###Sampling Spread Map######
###=---Cam's map---
library(sp) #already in raster
library(raster)# error
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
world<-ne_countries(scale="medium", returnclass = "sf")
oahu_raster <- raster(file.path("inputs/all_hi_bathy.tiff"))
oahu_df <- fortify(as.bathy(oahu_raster))
str(oahu_df)
oahu_map <- ggplot(data=world) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,1000))+new_scale_fill()+
  scale_fill_continuous(labels = scales::label_number(scale = 1000, suffix = "k"))+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-154.9,-161.2), ylim=c(19, 22.2))


oahu_map
shapes<-c(1,18)
library(tidyverse)
ga<-pa %>% filter(is.na(cruise_number)==F)
ga<-ga %>% filter(cruise_number!="")

oahu_map+geom_point(data=larvah, mapping=aes(y=latitude_start_dd,
                                             x=longitude_start_dd, 
                                             color=cruise_number,
                                             size=5,
                                             shape=as.factor(uku_present1_absent0)))+
  scale_color_viridis_d(option="B")+scale_shape_manual(values=shapes)+
  theme(axis.title = element_blank(),legend.position = "bottom",legend.box = "horizonal",
        axis.ticks = element_blank(),axis.text = element_blank(),
        legend.title=element_text(size=16),legend.text=element_text(size=20))+
  labs(color="Sampling Effort by Cruise Identifier", shape="Larva present (1) or absent(0)", size=element_blank())+
  guides(color = guide_legend(override.aes = list(size = 5), order=1),
         shape=guide_legend(override.aes = list(size = 5), order=2),
         fill= guide_colorbar(barwidth = 20, barheight = 10, order=3),
         size="none")



#####oahu inset####
str(oahu_df)
oahu_map <- ggplot(data=world)+
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,1000))+new_scale_fill()+
  theme(panel.background=element_blank(), panel.grid.major.x=element_line())+
  theme_bw()+geom_sf()+coord_sf(crs = oahu_raster@crs, xlim=c(-157.5,-158.4),ylim=c(21.2,21.75))
oahualua<-filter(larvah, Near_Island=="Oahu")
oahualua<-filter(oahualua, cruise_number!="")
oahalua<-filter(oahualua, longitude_start_dd!=(-157.91333333333))

oahu_map+geom_point(data=oahualua, mapping=aes(y=latitude_start_dd,
                                               x=longitude_start_dd, 
                                               color=cruise_number, 
                                               size=3,
                                               shape=as.factor(uku_present1_absent0)))+
  scale_color_viridis_d(option="B")+scale_shape_manual(values=shapes)+
  theme(legend.position = "bottom",axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_blank(),legend.box = "vertical")+
  labs(color="Sampling Effort by Cruise or Study Identifier", shape="Larva present (1) or absent(0)",
       size="")

#Sat. SST by year map####
#this code was from Coast Watch course taught by Melanie Abacassis, Tom Oliver and Johanna Wren in early 2022
###actual pa points#####
library(tidyverse)
library(lattice)
pa<-mutate(larvah, long_360=longitude_start_dd+0) #originally said +360, changed to 0 without changing names since there are dependencies throughout the script
p<-filter(pa, uku_present1_absent0==1)
a<-filter(pa, uku_present1_absent0==0)
uku_p_lon<-as.vector(p$long_360)
uku_p_lat<-as.vector(p$latitude_start_dd)
uku_a_lon<-as.vector(a$long_360)
uku_a_lat<-as.vector(a$latitude_start_dd)

### extract data####
library(ncdf4)
library(httr)
library(tidyverse)
#crs=oahu_raster@crs,xlim=c(-154.9,-161.2), ylim=c(19, 22.2)
#pull data of SST for June-October
#show P/A for the year in question ONLY
#Because ERDDAP includes RESTful services, you can download data listed on any ERDDAP platform from R using the URL structure.
#this is daily SST for september 1985 (starts 1 month before 1st uku collection 8/6/1985)
#adjusted to Oahualua only xlim=c(-157.5==202.5,-158.4==201.6),ylim=c(21.2,21.75))
junk <- GET('https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.nc?analysed_sst%5B(1985-06-06T12:00:00Z):1:(1985-10-30T12:00:00Z)%5D%5B(21.2):1:(21.75)%5D%5B(201.5):1:(202.5)%5D',
            write_disk("sst985.nc", overwrite=TRUE))
#this is daily SST for 1986 adjusted to Oahulaua only
junk2<-GET('https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.nc?analysed_sst%5B(1986-06-06T12:00:00Z):1:(1986-10-30T12:00:00Z)%5D%5B(21.2):1:(21.75)%5D%5B(201.5):1:(202.5)%5D',
           write_disk("sst686.nc", overwrite=TRUE))
#this is daily SST for 2011
junk3 <- GET('https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.nc?analysed_sst%5B(2011-06-06T12:00:00Z):1:(2011-10-30T12:00:00Z)%5D%5B(18.025):1:(23.025)%5D%5B(198.025):1:(206.025)%5D',
             write_disk("sst11.nc", overwrite=TRUE))
#this is daily SST for 2012
junk4<-GET('https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.nc?analysed_sst%5B(2012-06-06T12:00:00Z):1:(2012-10-30T12:00:00Z)%5D%5B(18.025):1:(23.025)%5D%5B(198.025):1:(206.025)%5D',
           write_disk("sst12.nc", overwrite=TRUE))

nc <- nc_open('sst985.nc')
nc2 <- nc_open('sst686.nc')
names(nc$var)
names(nc2$var)
nc3 <- nc_open('sst11.nc')
nc4 <- nc_open('sst12.nc')
names(nc3$var)
names(nc4$var)

# ###set up each variable####
v1 <- nc$var[[1]] #list of variables??
v2 <- nc2$var[[1]] #list of variables??
v3 <- nc3$var[[1]] #list of variables??
v4 <- nc4$var[[1]] #list of variables??
sst985 <- ncvar_get(nc,v1) #Extract analysed_sst, reads data from the netCDF file, only works if you have already opened the file, shows as a multi-dimensional array
sst686 <- ncvar_get(nc2,v2) #reads data from the netCDF file, only works if you have already opened the file, shows as a multi-dimensional array
sst11 <- ncvar_get(nc3,v3) #Extract analysed_sst, reads data from the netCDF file, only works if you have already opened the file, shows as a multi-dimensional array
sst12 <- ncvar_get(nc4,v4) #reads data from the netCDF file, only works if you have already opened the file, shows as a multi-dimensional array

dim(sst985)#examines the structure of sst, this has between 101-146 time points
dim(sst686)#examines the structure of sst, this has  101-146 time points
dim(sst11)#examines the structure of sst, this has  101-146 time points
dim(sst12)#examines the structure of sst, this has  101-146 time points


dates985 <- as.POSIXlt(v1$dim[[3]]$vals,origin='1970-01-01',tz='GMT') #get the dates for each time step
dates686<- as.POSIXlt(v2$dim[[3]]$vals,origin='1970-01-01',tz='GMT') #get the dates for each time step
dates11 <- as.POSIXlt(v3$dim[[3]]$vals,origin='1970-01-01',tz='GMT') #get the dates for each time step
dates12<- as.POSIXlt(v4$dim[[3]]$vals,origin='1970-01-01',tz='GMT') #get the dates for each time step

lon <- v1$dim[[1]]$vals #gives vector of longitude
lon=lon-360
lat <- v1$dim[[2]]$vals #gives vector of latitude
lon2 <- v2$dim[[1]]$vals #gives vector of longitude
lon2=lon2-360
lat2 <- v2$dim[[2]]$vals #gives vector of latitude
lon3 <- v3$dim[[1]]$vals #gives vector of longitude
lon3=lon3-360
lat3 <- v3$dim[[2]]$vals #gives vector of latitude
lon4 <- v4$dim[[1]]$vals #gives vector of longitude
lon4=lon4-360
lat4 <- v4$dim[[2]]$vals #gives vector of latitude

#Close the netcdf file and remove the data and files that are not needed anymore.
nc_close(nc) #this step is important, otherwise you risk data loss
rm(junk,v1)
file.remove('sst985.nc')

nc_close(nc2) #this step is important, otherwise you risk data loss
rm(junk2,v2)
file.remove('sst686.nc')

nc_close(nc3) #this step is important, otherwise you risk data loss
rm(junk3,v3)
file.remove('sst11.nc')

nc_close(nc4) #this step is important, otherwise you risk data loss
rm(junk4,v4)
file.remove('sst12.nc')


####wrangling all years####
df85<-sst985[,,mean(1:146)] #okay so basically set row and columns names as lat long THEN pivot longer so these all become their own columns???
rownames(df85, do.NULL = TRUE, prefix = "row")
rownames(df85) <- lon
colnames(df85, do.NULL = TRUE, prefix = "col")
colnames(df85) <-lat
df85real<-as.data.frame(as.table(df85))
df85real<-cbind(df85real, year=1985)
#df85real %>% df85real <-plyr::rename(c("x"=Var1,"y"=Var2, "temperature"=Freq))

df86<-sst686[,,mean(1:146)] #okay so basically set row and columns names as lat long THEN pivot longer so these all become their own columns???
rownames(df86, do.NULL = TRUE, prefix = "row")
rownames(df86) <- lon2
colnames(df86, do.NULL = TRUE, prefix = "col")
colnames(df86) <-lat2
df86real<-as.data.frame(as.table(df86))
df86real<-cbind(df86real, year=1986)

df11<-sst11[,,mean(1:146)] #okay so basically set row and columns names as lat long THEN pivot longer so these all become their own columns???
rownames(df11, do.NULL = TRUE, prefix = "row")
rownames(df11) <- lon3
colnames(df11, do.NULL = TRUE, prefix = "col")
colnames(df11) <-lat3
df11real<-as.data.frame(as.table(df11))
df11real<-cbind(df11real, year=2011)

df12<-sst12[,,mean(1:146)] #okay so basically set row and columns names as lat long THEN pivot longer so these all become their own columns???
rownames(df12, do.NULL = TRUE, prefix = "row")
rownames(df12) <- lon4
colnames(df12, do.NULL = TRUE, prefix = "col")
colnames(df12) <-lat4
df12real<-as.data.frame(as.table(df12))
df12real<-cbind(df12real, year=2012)

eighties_sst<-rbind(df85real,df86real)
tens_sst<-rbind(df11real, df12real)
#added 2023
all_sst<-rbind(df85real,df86real,df11real, df12real)
all_sst<-mutate(all_sst, x=as.numeric(levels(Var1))[Var1])
all_sst<-mutate(all_sst, y=as.numeric(levels(Var2))[Var2])
####
eighties_sst<-mutate(eighties_sst, x=as.numeric(levels(Var1))[Var1])
eighties_sst<-mutate(eighties_sst, y=as.numeric(levels(Var2))[Var2])
tens_sst<-mutate(tens_sst, x=as.numeric(levels(Var1))[Var1])
tens_sst<-mutate(tens_sst, y=as.numeric(levels(Var2))[Var2])

shapes<-c(1,18)
larv80<-filter(larvah, collection_year %in% (1984:1987))
larv10<-filter(larvah, collection_year %in% (2010:2013))  
#all years with ggplot#####
library(sp) #already in raster
library(raster)# error
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
world<-ne_countries(scale="medium", returnclass = "sf")

eighties<-ggplot(data=world)+geom_raster(data=eighties_sst,aes(x=x,y=y, fill=Freq))+facet_grid(~year)+
  scale_fill_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray90")+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),
        axis.text = element_text(size=10), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",strip.text.x=element_text(size=20))+
  labs(fill="SST (?C)")+
  theme_bw()+geom_sf()+coord_sf(crs = oahu_raster@crs, xlim=c(-157.5,-158.4),ylim=c(21.2,21.75))

eighties
eighties<-eighties+geom_point(data=larv80, mapping=aes(y=latitude_start_dd,
                                                       x=longitude_start_dd, 
                                                       size=2,
                                                       shape=as.factor(uku_present1_absent0)))+
  guides(size="none")+labs(shape="Larvae present\nor absent", x="Longitude", y="Latitude")+
  scale_shape_manual(values=shapes)


tens<-ggplot(data=world)+geom_raster(data=tens_sst,aes(x=x,y=y, fill=Freq))+facet_grid(~year)+
  scale_fill_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray90")+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",strip.text.y =element_text(size=20))+
  theme_bw()+
  labs(fill="SST (?C)")+
  geom_sf()+coord_sf(crs=oahu_raster@crs,xlim=c(-154.9,-161.2), ylim=c(19, 22.2))
tens
ten<-tens+geom_point(data=larv10, mapping=aes(y=latitude_start_dd,
                                              x=longitude_start_dd, 
                                              size=2,
                                              shape=as.factor(uku_present1_absent0)))+
  guides(size="none")+ labs(shape="Larvae present\nor absent", x="Longitude", y="Latitude")+
  scale_shape_manual(values=shapes)

library(patchwork)
eighties/ten+plot_annotation(tag_levels=c("A","B","C","D"))


####added 2023
allst<-ggplot(data=world)+geom_raster(data=all_sst,aes(x=x,y=y, fill=Freq))+facet_wrap(~year)+
  scale_fill_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray90")+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",strip.text.y =element_text(size=20))+
  theme_bw()+
  labs(fill="SST (?C)")+
  geom_sf()+coord_sf(crs=oahu_raster@crs,xlim=c(-154.9,-161.2), ylim=c(19, 22.2))
allst
####

#####1985:Working with the extracted data####
#run start
###Creating a map for one time step in September 1985
#[scale.R] needs to be in working directory
#set some color breaks
h <- hist(sst985[,,55], plot=T) #mean for 2nd to last time point
# #breaks <- h$breaks
breaks<-seq(from=24,to=28, length.out=100) #length out= number of color bands, need either by= or length.out, not both
n <- length(breaks)-1
# #define a color palette
jet.colors <- colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

#set color scale using the jet.colors palette
c <- jet.colors(n)
#use this  c<-gray.colors(n=n) if you want it in grayscale

#c<-scale_color_viridis_c(start=0, end=length(n))
sst985[,,mean(1:146)]
#
#prepare graphic window : left side for map, right side for color scale
layout(matrix(c(1,2,3,0,4,0), nrow=1, ncol=2), widths=c(5,1), heights=4)
layout.show(2)
par(mar=c(3,3,3,1))


image(lon,lat,sst985[,,mean(1:146)],xlab='',ylab,col=c,breaks=breaks,
      axes=TRUE,xaxs='i',yaxs='i',asp=1, main=paste("Mean SST", dates985[1],"to",dates985[146]))

#
# #example of how to add points to the map
ef<-filter(pa, collection_year==1985)
p<-filter(ef, uku_present1_absent0==1)
a<-filter(ef, uku_present1_absent0==0)
uku_p_lon<-as.vector(p$long_360)
uku_p_lat<-as.vector(p$latitude_start_dd)
uku_a_lon<-as.vector(a$long_360)
uku_a_lat<-as.vector(a$latitude_start_dd)
points(uku_p_lon,uku_p_lat, pch=18, cex=2) #uku present
points(uku_a_lon,uku_a_lat, pch=1, cex=2) #uku absent
# #example of how to add a contour (this is considered a new plot, not a feature, so you need to use par(new=TRUE)) to overlay it on top of the SST map
par(new=TRUE)
contour(lon,lat,sst985[,,1],levels=20,xaxs='i',yaxs='i',labcex=20,vfont = c("sans serif", "bold"),axes=FALSE,asp=1)
#
#plot color scale using 'image.scale' function from 'scale.R' script)
par(mar=c(3,1,3,3))
source('~/all_scripts_R/scale.R')
image.scale(sst985[,,55], col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='SST', cex=4)
axis(4, las=1)
box()
# #run end
# #####1986:Working with the extracted data####
image(lon,lat,sst686[,,mean(1:146)],xlab='',ylab,col=c,breaks=breaks,
      axes=TRUE,xaxs='i',yaxs='i',asp=1, main=paste("Mean SST", dates686[1],"to",dates686[146]))
#
# #example of how to add points to the map
es<-filter(pa, collection_year==1986)
p<-filter(es, uku_present1_absent0==1)
a<-filter(es, uku_present1_absent0==0)
uku_p_lon<-as.vector(p$long_360)
uku_p_lat<-as.vector(p$latitude_start_dd)
uku_a_lon<-as.vector(a$long_360)
uku_a_lat<-as.vector(a$latitude_start_dd)
points(uku_p_lon,uku_p_lat, pch=18, cex=2) #uku present
points(uku_a_lon,uku_a_lat, pch=1, cex=2) #uku absent
#example of how to add a contour (this is considered a new plot, not a feature, so you need to use par(new=TRUE)) to overlay it on top of the SST map
par(new=TRUE)
contour(lon,lat,sst686[,,30],levels=20,xaxs='i',yaxs='i',labcex=0.8,vfont = c("sans serif", "bold"),axes=FALSE,asp=1)
#
#plot color scale using 'image.scale' function from 'scale.R' script)
par(mar=c(3,1,3,3))
source('~/all_scripts_R/scale.R')

image.scale(sst686[,,30], col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='SST')
axis(4, las=1)
box()
# #run end
# #####2011:Working with the extracted data####
# #run start
h <- hist(sst11[,,55], plot=T) #mean for 2nd to last time point
#breaks <- h$breaks
breaks<-seq(from=24,to=28, length.out=100) #length out= number of color bands, need either by= or length.out, not both
n <- length(breaks)-1

#define a color palette
jet.colors <- colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# #set color scale using the jet.colors palette
c <- jet.colors(n)
#c<-scale_color_viridis_c(start=0, end=length(n))
sst11[,,mean(1:146)]
#
# #prepare graphic window : left side for map, right side for color scale
layout(matrix(c(1,2,3,0,4,0), nrow=1, ncol=2), widths=c(5,1), heights=4)
layout.show(2)
par(mar=c(3,3,3,1))
image(lon3,lat3,sst11[,,mean(1:146)],xlab='',ylab,col=c,breaks=breaks,
      axes=TRUE,xaxs='i',yaxs='i',asp=1, main=paste("Mean SST", dates11[1],"to",dates11[146]))

#example of how to add points to the map
el<-filter(pa, collection_year==2011)
p<-filter(el, uku_present1_absent0==1)
a<-filter(el, uku_present1_absent0==0)
uku_p_lon<-as.vector(p$long_360)
uku_p_lat<-as.vector(p$latitude_start_dd)
uku_a_lon<-as.vector(a$long_360)
uku_a_lat<-as.vector(a$latitude_start_dd)
points(uku_p_lon,uku_p_lat, pch=18, cex=2) #uku present
points(uku_a_lon,uku_a_lat, pch=1, cex=2) #uku absent
#example of how to add a contour (this is considered a new plot, not a feature, so you need to use par(new=TRUE)) to overlay it on top of the SST map
par(new=TRUE)
contour(lon3,lat3,(sst11[,,mean(1:146)]),levels=20,xaxs='i',yaxs='i',labcex=0.8,vfont = c("sans serif", "bold"),axes=FALSE,asp=1)
#
# #plot color scale using 'image.scale' function from 'scale.R' script)
par(mar=c(3,1,3,3))
source('~/all_scripts_R/scale.R')
image.scale(sst11[,,mean(1:146)], col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='SST')
axis(4, las=1)
box()
# #run end
# #####2012:Working with the extracted data####
# #run start
# ###Creating a map for one time step in june-october 2012
#[scale.R] needs to be in working directory
# #set some color breaks
h <- hist(sst12[,,mean(1:146)], plot=T) #mean for 2nd to last time point
# #breaks <- h$breaks
breaks<-seq(from=24,to=28, length.out=100) #length out= number of color bands, need either by= or length.out, not both
n <- length(breaks)-1
#
#define a color palette
jet.colors <- colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#
# #set color scale using the jet.colors palette
c <- jet.colors(n)
#c<-gray.colors(n=n) # for grayscale for tech memmo
# #c<-scale_color_viridis_c(start=0, end=length(n))
#
#prepare graphic window : left side for map, right side for color scale
layout(matrix(c(1,2,3,0,4,0), nrow=1, ncol=2), widths=c(5,1), heights=4)
layout.show(2)
par(mar=c(3,3,3,1))
image(lon4,lat4,sst12[,,mean(1:146)],xlab='',ylab,col=c,breaks=breaks,
      axes=TRUE,xaxs='i',yaxs='i',asp=1, main=paste("Mean SST", dates12[1],"to",dates12[146]))

#example of how to add points to the map
tw<-filter(pa, collection_year==2012)
p<-filter(tw, uku_present1_absent0==1)
a<-filter(tw, uku_present1_absent0==0)
uku_p_lon<-as.vector(p$long_360)
uku_p_lat<-as.vector(p$latitude_start_dd)
uku_a_lon<-as.vector(a$long_360)
uku_a_lat<-as.vector(a$latitude_start_dd)
points(uku_p_lon,uku_p_lat, pch=18, cex=2) #uku present
points(uku_a_lon,uku_a_lat, pch=1, cex=2) #uku absent
#example of how to add a contour (this is considered a new plot, not a feature, so you need to use par(new=TRUE)) to overlay it on top of the SST map
par(new=TRUE)
contour(lon4,lat4,sst12[,,1],levels=20,xaxs='i',yaxs='i',labcex=0.8,vfont = c("sans serif", "bold"),axes=FALSE,asp=1)
#
# #plot color scale using 'image.scale' function from 'scale.R' script)
par(mar=c(3,1,3,3))
source('~/all_scripts_R/scale.R')
image.scale(sst11[,,mean(1:146)], col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='SST')
axis(4, las=1)
box()
# #run end
#
#
#
#

#CTD Temp by Month#####
####map####
oahu_map
#points
larvahh<-larvah%>% mutate(uku_pa=as.factor(uku_present1_absent0))
larvahh<-rename(larvahh, max=max_temp)
larvahh<-rename(larvahh, min=min_temp)
larvahh<-pivot_longer(larvahh, cols=c(max, min),names_to="min_or_max", values_to = "temp_range")
larvahh<-mutate(larvahh, uku_density= (number_of_uku_per_Vm.3/V_in_m.3))
larvahh<-mutate(larvahh, length_mm=as.numeric(length_mm))
larvahh<-mutate(larvahh, latitude_start_dd=as.numeric(latitude_start_dd))
larvahh<-mutate(larvahh, longitude_start_dd=as.numeric(longitude_start_dd))
larvahh<-mutate(larvahh, as.factor(sampling_method))
larvahh<-mutate(larvahh, collection_month=as.numeric(collection_month))
larvahh<-mutate(larvahh, collection_year=as.numeric(collection_year))
larvahh$spawn_peak<-6
larvahh<-larvahh %>%mutate(time_since_peak_spawn=collection_month-spawn_peak)
p<-filter(larvahh, uku_pa==1)
a<-filter(larvahh, uku_pa==0)
#pall<-distinct(larvah,tow_id,.keep_all = TRUE) 
larvahh<-distinct(larvahh,tow_id,.keep_all = TRUE) 
shapes<-c(1,18)
#define a color palette
jet.colors <- colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
c <- jet.colors(n)
larvah<-larvah %>% mutate(named_month=month.name[collection_month])
#larvah<-larvah %>% mutate(named_month=(factor(levels=c('April','June','July','August','September',"October"))))
library(sp) #already in raster
library(raster)# error
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
world<-ne_countries(scale="medium", returnclass = "sf")
oahu_raster <- raster(file.path("inputs/all_hi_bathy.tiff"))
oahu_df <- fortify(as.bathy(oahu_raster))
str(oahu_df)
mhi_map <- ggplot(data=world) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,1000))+new_scale_fill()+
  scale_fill_continuous(labels = scales::label_number(scale = 1000, suffix = "k"))+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-154.9,-161.2), ylim=c(19, 22.2))+
  guides(fill="none")

p1<-mhi_map+geom_point(data=larvah, mapping=aes(y=latitude_start_dd,
                                                x=longitude_start_dd, 
                                                color=this_temp, 
                                                size=3,
                                                shape=as.factor(uku_present1_absent0)))+
  scale_shape_manual(values=shapes)+
  facet_wrap(~factor(named_month, levels=c('April','June','July','August','September',"October")), ncol=2)+
  theme(legend.position = "right",axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_text(size=10),legend.title= element_text(size=20),
        legend.text= element_text(size=15),legend.box = "vertical",
        strip.text.x=element_text(size=20),plot.margin = margin(0.1,0.1,0.1,0.1, "inches"))+
  labs(color="Temperature (?C)", shape="Larvae\npresent\nor absent")+
  scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 90")+
  #guides(size="none",shape="none", color="none", fill="none")
  guides(shape=guide_legend(override.aes = list(size = 10), order=2),size="none")
p1
ggsave("p1.png", height =5, width =6.5, units = "in")

#oahu_inset

oahu_map <- ggplot(data=world)+
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,1000))+new_scale_fill()+
  theme(panel.background=element_blank(), panel.grid.major.x=element_line())+guides(fill="none")+
  theme_bw()+geom_sf()+coord_sf(crs = oahu_raster@crs, xlim=c(-157.5,-158.4),ylim=c(21.2,21.75))
oahualua<-filter(larvah, Near_Island=="Oahu")
oahualua<-filter(oahualua, cruise_number!="")
oahalua<-filter(oahualua, longitude_start_dd!=(-157.91333333333))

inset<-oahu_map+geom_point(data=larvah, mapping=aes(y=latitude_start_dd,
                                                    x=longitude_start_dd, 
                                                    color=this_temp, 
                                                    size=2,
                                                    shape=as.factor(uku_present1_absent0)))+
  scale_shape_manual(values=shapes)+
  facet_wrap(~factor(named_month, levels=c('April','June','July','August','September',"October")), ncol=3)+
  theme(legend.position = "none",axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_blank(),legend.box = "horizonal")+
  labs(color="Temperature (?C)", shape="Larvae\npresent\nor absent")+
  #scale_color_viridis(option="magma") #switched out 9/30 to allow for graysclae
  scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 90")+
  theme(legend.position = "right",axis.title = element_blank(),plot.margin = margin(0.1,0.1,0.1,0.1, "inches"),
        axis.ticks = element_blank(),axis.text = element_text(size=10),legend.title= element_text(size=20),
        legend.text= element_text(size=15),legend.box = "vertical",
        strip.text.x=element_text(size=20))+guides(size="none",shape="none", color="none", fill="none")#=guide_legend(override.aes = list(size = 10), order=2))
inset

library(patchwork)
p1/inset+plot_annotation(tag_levels="A")

library(gtable)
library(cowplot)
#w
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}
plot_grid(shift_legend(p1))

####salinity####
oahu_map+geom_point(data=larvah, mapping=aes(y=latitude_start_dd,
                                             x=longitude_start_dd, 
                                             color=sal_at_depth, 
                                             size=2,
                                             shape=as.factor(uku_present1_absent0)))+
  scale_shape_manual(values=shapes)+
  theme(legend.position = "bottom",axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_blank(),legend.box = "horizontal")+
  labs(color="Salinity", shape="Larva present (1) or absent(0)")+
  scale_color_gradientn(colors=c("lightblue1","royalblue2","midnightblue"),na.value="gray 90")

####oahu inset####
oahu_map <- ggplot() +
  coord_sf(crs = oahu_raster@crs, xlim=c(-157.5,-158.7),ylim=c(21.2,21.75))+
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradientn(colours = c(grey(50/100), grey(80/100)),values = rescale(c(min(oahu_df$z, na.rm = TRUE), 0,  0.0001, max(oahu_df$z, na.rm = TRUE))), na.value = "transparent",guide = "none")+
  theme(panel.background=element_blank(), 
        panel.grid.major.x=element_line())
oahualua<-filter(pa, Near_Island=="Oahu")
oahualua<-filter(oahualua, cruise_number!="")
labell = "White color indicates NA values for temperature"
p2<-oahu_map+geom_point(data=maxt, mapping=aes(y=latitude_start_dd,
                                               x=longitude_start_dd, 
                                               color=temp_range, 
                                               size=2,
                                               shape=as.factor(uku_present1_absent0)))+
  scale_shape_manual(values=shapes)+
  theme(legend.position = "bottom",axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_blank(),legend.box = "horizontal")+
  labs(color="Temperature", shape="Larva present (1) or absent(0)")+
  facet_wrap(~collection_month)+
  scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 90")
p2
plot_grid(shift_legend(p2))

#Depth by family######
#depth as a univariate model###
model<-glm(mb$uku_pa~(mb$MAXIMUM_DEPTH),family=binomial)
plot(model)
qqnorm(resid(model)) 
plot(resid(model)~fitted(model))
hist(resid(model))
plot(cooks.distance(model),type="h")
summary(model) 
anova(model)

library(tidyverse)
mb<-read.csv("inputs/cast_data_with_coord_updated_DD.csv") #this is the MB data from inport with the coordinates that I added based on the station number from the PDFs in the data parcel from Inport
str(mb) #this is the framework to which 8504 and 8604 data will be appended
summary(mb$CRUISE_NUMBER)
mb$cruise_id<-gsub("_","",mb$cruise_id)
mb$tow_id<-gsub("TC_8","TC8",mb$tow_id)
mb<-mb %>% unite(col="moc_id",cruise_id, STATION_NUMBER,net_number, sep="_", remove=F) 
#this creates a consistent moc and cruise id which can be used across sheets
mb$moc_id
mb<- mb %>%unite(col="tow_id",cruise_id, STATION_NUMBER, sep="_", remove=F)
mb<-mb%>%mutate(uku_pa=ifelse(mb$SPECIES_CODE=="8554380401",1,0))
lutj<-filter(mb, grepl("8554380",SPECIES_CODE))
lutj<-lutj %>% mutate(spp_name=as.factor(ifelse(lutj$SPECIES_CODE==8554380401, "Aprion virescens",
                                                ifelse(lutj$SPECIES_CODE==8554380500, "Pristipomoides spp.",
                                                       ifelse(lutj$SPECIES_CODE== 8554380600, "Etelis spp.",
                                                              ifelse(lutj$SPECIES_CODE==8554380700, "Lutjanus spp.",
                                                                     ifelse(lutj$SPECIES_CODE==8554380705, "Lutjanus kasmira",
                                                                            ifelse(lutj$SPECIES_CODE==8554380100, "Symphysanodon spp.",
                                                                                   "Lutjanidae"))))))))
lutj<-lutj %>% mutate(MAX_D=as.numeric(MAXIMUM_DEPTH))
lutj<-lutj %>% mutate(CRUISE=as.numeric(CRUISE_NUMBER))
lutj<-lutj %>% mutate(fish_dens=(NUMBER_SPECIES/SAMPLE_VOLUME))
mlutj<-lutj %>%
  group_by(spp_name,MAXIMUM_DEPTH) %>%
  summarise(across(fish_dens, mean))
slutj<-lutj %>%
  group_by(spp_name,MAXIMUM_DEPTH,CRUISE_NUMBER) %>%
  summarise(across(fish_dens, mean))

nlutj<- lutj %>%
  mutate(day_night=ifelse(LOCATION_CODE_4==1,"day","night"))
nlutj<-nlutj %>%
  dplyr::group_by(spp_name,day_night,MAXIMUM_DEPTH) %>%
  dplyr::summarise(across(fish_dens, mean, .groups=c(spp_name,day_night,MAXIMUM_DEPTH)))

nlutj<-nlutj %>%
  dplyr::group_by(spp_name,day_night,MAXIMUM_DEPTH) %>%
  dplyr::summarise(across(fish_dens, mean, .groups=c(spp_name,day_night,MAXIMUM_DEPTH)))

mlutjnosymp<-filter(nlutj, spp_name!="Symphysanodon spp.")

fi10<-mlutjnosymp %>%
  dplyr::group_by(spp_name,MAXIMUM_DEPTH) %>%
  dplyr::summarise(across(fish_dens, mean, .groups=c(spp_name,MAXIMUM_DEPTH)))

slutj<- slutj %>% mutate(CRUISE_month=ifelse(CRUISE_NUMBER=="8504", "September", ifelse(CRUISE_NUMBER=="8505", "December", ifelse(CRUISE_NUMBER=="8602", "April", "June"))))
ld<-ggplot(mlutjnosymp,aes(color=as.factor(spp_name),y=as.factor(MAXIMUM_DEPTH*-1),x=fish_dens))+
  geom_point(size=5)+facet_grid(vars(CRUISE_month))+scale_color_viridis_d()+labs(y="Maximum net depth (m)", x="Specimen density per sample volume",color='Species name',strip="Cruise Number")
plot1<-ld+geom_path(aes(group=as.factor(spp_name),y=as.factor(MAXIMUM_DEPTH*-1),x=fish_dens))+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=15),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",plot.title=element_text(face="italic",size=30))+
  guides(colour = "none")
plot1


#make new df for all species and all depths and then merge this existing df into it and have NA/s blanks fill with fish dens0
spp_name<-as_vector(list(rep(levels(mlutjnosymp$spp_name),9)))
MAXIMUM_DEPTH<-as_vector(list(rep(c(1,10,20, 30,40,50,60,70,80),each=7)))

with0s<-as.data.frame(cbind(spp_name,MAXIMUM_DEPTH))
with0s$MAXIMUM_DEPTH=as.numeric(with0s$MAXIMUM_DEPTH)
df<-left_join(with0s,fi10)
df<-df %>% mutate(fish_dens=ifelse((is.na(fish_dens)==F), df$fish_dens,0)) 
df<- df %>% filter(spp_name != "Symphysanodon spp.")
figure10<-ggplot(df,aes(y=(MAXIMUM_DEPTH*-1),x=fish_dens))+
  geom_point(size=5)+facet_wrap(vars(spp_name), ncol=3)+#scale_color_viridis_d()+
  geom_path(aes(group=as.factor(spp_name),y=MAXIMUM_DEPTH*-1,x=fish_dens))+
  labs(y="Maximum net depth (m)", x=expression("Specimen density per sample volume (individual/m"^3~")"))+ #"[W" ~ m^-2~"]"))
  theme(legend.title=element_text(size=15),legend.text=element_text(size=15),
        axis.text = element_text(size=10), axis.title = element_text(size=15),
        legend.direction = "vertical", legend.box = "vertical",strip.text.x=element_text(face="italic",size=15))+
  scale_x_continuous(breaks=seq(0, max(df$fish_dens)+0.5, 0.5))
figure10



library(gtable)
library(cowplot)
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}
plot_grid(shift_legend(plot2))


library(patchwork)
plot1 + plot2

ggplot(nlutj,aes(color=spp_name,y=(MAXIMUM_DEPTH*-1),x=fish_dens))+
  geom_point(alpha=0.3,size=3)+facet_grid(~day_night)+scale_color_viridis_d()+
  geom_path(aes(group=as.factor(spp_name),y=MAXIMUM_DEPTH*-1,x=fish_dens))+
  labs(y="Maximum net depth (m)", x="Specimen density per sample volume",color='Species name',strip="Cruise Number")

figure9<-ggplot()+geom_density(data=mlutjnosymp,aes(color=day_night,  y=MAXIMUM_DEPTH*-1, size=1))+
  scale_color_manual(values=c("cornflowerblue","darkblue"))+facet_wrap(~spp_name, ncol=4)+
  labs(x="Kernel density", y="Maximum net depth (m)",color='Time caught',strip="Cruise Number")+
  theme(legend.title=element_text(size=15),legend.text=element_text(size=15),
        axis.text = element_text(size=10), axis.title = element_text(size=15),
        legend.direction = "vertical", legend.box = "vertical",
        strip.text.x=element_text(face="italic",size=15))+
  guides(size="none", color = guide_legend(override.aes = list(fill = c("cornflowerblue","darkblue"))))

figure9
fig9<-plot_grid(shift_legend(figure9))
fig9
library(patchwork)
#report figs9 and 10######
layout <-
  "
AAA
AAA
BBB
BBB
"
pg16<-fig9 / figure10+ plot_layout(design = layout)+plot_annotation(tag_levels="A") &
  theme(plot.tag = element_text(size =15))
pg16
#ggsave("pg16.png", width = 6, height = 9, units = "in")

#######
pp_hist<-ggplot(mlutjnosym,  aes(fill=day_night,  y=MAXIMUM_DEPTH*-1))+
  #geom_col()+
  geom_histogram()+
  #geom_histogram(position = position_dodge(width = 5), binwidth = 10, alpha=0.5)+
  scale_fill_manual(values=c("cornflowerblue","darkblue"))+facet_wrap(~spp_name)+
  labs(x="histogram, bin width 10, position dodge 5", y="Maximum net depth (m)",color='Time caught')
plot_grid(shift_legend(pp_hist))

lutj<- lutj %>% mutate(day_night=ifelse(LOCATION_CODE_4==1,"day","night"))
lutj_hist<-ggplot(mlutjnosym,  aes(fill=day_night,  y=MAXIMUM_DEPTH*-1))+
  #geom_col()+
  geom_histogram()+
  #geom_histogram(position = position_dodge(width = 5), binwidth = 10, alpha=0.5)+
  scale_fill_manual(values=c("cornflowerblue","darkblue"))+facet_wrap(~spp_name)+
  labs(x="histogram, bin width 30", y="Maximum net depth (m)",color='Time caught')
plot_grid(shift_legend(lutj_hist))


uku_lutj<-filter(mlutjnosym,spp_name=="Aprion virescens")
ggplot(uku_lutj,  aes(color=day_night,  y=MAXIMUM_DEPTH*-1))+
  geom_density(size=2)+scale_color_manual(values=c("cornflowerblue","darkblue"),guide = "none")+#facet_grid(~spp_name)+
  labs(x="Kernel density", y="Maximum net depth (m)",color='Time caught',title="Uku/ Aprion virescens")+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",plot.title=element_text(face="italic",size=30))+
  ylim(-80,0)

paka_lutj<-filter(nlutj,spp_name=="Pristipomoides spp.")
ggplot(paka_lutj,  aes(color=day_night,  y=MAXIMUM_DEPTH*-1))+
  geom_density(size=2)+scale_color_manual(values=c("cornflowerblue","darkblue"),guide = "none")+#facet_grid(~spp_name)+
  labs(x="Kernel density", y="Maximum net depth (m)",color='Time caught', title = "'Opakapaka/ Pristipomoides spp.")+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",plot.title=element_text(face="italic",size=30))

kazzy_lutj<-filter(nlutj,spp_name=="Lutjanus kasmira")
ggplot(kazzy_lutj,  aes(color=day_night,  y=MAXIMUM_DEPTH*-1))+
  geom_density(size=2)+scale_color_manual(values=c("cornflowerblue","darkblue"))+#facet_grid(~spp_name)+
  labs(x="Kernel density", y="Maximum net depth (m)",color='Time caught', title = "Ta'ape/ Lutjanus kasmira" )+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",plot.title=element_text(face="italic",size=30))


#histograms#######
poster_pa<-larvah %>% distinct((tow_id),.keep_all = TRUE)
poster_pa<-poster_pa %>% filter(site!= "Kahe Point")
plot(poster_pa$uku_present1_absent0~poster_pa$this_temp)
pg13b<-ggplot(poster_pa, aes(fill=as.factor(uku_present1_absent0), x=this_temp))+geom_histogram()+scale_fill_manual(values=c("hotpink1","#7F0000"))+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=40),legend.position = "bottom")+
  labs(x="Maximum temperature (?C)", y="Number of trawls", fill="Larva present (1) or absent(0)")
pg13b

pinktemp<-ggplot(poster_pa, aes(fill=as.factor(uku_present1_absent0), x=this_temp))+geom_histogram()+scale_fill_manual(values=c("hotpink1","#7F0000"))+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),legend.position = "right")+
  labs(x="Maximum temperature (?C)", y="Number of trawls", fill="Larvae present\n (1) or absent(0)")


#in report page 13#####
pg13<-plot.gam(model,select=1, ylab="Conditional effect of temperature\n on uku occurrence", 
               xlab="Observed temperature (?C)",cex.axis=2.5, cex.lab=3, shade=T, trans=function(x)exp(x)/(1+exp(x)))#(exp(model$linear.predictors)/(1+exp(model$linear.predictors))))
+pg13b+ plot_layout(design = layout)+plot_annotation(tag_levels="A") &
  theme(plot.tag = element_text(size =15))

#######
p<-filter(poster_pa, pa==1)
summary(p$this_temp)
j<-as.numeric(p$max_temp)
min(j)
p$mean_sal
purplesal<-ggplot(larvah, aes(fill=as.factor(uku_present1_absent0), x=this_sal))+geom_histogram()+scale_fill_manual(values=c("mediumpurple1","mediumpurple4"))+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),legend.position = "right")+
  labs(x="Surface salinity (PSU)", y="Number of trawls", fill="Larvae present\n (1) or absent(0)")+
  xlim(34.5,35.5)

library(patchwork)
pinktemp/purplesal+plot_annotation(tag_levels=c("A","B"))

ggplot(larvah, aes(x=log(Dist2Shore_m), y=log(volume), fill=as.factor(uku_present1_absent0)))+
  geom_col()+scale_fill_manual(values=c("grey", "black"))


#univariate pearson corellations as further diagnostics####
cor.test(larvah$this_temp, larvah$uku_present1_absent0,method="pearson") 
cor.test(larvah$collection_month, larvah$uku_present1_absent0,method="pearson")
cor.test(larvah$time_since_peak_spawn, larvah$uku_present1_absent0,method="pearson") 
cor.test(larvah$collection_depth_m_max, larvah$uku_present1_absent0,method="pearson") 
cor.test(larvah$Dist2Shore_m, larvah$uku_present1_absent0,method="pearson") 
cor.test(log(larvah$Dist2Shore_m), larvah$uku_present1_absent0,method="pearson") 
cor.test(larvah$this_sal,larvah$uku_present1_absent0,method="pearson") 
=======
#libraries#####
library(tidyverse)
library(sp) 
library(raster)
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
library(lattice)
library(suncalc)
library(ggResidpanel)
library(mgcv)

#restarted again####
#ideally this is very reproducible, includes all the changes to any datasets downloaded form inport
#each new csv explains what scripts something else comes from 
#changes to pa in google sheets made right here
#csvs#####
#oes11_06, oes12_06, tc32####
matchy<-read.csv("inputs/matched_all.csv") 
#^from Matching_CTD_Locations_toSamples.R. (ask Justin if okay to post or if he has a link to this)
#SE12_06 and SE11_06 means used in Matching_CTD_Locations_toSamples.R. were pulled via sette2011_2012_CTD_means_function.R
#which consisted of data from CTD log sheets obtained via PIFSC data request PICDR-113137 

#tc8504, tc8604, tc8605#####
#the "all_tcs.csv" pulls only the top 40m but also loses the depths (nets only which vary between tow)
#so, Andrea pulled the cruise level csvs (which have been QC'd and converted from .MOC files)
#and pulled all surface temps only from these cruises L
tc8504_vars<-read.csv("inputs/TC8504_moc_data_allQCd.csv")
#mocness data with salinity values typed by hand by Andrea Schmidt off PDFs obtained via PIFC data request PICDR-113141
#these csvs were pulled together and quality control checked in the script: new_sal_tc8504_june2022.R
tc8504_vars<- tc8504_vars %>% rename("depth_m"=depth, "salinity"=sal, "temperature"=temp)
tc8504_vars<- tc8504_vars %>% unite("tow_id",c(cruise_number, station), sep="_", remove=F)

mini_8504<-tibble("mocid"=tc8504_vars$moc_id,"tow_id"=tc8504_vars$tow_id, "cruise_number"=tc8504_vars$cruise_number, "station"=tc8504_vars$station, "net_number"=tc8504_vars$net_number,"depth_m"=tc8504_vars$depth_m, "temperature"=tc8504_vars$temperature, "salinity"=tc8504_vars$salinity)


tc8604_vars<-read.csv("inputs/8604_moc_data_all_feb23.csv")
#temperature and salinity values came straight from the mocness_function.R processed .MOC files obtained from Bruce Mundy in late 2021.
#The csvs of these .MOC files will be posted to NCEI in the near future
tc8604_summary<-read.csv("inputs/tc8604_summary_stats_aug22.csv")

mini_8604<-tibble("mocid"=tc8604_vars$moc_id,"tow_id"=tc8604_vars$tow_id, "cruise_number"=tc8604_vars$cruise_number, "station"=tc8604_vars$station, "net_number"=tc8604_vars$net_number,"depth_m"=tc8604_vars$depth_m, "temperature"=tc8604_vars$temp_DegC, "salinity"=tc8604_vars$sal)

tc8602_vars<-read.csv("inputs/TC8602_moc_data_all.csv")
#tc8602 files were all bound together from the mocness_function.R processed .MOC files obtained from Bruce Mundy in late 2021
tc8602_summary<-read.csv("inputs/TC8602_tow_summaries.csv")
tc8602_vars<- tc8602_vars %>% rename("cruise_number"=cruise_id, "station"=station_id,"temperature"=temp_DegC, "salinity"=sal)
mini_8602<-tibble("mocid"=tc8602_vars$moc_id,"tow_id"=tc8602_vars$tow_id, "cruise_number"=tc8602_vars$cruise_number, "station"=tc8602_vars$station, "net_number"=tc8602_vars$net_number,"depth_m"=tc8602_vars$depth_m, "temperature"=tc8602_vars$temperature, "salinity"=tc8602_vars$salinity)
all_tcs<-rbind(mini_8602, mini_8504, mini_8604)

##in order to collapse mocness by depth one needs to create depth bins (top 20m)
all_tcs<-all_tcs %>% mutate(depth_bins=ifelse(depth_m<20, "0-20m", "20-200m"))
#filter to only this depth range (nets for offshore trawls sampled )
tcs_20<-all_tcs %>% filter(depth_bins=="0-20m")
#filter again to get rid of really errant data
tcs_20<-tcs_20 %>% filter(depth_m>0)
#filter again to get smaller depth range (0-5m)
c<-tcs_20 %>% filter(depth_m<5)
#impossible values check
c<-filter(c, (12)<temperature & temperature<=30) #min and max values from HOTS CTD potential temperature 0-1000m, 1989-2019
c<-filter(c, 33<salinity& salinity<37) 

#take means per tow_id
tcsummary<-c %>%
  group_by(tow_id) %>% #in order to mesh appropraitely with pa
  summarise_at(vars("salinity", "temperature"),c(min,max,mean))
all_tcs <- tcsummary %>% rename("min_sal"=salinity_fn1,"min_temp"= temperature_fn1, "max_sal"=salinity_fn2,"max_temp"=temperature_fn2, "mean_sal"=salinity_fn3, "mean_temp"=temperature_fn3)
#final QC
all_tcs<-all_tcs%>% mutate(mean_sal=(ifelse(mean_sal>39,mean_sal-6.323265288,mean_sal))) 
#applied correction factor to all to deal with massive salinity errors from tc8504. 
#See techincal memo Appendix 1, section A6 on page 38 for how this value was calculated.
all_tcs<-all_tcs%>% mutate(min_sal=(ifelse(min_sal>39,min_sal-6.323265288,min_sal)))
all_tcs<-all_tcs%>% mutate(max_sal=(ifelse(max_sal>39,max_sal-6.323265288,max_sal)))

#most upto date presence-absence sheet from GoogleSheets#####
pa<-read.csv("inputs/Relevant_Aprion_virescens_literature - presence_absence_sept22.csv")

#Boehlert+Mundy 1996 data####
mb<-read.csv("C:/Users/Andrea.Schmidt/Documents/M&B_larval_dist_csvs/cast_data_with_coord_updated_DD.csv", stringsAsFactors=FALSE, fileEncoding="latin1") 
#^this is equivalent to the MB data from inport with the coordinates that I added based on the station number from the PDFs in the data parcel from Inport
#source: GUID: gov.noaa.nmfs.inport:56552 
mb$cruise_id<-gsub("_","",mb$cruise_id)
mb$tow_id<-gsub("TC_8","TC8",mb$tow_id)
mb<-mb %>% unite(col="moc_id",cruise_id, STATION_NUMBER,net_number, sep="_", remove=F) #this creates a consistent moc and cruise id which can be used across sheets
mb<- mb %>%unite(col="tow_id",cruise_id, STATION_NUMBER, sep="_", remove=F)#this creates a consistent tow id which can be used to meld with pa
mb$X<-NULL
mb<-mb %>% filter(CRUISE_NUMBER != 8505) #removed because no included in presence absence dataset
##in order to collapse mocness by depth
only_volume<-mb %>% group_by(tow_id) %>% summarise_at(vars(WATER_VOLUME_FILTERED),sum)
only_volume<-only_volume %>% rename("sum_vol"="WATER_VOLUME_FILTERED")
#merging####
#meld will be a new dataframe consisting of only things one really actually needs
#first combine pa and matchy
meld<-full_join(pa,matchy) # presence-absence data combined with matched temperature/salinity data from OES12-06, 11-06, TC32 via Matching_CTD_Locations_toSamples.R
meld<-filter(meld, site!="Kahe Point")
#^removes Kahe point Larvae because they are such an outlier in regards to sampling. Will be treated as a case study.
#please see Tech Memo Appendix A, section on Westree et al. 1972 for further details.

mb<-dplyr::distinct(mb, tow_id, .keep_all=TRUE)
bo_mu<-full_join(mb,all_tcs)
#lines 111 to 114 change Bohlert and Mundy 1996 data outputs to be consistent with presence-absence datasheet
bo_mu<-bo_mu %>%mutate(LOCATION_CODE_1="Oahu")
bo_mu<-bo_mu %>%mutate(LOCATION_CODE_2=ifelse(3,"windward","leeward")) # used to say "windward/leeward, pelagic"
bo_mu<-bo_mu %>%mutate(LOCATION_CODE_3=ifelse(1, 1.5, (ifelse(LOCATION_CODE_3==2,5,15))))
bo_mu<-bo_mu %>%mutate(LOCATION_CODE_4=ifelse(1, "day","night"))
bo_mu<-add_column(bo_mu, MOCNESS="MOCNESS")
bm<-left_join(bo_mu, only_volume)

#finally, combine bm and pa data frames
meld2<-full_join(meld, bm)
#lines 121 to 141 are final quality control and consistency checks across the various data sources amalgamated in meld2
meld2<-meld2 %>% filter(cruise_number != "TC8505") #most MOCNESS data from this cruise leg are incomplete, hence its removal
meld2<-meld2 %>% mutate(this_sal= coalesce(Nearest_Sal_Val,mean_sal), .keep="unused")
meld2<-meld2 %>% mutate(this_sal= (ifelse(this_sal>39,this_sal-6.323265288,this_sal)))
meld2<-meld2 %>% mutate(this_temp= coalesce(max_temp, Nearest_Temp_Val))
meld2<-meld2 %>% mutate(mean_temp= coalesce(mean_temp, Nearest_Temp_Val))
meld2<-meld2 %>% mutate(min_temp= coalesce(min_possible_temp, this_temp, min_temp))
meld2<-meld2 %>% mutate(sampling_method= coalesce(sampling_method, MOCNESS), .keep="unused")
meld2<-meld2 %>% mutate(isle_region=ifelse(str_detect(site, "lee")==T, "leeward", site))
meld2<-meld2 %>% mutate(isle_region=ifelse(str_detect(isle_region, "L")==T, "leeward", isle_region))
meld2<-meld2 %>% mutate(isle_region=ifelse(str_detect(isle_region, "17")==T, "leeward", isle_region))
meld2<-meld2 %>% mutate(isle_region=ifelse(str_detect(isle_region, "30")==T, "leeward", isle_region))
meld2<-meld2 %>% mutate(isle_region=ifelse(str_detect(isle_region, "W")==T, "windward", isle_region))
meld2<-meld2  %>% mutate(min_depth= coalesce(MINIMUM_DEPTH, collection_depth_m_min), .keep="unused")
meld2<-meld2 %>% mutate(collection_day= coalesce(collection_day,TRAWL_DAY), .keep="unused")
meld2<-meld2 %>% mutate(collection_month= coalesce(collection_month,TRAWL_MONTH), .keep="unused")
meld2<-meld2 %>% mutate(collection_year= coalesce(collection_year,TRAWL_YEAR), .keep="unused")
meld2<-meld2  %>% filter(max_cast_depth.m. != "failed")
meld2<-meld2  %>% mutate(max_depth= coalesce(MAXIMUM_DEPTH, as.integer(collection_depth_m_max),as.integer(max_cast_depth.m.)), .keep="unused") #as integer related error is okay. its turning one of those 
meld2<-meld2  %>% mutate(day_or_night= coalesce(LOCATION_CODE_4, day_night), .keep="unused")
meld2<-meld2 %>% mutate(volume= as.numeric(coalesce(V_in_m.3, sum_vol)))#WATER_VOLUME_FILTERED, was included but it masked the sum_vol and was thus left out. Reinclude this if returning to depth integrate data, .keep="unused") #still missing some values which were not in the original BM document
meld2<-meld2 %>%  mutate(volume=ifelse(sampling_method=="MOCNESS", ifelse(volume<sum_vol, sum_vol, volume), volume))#targets MOCNESS only, replaces volumes for each mocness net with depth integrated total volume for all nets in a given tow

#add moon date
larvah<-unite(meld2, "collection_date", c(collection_day, collection_month, collection_year), sep="/", remove=F)
larvah$collection_date<-lubridate::as_date(larvah$collection_date, format="%d/%m/%Y")
larvah<-mutate(larvah, "moon_date"=suncalc::getMoonIllumination(date = larvah$collection_date,keep = "phase"))

#add in gear/fishing style effect
larvah<-larvah %>% mutate(sampling_style=ifelse(sampling_method=="MOCNESS", 1, ifelse(sampling_method=="COBB TRAWL",2,3)))
larvah<-larvah %>% filter(cruise_number != "TC8505") #ensuring that tc8505 data are removed since no environmental data are available (see line 121 above)

#plots of volume filtered asit relates to by distance from shore#####
ggplot(larvah, aes(x=temp_at_colllection_depth, y=Depth_m_v21_50m))+geom_point()
ggplot(larvah, aes(x=temp_at_colllection_depth, y=(-1*max_depth)))+geom_point()
ggplot(larvah, aes(x=this_temp, y=(-1*max_depth)))+geom_point()

ggplot(larvah, aes(x=this_temp, y=Depth_m_v21_50m))+geom_point()
ggplot(larvah, aes(x=volume,fill=uku_present1_absent0))+geom_histogram()
ggplot(larvah, aes(y=log(volume), fill=uku_present1_absent0))+geom_bar()
ggplot(larvah, aes(y=log(Dist2Shore_m), x=log(volume), color=cruise_number))+geom_point()
ggplot(larvah, aes(y=log(Dist2Shore_m), x=log(volume), color=cruise_number,shape=as.factor(uku_present1_absent0)))+
  geom_point(size=4)
ggplot(larvah, aes(fill=as.factor(uku_present1_absent0),x=(Dist2Shore_m/1000), y=(log10(volume))))+
  geom_col(width=1, alpha=0.5)+scale_fill_manual(values=c("bisque4","darkgreen"))+
  labs(fill="Larvae present or absent", y=bquote("Log 10 volume of water filtered m"^"3"),x="Distance to shore (km)" )+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "horizontal", legend.box = "horizontal", legend.position = "bottom")


#statistcal modelling#####
#plots to test for correlation, generated with help of J.S. and J.W.
ggplot(larvah, aes(x=log(Dist2Shore_m), y=this_temp, color=cruise_number))+geom_point()
ggplot(larvah, aes(x=log(Dist2Shore_m), y=this_sal, color=cruise_number))+geom_point()
ggplot(larvah, aes(y=log(Dist2Shore_m), x=Near_Island, color=cruise_number))+geom_point()
ggplot(larvah, aes(x=this_temp, y=isle_region, color=cruise_number))+geom_point()
ggplot(larvah, aes(x=this_temp, y=this_sal, color=cruise_number))+geom_point(size=5)+ylim(34,36)+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=15),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical")+
  labs(x="surface temperature values (?C)", y="surface salinities", color="cruise identifier")


ggplot(larvah, aes(x=collection_month, y=this_sal, color=cruise_number))+geom_point()+ylim(34,36)
ggplot(larvah, aes(x=collection_month, y=this_temp, color=cruise_number))+geom_point()
ggplot(larvah, aes(y=min_depth, x=this_temp, color=cruise_number))+geom_point()
ggplot(larvah, aes(y=max_depth, x=this_temp, color=cruise_number))+geom_point()

ggplot(larvah, aes(y=log(Dist2Shore_m), x=log(volume), color=cruise_number))+geom_point()


#Generalized Additive Models###########
#these models were compared and informed the presentation of certain environmental factors as informative in larval uku distributions in the techincal memo
model1<-mgcv::gam(data=larvah, uku_present1_absent0~sampling_style+cruise_number+
                    s(this_sal, k=4)+
                    s(log(Dist2Shore_m),k=4)+
                    offset(log10(volume)),family=binomial) #for month could add bs="cc" creates cyclic spline to connect dots at the end; might need
mod1_param_no<-5
model2<-mgcv::gam(data=larvah, uku_present1_absent0~(sampling_style)+
                    cruise_number+
                    s(this_temp, k=4)+
                    #te(latitude_start_dd,longitude_start_dd)+
                    s((log(Dist2Shore_m)), k=4)+
                    offset(log10(volume)),family = binomial) #can temp explain the same or better than month and sal together
mod2_param_no<-5
model3<-mgcv::gam(data=larvah, uku_present1_absent0~sampling_style+
                    cruise_number+s(collection_month, k=4)+s((log(Dist2Shore_m)), k=4)+
                    offset(log(volume)),family = binomial) #can temp explain the same or better than month and sal together
mod3_param_no<-5
model4<-mgcv::gam(data=larvah, uku_present1_absent0~(sampling_style)+cruise_number+
                    s(this_temp, k=4)+
                    s(moon_date$phase, k=4)+
                    te(latitude_start_dd,longitude_start_dd)+
                    s((log(Dist2Shore_m)), k=4)+
                    offset(log10(volume)),family = binomial)
mod4_param_no<-7
model5<-mgcv::gam(data=larvah, uku_present1_absent0~(sampling_style)+
                    cruise_number+s(this_sal, k=4)+s(moon_date$phase, k=4)+
                    te(latitude_start_dd,longitude_start_dd)+
                    s((log(Dist2Shore_m)), k=4)+offset(log10(volume)),family = binomial) #can temp explain the same or better than month and sal together
mod5_param_no<-6
model6<-mgcv::gam(data=larvah, uku_present1_absent0~(sampling_style)+
                    cruise_number+
                    s(this_sal, k=4)+
                    te(latitude_start_dd,longitude_start_dd)+
                    s((log(Dist2Shore_m)), k=4)+
                    offset(log10(volume)),family = binomial) 
mod6_param_no<-6
model7<-mgcv::gam(data=larvah, uku_present1_absent0~(sampling_style)+
                    cruise_number+
                    s(this_temp, k=4)+
                    te(latitude_start_dd,longitude_start_dd)+
                    s((log(Dist2Shore_m)), k=4)+
                    offset(log10(volume)),family = binomial) 
mod7_param_no<-6
#compare all models with AIC
AIC(model1, model2, model3, model4, model5, model6, model7) 
#top two candidate models (assess with AICc)
AIC(model4, model7)
library(MuMIn)
AICc(model1, model2, model3, model4, model5, model6, model7)
m4<-vcov(model4)
m7<-vcov(model7)
#to also calculate AICc by hand as a sanity check, formula from Wikipedia
mc4<-86.98395+((2*(40^2)+(2*40))/(205-40-1))
mc7<-84.91197+((2*(37^2)+(2*37))/(205-37-1))

#plot that is in the Tech memo, pg13a#
plot.gam(model7,select=1, ylab="Conditional effect of temperature\n on uku occurrence", 
         xlab="Observed temperature (°C)",cex.axis=1.5, cex.lab=2, shade=F, trans=function(x)exp(x)/(1+exp(x)))

#model diagnostics
model<-model7
plot(model)
lerg=function(x)exp(x)/(1+exp(x))
par(mar=c(5,9,4,1)+1)
#for aic formula k= number of parameters

#further model assessments between models 4 and 7
library(visreg)
visreg(model, "this_temp", scale = "response", ylab = "", xlab = "this_temp (deg.C)", alpha=0.01, rug=1)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(mgcv)
library(tidymv)
plot(log(model$linear.predictors)~model$model$this_temp)
plot.gam(model7,select=1, ylab="Conditional effect of temperature\n on uku occurrence",
         xlab="Observed temperature (°C)",cex.axis=2.5, cex.lab=3, trans=plogis)


plot.gam(model4,select=1, ylab="Conditional effect of temperature on uku occurrence",
         xlab="Observed temperature (°C)",cex.axis=1, cex.lab=1, trans=plogis)
#get_smooths_difference(model)
model<-model4
summary(model) 
anova(model) 
qqnorm(resid(model))
gam.check(model)
hist(resid(model))
#model 1,2 suggestive of normal, need more binss?
plot(resid(model)~fitted(model))#model1 nice
plot(cooks.distance(model),type="h") 
#one concern is spatial autocorrelation of residuals
#simply plotted on a map to test
#map libraries:
library(sp) #already in raster
library(raster)# error
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
#map codes
#for all main hawaiian islands:
oahu_raster <- raster(file.path("inputs/all_hi_bathy.tiff"))
oahu_df <- fortify(as.bathy(oahu_raster))
str(oahu_df)
world<-ne_countries(scale="medium", returnclass = "sf")

oahu_map <- ggplot(data=world) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,0))+new_scale_fill()+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-154.9,-161.2), ylim=c(19, 22.2))

df2<-tibble(resid(model7))
larvahh<-filter(larvah,is.na(larvah$volume)==F)
larvahh<-filter(larvahh,is.na(larvahh$this_temp)==F)
larvahhh<-larvahh %>% mutate(model2_resids=add_column(df2))
larvahhh$latitude_start_dd <- unlist(larvahhh$latitude_start_dd)
larvahhh$longitude_start_dd <- unlist(larvahhh$longitude_start_dd)
larvahhh$model2_resids<-unlist(larvahhh$model2_resids)
model.assesment.colors <- colorRampPalette(c("blue", "white", "red"))
c<-model.assesment.colors(10)
oahu_map+geom_point(data=larvahhh, mapping=aes(y=latitude_start_dd,x=longitude_start_dd,
                                               color=model2_resids, size=3))+
  scale_color_gradientn(colours = c)+labs(color = "Model residuals")+
  theme(legend.direction = 'horizontal', legend.position = "bottom")

#oahu only:
str(oahu_df)
oahu_map <- ggplot() +
  coord_sf(crs = oahu_raster@crs, xlim=c(-157.5,-158.4),ylim=c(21.2,21.75))+
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,1000))+new_scale_fill()+
  theme(panel.background=element_blank(), 
        panel.grid.major.x=element_line())
oahualua<-filter(larvahhh, Near_Island=="Oahu")
oahalua<-filter(oahualua, longitude_start_dd!=(-157.91333333333))

oahu_map+geom_point(data=larvahhh, mapping=aes(y=latitude_start_dd,x=longitude_start_dd, color=model2_resids))+
  scale_color_gradientn(colours = c("blue","red"))
meld2oahu<-filter(meld2, Near_Island=="Oahu")

# to test univariate regressions*####
model1<-gam(larvah$uku_present1_absent0~s(larvah$this_temp),family=binomial)
model2<-gam(larvah$uku_present1_absent0~s(sqrt(larvah$this_temp)),family=binomial)
model3<-gam(data=larvah, uku_present1_absent0~(collection_month),family=binomial)
model4<-gam(larvah$uku_present1_absent0~s(larvah$Dist2Shore_m),family=binomial) 
model5<-gam(larvah$uku_present1_absent0~s(log(larvah$Dist2Shore_m)),family=binomial) 
model6<-gam(larvah$uku_present1_absent0~s(larvah$this_sal),family=binomial)
model7<-gam(larvah$uku_present1_absent0~larvah$sampling_style,family=binomial)
larvah<-larvah %>% mutate(cruise_factor=ifelse(cruise_number=="11_06", 11, ifelse(sampling_method=="12_06",12,ifelse(sampling_method=="32",13,ifelse(sampling_method=="TC8504",14,ifelse(sampling_method=="TC8602",15,16))))))
model8<-gam(larvah$uku_present1_absent0~larvah$cruise_factor,family=binomial)
model9<-gam(larvah$uku_present1_absent0~log(larvah$volume),family=binomial)
larvah<-larvah %>% mutate(dn=ifelse(day_or_night=="day",5,6))
model10<-gam(larvah$uku_present1_absent0~larvah$dn,family=binomial)
model11<-gam(larvah$uku_present1_absent0~s(larvah$moon_date$phase),family=binomial)
model12<-gam(larvah$uku_present1_absent0~te(larvah$latitude_start_dd,larvah$longitude_start_dd),family=binomial)

AIC(model1, model2, model3,  model4,  model5, model6, model7, model8, model9, model10, model11,model12) #model9
model<-model5
plot(model)
qqnorm(resid(model)) 
plot(resid(model)~fitted(model))
hist(resid(model))
plot(cooks.distance(model),type="h")
summary(model) 
anova(model)


#to test multivariate regressions##
#ultimately fruitless and not included in report, included here as a sanity check
model12<-glm(data=larvah, uku_present1_absent0~this_temp*moon_date$phase+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model13<-glm(data=larvah, uku_present1_absent0~dn*moon_date$phase+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model14<-glm(data=larvah, uku_present1_absent0~collection_month*moon_date$phase+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model15<-glm(data=larvah, uku_present1_absent0~log(Dist2Shore_m)*moon_date$phase+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model16<-glm(data=larvah, uku_present1_absent0~this_sal*moon_date$phase+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model17<-glm(data=larvah, uku_present1_absent0~this_sal*this_temp+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model18<-glm(data=larvah, uku_present1_absent0~log(Dist2Shore_m)*this_temp+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
model19<-glm(data=larvah, uku_present1_absent0~log(Dist2Shore_m)*collection_month+(1|sampling_style)+(1|log10(volume))+(1|cruise_factor), family=binomial)
AIC(model12, model13, model14, model15, model16, model17, model18, model19)
library(wiqid)
wiqid::AICc(model12, model13, model14, model15, model16, model17, model18, model19)
model<-model19
summary(model) 
anova(model)
qqnorm(resid(model)) 
plot(resid(model)~fitted(model))
hist(resid(model))
plot(cooks.distance(model),type="h")
plot(model)



###Sampling Spread Map######
###=---Cam's map---
library(sp) #already in raster
library(raster)# error
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
world<-ne_countries(scale="medium", returnclass = "sf")
oahu_raster <- raster(file.path("inputs/all_hi_bathy.tiff"))
oahu_df <- fortify(as.bathy(oahu_raster))
str(oahu_df)
oahu_map <- ggplot(data=world) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,1000))+new_scale_fill()+
  scale_fill_continuous(labels = scales::label_number(scale = 1000, suffix = "k"))+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-154.9,-161.2), ylim=c(19, 22.2))


oahu_map
shapes<-c(1,18)
library(tidyverse)
ga<-pa %>% filter(is.na(cruise_number)==F)
ga<-ga %>% filter(cruise_number!="")

oahu_map+geom_point(data=larvah, mapping=aes(y=latitude_start_dd,
                                             x=longitude_start_dd, 
                                             color=cruise_number,
                                             size=5,
                                             shape=as.factor(uku_present1_absent0)))+
  scale_color_viridis_d(option="B")+scale_shape_manual(values=shapes)+
  theme(axis.title = element_blank(),legend.position = "bottom",legend.box = "horizonal",
        axis.ticks = element_blank(),axis.text = element_blank(),
        legend.title=element_text(size=16),legend.text=element_text(size=20))+
  labs(color="Sampling Effort by Cruise Identifier", shape="Larva present (1) or absent(0)", size=element_blank())+
  guides(color = guide_legend(override.aes = list(size = 5), order=1),
         shape=guide_legend(override.aes = list(size = 5), order=2),
         fill= guide_colorbar(barwidth = 20, barheight = 10, order=3),
         size="none")



#####oahu inset####
str(oahu_df)
oahu_map <- ggplot(data=world)+
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,1000))+new_scale_fill()+
  theme(panel.background=element_blank(), panel.grid.major.x=element_line())+
  theme_bw()+geom_sf()+coord_sf(crs = oahu_raster@crs, xlim=c(-157.5,-158.4),ylim=c(21.2,21.75))
oahualua<-filter(larvah, Near_Island=="Oahu")
oahualua<-filter(oahualua, cruise_number!="")
oahalua<-filter(oahualua, longitude_start_dd!=(-157.91333333333))

oahu_map+geom_point(data=oahualua, mapping=aes(y=latitude_start_dd,
                                               x=longitude_start_dd, 
                                               color=cruise_number, 
                                               size=3,
                                               shape=as.factor(uku_present1_absent0)))+
  scale_color_viridis_d(option="B")+scale_shape_manual(values=shapes)+
  theme(legend.position = "bottom",axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_blank(),legend.box = "vertical")+
  labs(color="Sampling Effort by Cruise or Study Identifier", shape="Larva present (1) or absent(0)",
       size="")

#Sat. SST by year map####
#this code was from Coast Watch course taught by Melanie Abacassis, Tom Oliver and Johanna Wren in early 2022
###actual pa points#####
library(tidyverse)
library(lattice)
pa<-mutate(larvah, long_360=longitude_start_dd+0) #originally said +360, changed to 0 without changing names since there are dependencies throughout the script
p<-filter(pa, uku_present1_absent0==1)
a<-filter(pa, uku_present1_absent0==0)
uku_p_lon<-as.vector(p$long_360)
uku_p_lat<-as.vector(p$latitude_start_dd)
uku_a_lon<-as.vector(a$long_360)
uku_a_lat<-as.vector(a$latitude_start_dd)

### extract data####
library(ncdf4)
library(httr)
library(tidyverse)
#crs=oahu_raster@crs,xlim=c(-154.9,-161.2), ylim=c(19, 22.2)
#pull data of SST for June-October
#show P/A for the year in question ONLY
#Because ERDDAP includes RESTful services, you can download data listed on any ERDDAP platform from R using the URL structure.
#this is daily SST for september 1985 (starts 1 month before 1st uku collection 8/6/1985)
#adjusted to Oahualua only xlim=c(-157.5==202.5,-158.4==201.6),ylim=c(21.2,21.75))
junk <- GET('https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.nc?analysed_sst%5B(1985-06-06T12:00:00Z):1:(1985-10-30T12:00:00Z)%5D%5B(21.2):1:(21.75)%5D%5B(201.5):1:(202.5)%5D',
            write_disk("sst985.nc", overwrite=TRUE))
#this is daily SST for 1986 adjusted to Oahulaua only
junk2<-GET('https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.nc?analysed_sst%5B(1986-06-06T12:00:00Z):1:(1986-10-30T12:00:00Z)%5D%5B(21.2):1:(21.75)%5D%5B(201.5):1:(202.5)%5D',
           write_disk("sst686.nc", overwrite=TRUE))
#this is daily SST for 2011
junk3 <- GET('https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.nc?analysed_sst%5B(2011-06-06T12:00:00Z):1:(2011-10-30T12:00:00Z)%5D%5B(18.025):1:(23.025)%5D%5B(198.025):1:(206.025)%5D',
             write_disk("sst11.nc", overwrite=TRUE))
#this is daily SST for 2012
junk4<-GET('https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.nc?analysed_sst%5B(2012-06-06T12:00:00Z):1:(2012-10-30T12:00:00Z)%5D%5B(18.025):1:(23.025)%5D%5B(198.025):1:(206.025)%5D',
           write_disk("sst12.nc", overwrite=TRUE))

nc <- nc_open('sst985.nc')
nc2 <- nc_open('sst686.nc')
names(nc$var)
names(nc2$var)
nc3 <- nc_open('sst11.nc')
nc4 <- nc_open('sst12.nc')
names(nc3$var)
names(nc4$var)

# ###set up each variable####
v1 <- nc$var[[1]] #list of variables??
v2 <- nc2$var[[1]] #list of variables??
v3 <- nc3$var[[1]] #list of variables??
v4 <- nc4$var[[1]] #list of variables??
sst985 <- ncvar_get(nc,v1) #Extract analysed_sst, reads data from the netCDF file, only works if you have already opened the file, shows as a multi-dimensional array
sst686 <- ncvar_get(nc2,v2) #reads data from the netCDF file, only works if you have already opened the file, shows as a multi-dimensional array
sst11 <- ncvar_get(nc3,v3) #Extract analysed_sst, reads data from the netCDF file, only works if you have already opened the file, shows as a multi-dimensional array
sst12 <- ncvar_get(nc4,v4) #reads data from the netCDF file, only works if you have already opened the file, shows as a multi-dimensional array

dim(sst985)#examines the structure of sst, this has between 101-146 time points
dim(sst686)#examines the structure of sst, this has  101-146 time points
dim(sst11)#examines the structure of sst, this has  101-146 time points
dim(sst12)#examines the structure of sst, this has  101-146 time points


dates985 <- as.POSIXlt(v1$dim[[3]]$vals,origin='1970-01-01',tz='GMT') #get the dates for each time step
dates686<- as.POSIXlt(v2$dim[[3]]$vals,origin='1970-01-01',tz='GMT') #get the dates for each time step
dates11 <- as.POSIXlt(v3$dim[[3]]$vals,origin='1970-01-01',tz='GMT') #get the dates for each time step
dates12<- as.POSIXlt(v4$dim[[3]]$vals,origin='1970-01-01',tz='GMT') #get the dates for each time step

lon <- v1$dim[[1]]$vals #gives vector of longitude
lon=lon-360
lat <- v1$dim[[2]]$vals #gives vector of latitude
lon2 <- v2$dim[[1]]$vals #gives vector of longitude
lon2=lon2-360
lat2 <- v2$dim[[2]]$vals #gives vector of latitude
lon3 <- v3$dim[[1]]$vals #gives vector of longitude
lon3=lon3-360
lat3 <- v3$dim[[2]]$vals #gives vector of latitude
lon4 <- v4$dim[[1]]$vals #gives vector of longitude
lon4=lon4-360
lat4 <- v4$dim[[2]]$vals #gives vector of latitude

#Close the netcdf file and remove the data and files that are not needed anymore.
nc_close(nc) #this step is important, otherwise you risk data loss
rm(junk,v1)
file.remove('sst985.nc')

nc_close(nc2) #this step is important, otherwise you risk data loss
rm(junk2,v2)
file.remove('sst686.nc')

nc_close(nc3) #this step is important, otherwise you risk data loss
rm(junk3,v3)
file.remove('sst11.nc')

nc_close(nc4) #this step is important, otherwise you risk data loss
rm(junk4,v4)
file.remove('sst12.nc')


####wrangling all years####
df85<-sst985[,,mean(1:146)] #okay so basically set row and columns names as lat long THEN pivot longer so these all become their own columns???
rownames(df85, do.NULL = TRUE, prefix = "row")
rownames(df85) <- lon
colnames(df85, do.NULL = TRUE, prefix = "col")
colnames(df85) <-lat
df85real<-as.data.frame(as.table(df85))
df85real<-cbind(df85real, year=1985)
#df85real %>% df85real <-plyr::rename(c("x"=Var1,"y"=Var2, "temperature"=Freq))

df86<-sst686[,,mean(1:146)] #okay so basically set row and columns names as lat long THEN pivot longer so these all become their own columns???
rownames(df86, do.NULL = TRUE, prefix = "row")
rownames(df86) <- lon2
colnames(df86, do.NULL = TRUE, prefix = "col")
colnames(df86) <-lat2
df86real<-as.data.frame(as.table(df86))
df86real<-cbind(df86real, year=1986)

df11<-sst11[,,mean(1:146)] #okay so basically set row and columns names as lat long THEN pivot longer so these all become their own columns???
rownames(df11, do.NULL = TRUE, prefix = "row")
rownames(df11) <- lon3
colnames(df11, do.NULL = TRUE, prefix = "col")
colnames(df11) <-lat3
df11real<-as.data.frame(as.table(df11))
df11real<-cbind(df11real, year=2011)

df12<-sst12[,,mean(1:146)] #okay so basically set row and columns names as lat long THEN pivot longer so these all become their own columns???
rownames(df12, do.NULL = TRUE, prefix = "row")
rownames(df12) <- lon4
colnames(df12, do.NULL = TRUE, prefix = "col")
colnames(df12) <-lat4
df12real<-as.data.frame(as.table(df12))
df12real<-cbind(df12real, year=2012)

eighties_sst<-rbind(df85real,df86real)
tens_sst<-rbind(df11real, df12real)
#added 2023
all_sst<-rbind(df85real,df86real,df11real, df12real)
all_sst<-mutate(all_sst, x=as.numeric(levels(Var1))[Var1])
all_sst<-mutate(all_sst, y=as.numeric(levels(Var2))[Var2])
####
eighties_sst<-mutate(eighties_sst, x=as.numeric(levels(Var1))[Var1])
eighties_sst<-mutate(eighties_sst, y=as.numeric(levels(Var2))[Var2])
tens_sst<-mutate(tens_sst, x=as.numeric(levels(Var1))[Var1])
tens_sst<-mutate(tens_sst, y=as.numeric(levels(Var2))[Var2])

shapes<-c(1,18)
larv80<-filter(larvah, collection_year %in% (1984:1987))
larv10<-filter(larvah, collection_year %in% (2010:2013))  
#all years with ggplot#####
library(sp) #already in raster
library(raster)# error
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
world<-ne_countries(scale="medium", returnclass = "sf")

eighties<-ggplot(data=world)+geom_raster(data=eighties_sst,aes(x=x,y=y, fill=Freq))+facet_grid(~year)+
  scale_fill_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray90")+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),
        axis.text = element_text(size=10), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",strip.text.x=element_text(size=20))+
  labs(fill="SST (?C)")+
  theme_bw()+geom_sf()+coord_sf(crs = oahu_raster@crs, xlim=c(-157.5,-158.4),ylim=c(21.2,21.75))

eighties
eighties<-eighties+geom_point(data=larv80, mapping=aes(y=latitude_start_dd,
                                                       x=longitude_start_dd, 
                                                       size=2,
                                                       shape=as.factor(uku_present1_absent0)))+
  guides(size="none")+labs(shape="Larvae present\nor absent", x="Longitude", y="Latitude")+
  scale_shape_manual(values=shapes)


tens<-ggplot(data=world)+geom_raster(data=tens_sst,aes(x=x,y=y, fill=Freq))+facet_grid(~year)+
  scale_fill_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray90")+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",strip.text.y =element_text(size=20))+
  theme_bw()+
  labs(fill="SST (?C)")+
  geom_sf()+coord_sf(crs=oahu_raster@crs,xlim=c(-154.9,-161.2), ylim=c(19, 22.2))
tens
ten<-tens+geom_point(data=larv10, mapping=aes(y=latitude_start_dd,
                                              x=longitude_start_dd, 
                                              size=2,
                                              shape=as.factor(uku_present1_absent0)))+
  guides(size="none")+ labs(shape="Larvae present\nor absent", x="Longitude", y="Latitude")+
  scale_shape_manual(values=shapes)

library(patchwork)
eighties/ten+plot_annotation(tag_levels=c("A","B","C","D"))


####added 2023
allst<-ggplot(data=world)+geom_raster(data=all_sst,aes(x=x,y=y, fill=Freq))+facet_wrap(~year)+
  scale_fill_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray90")+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",strip.text.y =element_text(size=20))+
  theme_bw()+
  labs(fill="SST (?C)")+
  geom_sf()+coord_sf(crs=oahu_raster@crs,xlim=c(-154.9,-161.2), ylim=c(19, 22.2))
allst
####

#####1985:Working with the extracted data####
#run start
###Creating a map for one time step in September 1985
#[scale.R] needs to be in working directory
#set some color breaks
h <- hist(sst985[,,55], plot=T) #mean for 2nd to last time point
# #breaks <- h$breaks
breaks<-seq(from=24,to=28, length.out=100) #length out= number of color bands, need either by= or length.out, not both
n <- length(breaks)-1
# #define a color palette
jet.colors <- colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

#set color scale using the jet.colors palette
c <- jet.colors(n)
#use this  c<-gray.colors(n=n) if you want it in grayscale

#c<-scale_color_viridis_c(start=0, end=length(n))
sst985[,,mean(1:146)]
#
#prepare graphic window : left side for map, right side for color scale
layout(matrix(c(1,2,3,0,4,0), nrow=1, ncol=2), widths=c(5,1), heights=4)
layout.show(2)
par(mar=c(3,3,3,1))


image(lon,lat,sst985[,,mean(1:146)],xlab='',ylab,col=c,breaks=breaks,
      axes=TRUE,xaxs='i',yaxs='i',asp=1, main=paste("Mean SST", dates985[1],"to",dates985[146]))

#
# #example of how to add points to the map
ef<-filter(pa, collection_year==1985)
p<-filter(ef, uku_present1_absent0==1)
a<-filter(ef, uku_present1_absent0==0)
uku_p_lon<-as.vector(p$long_360)
uku_p_lat<-as.vector(p$latitude_start_dd)
uku_a_lon<-as.vector(a$long_360)
uku_a_lat<-as.vector(a$latitude_start_dd)
points(uku_p_lon,uku_p_lat, pch=18, cex=2) #uku present
points(uku_a_lon,uku_a_lat, pch=1, cex=2) #uku absent
# #example of how to add a contour (this is considered a new plot, not a feature, so you need to use par(new=TRUE)) to overlay it on top of the SST map
par(new=TRUE)
contour(lon,lat,sst985[,,1],levels=20,xaxs='i',yaxs='i',labcex=20,vfont = c("sans serif", "bold"),axes=FALSE,asp=1)
#
#plot color scale using 'image.scale' function from 'scale.R' script)
par(mar=c(3,1,3,3))
source('~/all_scripts_R/scale.R')
image.scale(sst985[,,55], col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='SST', cex=4)
axis(4, las=1)
box()
# #run end
# #####1986:Working with the extracted data####
image(lon,lat,sst686[,,mean(1:146)],xlab='',ylab,col=c,breaks=breaks,
      axes=TRUE,xaxs='i',yaxs='i',asp=1, main=paste("Mean SST", dates686[1],"to",dates686[146]))
#
# #example of how to add points to the map
es<-filter(pa, collection_year==1986)
p<-filter(es, uku_present1_absent0==1)
a<-filter(es, uku_present1_absent0==0)
uku_p_lon<-as.vector(p$long_360)
uku_p_lat<-as.vector(p$latitude_start_dd)
uku_a_lon<-as.vector(a$long_360)
uku_a_lat<-as.vector(a$latitude_start_dd)
points(uku_p_lon,uku_p_lat, pch=18, cex=2) #uku present
points(uku_a_lon,uku_a_lat, pch=1, cex=2) #uku absent
#example of how to add a contour (this is considered a new plot, not a feature, so you need to use par(new=TRUE)) to overlay it on top of the SST map
par(new=TRUE)
contour(lon,lat,sst686[,,30],levels=20,xaxs='i',yaxs='i',labcex=0.8,vfont = c("sans serif", "bold"),axes=FALSE,asp=1)
#
#plot color scale using 'image.scale' function from 'scale.R' script)
par(mar=c(3,1,3,3))
source('~/all_scripts_R/scale.R')

image.scale(sst686[,,30], col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='SST')
axis(4, las=1)
box()
# #run end
# #####2011:Working with the extracted data####
# #run start
h <- hist(sst11[,,55], plot=T) #mean for 2nd to last time point
#breaks <- h$breaks
breaks<-seq(from=24,to=28, length.out=100) #length out= number of color bands, need either by= or length.out, not both
n <- length(breaks)-1

#define a color palette
jet.colors <- colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# #set color scale using the jet.colors palette
c <- jet.colors(n)
#c<-scale_color_viridis_c(start=0, end=length(n))
sst11[,,mean(1:146)]
#
# #prepare graphic window : left side for map, right side for color scale
layout(matrix(c(1,2,3,0,4,0), nrow=1, ncol=2), widths=c(5,1), heights=4)
layout.show(2)
par(mar=c(3,3,3,1))
image(lon3,lat3,sst11[,,mean(1:146)],xlab='',ylab,col=c,breaks=breaks,
      axes=TRUE,xaxs='i',yaxs='i',asp=1, main=paste("Mean SST", dates11[1],"to",dates11[146]))

#example of how to add points to the map
el<-filter(pa, collection_year==2011)
p<-filter(el, uku_present1_absent0==1)
a<-filter(el, uku_present1_absent0==0)
uku_p_lon<-as.vector(p$long_360)
uku_p_lat<-as.vector(p$latitude_start_dd)
uku_a_lon<-as.vector(a$long_360)
uku_a_lat<-as.vector(a$latitude_start_dd)
points(uku_p_lon,uku_p_lat, pch=18, cex=2) #uku present
points(uku_a_lon,uku_a_lat, pch=1, cex=2) #uku absent
#example of how to add a contour (this is considered a new plot, not a feature, so you need to use par(new=TRUE)) to overlay it on top of the SST map
par(new=TRUE)
contour(lon3,lat3,(sst11[,,mean(1:146)]),levels=20,xaxs='i',yaxs='i',labcex=0.8,vfont = c("sans serif", "bold"),axes=FALSE,asp=1)
#
# #plot color scale using 'image.scale' function from 'scale.R' script)
par(mar=c(3,1,3,3))
source('~/all_scripts_R/scale.R')
image.scale(sst11[,,mean(1:146)], col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='SST')
axis(4, las=1)
box()
# #run end
# #####2012:Working with the extracted data####
# #run start
# ###Creating a map for one time step in june-october 2012
#[scale.R] needs to be in working directory
# #set some color breaks
h <- hist(sst12[,,mean(1:146)], plot=T) #mean for 2nd to last time point
# #breaks <- h$breaks
breaks<-seq(from=24,to=28, length.out=100) #length out= number of color bands, need either by= or length.out, not both
n <- length(breaks)-1
#
#define a color palette
jet.colors <- colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#
# #set color scale using the jet.colors palette
c <- jet.colors(n)
#c<-gray.colors(n=n) # for grayscale for tech memmo
# #c<-scale_color_viridis_c(start=0, end=length(n))
#
#prepare graphic window : left side for map, right side for color scale
layout(matrix(c(1,2,3,0,4,0), nrow=1, ncol=2), widths=c(5,1), heights=4)
layout.show(2)
par(mar=c(3,3,3,1))
image(lon4,lat4,sst12[,,mean(1:146)],xlab='',ylab,col=c,breaks=breaks,
      axes=TRUE,xaxs='i',yaxs='i',asp=1, main=paste("Mean SST", dates12[1],"to",dates12[146]))

#example of how to add points to the map
tw<-filter(pa, collection_year==2012)
p<-filter(tw, uku_present1_absent0==1)
a<-filter(tw, uku_present1_absent0==0)
uku_p_lon<-as.vector(p$long_360)
uku_p_lat<-as.vector(p$latitude_start_dd)
uku_a_lon<-as.vector(a$long_360)
uku_a_lat<-as.vector(a$latitude_start_dd)
points(uku_p_lon,uku_p_lat, pch=18, cex=2) #uku present
points(uku_a_lon,uku_a_lat, pch=1, cex=2) #uku absent
#example of how to add a contour (this is considered a new plot, not a feature, so you need to use par(new=TRUE)) to overlay it on top of the SST map
par(new=TRUE)
contour(lon4,lat4,sst12[,,1],levels=20,xaxs='i',yaxs='i',labcex=0.8,vfont = c("sans serif", "bold"),axes=FALSE,asp=1)
#
# #plot color scale using 'image.scale' function from 'scale.R' script)
par(mar=c(3,1,3,3))
source('~/all_scripts_R/scale.R')
image.scale(sst11[,,mean(1:146)], col=c, breaks=breaks, horiz=FALSE, yaxt="n",xlab='',ylab='',main='SST')
axis(4, las=1)
box()
# #run end
#
#
#
#

#CTD Temp by Month#####
####map####
oahu_map
#points
larvahh<-larvah%>% mutate(uku_pa=as.factor(uku_present1_absent0))
larvahh<-rename(larvahh, max=max_temp)
larvahh<-rename(larvahh, min=min_temp)
larvahh<-pivot_longer(larvahh, cols=c(max, min),names_to="min_or_max", values_to = "temp_range")
larvahh<-mutate(larvahh, uku_density= (number_of_uku_per_Vm.3/V_in_m.3))
larvahh<-mutate(larvahh, length_mm=as.numeric(length_mm))
larvahh<-mutate(larvahh, latitude_start_dd=as.numeric(latitude_start_dd))
larvahh<-mutate(larvahh, longitude_start_dd=as.numeric(longitude_start_dd))
larvahh<-mutate(larvahh, as.factor(sampling_method))
larvahh<-mutate(larvahh, collection_month=as.numeric(collection_month))
larvahh<-mutate(larvahh, collection_year=as.numeric(collection_year))
larvahh$spawn_peak<-6
larvahh<-larvahh %>%mutate(time_since_peak_spawn=collection_month-spawn_peak)
p<-filter(larvahh, uku_pa==1)
a<-filter(larvahh, uku_pa==0)
#pall<-distinct(larvah,tow_id,.keep_all = TRUE) 
larvahh<-distinct(larvahh,tow_id,.keep_all = TRUE) 
shapes<-c(1,18)
#define a color palette
jet.colors <- colorRampPalette(c("blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
c <- jet.colors(n)
larvah<-larvah %>% mutate(named_month=month.name[collection_month])
#larvah<-larvah %>% mutate(named_month=(factor(levels=c('April','June','July','August','September',"October"))))
library(sp) #already in raster
library(raster)# error
library(ggplot2)
library(scales)
library(rgdal)
library(marmap)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggnewscale)
world<-ne_countries(scale="medium", returnclass = "sf")
oahu_raster <- raster(file.path("inputs/all_hi_bathy.tiff"))
oahu_df <- fortify(as.bathy(oahu_raster))
str(oahu_df)
mhi_map <- ggplot(data=world) +
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +labs(fill = "Depth (m)")+
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,1000))+new_scale_fill()+
  scale_fill_continuous(labels = scales::label_number(scale = 1000, suffix = "k"))+
  theme_bw()+geom_sf()+coord_sf(xlim=c(-154.9,-161.2), ylim=c(19, 22.2))+
  guides(fill="none")

p1<-mhi_map+geom_point(data=larvah, mapping=aes(y=latitude_start_dd,
                                                x=longitude_start_dd, 
                                                color=this_temp, 
                                                size=3,
                                                shape=as.factor(uku_present1_absent0)))+
  scale_shape_manual(values=shapes)+
  facet_wrap(~factor(named_month, levels=c('April','June','July','August','September',"October")), ncol=2)+
  theme(legend.position = "right",axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_text(size=10),legend.title= element_text(size=20),
        legend.text= element_text(size=15),legend.box = "vertical",
        strip.text.x=element_text(size=20),plot.margin = margin(0.1,0.1,0.1,0.1, "inches"))+
  labs(color="Temperature (?C)", shape="Larvae\npresent\nor absent")+
  scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 90")+
  #guides(size="none",shape="none", color="none", fill="none")
  guides(shape=guide_legend(override.aes = list(size = 10), order=2),size="none")
p1
ggsave("p1.png", height =5, width =6.5, units = "in")

#oahu_inset

oahu_map <- ggplot(data=world)+
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradient(high = "lightskyblue1", low = "cornflowerblue",limits=c(-6000,1000))+new_scale_fill()+
  theme(panel.background=element_blank(), panel.grid.major.x=element_line())+guides(fill="none")+
  theme_bw()+geom_sf()+coord_sf(crs = oahu_raster@crs, xlim=c(-157.5,-158.4),ylim=c(21.2,21.75))
oahualua<-filter(larvah, Near_Island=="Oahu")
oahualua<-filter(oahualua, cruise_number!="")
oahalua<-filter(oahualua, longitude_start_dd!=(-157.91333333333))

inset<-oahu_map+geom_point(data=larvah, mapping=aes(y=latitude_start_dd,
                                                    x=longitude_start_dd, 
                                                    color=this_temp, 
                                                    size=2,
                                                    shape=as.factor(uku_present1_absent0)))+
  scale_shape_manual(values=shapes)+
  facet_wrap(~factor(named_month, levels=c('April','June','July','August','September',"October")), ncol=3)+
  theme(legend.position = "none",axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_blank(),legend.box = "horizonal")+
  labs(color="Temperature (?C)", shape="Larvae\npresent\nor absent")+
  #scale_color_viridis(option="magma") #switched out 9/30 to allow for graysclae
  scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 90")+
  theme(legend.position = "right",axis.title = element_blank(),plot.margin = margin(0.1,0.1,0.1,0.1, "inches"),
        axis.ticks = element_blank(),axis.text = element_text(size=10),legend.title= element_text(size=20),
        legend.text= element_text(size=15),legend.box = "vertical",
        strip.text.x=element_text(size=20))+guides(size="none",shape="none", color="none", fill="none")#=guide_legend(override.aes = list(size = 10), order=2))
inset

library(patchwork)
p1/inset+plot_annotation(tag_levels="A")

library(gtable)
library(cowplot)
#w
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}
plot_grid(shift_legend(p1))

####salinity####
oahu_map+geom_point(data=larvah, mapping=aes(y=latitude_start_dd,
                                             x=longitude_start_dd, 
                                             color=sal_at_depth, 
                                             size=2,
                                             shape=as.factor(uku_present1_absent0)))+
  scale_shape_manual(values=shapes)+
  theme(legend.position = "bottom",axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_blank(),legend.box = "horizontal")+
  labs(color="Salinity", shape="Larva present (1) or absent(0)")+
  scale_color_gradientn(colors=c("lightblue1","royalblue2","midnightblue"),na.value="gray 90")

####oahu inset####
oahu_map <- ggplot() +
  coord_sf(crs = oahu_raster@crs, xlim=c(-157.5,-158.7),ylim=c(21.2,21.75))+
  geom_raster(data = oahu_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradientn(colours = c(grey(50/100), grey(80/100)),values = rescale(c(min(oahu_df$z, na.rm = TRUE), 0,  0.0001, max(oahu_df$z, na.rm = TRUE))), na.value = "transparent",guide = "none")+
  theme(panel.background=element_blank(), 
        panel.grid.major.x=element_line())
oahualua<-filter(pa, Near_Island=="Oahu")
oahualua<-filter(oahualua, cruise_number!="")
labell = "White color indicates NA values for temperature"
p2<-oahu_map+geom_point(data=maxt, mapping=aes(y=latitude_start_dd,
                                               x=longitude_start_dd, 
                                               color=temp_range, 
                                               size=2,
                                               shape=as.factor(uku_present1_absent0)))+
  scale_shape_manual(values=shapes)+
  theme(legend.position = "bottom",axis.title = element_blank(),
        axis.ticks = element_blank(),axis.text = element_blank(),legend.box = "horizontal")+
  labs(color="Temperature", shape="Larva present (1) or absent(0)")+
  facet_wrap(~collection_month)+
  scale_color_gradientn(colors=c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),na.value="gray 90")
p2
plot_grid(shift_legend(p2))

#Depth by family######
#depth as a univariate model###
model<-glm(mb$uku_pa~(mb$MAXIMUM_DEPTH),family=binomial)
plot(model)
qqnorm(resid(model)) 
plot(resid(model)~fitted(model))
hist(resid(model))
plot(cooks.distance(model),type="h")
summary(model) 
anova(model)

library(tidyverse)
mb<-read.csv("inputs/cast_data_with_coord_updated_DD.csv") #this is the MB data from inport with the coordinates that I added based on the station number from the PDFs in the data parcel from Inport
str(mb) #this is the framework to which 8504 and 8604 data will be appended
summary(mb$CRUISE_NUMBER)
mb$cruise_id<-gsub("_","",mb$cruise_id)
mb$tow_id<-gsub("TC_8","TC8",mb$tow_id)
mb<-mb %>% unite(col="moc_id",cruise_id, STATION_NUMBER,net_number, sep="_", remove=F) 
#this creates a consistent moc and cruise id which can be used across sheets
mb$moc_id
mb<- mb %>%unite(col="tow_id",cruise_id, STATION_NUMBER, sep="_", remove=F)
mb<-mb%>%mutate(uku_pa=ifelse(mb$SPECIES_CODE=="8554380401",1,0))
lutj<-filter(mb, grepl("8554380",SPECIES_CODE))
lutj<-lutj %>% mutate(spp_name=as.factor(ifelse(lutj$SPECIES_CODE==8554380401, "Aprion virescens",
                                                ifelse(lutj$SPECIES_CODE==8554380500, "Pristipomoides spp.",
                                                       ifelse(lutj$SPECIES_CODE== 8554380600, "Etelis spp.",
                                                              ifelse(lutj$SPECIES_CODE==8554380700, "Lutjanus spp.",
                                                                     ifelse(lutj$SPECIES_CODE==8554380705, "Lutjanus kasmira",
                                                                            ifelse(lutj$SPECIES_CODE==8554380100, "Symphysanodon spp.",
                                                                                   "Lutjanidae"))))))))
lutj<-lutj %>% mutate(MAX_D=as.numeric(MAXIMUM_DEPTH))
lutj<-lutj %>% mutate(CRUISE=as.numeric(CRUISE_NUMBER))
lutj<-lutj %>% mutate(fish_dens=(NUMBER_SPECIES/SAMPLE_VOLUME))
mlutj<-lutj %>%
  group_by(spp_name,MAXIMUM_DEPTH) %>%
  summarise(across(fish_dens, mean))
slutj<-lutj %>%
  group_by(spp_name,MAXIMUM_DEPTH,CRUISE_NUMBER) %>%
  summarise(across(fish_dens, mean))

nlutj<- lutj %>%
  mutate(day_night=ifelse(LOCATION_CODE_4==1,"day","night"))
nlutj<-nlutj %>%
  dplyr::group_by(spp_name,day_night,MAXIMUM_DEPTH) %>%
  dplyr::summarise(across(fish_dens, mean, .groups=c(spp_name,day_night,MAXIMUM_DEPTH)))

nlutj<-nlutj %>%
  dplyr::group_by(spp_name,day_night,MAXIMUM_DEPTH) %>%
  dplyr::summarise(across(fish_dens, mean, .groups=c(spp_name,day_night,MAXIMUM_DEPTH)))

mlutjnosymp<-filter(nlutj, spp_name!="Symphysanodon spp.")

fi10<-mlutjnosymp %>%
  dplyr::group_by(spp_name,MAXIMUM_DEPTH) %>%
  dplyr::summarise(across(fish_dens, mean, .groups=c(spp_name,MAXIMUM_DEPTH)))

slutj<- slutj %>% mutate(CRUISE_month=ifelse(CRUISE_NUMBER=="8504", "September", ifelse(CRUISE_NUMBER=="8505", "December", ifelse(CRUISE_NUMBER=="8602", "April", "June"))))
ld<-ggplot(mlutjnosymp,aes(color=as.factor(spp_name),y=as.factor(MAXIMUM_DEPTH*-1),x=fish_dens))+
  geom_point(size=5)+facet_grid(vars(CRUISE_month))+scale_color_viridis_d()+labs(y="Maximum net depth (m)", x="Specimen density per sample volume",color='Species name',strip="Cruise Number")
plot1<-ld+geom_path(aes(group=as.factor(spp_name),y=as.factor(MAXIMUM_DEPTH*-1),x=fish_dens))+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=15),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",plot.title=element_text(face="italic",size=30))+
  guides(colour = "none")
plot1


#make new df for all species and all depths and then merge this existing df into it and have NA/s blanks fill with fish dens0
spp_name<-as_vector(list(rep(levels(mlutjnosymp$spp_name),9)))
MAXIMUM_DEPTH<-as_vector(list(rep(c(1,10,20, 30,40,50,60,70,80),each=7)))

with0s<-as.data.frame(cbind(spp_name,MAXIMUM_DEPTH))
with0s$MAXIMUM_DEPTH=as.numeric(with0s$MAXIMUM_DEPTH)
df<-left_join(with0s,fi10)
df<-df %>% mutate(fish_dens=ifelse((is.na(fish_dens)==F), df$fish_dens,0)) 
df<- df %>% filter(spp_name != "Symphysanodon spp.")
figure10<-ggplot(df,aes(y=(MAXIMUM_DEPTH*-1),x=fish_dens))+
  geom_point(size=5)+facet_wrap(vars(spp_name), ncol=3)+#scale_color_viridis_d()+
  geom_path(aes(group=as.factor(spp_name),y=MAXIMUM_DEPTH*-1,x=fish_dens))+
  labs(y="Maximum net depth (m)", x=expression("Specimen density per sample volume (individual/m"^3~")"))+ #"[W" ~ m^-2~"]"))
  theme(legend.title=element_text(size=15),legend.text=element_text(size=15),
        axis.text = element_text(size=10), axis.title = element_text(size=15),
        legend.direction = "vertical", legend.box = "vertical",strip.text.x=element_text(face="italic",size=15))+
  scale_x_continuous(breaks=seq(0, max(df$fish_dens)+0.5, 0.5))
figure10



library(gtable)
library(cowplot)
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}
plot_grid(shift_legend(plot2))


library(patchwork)
plot1 + plot2

ggplot(nlutj,aes(color=spp_name,y=(MAXIMUM_DEPTH*-1),x=fish_dens))+
  geom_point(alpha=0.3,size=3)+facet_grid(~day_night)+scale_color_viridis_d()+
  geom_path(aes(group=as.factor(spp_name),y=MAXIMUM_DEPTH*-1,x=fish_dens))+
  labs(y="Maximum net depth (m)", x="Specimen density per sample volume",color='Species name',strip="Cruise Number")

figure9<-ggplot()+geom_density(data=mlutjnosymp,aes(color=day_night,  y=MAXIMUM_DEPTH*-1, size=1))+
  scale_color_manual(values=c("cornflowerblue","darkblue"))+facet_wrap(~spp_name, ncol=4)+
  labs(x="Kernel density", y="Maximum net depth (m)",color='Time caught',strip="Cruise Number")+
  theme(legend.title=element_text(size=15),legend.text=element_text(size=15),
        axis.text = element_text(size=10), axis.title = element_text(size=15),
        legend.direction = "vertical", legend.box = "vertical",
        strip.text.x=element_text(face="italic",size=15))+
  guides(size="none", color = guide_legend(override.aes = list(fill = c("cornflowerblue","darkblue"))))

figure9
fig9<-plot_grid(shift_legend(figure9))
fig9
library(patchwork)
#report figs9 and 10######
layout <-
  "
AAA
AAA
BBB
BBB
"
pg16<-fig9 / figure10+ plot_layout(design = layout)+plot_annotation(tag_levels="A") &
  theme(plot.tag = element_text(size =15))
pg16
#ggsave("pg16.png", width = 6, height = 9, units = "in")

#######
pp_hist<-ggplot(mlutjnosym,  aes(fill=day_night,  y=MAXIMUM_DEPTH*-1))+
  #geom_col()+
  geom_histogram()+
  #geom_histogram(position = position_dodge(width = 5), binwidth = 10, alpha=0.5)+
  scale_fill_manual(values=c("cornflowerblue","darkblue"))+facet_wrap(~spp_name)+
  labs(x="histogram, bin width 10, position dodge 5", y="Maximum net depth (m)",color='Time caught')
plot_grid(shift_legend(pp_hist))

lutj<- lutj %>% mutate(day_night=ifelse(LOCATION_CODE_4==1,"day","night"))
lutj_hist<-ggplot(mlutjnosym,  aes(fill=day_night,  y=MAXIMUM_DEPTH*-1))+
  #geom_col()+
  geom_histogram()+
  #geom_histogram(position = position_dodge(width = 5), binwidth = 10, alpha=0.5)+
  scale_fill_manual(values=c("cornflowerblue","darkblue"))+facet_wrap(~spp_name)+
  labs(x="histogram, bin width 30", y="Maximum net depth (m)",color='Time caught')
plot_grid(shift_legend(lutj_hist))


uku_lutj<-filter(mlutjnosym,spp_name=="Aprion virescens")
ggplot(uku_lutj,  aes(color=day_night,  y=MAXIMUM_DEPTH*-1))+
  geom_density(size=2)+scale_color_manual(values=c("cornflowerblue","darkblue"),guide = "none")+#facet_grid(~spp_name)+
  labs(x="Kernel density", y="Maximum net depth (m)",color='Time caught',title="Uku/ Aprion virescens")+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",plot.title=element_text(face="italic",size=30))+
  ylim(-80,0)

paka_lutj<-filter(nlutj,spp_name=="Pristipomoides spp.")
ggplot(paka_lutj,  aes(color=day_night,  y=MAXIMUM_DEPTH*-1))+
  geom_density(size=2)+scale_color_manual(values=c("cornflowerblue","darkblue"),guide = "none")+#facet_grid(~spp_name)+
  labs(x="Kernel density", y="Maximum net depth (m)",color='Time caught', title = "'Opakapaka/ Pristipomoides spp.")+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",plot.title=element_text(face="italic",size=30))

kazzy_lutj<-filter(nlutj,spp_name=="Lutjanus kasmira")
ggplot(kazzy_lutj,  aes(color=day_night,  y=MAXIMUM_DEPTH*-1))+
  geom_density(size=2)+scale_color_manual(values=c("cornflowerblue","darkblue"))+#facet_grid(~spp_name)+
  labs(x="Kernel density", y="Maximum net depth (m)",color='Time caught', title = "Ta'ape/ Lutjanus kasmira" )+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.direction = "vertical", legend.box = "vertical",plot.title=element_text(face="italic",size=30))


#histograms#######
poster_pa<-larvah %>% distinct((tow_id),.keep_all = TRUE)
poster_pa<-poster_pa %>% filter(site!= "Kahe Point")
plot(poster_pa$uku_present1_absent0~poster_pa$this_temp)
pg13b<-ggplot(poster_pa, aes(fill=as.factor(uku_present1_absent0), x=this_temp))+geom_histogram()+scale_fill_manual(values=c("hotpink1","#7F0000"))+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=40),legend.position = "bottom")+
  labs(x="Maximum temperature (?C)", y="Number of trawls", fill="Larva present (1) or absent(0)")
pg13b

pinktemp<-ggplot(poster_pa, aes(fill=as.factor(uku_present1_absent0), x=this_temp))+geom_histogram()+scale_fill_manual(values=c("hotpink1","#7F0000"))+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),legend.position = "right")+
  labs(x="Maximum temperature (?C)", y="Number of trawls", fill="Larvae present\n (1) or absent(0)")


#in report page 13#####
pg13<-plot.gam(model,select=1, ylab="Conditional effect of temperature\n on uku occurrence", 
               xlab="Observed temperature (?C)",cex.axis=2.5, cex.lab=3, shade=T, trans=function(x)exp(x)/(1+exp(x)))#(exp(model$linear.predictors)/(1+exp(model$linear.predictors))))
+pg13b+ plot_layout(design = layout)+plot_annotation(tag_levels="A") &
  theme(plot.tag = element_text(size =15))

#######
p<-filter(poster_pa, pa==1)
summary(p$this_temp)
j<-as.numeric(p$max_temp)
min(j)
p$mean_sal
purplesal<-ggplot(larvah, aes(fill=as.factor(uku_present1_absent0), x=this_sal))+geom_histogram()+scale_fill_manual(values=c("mediumpurple1","mediumpurple4"))+
  theme(legend.title=element_text(size=20),legend.text=element_text(size=20),strip.text=element_text(size=20),
        axis.text = element_text(size=20), axis.title = element_text(size=20),legend.position = "right")+
  labs(x="Surface salinity (PSU)", y="Number of trawls", fill="Larvae present\n (1) or absent(0)")+
  xlim(34.5,35.5)

library(patchwork)
pinktemp/purplesal+plot_annotation(tag_levels=c("A","B"))

ggplot(larvah, aes(x=log(Dist2Shore_m), y=log(volume), fill=as.factor(uku_present1_absent0)))+
  geom_col()+scale_fill_manual(values=c("grey", "black"))


#univariate pearson corellations as further diagnostics####
cor.test(larvah$this_temp, larvah$uku_present1_absent0,method="pearson") 
cor.test(larvah$collection_month, larvah$uku_present1_absent0,method="pearson")
cor.test(larvah$time_since_peak_spawn, larvah$uku_present1_absent0,method="pearson") 
cor.test(larvah$collection_depth_m_max, larvah$uku_present1_absent0,method="pearson") 
cor.test(larvah$Dist2Shore_m, larvah$uku_present1_absent0,method="pearson") 
cor.test(log(larvah$Dist2Shore_m), larvah$uku_present1_absent0,method="pearson") 
cor.test(larvah$this_sal,larvah$uku_present1_absent0,method="pearson") 
>>>>>>> f7f366ff7efa234ece1dbd04805700fc801f8031
