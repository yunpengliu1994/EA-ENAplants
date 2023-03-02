#######################################
## checklist of ENA and EA naturalized#
## sp from EA-ENA genera ##############
#######################################
	library(data.table)	
	#EA-ENA disjuct
	disjuct=read.csv("Wen.annurev.ecolsys.EA-ENAchecklist.csv")

	#Kleunen et al. nature,(2015) 525(7567): 100-103�? https://doi.org/10.1002/ecy.2542
	region.glonaf=read.csv("Region_gloNAF.csv")
	taxa.glonaf=as.data.frame(fread("Taxon_gloNAF.csv"))
	glonaf=cbind(taxa.glonaf[,c("genus","standardized_name","family_tpl","status")],region.glonaf[match(taxa.glonaf$region_id,region.glonaf$region_id),c("tdwg4_name","tdwg1_name","country")])
	ea.glonaf=subset(glonaf,glonaf$country%in%c("China","Hong Kong","Macao","Taiwan (Province of China)","Japan","Korea (the Republic of)","Korea (the Democratic People's Republic of)","Mongolia")|(glonaf$country%in%"Russian Federation (the)"&glonaf$tdwg1_name%in%"Asia-Temperate"))
	ena.glonaf=subset(glonaf,glonaf$tdwg1_name%in%"Northern America")

	#Heberling et al. Global Ecology and Biogeography,(2017) 26, 447�?458
	geb12551=read.csv("EA invasive geb12551.csv")[,c("Genus","Scientific.Name","family.EAS","invader.China","Data.Source.China.Invasive","Data.Source.China.Naturalized","invader.NKorea2","invader.Korea","Data.Source.Korea","Data.Source.Nkorea")]
	geb.t1a=data.frame(subset(geb12551[,c(1:3,6)],geb12551$invader.China==0),status="alien")
	colnames(geb.t1a)[4]="invader.datascource"
	geb.t1a[geb.t1a[,4]!="","status"]="naturalized"
	geb.t1b=data.frame(subset(geb12551[,c(1:3,5)],geb12551$invader.China==1),status="invasive")
	colnames(geb.t1b)[4]="invader.datascource"
	geb.t1=rbind(geb.t1a,geb.t1b)
	geb.t2=rbind(data.frame(subset(geb12551[,c(1:3,9)],geb12551$invader.Korea==0),status="alien"),data.frame(subset(geb12551[,c(1:3,9)],geb12551$invader.Korea==1),status="invasive"))
	colnames(geb.t2)[4]="invader.datascource"
	geb.t3=subset(geb12551[,c(1:3,10,7)],!geb12551$invader.NKorea2%in%"")
	colnames(geb.t3)[4:5]=c("invader.datascource","status")
	ea.geb=unique(rbind(data.frame(geb.t1,country="China"),data.frame(geb.t2,country="Korea (the Republic of)"),data.frame(geb.t3,country="Korea (the Democratic People's Republic of)")))
	ea.geb[ea.geb$invader.datascource=="","invader.datascource"]="Heberling2017GEB"
	#https://www.invasiveplantatlas.org/distribution.cfm
	us.alien=read.csv("The Invasive Plant Atlas of the United States.csv")

	#Jason D. Fridley(2008)PlosOne: Of Asian Forests and European Fields: Eastern U.S. Plant Invasions in a Global Floristic Context
	us.alien2=read.csv("ENA alien2008.csv")[,c(2,4:6,15)]
	status=ifelse(us.alien2$Invasive.in.ENA%in%"","naturalized","invasive")
	us.alien2=data.frame(us.alien2[,1:4],status=status,invader.datascource=us.alien2[,5])
	us.alien2[us.alien2$invader.datascource=="","invader.datascource"]="Fridley2008PlosOne"
	#中国外来入侵植物的等级划分与地理分布格局分析 Yan et al. 2014, Biodiversity Science, 22, 667-676
	#入侵等级1-3定为invasive�?4-5定为alien
	#1�?: 恶性入侵植物。在国家层面已经对经济或生态环境造成巨大损失与严重影�?, 入侵范围超过1个以上自然地理区域�?
	#2�?: 严重入侵植物。在国家层面上对经济和生态环境造成较大损失或明显影�?, 并且至少�?1个以上自然地理区域分布�?
	#3�?: 局部入侵植物。在1个或1个以上自然地理区域分布并造成局部危�?, 但目前没有造成国家层面的大规模危害�?
	#4�?: 一般入侵植物。不论入侵范围广泛与�?,根据其生物学和生态学特性已经确定其危害不大或不明显, 并且难以形成新的入侵发展趋势�?
	#5�?: 有待观察类。此类物种研究不够充�?, 主要是一些出现时间短或最新报道的、目前了解不深入而无法确定未来发展趋势的物种�?
	china.alien=read.csv("Chinese invasive plants.csv")
	status=ifelse(china.alien$Invasive.level<=3,"invasive","naturalized")
	china.alien=cbind(china.alien,status,invader.datascource="Yan2014Biodiversity Science")

	#alien combined
	us.glonaf=subset(ena.glonaf,ena.glonaf$country%in%"United States of America (the)")
	us.comb=rbind(unique(data.frame(Family=us.alien2$Family,Genus=us.alien2$Genus,Species=us.alien2$Scientific.Name,status=us.alien2$status,country="United States of America (the)",datascource="Fridley2008PlosOne",invader.datascource=us.alien2$invader.datascource)),
	unique(data.frame(Family=us.glonaf$family_tpl,Genus=us.glonaf$genus,Species=us.glonaf$standardized_name,status="",country=us.glonaf$country,datascource="Kleunen2015nature",invader.datascource="")),
		unique(data.frame(Family="",Genus=us.alien$Genus,Species=us.alien$Scientific.Name2,status="",country="United States of America (the)",datascource="invasiveplantatlas.org",invader.datascource=""))	
	)
	us.data=tapply(us.comb$datascource,us.comb$Species,paste,collapse=",")
	us.data2=tapply(us.comb$invader.datascource,us.comb$Species,paste,collapse=",")
	us.comb2=cbind(us.comb[match(names(us.data),us.comb$Species),-c(6:7)],datascource=us.data,invader.datascource=us.data2)

	china.glonaf=subset(ea.glonaf,ea.glonaf$country%in%"China")
	china.geb=subset(ea.geb,ea.geb$country%in%"China")
	china.comb=rbind(unique(data.frame(Family=china.alien$Family,Genus=china.alien$Genus,Species=china.alien$Species,status
	=china.alien$status,country="China",datascource="Yan2014Biodiversity Science",invader.datascource="Yan2014Biodiversity Science")),
		unique(data.frame(Family=china.geb$family.EAS,Genus=china.geb$Genus,Species=china.geb$Scientific.Name,status=china.geb$status,country="China",datascource="Heberling2017GEB",invader.datascource=china.geb$invader.datascource)),
		unique(data.frame(Family=china.glonaf$family_tpl,Genus=china.glonaf$genus,Species=china.glonaf$standardized_name,status
	="",country=china.glonaf$country,datascource="Kleunen2015nature",invader.datascource=""))		
	)
	china.comb[china.comb$datascource=="Heberling2017GEB"&china.comb$status!="invasive",]=""
	china.data=tapply(china.comb$datascource,china.comb$Species,paste,collapse=",")
	china.data2=tapply(china.comb$invader.datascource,china.comb$Species,paste,collapse=",")
	china.comb2=cbind(china.comb[match(names(china.data),china.comb$Species),-c(6:7)],datascource=china.data,invader.datascource=china.data2)
	tmp=subset(china.comb2,china.comb2$status=="")
	china.geb2=china.geb[china.geb$status!="invasive",]
	tmp2=china.geb2[match(tmp$Species,china.geb2$Scientific.Name),c("Scientific.Name","status","invader.datascource")]
	tmp[,c("status","invader.datascource")]=tmp2[,-1]
	tmp[is.na(tmp[,"status"]),c("status","invader.datascource")]=c("","")
	china.comb3=rbind(subset(china.comb2,china.comb2$status!=""),tmp)

	#NKorea
	Nkorea.geb=subset(ea.geb,ea.geb$country%in%"Korea (the Republic of)")
	Nkorea.glonaf=subset(ea.glonaf,ea.glonaf$country%in%"Korea (the Republic of)")
	Nkorea.comb=rbind(unique(data.frame(Family=Nkorea.geb$family.EAS,Genus=Nkorea.geb$Genus,Species=Nkorea.geb$Scientific.Name,status=Nkorea.geb$status,country="Korea (the Republic of)",datascource="Heberling2017GEB",invader.datascource=Nkorea.geb$invader.datascource)),
		unique(data.frame(Family=Nkorea.glonaf$family_tpl,Genus=Nkorea.glonaf$genus,Species=Nkorea.glonaf$standardized_name,status
	="",country="Korea (the Republic of)",datascource="Kleunen2015nature",invader.datascource=""))		
	)
	Nkorea.data=tapply(Nkorea.comb$datascource,Nkorea.comb$Species,paste,collapse=",")
	Nkorea.data2=tapply(Nkorea.comb$invader.datascource,Nkorea.comb$Species,paste,collapse=",")
	Nkorea.comb2=cbind(Nkorea.comb[match(names(Nkorea.data),Nkorea.comb$Species),-c(6:7)],datascource=Nkorea.data,invader.datascource=Nkorea.data2)

	#SKorea
	Skorea.geb=subset(ea.geb,ea.geb$country%in%"Korea (the Democratic People's Republic of)")
	Skorea.glonaf=subset(ea.glonaf,ea.glonaf$country%in%"Korea (the Democratic People's Republic of)")
	Skorea.comb=rbind(unique(data.frame(Family=Skorea.geb$family.EAS,Genus=Skorea.geb$Genus,Species=Skorea.geb$Scientific.Name,status=Skorea.geb$status,country="Korea (the Democratic People's Republic of)",datascource="Heberling2017GEB",invader.datascource=Skorea.geb$invader.datascource)),
		unique(data.frame(Family=Skorea.glonaf$family_tpl,Genus=Skorea.glonaf$genus,Species=Skorea.glonaf$standardized_name,status
	="",country="Korea (the Democratic People's Republic of)",datascource="Kleunen2015nature",invader.datascource=""))		
	)
	Skorea.data=tapply(Skorea.comb$datascource,Skorea.comb$Species,paste,collapse=",")
	Skorea.data2=tapply(Skorea.comb$invader.datascource,Skorea.comb$Species,paste,collapse=",")
	Skorea.comb2=cbind(Skorea.comb[match(names(Skorea.data),Skorea.comb$Species),-c(6:7)],datascource=Skorea.data,invader.datascource=Skorea.data2)

	#combined all
	glonaf.ea.ena=rbind(ea.glonaf,ena.glonaf)
	other.glonaf=subset(glonaf.ea.ena,!glonaf.ea.ena$country%in%c("China","United States of America (the)","Korea (the Democratic People's Republic of)","Korea (the Republic of)"))
	re=rbind(china.comb3,us.comb2,Skorea.comb2,Nkorea.comb2,
		unique(data.frame(Family=other.glonaf$family_tpl,Genus=other.glonaf$genus,Species=other.glonaf$standardized_name,status="",country=other.glonaf$country,datascource="Kleunen2015nature",invader.datascource="")))
	re.ea.ena0=cbind(re,disjuct=disjuct[match(re$Genus,disjuct$Genus),c("DisjuctDataScources")])

	#correct sp names using TPL (Date 2015)
	library(data.table)
	tpl.ac=unique(as.data.frame(fread("TPL14Ac.csv",header=T)))
	tpl.sy=unique(as.data.frame(fread("TPL14Sy.csv",header=T)))
	corrt.1=cbind(re.ea.ena0,Accepted_ID=tpl.ac[match(re.ea.ena0$Species,tpl.ac$Acname),"ID"])
	unmat=subset(corrt.1,is.na(corrt.1$Accepted_ID))
	corrt.2=cbind(unmat[,-9],Accepted_ID=tpl.sy[match(unmat$Species,tpl.sy$Syname),"Accepted ID"])
	corrt.3=rbind(subset(corrt.1,!is.na(corrt.1$Accepted_ID)),corrt.2)
	corrt.4=cbind(corrt.3,tpl.ac[match(corrt.3$Accepted_ID,tpl.ac$ID),c("Major group","Family","Genus","Acname","Taxonomic status in TPL","Confidence level")])
	colnames(corrt.4)[1:3]=c("Family_ori","Genus_ori","Species_ori");colnames(corrt.4)[13]="Species_TPL"
	unmat2=subset(corrt.4,is.na(corrt.4$Family))
	corrt.5=cbind(unmat2[,1:9],tpl.ac[match(unmat2$Genus_ori,tpl.ac$Genus),c("Major group","Genus","Family")],unmat2[,13:15])
	corrt=rbind(subset(corrt.4,!is.na(corrt.4$Family)),corrt.5)
	#divide EA and ENA
	re.ea=subset(corrt,corrt$country%in%c("China","Korea (the Republic of)","Korea (the Democratic People's Republic of)","Hong Kong","Macao","Japan","Russian Federation (the)","Taiwan (Province of China)","Mongolia"))
	re.ena=subset(corrt,corrt$country%in%c("Canada","Greenland","Mexico","Saint Pierre and Miquelon","United States of America (the)"))
	re.ea.ena=rbind(cbind(alien.reg="EAsia",re.ea),cbind(alien.reg="NAma",re.ena))
	write.csv(re.ea.ena,"alien.EA-ENA.csv")
	alien.disjuct=subset(re.ea.ena,!is.na(re.ea.ena$disjuct))
	write.csv(alien.disjuct,"alien.EA-ENA.disjuct.csv")
	#手动补充了invasiveplantatlas.org以及刘全儒等中国外来入侵植物志的status；用usda官网记录进一步筛选，部分glonaf的物种被usda定义为native
	#remove species which are recorded as native in USDA and then add introduced year based on Mannual of woody landscape plants: their identification, ornamental characteristics, culture, propagation and uses. Fifth Edition. Dirr, M.A.
	#remove species which mainly distributed in WNA or Mexico
	#finally get the naturalized species checklist EA-ENA.splist.csv (23 species in total)

#######################################
## corrected speceis names and extract#
## the distribution of native and #####
## naturalized species ################
#######################################
	##native distribution
	library(data.table)
	dis.ori=as.data.frame(fread("SpLevDis.csv"))[,-1]	
	splist1=unique(dis.ori$Species_E1)
	a=read.csv("EA-ENA.splist.csv") 
	splist2=unique(a$Species_TPL) 
	splist2[!splist2%in%splist1]
	dis.add=read.csv("Wisteria floribunda.Tropicos.csv") #add naturalized sp without native distribution data from tropicos
	dis=unique(rbind(dis.ori,dis.add))# only Wisteria x formosa did not shown in the datasets for native distribution
	#correct sp names using TPL (Date 2015)
	corrt.sp=function(dat,spcol){
		require(data.table)
		tpl.ac=unique(as.data.frame(fread("TPL14Ac.csv",header=T)))
		tpl.sy=unique(as.data.frame(fread("TPL14Sy.csv",header=T)))
		corrt.1=cbind(dat,Accepted_ID=tpl.ac[match(dat[,spcol],tpl.ac$Acname),"ID"])
		unmat=subset(corrt.1,is.na(corrt.1$Accepted_ID))
		corrt.2=cbind(unmat[,-dim(unmat)[2]],Accepted_ID=tpl.sy[match(unmat[,spcol],tpl.sy$Syname),"Accepted ID"])
		corrt.3=rbind(subset(corrt.1,!is.na(corrt.1$Accepted_ID)),corrt.2)
		corrt.4=cbind(corrt.3,tpl.ac[match(corrt.3$Accepted_ID,tpl.ac$ID),c("Family","Genus","Acname","Taxonomic status in TPL")])
		return(corrt.4)
	}
	dis2=unique(corrt.sp(dis,"Species_E2"))	
	splist1=unique(dis2$Acname)
	write.csv(dis2,"SpLevDis.csv")
	
	##naturalizd distribution: combine distribution with glonaf and add more distribution information from usda(https://plants.usda.gov/home/plantProfile?symbol=MAFR)
	region.glonaf=read.csv("Region_gloNAF.csv")
	taxa.glonaf=as.data.frame(fread("Taxon_gloNAF.csv"))
	glonaf=cbind(taxa.glonaf[,c("genus","standardized_name","family_tpl","status")],region.glonaf[match(taxa.glonaf$region_id,region.glonaf$region_id),c("OBJIDsic","tdwg4_name","tdwg1_name","country","island")])
	library(sp);library(maptools)
	shape.all=readShapeSpatial("area/NewMapUnit_Intersect2.shp")
	geo.table=unique(shape.all@data[,c("ADCODE99","NAME99","Lon","Lat","OBJIDsic","LAT_1","LON_1")])
	glonaf2=cbind(glonaf,geo.table[match(glonaf$OBJIDsic,geo.table$OBJIDsic),])
	
	alien.dis=read.csv("alien.us.dis.csv") #alien species dis in us from usda
	splist.alien.dis=unique(alien.dis$Species_E2)
	alien.dis2=unique(glonaf2[glonaf2$standardized_name%in%splist.alien.dis,c("standardized_name","ADCODE99")])
	colnames(alien.dis2)=c("Species_E2","Adcode99")
	alien.dis3=unique(rbind(alien.dis[,c("Species_E2","Adcode99")],alien.dis2))
	taxa=unique(alien.dis[,-5])
	alien.dis4=cbind(alien.dis3,taxa[match(alien.dis3$Species_E2,taxa$Species_E2),-3])
	write.csv(na.omit(alien.dis4),"alien.us.dis.V2.csv") #distribution of ENA naturalized species,updated 4/10/2022
	
#######################################
## DOWNLOAD AND CLEAN occurence DATA ##
#######################################
	# presearch and add some sym names which are not found in GBIF
	a=read.csv("EA-ENA.splist.csv")
	dis=na.omit(read.csv("SpLevDis.csv"))
	myspecies1=c(unique(dis[dis$Genus_E%in%unique(a$Genus),"Acname"]),"Wisteria formosa")	
	gbif_data <- rgbif::occ_data(scientificName = myspecies1, hasCoordinate = TRUE, limit = 1,decimalLatitude = "-12, 90")
	# check the number of records
	re2=c()
	for (i in myspecies1){
	re=data.frame(Acname=i,occ=gbif_data[[i]]$meta$count)
	re2=rbind(re2,re)
	}
	re2=cbind(splist,re2[match(splist$sp.gbif,re2$Acname),])
	write.csv(re2,"splist.gbif.csv")

	#Then remove the "x" in hybids and correct the spname for the species with 0 gbif record and do the formal search
	library(spocc)
	splist=read.csv("splist.gbif.csv")	
	date()
	occ_data=occ(query = as.character(splist$sp.gbif),from=c("gbif","bison","idigbio","inat"),limit=100000)
	date()# ca.8 h
	save(occ_data,file="occ_data.Rdata")
	
	#remove unwanted cols and combined the data into a dataframe
	load("occ_data.Rdata")
	reduce_columns <- function(spocc, fields){
	  diff1 <- setdiff(fields, colnames(spocc))
	  newframe <- data.frame(matrix(, nrow = 1 , ncol= as.numeric(length(diff1))))
	  colnames(newframe) <- diff1
	  same1=fields[!fields%in%diff1]
	  spocc_new <- cbind(spocc[,colnames(spocc)%in%same1], newframe)
	  spocc_new=spocc_new[,fields]
	  colnames(spocc_new)=c("spname","prov","key","basis","Lon","Lat","coordinateuncertainityInMeters","year","eventDate","continent","countryCode","country","province","county")
	  return(spocc_new)
	}
	fields.all=data.frame(
		gbif=c("name","prov","key","basisOfRecord","longitude","latitude","coordinateUncertaintyInMeters","year","eventDate","continent","countryCode","country","stateProvince","county"),
		bison=c("name","prov","occurrenceID","basisOfRecord","longitude","latitude","coordinateUncertaintyInMeters","year","date","continent","countryCode","country","stateProvince","county"),
		idigbio=c("name","prov","uuid","basisofrecord","longitude","latitude","coordinateuncertainty","year","eventdate","continent","countrycode","country","stateprovince","county"),                         
		inat=c("name","prov","uuid","basisOfRecord","longitude","latitude","coordinateUncertaintyInMeters","year","created_at","continent","countryCode","country","stateProvince","county")
	)
	
	sp=as.character(splist$sp.gbif)
	prov.all=c("gbif","bison","idigbio","inat")
	for (i in 1:length(prov.all)) names(occ_data[[prov.all[i]]]$data)=sp
	
	for (i in 1:length(prov.all)){
		dat.tmp=c()
		prov=prov.all[i]
		for(j in 1:length(sp)){
			spname=sp[j]
			dat=occ_data[[prov]]$data[[spname]]
			if (nrow(dat)==0) next;
			dat.new=cbind(Acname=spname,reduce_columns(dat,fields.all[,prov]))			
			dat.tmp=rbind(dat.tmp,dat.new)	
		}
		write.csv(dat.tmp,paste("dat",prov,"csv",sep="."))
		rm(dat.tmp)
	}
	
	#combined with the pervious downloaded gbif data
	# library(data.table)
	# gbif1=as.data.frame(fread("GBIF record_cleaned.csv"))[,c("Acname","sp.gbif","key","basisOfRecord","decimalLongitude","decimalLatitude","coordinateuncertaintyinmeters","year","eventDate")]
	# gbif2=data.frame(gbif1[,c(1:2)],prov="gbif",gbif1[,c(3:9)],continent=NA,countryCode=NA,country=NA,province=NA,county=NA)
	# colnames(gbif2)=c("Acname","spname","prov","key","basis","Lon","Lat","coordinateuncertainityInMeters","year","eventDate","continent","countryCode","country","province","county")
	# gbif3=as.data.frame(fread("dat.gbif.csv"))[,-1]
	# gbif4=rbind(gbif3,gbif2)
	# write.csv(gbif4,"dat.gbif.csv")
		
	#collect the record year
	#capital the first letters and then match the country, state, county with the state, state code and county in ENA
	#match both lat/lon and geo locations wiith ENA and then get the earlist year for each species 
	library(data.table)
	dat.all=c()
	for (i in 1:length(prov.all)){
		tmp=as.data.frame(fread(paste("dat",prov.all[i],"csv",sep=".")))[,-1]
		dat.all=rbind(dat.all,unique(tmp[,-c(2:4)]))
	}
	year1=as.numeric(substr(as.character(dat.all$eventDate),1,4))
	dat.all=cbind(dat.all,year2=year1)
	dat.all[dat.all$year2<1000|is.na(dat.all$year2),"year2"]=NA
	dat.all[is.na(dat.all$year2)&(!is.na(dat.all$year)),"year2"]=dat.all[is.na(dat.all$year2)&(!is.na(dat.all$year)),"year"]
	
	dat.all[dat.all$country%in%"united states","country"]="United States of America"
	dat.all[dat.all$country%in%c("china: people's republic of china","china"),"country"]="China"
	dat.all[dat.all$country%in%c("méxico","mexico"),"country"]="Mexico"
	dat.all[dat.all$country%in%c("canada","canadá"),"country"]="Canada"
	dat.all[dat.all$continent%in%c("NORTH_AMERICA","northern america","north america - neotropics"),"continent"]="NORTH AMERICA"
	dat.all[,"continent"]=toupper(dat.all[,"continent"]);dat.all[,"country"]=toupper(dat.all[,"country"])
	dat.all[,"county"]=toupper(dat.all[,"county"]);	dat.all[,"province"]=toupper(dat.all[,"province"])
	dat.all[,"countryCode"]=toupper(dat.all[,"countryCode"])
	dat.all[dat.all$Acname%in%"Wisteria formosa","Acname"]="Wisteria x formosa"
	# CLEAN THE DATASET
	library(scrubr)
	nrow(dat.all) #1738238 records
	dat.all2 <- coord_incomplete(coord_impossible(coord_unlikely(dat.all)))
	nrow(dat.all2) #1568687 records 
	dat.all2=subset(dat.all2,(dat.all2$Lon>=-180&dat.all2$Lon<=-52&dat.all2$Lat>7.2)|(dat.all2$Lon>=26&dat.all2$Lon<=180&dat.all2$Lat>-11))
	nrow(dat.all2) #979930 records 	
	write.csv(dat.all2,"occ_data.clean.csv")
	
	# find the earlist record
	geo=read.csv("list_of_geo_NA.csv")
	namlst=unique(c(geo$Country,geo$states,geo$County))
	dat.na=subset(dat.all,(dat.all$continent%in%"NORTH AMERICA"|dat.all$country%in%c("NORTH AMERICA",namlst)|
				dat.all$countryCode%in%unique(geo$CountryCode)|dat.all$province%in%c("NORTH AMERICA",namlst)|
				dat.all$county%in%c("NORTH AMERICA",namlst)|(dat.all$Lon>=-168.1&dat.all$Lon<=-52&dat.all$Lat>7.2))&
				!is.na(dat.all$year2))
	EarlyRecordyear=tapply(dat.na$year2,dat.na$Acname,min)
	a=read.csv("EA-ENA.splist.csv")[,-1]	
	a.occ=cbind(a,Occyear=EarlyRecordyear[match(a$Species_TPL,names(EarlyRecordyear))])
	write.csv(a.occ,"EA-ENA.splist.csv")
	
	# map the cleaned occurrence data:
	library(maps)
	map("world", xlim = range(dat.all2$Lon), ylim = range(dat.all2$Lat))
	points(dat.all2[ , c("Lon", "Lat")], pch = 20, cex = 0.5, col = "turquoise")
	
############################################################################################
## using GBIF data to download the spatial resolution of the state-level distribution map ##
############################################################################################
#occCite package in R for citing the data scources Hannah Owens 2021 Ecography
#### using GBIF data to download the spatial resolution of the state-level distribution map generated by USDA (for naturalized range)and PKU database (for native range) -----
	### DATA 1: GBIF data (4/10/2022)	
	library(data.table)
	gbif0=as.data.frame(fread("occ_data.clean.csv"))[,-1]
	# gbif0[gbif0$Acname%in%c("Magnolia ¡Á soulangeana","Magnolia soulangeana"),"Acname"]="Magnolia x soulangeana"
	# write.csv(gbif0,"occ_data.clean.csv")
	gbif0[,c("Lon","Lat")]=round(gbif0[,c("Lon","Lat")],0)	#remove points that are too close to each other, and the follow will 1 x 1 arc degree buffer
	gbif=unique(gbif0[,c("Acname","Lon","Lat")])
	
	### DATA 2: state-level distribution of alien and native species in ENA, TPL corrected. Date:April 1, 2022
	dis.ori=as.data.frame(fread("SpLevDis.csv"))[,-1]
	# dis.ori[dis.ori$Acname%in%c("Magnolia ¡Á soulangeana","Magnolia soulangeana"),"Acname"]="Magnolia x soulangeana"
	# write.csv(dis.ori,"SpLevDis.csv")
	dis.alien=read.csv("alien.us.dis.V2.csv")[,c("Adcode99","Family_E","Genus_E","Acname")]#aliens in North Ameirica
	splist.alien=unique(dis.alien$Acname) 
	dis.alien.ori=dis.ori[dis.ori$Acname%in%splist.alien,c("Adcode99","Family_E","Genus_E","Acname")]
	genuslist=unique(dis.alien$Genus_E)
	splist=unique(dis.ori[dis.ori$Genus%in%genuslist,"Acname"])
	splist.native=splist[!splist%in%splist.alien]
	dis.native=dis.ori[dis.ori$Acname%in%splist.native,c("Adcode99","Family_E","Genus_E","Acname")]
	
	#remove Asia records in dis.alien and North America records in dis.native and dis.alien.ori
	geo=read.csv("Geo-isrm.csv")
	adc.ea=geo[geo$continent=="Asia","ADCODE99"]#Asia
	adc.ena=geo[geo$continent=="NAmerica","ADCODE99"]#North America
	adc.arc=geo[geo$Lat>=60,"ADCODE99"]	
	dis.native.ena=dis.native[dis.native$Adcode99%in%adc.ena,] #124 sp, 676 records
	dis.alien.ori2=dis.alien.ori[dis.alien.ori$Adcode99%in%adc.ea,] #25 sp, 415 records 
	dis.alien2=dis.alien[dis.alien$Adcode99%in%adc.ena,] #29 sp, 217 records,aliens in North Ameirica
	dis.all=rbind(cbind(dis.native.ena,status="ENA.native"),cbind(dis.alien.ori2,status="alien.original"),cbind(dis.alien2,status="alien"))
	#write.csv(dis.all,"ENA.alien.native.csv")#distribution of alien and native species in ENA (without gbif corrected) 
	
	splist2=unique(dis.all$Acname)
	a=unique(dis.alien.ori2$Acname)
	splist.ori.miss=splist.alien[!splist.alien%in%a]

	###union gbif data point and the state-level distribution map generated by USDA (for naturalized range)and PKU database (for native range)
	###STEP 1: We set 1 arc degree buffer for each gbif point and then over the GBIF points with 50 km grid cell
	###STEP 2: for each adcode polygon of each species, if over 10% of adcode polygon are covered with gbif grids or the location is in arctic (lat of adcode> 66.56667),
	###   use gibf grids instead, otherwise keep the original state-level distribution
	library(data.table)
	library(sp)
	library(maptools)
	library(raster)
	library(rgeos)
	library(sf)
	grid50=readShapeSpatial("area/50km/grid_50km.shp")
	# combind grid with adcode and climate
	# clim=as.data.frame(fread("area/50km/clim_50.csv"))
	# grid50@data=cbind(coordinates(grid50),grid50@data[,-c(7:8)],clim[match(grid50@data$GRIDCODE,clim$GRIDCODE),-1])
	# colnames(grid50@data)[1:2]=c("Lon","Lat")
	# foreign::write.dbf(grid50@data,"area/50km/grid_50km.dbf")
	# EA=subset(grid50,grid50@data$ADCODE99%in%adc.ea)
	# ENA=subset(grid50,grid50@data$ADCODE99%in%adc.ena)
	EA_ENA=subset(grid50,grid50@data$ADCODE99%in%c(adc.ea,adc.ena))
	EA=subset(grid50,grid50@data$ADCODE99%in%adc.ea)
	#plot(EA_ENA)
	
	##Creating buffers around points and merging with SpatialPolygonsDataFrame to create a list of matched polygons and area of intersection
	##REF: https://gis.stackexchange.com/questions/292327/creating-buffers-around-points-and-merging-with-spatialpolygonsdataframe-to-crea
	#1. Convert gbif data frame to sf with coordinates and Set the right crs on sf object:
	gbif.points = st_as_sf(gbif,coords=c("Lon","Lat"))%>% st_as_sf("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km")
	
	#2. Convert sp to sf:
	villages = st_as_sf(EA_ENA)%>% st_set_crs("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km")	
	villages.ea = st_as_sf(EA) %>% st_set_crs("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km")
	
	#3. Create 1 x 1 arc degree SQUARE buffer zone:
	gbif_buffer = st_buffer(gbif.points, 1,endCapStyle="SQUARE")
	#4. Extract the range sizes for each species based on state-level datasets (USDA + PKUdata) and intersect with the grid cell with GBIF occurence records
	occ.clean=vector("list",length(splist2));names(occ.clean)=splist2	
	for (i in 1:length(splist2)){
		adcode=sort(unique(dis.all[dis.all$Acname%in%splist2[i],"Adcode99"]))
		grid.sp=subset(villages,villages$ADCODE99%in%adcode)
		gbif.sp=subset(gbif_buffer,gbif_buffer$Acname%in%splist2[i])
		gbif_villages = st_intersection(gbif.sp, grid.sp)
		if (nrow(gbif_villages)==0){
			grid.keep=unique(as.data.frame(grid.sp)[,c("Lon","Lat","GRIDCODE","ADCODE99","Area")])
		} else{
			grid.sp2=unique(as.data.frame(grid.sp)[,c("Lon","Lat","GRIDCODE","ADCODE99","Area")])
			gbif_villages2=unique(as.data.frame(gbif_villages)[,c("Lon","Lat","GRIDCODE","ADCODE99","Area")])		
			record.ori=tapply(grid.sp2$Area,grid.sp2$ADCODE99,sum)
			record.int=tapply(gbif_villages2$Area,gbif_villages2$ADCODE99,sum)
			record=cbind(ori=record.ori,int=record.int[match(names(record.ori),names(record.int))])
			record[is.na(record)]=0
			record=cbind(record,por=record[,2]/record[,1]*100)
			adc.keep=rownames(record[record[,3]<10,])
			adc.keep=adc.keep[!adc.keep%in%adc.arc]		
			grid.keep=rbind(grid.sp2[grid.sp2$ADCODE99%in%adc.keep,],gbif_villages2[!gbif_villages2$ADCODE99%in%adc.keep,])
		} 		
		if (splist2[i]%in%splist.ori.miss){
			gbif_villages.t = st_intersection(gbif.sp, villages.ea)
			gbif_villages.t2 = unique(as.data.frame(gbif_villages.t)[,c("Lon","Lat","GRIDCODE","ADCODE99","Area")])
			grid.keep=rbind(grid.keep,gbif_villages.t2)				
		} 
		grid.sp.keep=villages[villages$GRIDCODE%in%unique(grid.keep$GRIDCODE),]		
		# plot(grid.sp.keep[,1])
		# plot(gbif.sp[,1])		
		# plot(grid.sp[,1])
		grid.keep=c()
		occ.clean[[i]]= grid.sp.keep
		#occ.clean[[i]]= grid.sp
	}
	#for (i in 1:length(splist) ) print(dim(gbif.clean[[i]])) #map of Magnolia acuminata var. subcordata and Magnolia soulangeana did not match any gbif record	
	save(occ.clean, file="occ.clean.Rdata")
	
	#Then check names using TNRS https://tnrs.biendata.org/#	
	#correct distributions based on discusion with soltis in 10/21/2022
	tnrs=read.csv("tnrs_result.csv")#add drop column based on discusion with soltis, to remove species that major distribution are in WNA or Mexico
	tnrs=subset(tnrs,tnrs$Drop==F)
	occ.clean=get(load("occ.clean.Rdata"))	
	occ.clean2=occ.clean[names(occ.clean)%in%tnrs$Accepted_name]
	#clean distribution out of EA/ENA
	grid50=foreign::read.dbf("area/50km/grid_50km.dbf")				
	geo=read.csv("Geo-isrm.csv")
	adc.asia=geo[geo$continent=="Asia","ADCODE99"]
	adc.na=geo[geo$continent=="NAmerica","ADCODE99"]
	ENA=subset(grid50,grid50$ADCODE99%in%adc.na&grid50$Lon>=-105&grid50$Lon<=-45&grid50$Lat<60&grid50$Lat>10)
	EA=subset(grid50,grid50$ADCODE99%in%adc.asia&grid50$Lon>=100&grid50$Lat<60&grid50$Lat>18)
		
	grid.ena=unique(ENA$GRIDCODE);grid.ea=unique(EA$GRIDCODE)
	splist.alien=sort(read.csv("EA-ENA.splist.csv")[,"Species_TPL"])#23 spp, final cleaned alien spp
	splist.native=sort(names(occ.clean2)[!names(occ.clean2)%in%splist.alien])
	occ.clean3=list()
	for (i in splist.alien){
		occ.clean3[[i]]=occ.clean2[[i]][occ.clean2[[i]]$Lon<0|occ.clean2[[i]]$GRIDCODE%in%grid.ea,]		
	}#min lon -125  
	for (j in splist.native){
		tmp=occ.clean2[[j]][occ.clean2[[j]]$GRIDCODE%in%grid.ena,]
		los=(nrow(tmp)-nrow(occ.clean[[j]]))/nrow(occ.clean[[j]])*100
		if (los>=-90) occ.clean3[[j]]=tmp
		if (los<=-90)print(paste(j,los))
	}#Hydrangea petiolaris;Magnolia allenii; Magnolia chocoensis; Magnolia morii are removed
	save(occ.clean3, file="occ.clean2.Rdata")
	
#################################
## statistic species number ########
#################################		
	#combine global distribution and alien.disj species
	library(data.table)
	dis=as.data.frame(fread("SpLevDis.csv"))[,-1]
	alien0=read.csv("manuscript/Appendix2.csv")	#checklist of ENA (23)and EA (5))aliens	
	alien.ena=alien0[alien0$Region.naturalized=="ENA",c("Family","Genus","Acname")]
	alien.ea=alien0[alien0$Region.naturalized=="EA",c("Family","Genus","Acname")]	
	geo=read.csv("Geo-isrm.csv")
	grid50=foreign::read.dbf("area/50km/grid_50km.dbf")				
	adc.asia=geo[geo$continent=="Asia","ADCODE99"]
	adc.na=geo[geo$continent=="NAmerica","ADCODE99"]
	ENA=subset(grid50,grid50$ADCODE99%in%adc.na&grid50$Lon>=-105&grid50$Lon<=-45&grid50$Lat<60&grid50$Lat>10)
	EA=subset(grid50,grid50$ADCODE99%in%adc.asia&grid50$Lon>=100&grid50$Lat<60&grid50$Lat>18)
	adc.ea=unique(EA[,"ADCODE99"])
	adc.ena=unique(ENA[,"ADCODE99"])
	
	splist.ena=unique(na.omit(dis[dis$Genus_E%in%unique(sort(alien0$Genus))&dis$Adcode99%in%adc.ena&(!dis$Acname%in%alien.ena$Acname),c("Family","Genus","Acname")]))
	splist.ea=unique(na.omit(dis[dis$Genus_E%in%unique(sort(alien0$Genus))&dis$Adcode99%in%adc.ea&(!dis$Acname%in%alien.ea$Acname),c("Family","Genus","Acname")]))
	splist=rbind(data.frame(splist.ea[!splist.ea$Acname%in%splist.ena$Acname,],status="EA.native"),
		data.frame(splist.ena[!splist.ena$Acname%in%splist.ea$Acname,],status="ENA.native"),
		data.frame(splist.ena[splist.ena$Acname%in%splist.ea$Acname,],status="Both.native"),
		data.frame(alien.ena,status="ENA.naturalized"),data.frame(alien.ea,status="EA.naturalized")
	)
	
	write.csv(splist,"splist.csv")#checklist of ENA native/alien and EA naitve/alien species,
	
	#manully check in Tropicos and POWO the species listes as "both native",and update them into EA or ENA native
	splist=read.csv("splist.csv")
	tapply(splist$Acname,splist$status,length)	
	# artifical hybrid                        Both.native 
                                 # 1                                  1 
                  # Centural America                          EA.native 
                                 # 1                                374 
                    # EA.naturalized                         ENA.native 
                                 # 5                                108 
                   # ENA.naturalized sym to Parthenocissus quinquefolia 
                                # 23                                  1 
	occ.clean=get(load("occ.clean2.Rdata"))	
	occ.clean2=subset(occ.clean,names(occ.clean)%in%splist[splist$status%in%c("ENA.native","ENA.naturalized"),"Acname"])
	occ.clean2=subset(occ.clean2,!names(occ.clean2)%in%"Osmanthus heterophyllus")#no ENA native species in Osmanthus	
	save(occ.clean2, file="occ.clean3.Rdata")
#################################
## plot distribution map ########
#################################	
	library(data.table)
	library(sf)
	library(sp)
	library(maptools)
	library(ggpubr)
	library(dplyr)
	grid50=readShapeSpatial("area/50km/grid_50km.shp")	
	geo=read.csv("Geo-isrm.csv")
	adc.ea=geo[geo$continent=="Asia","ADCODE99"]
	adc.ena=geo[geo$continent=="NAmerica","ADCODE99"]
	
	ENA=subset(grid50,grid50@data$ADCODE99%in%adc.ena)
	EA=subset(grid50,grid50@data$ADCODE99%in%adc.ea)
	
	villages.ena = st_as_sf(ENA)%>%filter(Lon>=-125&Lon<=-50&Lat>=10&Lat<=60)
	villages.ea = st_as_sf(EA)
	#caculate richness
	occ.clean=get(load("occ.clean3.Rdata"))	
	alien0=read.csv("EA-ENA.splist.csv")[-18,]	
	splist.alien=sort(alien0$Species_TPL)
	splist.native=sort(names(occ.clean)[!names(occ.clean)%in%splist.alien])
	
	alien=c();native=c()
	for (i in splist.alien){
	tmp=unique(as.data.frame(occ.clean[[i]])[,c("Lat","Lon","GRIDCODE","ADCODE99","Area")])
	alien=rbind(alien,data.frame(Species=i,tmp))
	}
	for (i in splist.native){
	tmp=unique(as.data.frame(occ.clean[[i]])[,c("Lat","Lon","GRIDCODE","ADCODE99","Area")])
	native=rbind(native,data.frame(Species=i,tmp))
	}
	rich.alien=tapply(alien$Species,alien$GRIDCODE,length)
	rich.native=tapply(native$Species,native$GRIDCODE,length)
	#draw maps
	map.alien=cbind(Richness=rich.alien[match(villages.ena$GRIDCODE,names(rich.alien))],villages.ena)
	map.native=cbind(Richness=rich.native[match(villages.ena$GRIDCODE,names(rich.native))],villages.ena)
	map.ori=cbind(Richness=rich.alien[match(villages.ea$GRIDCODE,names(rich.alien))],villages.ea)
	map.native[is.na(map.native)]=0
	map.alien[is.na(map.alien)]=0
	map.ori[is.na(map.ori)]=0
	p1=ggplot(map.native,aes(fill = Richness,color=Richness)) +
	geom_sf() +scale_color_gradient(low="lightgray",high="darkblue")+guides(color="none")+
	scale_fill_gradient(low="lightgray",high="darkblue",name="Native Species\nRichness in ENA")+		
	theme_light()+ theme(panel.background = element_rect(fill = '#619CFF', colour = 'black'),
		axis.text = element_text(size=12,color='black',angle=0),
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))+
	xlim(-125,-50)+ylim(10,60)
	
	p2=ggplot(map.alien,aes(fill = Richness,color=Richness)) +
	geom_sf() +scale_color_gradient(low="lightgray",high="darkred")+guides(color="none")+
	scale_fill_gradient(low="lightgray",high="darkred",name="Naturalizd Species\nRichness in ENA")+				
	theme_light()+ 
	theme(panel.background = element_rect(fill = '#619CFF', colour = 'black'),
		axis.text = element_text(size=12,color='black',angle=0),
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))+
	xlim(-125,-50)+ylim(10,60)
	
	p3=ggplot(map.ori,aes(fill = Richness,color=Richness)) +
	geom_sf() +scale_color_gradient(low="lightgray",high="#F4A460")+guides(color="none")+
	scale_fill_gradient(low="lightgray",high="#F4A460",name="ENA Naturalizd Species\nRichness in EA")+				
	theme_light()+ theme(panel.background = element_rect(fill = '#619CFF', colour = 'black'),
		axis.text = element_text(size=12,color='black',angle=0),
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))+
		xlim(100,160)+ylim(18,60)
	
	ggarrange(p1,p2,p3,nrow=3,ncol = 1,labels=c("a","b","c"),font.label = list(size = 25),vjust=0.7,hjust=0.1)	
	#plot distribution divergences for each genus
	genuslist=sort(unique(alien0$Genus))
	plt=list()
	for(j in genuslist){
		alien.t=splist.alien[grep(j,splist.alien)]
		native.t=splist.native[grep(j,splist.native)]
		gen.alien=c()
		for (i in alien.t){
		tmp=cbind(status="naturalized",unique(as.data.frame(occ.clean[[i]])[,c("Lat","Lon","GRIDCODE","ADCODE99")]))
		tmp2=subset(tmp,tmp$ADCODE99%in%adc.ena)	
		gen.alien=rbind(gen.alien,data.frame(Species=i,tmp2))
		}	
		gen.native=c()
		for (i in native.t){
		tmp=cbind(status="native",unique(as.data.frame(occ.clean[[i]])[,c("Lat","Lon","GRIDCODE","ADCODE99")]))
		gen.native=rbind(gen.native,data.frame(Species=i,tmp))
		}
		#gen.dis.t=cbind(Genus=j,rbind(data.frame(status="alien",gen.alien),data.frame(status="native",gen.native)))		
		rich.alien=tapply(gen.alien$Species,gen.alien$GRIDCODE,length)			
		rich.native=tapply(gen.native$Species,gen.native$GRIDCODE,length)
		map.alien=na.omit(cbind(Richness=rich.alien[match(villages.ena$GRIDCODE,names(rich.alien))],villages.ena))	
		map.native=na.omit(cbind(Richness=rich.native[match(villages.ena$GRIDCODE,names(rich.native))],villages.ena))		
		getColor = function(richdata,mapdata,fillcol){
			pop=richdata[richdata!=0];provname=names(pop)
			if(length(unique(pop))<=2){
				col.t=data.frame(rich=unique(pop[order(pop)]),col=RColorBrewer::brewer.pal(3,fillcol)[2:3])			
			} else{
				if (length(unique(pop))>9){
				    rep.t=rep(RColorBrewer::brewer.pal(9,fillcol)[1],length(unique(pop))-9)
					col.t=data.frame(rich=unique(pop[order(pop)]),col=c(rep.t,RColorBrewer::brewer.pal(9,fillcol)))
				} else{
					col.t=data.frame(rich=unique(pop[order(pop)]),col=RColorBrewer::brewer.pal(length(unique(pop)),fillcol))
				}				
			}
			col=data.frame(cwe=pop[order(pop)],col=col.t[match(pop[order(pop)],col.t$rich),"col"])
			provcol=as.character(col[match(pop,col$cwe),2])		
			f = function(x, y) ifelse(x %in% y, which(y == x), 0);
			colIndex = sapply(mapdata$GRIDCODE, f, provname);
			fg = provcol[colIndex];
			return(fg);
		}			
		plt[[j]]=ggplot() +
			geom_sf(data=villages.ena,colour = "black", fill = "black")+
			geom_sf(data=map.alien,fill=getColor(rich.alien,map.alien,"Reds"),color=NA,alpha=1)+		
			geom_sf(data=map.native,fill=getColor(rich.native,map.native,"Blues"),color=NA,alpha=0.8)+		
			theme_light()+ 
			ggtitle(j)+
			theme(panel.background = element_rect(fill = '#619CFF', colour = 'black'),
			axis.text = element_text(size=10,color='black',angle=0),
			axis.title = element_blank())
	}
	
	annotate_figure(ggarrange(plt[[1]],plt[[2]],plt[[3]],plt[[4]],plt[[5]],plt[[6]],plt[[7]],plt[[8]],plt[[9]],plt[[10]],plt[[11]],plt[[12]],plt[[13]],
		nrow=3,ncol = 5),
			bottom = text_grob("Longitude", size=15),left = text_grob("Latitude", rot = 90,size=15))
		
		
	#plot distribution map for species which outside ENA
	dis.dat=c()
	for (i in splist.native){
		tmp=unique(as.data.frame(occ.clean[[i]])[,c("Lat","Lon","GRIDCODE","ADCODE99")])
		if (min(tmp$Lon)<=-105){
			tmp=cbind(status="native",Genus=strsplit(names(occ.clean[i])," ")[[1]][1],Species=names(occ.clean[i]),tmp)
			dis.dat=rbind(dis.dat,tmp)
		}						
	}
	gen=sort(unique(dis.dat$Genus))
	spinge=unique(dis.dat[,c("Genus","Species","status")])
	tapply(spinge$Species,spinge$Genus,length)
	plt.map=function(genus,dis.dat){
		dis.dat.t=subset(dis.dat,dis.dat$Genus%in%genus)
		sprich=length(unique(dis.dat.t$Species))	
		p=ggplot(dis.dat.t,aes(x=Lon,y=Lat,fill=status)) +
		geom_sf(data=villages.ena,colour = "gray", fill = "gray")+stat_bin2d(bins=100,alpha = 0.5)+
		theme_light()+ facet_wrap(~Species, ncol = round(sqrt(sprich)))+
		theme(panel.background = element_rect(fill = '#619CFF', colour = 'black'),
			axis.text = element_text(size=15,color='black',angle=0),
			axis.title = element_text(size=18,color='black',angle=0),
			strip.text = element_text(colour = 'black', face = 'italic', size = rel(1.5)), 
			legend.position = "none",
			strip.background = element_rect(fill = 'white', colour = 'darkgray', size = rel(2), linetype = 2))
		return(p)
	}
	plt.map(gen[4],dis.dat)
	plt.map(gen[-4],dis.dat)
	#plot EA distribution 
	dis.dat=c()
	for (i in splist.alien){
		tmp=unique(as.data.frame(occ.clean[[i]])[,c("Lat","Lon","GRIDCODE","ADCODE99")])
		tmp=cbind(status="naturalized",Genus=strsplit(names(occ.clean[i])," ")[[1]][1],Species=i,subset(tmp,tmp$Lon>80))
		dis.dat=rbind(dis.dat,tmp)		
	}
	gen=sort(unique(dis.dat$Genus))
	plt.map(gen,dis.dat)
	
	#compare range sizes	
	natura.ena=subset(alien,alien$ADCODE99%in%adc.ena)
	natura.ea=subset(alien,alien$ADCODE99%in%adc.ea)
	range.ori=tapply(natura.ea$Area,natura.ea$Species,sum)
	range.natur=tapply(natura.ena$Area,natura.ena$Species,sum)
	range.native=tapply(native$Area,native$Species,sum)
	range.size=rbind(data.frame(Species=names(range.ori),rangesize=range.ori,status="Original range sizes of exotics"),
		data.frame(Species=names(range.natur),rangesize=range.natur,status="Naturalized range size of exotics"),
		data.frame(Species=names(range.native),rangesize=range.native,status="Range sizes of ENA natives"))
	compar=summarySE(range.size, measurevar="rangesize", groupvars="status")
	library(scales)
	library(ggpubr)	
	my_compar=list(as.character(unique(range.size$status)))
	ggviolin(range.size,x="status", y="rangesize",fill="status",add="boxplot",add.params=list(fill="white"))+
		stat_compare_means(comparisons=my_compar,label="p.signif",bracket.size=1.2,exact=TRUE)+theme_bw()+	
		theme(axis.text.x = element_blank(),
		axis.text.y = element_text(size=15,color='black',angle=30,vjust=0.2),
		axis.title.x = element_text(size=20),
		axis.title.y = element_text(size=18,color='black',angle=90))+xlab(" ")#ns: p > 0.05,*: p <= 0.05,**: p <= 0.01
		
#################################
## Maxent in R ########
#################################
#also see:	https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md
#ref: https://github.com/soltislab/BotanyENMWorkshops
# This script is for generating and testing ENMs using ENMEval. Please see 
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13628
# for the paper describing ENMEval and 
# https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html
# for the vignette.

# Set up java memory 
options(java.parameters = "- Xmx16g") # increase memory that can be used
# Load Packages
library(data.table)
library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(gtools)
library(dismo)
library(ENMeval)
library(viridis)
library(kuenm)

#devtools::install_github("marlonecobos/kuenm")
# Load Function
source("ENMevaluation.R")

# load sp dis
alldf <- get(load("occ.clean2.Rdata"))
alien0=read.csv("EA-ENA.splist.csv")		
splist.alien=sort(alien0$Species_TPL)
splist.native=sort(names(alldf)[!names(alldf)%in%splist.alien])
genuslist=sort(unique(alien0$Genus))
# load maps
var.list=c(paste("bio0",1:9,sep=""),paste("bio",10:19,sep=""))
convert_rst=function(vars,dis){
	rst <-as_Spatial(dis[,c(vars,"geometry")])
	r <- raster(ncol=length(unique(dis$Lon)), nrow=length(unique(dis$Lat)))
	extent(r) <- extent(rst)
	parcel_ras<-rasterize(rst, r, vars)
	return(parcel_ras)
}

# grid50=maptools::readShapeSpatial("area/50km/grid_50km.shp")				
# geo=read.csv("Geo-isrm.csv")
# adc.ea=geo[geo$continent=="Asia","ADCODE99"]
# adc.ena=geo[geo$continent=="NAmerica","ADCODE99"]
# ENA=subset(grid50,grid50@data$ADCODE99%in%adc.ena&grid50@data$Lon>=-105&grid50@data$Lon<=-45&grid50@data$Lat<60&grid50@data$Lat>10)
# EA=subset(grid50,grid50@data$ADCODE99%in%adc.ea&grid50@data$Lon>=100&grid50@data$Lat<60&grid50@data$Lat>18)
# # pca env vars
# clim.ena=na.omit(ENA@data)[,c("Lon","Lat","GRIDCODE","ADCODE99",var.list)]
# rownames(clim.ena)=clim.ena$GRIDCODE
# ## dudi.PCA to reduce variables, using the two first axes of a PCA calibrated on occurence data to keep consistent with measures on niche overlap 
# pca.env.ena <- ade4::dudi.pca(clim.ena[,var.list],center = TRUE,scale = T,scannf = FALSE,nf = 5)
# summary(pca.env.ena)#Cumulative Proportion of Variance in first five axis =96.6% 
# scores.clim.ena <- pca.env.ena$li
# ENA@data=cbind(ENA@data,scores.clim.ena[match(ENA@data$GRIDCODE,rownames(scores.clim.ena)),])

# clim.ea=na.omit(EA@data)[,c("Lon","Lat","GRIDCODE","ADCODE99",var.list)]
# rownames(clim.ea)=clim.ea$GRIDCODE
# pca.env.ea <- ade4::dudi.pca(clim.ea[,var.list],center = TRUE,scale = T,scannf = FALSE,nf = 5)
# summary(pca.env.ea)#Cumulative Proportion of Variance in first five axis =97.7% 
# scores.clim.ea <- pca.env.ea$li
# EA@data=cbind(EA@data,scores.clim.ea[match(EA@data$GRIDCODE,rownames(scores.clim.ea)),])

# # creat env layers
# sf.ena=ENA%>% st_as_sf()%>% st_set_crs("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# sf.ea=EA%>% st_as_sf()%>% st_set_crs("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# writ.rst=function(i,sf.obj,reg,var.typ){
	# if(var.typ=="pca")	rst=convert_rst(paste("Axis",i,sep=""),sf.obj)
	# if(var.typ=="bioclim")	rst=convert_rst(i,sf.obj)
	# rgdal::writeGDAL(as(rst, "SpatialGridDataFrame"),paste("clim.rst/Axis",i,reg,"asc",sep="."),drivername = "AAIGrid")
# }
# lapply(1:5,writ.rst,sf.ena,"ena","pca")
# lapply(1:5,writ.rst,sf.ea,"ea","pca")
# lapply(var.list,writ.rst,sf.ena,"ena","bioclim")
# lapply(var.list,writ.rst,sf.ea,"ea","bioclim")

# #pca
# allstack.ena=do.call(stack,lapply(1:5,function(i) raster(paste("clim.rst/Axis",i,"ena.asc",sep="."))))
# allstack.ea=do.call(stack,lapply(1:5,function(i) raster(paste("clim.rst/Axis",i,"ea.asc",sep="."))))

#bioclim
allstack.ena=do.call(stack,lapply(var.list,function(i) raster(paste("clim.rst/Axis",i,"ena.asc",sep="."))))
allstack.ea=do.call(stack,lapply(var.list,function(i) raster(paste("clim.rst/Axis",i,"ea.asc",sep="."))))
names(allstack.ena)=var.list;names(allstack.ea)=var.list
# Use Variable inflation factor (VIF) and the MaxEnt permutation importance to select the best variables for your model.
### VIF can detect for multicollinearity in a set of multiple regression variables. 
set.seed(195)
m=c()
#for(i in  splist.alien){
    get.imp=function(i,alldf,allstack,reg){
		source("functions/VIFLayerSelect.R");require(dplyr)
		if (reg=="ena") spp_df <-  as.data.frame(alldf[[i]])[,c("Lon","Lat")]%>%filter(Lon<0)%>%distinct()
		if (reg=="ea") spp_df <-  as.data.frame(alldf[[i]])[,c("Lon","Lat")]%>%filter(Lon>0)%>%distinct()
		model <- dismo::maxent(x = allstack, p = spp_df, progress = "text", silent = FALSE) 
		m.t <- vimportance(model)# Calculate variable importance from MaxEnt model
		return(m.t)
	}#m[[i]]=m.t
#}
library(parallel)
no_cores <- detectCores() - 1
mycl <- makePSOCKcluster(no_cores);
## Run a simple maxent model for every species and calculate the average permutation contribution
### Loop through each species and save permutation importance in list
mc.ena <- do.call(rbind,parLapply(cl=mycl,X=splist.alien,get.imp,alldf,allstack.ena,"ena"))
mc_average.ena <- aggregate(mc.ena[, 2], list(mc.ena$Variables), mean)%>%
              dplyr::select(Variables = Group.1, permutation.importance = x)
# While VIF of all env rasters >10, drop the env variable with the highest VIF and lowest model importance,and 
# reevaluate the VIF and redo the drop process until VIF <=10
selectedlayers.ena <- VIF_layerselect(allstack.ena, mc_average.ena)

mc.ea <- do.call(rbind,parLapply(cl=mycl,X=splist.alien,get.imp,alldf,allstack.ea,"ea"))
write.csv(mc.ea,"mc.ea.csv")
mc_average.ea <- aggregate(mc.ea[, 2], list(mc.ea$Variables), mean)%>%
              dplyr::select(Variables = Group.1, permutation.importance = x)
selectedlayers.ea <- VIF_layerselect(allstack.ea, mc_average.ea)
stopCluster(mycl)
## Since this can vary per system (despite setting seed), we added this line to keep our files consistent for the workshop
sl.ena <- c("bio02","bio03","bio05","bio08","bio13","bio15","bio18","bio19")
sl.ea  <- c("bio02","bio03","bio08","bio14","bio15","bio18")
allstack.ena2=do.call(stack,lapply(sl.ena,function(i) raster(paste("clim.rst/Axis",i,"ena.asc",sep="."))))
allstack.ea2=do.call(stack,lapply(sl.ea,function(i) raster(paste("clim.rst/Axis",i,"ea.asc",sep="."))))
names(allstack.ena2)=sl.ena;names(allstack.ea2)=sl.ea
	
get.maxent=function(j,sl,alldf,allstack,reg){
	 # j=splist.alien[5]
	 # allstack=allstack.ena2
	 # sl=sl.ena
	 # reg="ena"
  
	# sp.alien.t=splist.alien[grep(j,splist.alien)]
	# sp.native.t=splist.native[grep(j,splist.native)]
	# p.alien <- do.call(rbind,lapply(sp.alien.t,function(x,alldf) as.data.frame(alldf[[x]])[,c("Lon","Lat","GRIDCODE")],alldf))
	# p.alien.ena = p.alien%>%filter(Lon<0)%>%distinct()
	# p.alien.ea = p.alien%>%filter(Lon>0)%>%distinct()
	# p.native <- do.call(rbind,lapply(sp.native.t,function(x,alldf) as.data.frame(alldf[[x]])[,c("Lon","Lat","GRIDCODE")],alldf))%>%distinct()
	
	if (reg=="ena") spdf=alldf[[j]]%>%filter(Lon<0)%>%distinct()
	if (reg=="ea") spdf=alldf[[j]]%>%filter(Lon>0)%>%distinct()	
	p=as.data.frame(spdf)[,c("Lon","Lat")]
	rst=do.call(stack,lapply(sl,convert_rst,alldf[[j]]))
	names(rst)=names(allstack)
		
	eval1 <- ENMeval::ENMevaluate(occ = p, 
								  env = rst,
								  tune.args = list(fc = c("L","Q"), rm = 1:length(sl)), # test the feature classes L = linear and Q = quadratic
								  partitions = "block",#
								  parallel = FALSE,
								  algorithm = 'maxent.jar', 
								  user.eval = proc)
	# evaldis <- dismo::maxent(x = rst, p = p,args = c("-X", 20,"jackknife"))#20% reserved for testing
                                  # "randomseed", "randomtestpoints=25"), # randomly set aside 25% of the sample records for testing.
                         # removeDuplicates = TRUE)
						 
	### Inspect the results
	### Identify the best model
	#### selecting models with the lowest average test omission rate and 
	#### the highest average validation AUC and lowest AICc
	results <- eval.results(eval1)
	opt.seq <- results %>% 
            dplyr::filter(or.10p.avg == min(or.10p.avg)) %>% 
            dplyr::filter(auc.val.avg == max(auc.val.avg))
	if (nrow(opt.seq)>1) opt.seq = opt.seq	%>% dplyr::filter(delta.AICc == min(delta.AICc))
	#plot(eval1@predictions[[which (eval1@results$delta.AICc == 0) ]])
	### Subset model	
	mod.seq <- eval.models(eval1)[[opt.seq$tune.args[1]]]
	predic <- predict(mod.seq, allstack) 
	p_df <-  as.data.frame(predic, xy = TRUE)	
	write.csv(p_df,paste("sdm.map/p_df",j,reg,"csv",sep="."))
	save(eval1,file=paste("sdm.map/sdm",reg,j,"Rdata",sep="."))	
}		

for (j in splist.alien){
get.maxent(j,sl.ena,alldf,allstack.ena2,"ena")
get.maxent(j,sl.ea,alldf,allstack.ea2,"ea")
}

### Visualize
### Make p plottable 
get.plt=function(j,reg,alldf){
	if (reg=="ena") spdf=alldf[[j]]%>%filter(Lon<0)%>%distinct()
	if (reg=="ea") spdf=alldf[[j]]%>%filter(Lon>0)%>%distinct()	
	p=as.data.frame(spdf)[,c("Lon","Lat")]	
	p_df=fread(paste("p_df",j,reg,"csv",sep="."))
	### Plot
	plt=ggplot() + geom_raster(data = p_df, aes(x = x, y = y, fill = layer))
	if (reg=="ena") p = p[p$Lon>=-105,]
	plt= plt +  geom_point(data= p,mapping = aes(x = Lon, y = Lat),col='red', cex=0.5,alpha=0.8) +
	  coord_quickmap() +  theme_bw() + 
	  scale_fill_gradientn(colours = viridis::viridis(99),na.value = "black")
	if (reg=="ena") plt=plt+theme(axis.title=element_blank())+annotate("text", x=-90 , y=12 ,size=4,label=j,colour="white")	
	if (reg=="ea") plt=plt+theme(axis.title=element_blank())+annotate("text", x=130 , y=20 ,size=4,label=j,colour="white")	
	return(plt)
}
png(paste(reg,splist.alien[1],"png",sep="."),bg = "transparent");
get.plt(splist.alien[1],"ea",alldf);dev.off()
			
############################################################################################
## caculate niche overlap following Early and Sax, GEB, 2014 and Broennimann et. al., GEB, 2012##
############################################################################################
#### code are modified from Broennimann et. al., GEB, 2012, 21, 481�?497
	library(data.table)	
	#devtools::install_github('https://github.com/ClementCalenge/adehabitat')	
	gbif.clean=get(load("occ.clean3.Rdata"))
	
##STEP1 niche shift of naturalized species
	#The environmental space is bounded by the minimum and maximum environmental values found across the entire study region
	var.list=c(paste("bio0",1:9,sep=""),paste("bio",10:19,sep=""))
	grid50=maptools::readShapeSpatial("area/50km/grid_50km.shp")				
	geo=read.csv("Geo-isrm.csv")
	adc.ea=geo[geo$continent=="Asia","ADCODE99"]
	adc.ena=geo[geo$continent=="NAmerica","ADCODE99"]
	ENA=subset(grid50,grid50@data$ADCODE99%in%adc.ena&grid50@data$Lon>=-105&grid50@data$Lon<=-45&grid50@data$Lat<60&grid50@data$Lat>10)
	EA=subset(grid50,grid50@data$ADCODE99%in%adc.ea&grid50@data$Lon>=100&grid50@data$Lat<60&grid50@data$Lat>18)
	clim1=na.omit(EA@data)[,c("Lat","Lon",var.list)]
	clim2=na.omit(ENA@data)[,c("Lat","Lon",var.list)]
	
	niche.cal=function(spname,clim1,clim2,gbif.clean,adc.ea,adc.ena,vlist,R){
		# for (i in 1:length(splist)){
		# spname=splist[i]
		require(ade4)
		require(adehabitat)	
		source("niche.overlap.functions.R")
		source("occ.prep.functions.R")
		
		occ.gbif=gbif.clean[[spname]]
		occ.sp1.sf<-subset(occ.gbif,occ.gbif$ADCODE99%in%adc.ea)#original distribution in EA
		occ.sp2.sf<-subset(occ.gbif,occ.gbif$ADCODE99%in%adc.ena)#naturalized distribution of aliens in ENA
		# plot(occ.sp1.sf[,1])
		# plot(occ.sp2.sf[,1])	
		occ.sp1=unique(na.omit(as.data.frame(occ.sp1.sf)[,c("Lat","Lon",vlist)]))#there are few grid cells without climate information
		occ.sp2=unique(na.omit(as.data.frame(occ.sp2.sf)[,c("Lat","Lon",vlist)]))
		
		# global dataset for the analysis and rows for each sub dataset
		data.env.occ<-rbind(clim1,clim2,occ.sp1,occ.sp2)
		row.clim1<-1:nrow(clim1)
		row.clim2<-(nrow(clim1)+1):(nrow(clim1)+nrow(clim2))
		row.clim12<-1:(nrow(clim1)+nrow(clim2))
		row.sp1<-(nrow(clim1)+nrow(clim2)+1):(nrow(clim1)+nrow(clim2)+nrow(occ.sp1))
		row.sp2<-(nrow(clim1)+nrow(clim2)+nrow(occ.sp1)+1):(nrow(clim1)+nrow(clim2)+nrow(occ.sp1)+nrow(occ.sp2))
		
		# measures niche overlap along the two first axes of a PCA calibrated on occurence data
		row.w.1.env<-1-(nrow(clim1)/(nrow(clim1)+nrow(clim2)))  # prevalence of clim1
		row.w.2.env<-1-(nrow(clim2)/(nrow(clim1)+nrow(clim2)))  # prevalence of clim2
		row.w.env<-c(rep(row.w.1.env, nrow(clim1)),rep(row.w.2.env, nrow(clim2)),rep(0, nrow(occ.sp1)),rep(0, nrow(occ.sp2)))			
		pca.cal <-dudi.pca(data.env.occ[,-c(1:2)],row.w = row.w.env, center = T, scale = T, scannf = F, nf = 2)
		
		# predict the scores on the axes
		scores.clim12<- pca.cal$li[row.clim12,]
		scores.clim1<- pca.cal$li[row.clim1,]
		scores.clim2<- pca.cal$li[row.clim2,]
		scores.sp1<- pca.cal$li[row.sp1,]
		scores.sp2<- pca.cal$li[row.sp2,]
		# calculation of occurence density and test of niche similarity 
		z1<- grid.clim(scores.clim12,scores.clim1,scores.sp1,R)
		z2<- grid.clim(scores.clim12,scores.clim2,scores.sp2,R)
		#}
						
		rangesize1=unique(as.data.frame(occ.sp1.sf)[,c("Area","GRIDCODE")])#area unit: km2
		rangesize2=unique(as.data.frame(occ.sp2.sf)[,c("Area","GRIDCODE")])#area unit: km2
				
		# length(z1$Z[z1$Z>0]) #number of climate bins of the study area 1 (EA)		
		# climate bins occupied by the species
		p1<-z1$z.cor/sum(z1$z.cor)	# rescale occurence densities so that the sum of densities is the same for both species
		p2<-z2$z.cor/sum(z2$z.cor)	# rescale occurence densities so that the sum of densities is the same for both species				
		
		niche.ovlp=1-(0.5*(sum(abs(p1-p2))))		
		niche.size.EA=length(p1[p1>0]);niche.size.ENA=length(p2[p2>0])	
		niche.expansion=ifelse(niche.ovlp==0,NA,length(p2[p2>0&p1==0])/niche.size.EA)#following Early and Sax, GEB, 2014
		niche.loss=ifelse(niche.ovlp==0,NA,length(p1[p1>0&p2==0])/niche.size.EA)		
		range.size.EA=sum(rangesize1$Area);	range.size.ENA=sum(rangesize2$Area)		
		
		# runs niche similarity test(see Warren et al 2008) based on two species occurrence density grids			
		a<-niche.equivalency.test(z1,z2,rep=100)
		P = a[[3]]
		Px=ifelse(P <= 0.0001,"****",
							ifelse(P>0.0001&P<=0.001,"***",
							ifelse(P>0.001&P<=0.01,"**",
							ifelse(P>0.01&P<=0.05,"*",
							#ifelse(P>0.05&P<0.1,"",
							ifelse(P>0.05,"ns",P)))))		
		overlap.t=data.frame(Species=spname,
			niche.overlap=niche.ovlp,niche.overlap.p1 = paste(round(niche.ovlp,3),Px,sep=""),niche.overlap.p2 = P,
			niche.expansion=niche.expansion,niche.loss=niche.loss,
			niche.size.EA=niche.size.EA,niche.size.ENA=niche.size.ENA,
			niche.size.change=(niche.size.ENA-niche.size.EA)/niche.size.EA*100,
			range.size.EA=range.size.EA,range.size.ENA=range.size.ENA,
			range.change=(range.size.ENA-range.size.EA)/range.size.EA*100
		)
		
		result.sp=list(z1,z2,overlap.t)
		return(result.sp)		
	}	
	
	alien=read.csv("EA-ENA.splist.csv")[-18,]	
	splist=sort(alien$Species_TPL)	
	R=100 #resolution of the gridding of the climate space
	
	require(parallel)
	no_cores <- detectCores() - 1
	mycl <- makePSOCKcluster(no_cores); 
	date()
	sp.niche=parLapply(cl=mycl,splist,niche.cal,clim1,clim2,gbif.clean,adc.ea,adc.ena,var.list,R)
	names(sp.niche)=splist	
	stopCluster(mycl)	
	date()#20 min
	save(sp.niche, file="sp.niche2.Rdata")
	
	sp.niche=get(load("sp.niche2.Rdata"))
	sp.niche=subset(sp.niche,!names(sp.niche)%in%"Osmanthus heterophyllus")
	overlap=c()
	for (i in splist) overlap=rbind(overlap,sp.niche[[i]][[3]])		
	overlap=cbind(overlap,EarlyRecordyear=alien[match(overlap$Species,alien$Species_TPL),"EarlyRecordyear"])	#plot idexes of niche overlap
		
	#plot niche overlap			
	layout(matrix(1:24, 4, 6, byrow = TRUE))
	par(mar=c(1.2,1,0.02,0.17), cex=1, cex.axis=0.8, cex.lab=1.2,  tck=-0.04,oma=c(1.5,1.5,0,0.15))
	seq=1
	for (i in sort(splist)){		
		# the plot region color
		plot.new()
		rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col = "lightblue")
		# adding the new plot
		par(new = TRUE)
		z1=sp.niche[[i]][[1]];z2=sp.niche[[i]][[2]]			
		if (seq<=18){
		if (seq%in%c(1,7,13)){
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="#F4A460",xaxt='n',cex.axis=2,tck=-0.02,mgp = c(3, 0.6, 0))#original
		}else{
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="#F4A460",xaxt='n',yaxt='n')
		}		
		}else{
		if (seq==19){
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="#F4A460",cex.axis=2,tck=-0.02,mgp = c(3, 0.6, 0))
		}else{
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="#F4A460",yaxt='n',cex.axis=2,tck=-0.02,mgp = c(3, 0.6, 0))
		}
		}		
		contour(z2$x,z1$y,z2$z.cor,add=T,levels=quantile(z2$z.cor[z2$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="darkred")#naturalized
		nichove=overlap[overlap$Species==i,"niche.overlap.p1"]
		niche.p=overlap[overlap$Species==i,"niche.overlap.p1"]		
		spn=ifelse(length(unlist(strsplit(i," ")))>2,paste(substr(unlist(strsplit(i," "))[1],start = 1,stop = 1),paste(unlist(strsplit(i," "))[-1],collapse=" "),sep=". "),paste(substr(unlist(strsplit(i," "))[1],start = 1,stop = 1),unlist(strsplit(i," "))[2],sep=". "))
		mtext(paste(spn,"\n","D=",niche.p),side=3,adj=0.1,line=-2,cex=1,col="black",font=3)
		seq=seq+1
	}
	
	# plot an example
	layout(matrix(1:2, 1, 2, byrow = TRUE))
	image(z1$x,z1$y,z1$z.cor,col =colorRampPalette(c("white","#F0E442"))(100) ,zlim=c(0.000001,max(z1$z.cor)),xlab="",ylab="")
	contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),ad=T,drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="#F0E442")
	contour(z1$x,z1$y,z1$Z,add=T,levels=quantile(z1$Z[z1$Z>0],c(0,0.5)),drawlabels=F,lty=c(2,1),lwd=c(1,1),col="gray")
	title(main="Original niche space",xlab="Niche PCA axis 1",ylab="Niche PCA axis 2",cex.main=2,cex.lab=2)
	mtext(paste(i,"\n","niche overlap:","D=",nichove),side=1,adj=0.1,line=-1.5,cex=2)
	
	image(z2$x,z2$y,z2$z.cor,col =colorRampPalette(c("white","darkred"))(100) ,zlim=c(0.000001,max(z2$z.cor)),xlab="",ylab="")
	contour(z2$x,z2$y,z2$z.cor,levels=quantile(z2$z.cor[z2$z.cor>0],c(0,0.25,0.5,0.75)),ad=T,drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="darkred")
	contour(z2$x,z2$y,z2$Z,add=T,levels=quantile(z2$Z[z2$Z>0],c(0,0.5)),drawlabels=F,lty=c(2,1),lwd=c(1,1),col="gray")
	title(main="Naturalized niche space",xlab="Niche PCA axis 1",ylab="",cex.main=2,cex.lab=2)

#caculate % of occupied niche in all suitable area
	require(ade4)
	require(adehabitat)	
	require(dplyr)
	library(ENMeval)
	library(dismo)
	source("niche.overlap.functions.R")
	source("occ.prep.functions.R")
	sl.ena <- c("bio02","bio03","bio05","bio08","bio13","bio15","bio18","bio19")
	sl.ea  <- c("bio02","bio03","bio08","bio14","bio15","bio18")
	alien0=read.csv("EA-ENA.splist.csv")		
	splist.alien=sort(alien0$Species_TPL)
	alldf <- get(load("occ.clean2.Rdata"))
	grid50=foreign::read.dbf("area/50km/grid_50km.dbf")				
	geo=read.csv("Geo-isrm.csv")
	adc.ea=geo[geo$continent=="Asia","ADCODE99"]
	adc.ena=geo[geo$continent=="NAmerica","ADCODE99"]
	ENA=subset(grid50,grid50$ADCODE99%in%adc.ena&grid50$Lon>=-105&grid50$Lon<=-45&grid50$Lat<60&grid50$Lat>10)
	EA=subset(grid50,grid50$ADCODE99%in%adc.ea&grid50$Lon>=100&grid50$Lat<60&grid50$Lat>18)
	clim1=na.omit(EA)[,c("Lon","Lat",sl.ea)]
	clim2=na.omit(ENA)[,c("Lon","Lat",sl.ena)]
	sp.niche=get(load("sp.niche2.Rdata"))
	niche.suitable=c()
	for (j in splist.alien){
		#j=splist.alien[1]
		me.ea=get(load(paste("sdm.map/sdm.ea",j,"Rdata",sep=".")))
		me.ena=get(load(paste("sdm.map/sdm.ena",j,"Rdata",sep=".")))
		get.z=function(clim,occ.sp,me,spdf,reg,sl){
			#clim=clim1;occ.sp=occ.sp1;me=me.ea;spdf=alldf[[j]];reg="ea",sl=sl.ea
			opt.seq= eval.results(me)%>% 
				dplyr::filter(or.10p.avg == min(or.10p.avg)) %>% dplyr::filter(auc.val.avg == max(auc.val.avg))%>% 
				dplyr::filter(delta.AICc == min(delta.AICc))
			mod.seq <- eval.models(me)[[opt.seq$tune.args[1]]]
			scores.clim.MAXENT <- data.frame(predict(mod.seq, clim[,-c(1:2)], progress="text"))
			if (reg=="ea") occ.sp.sf<-subset(spdf,spdf$Lon>0)#original distribution in EA
			if (reg=="ena") occ.sp.sf<-subset(spdf,spdf$Lon<0)#naturalized distribution of aliens in ENA
			occ.sp=unique(na.omit(as.data.frame(occ.sp.sf)[,c("Lat","Lon",sl)]))#there are few grid cells without climate information
			scores.sp.MAXENT <- data.frame(predict(mod.seq, occ.sp[,-c(1:2)], progress="text"))
			z.maxent<- grid.clim(scores.clim.MAXENT,scores.clim.MAXENT,scores.sp.MAXENT,100)
			return(z.maxent)
		}
		z.ea.maxent =	get.z(clim1,occ.sp1,me.ea,alldf[[j]],"ea",sl.ea)
		z.ena.maxent =	get.z(clim2,occ.sp2,me.ena,alldf[[j]],"ena",sl.ena)
		z.ea=sp.niche[[j]][[1]]	
		z.ena=sp.niche[[j]][[2]]	
		niche.ovlp.maxent=1-(0.5*(sum(abs(z.ea.maxent$z.cor/sum(z.ea.maxent$z.cor)-z.ena.maxent$z.cor/sum(z.ena.maxent$z.cor)))))		
			
		get.niche.stat=function(z1,z2){
			# rescale occurence densities so that the sum of densities is the same for both species
			if (length(z1$z.cor)>100) {p1= rowSums(z1$z.cor)/sum(z1$z.cor)}else{p1<-z1$z.cor/sum(z1$z.cor)}
			p2<-z2$z.cor/sum(z2$z.cor)		
			re=data.frame(niche.suitable=sum(p1[p1>0&p2>0]),niche.occupy=length(p2[p2>0&p1>0])/length(p2[p2>0]))
			return(re)
		}
		niche.stat.ea=get.niche.stat(z.ea,z.ea.maxent)
		colnames(niche.stat.ea)=paste("ea",colnames(niche.stat.ea),sep=".")
		niche.stat.ena=get.niche.stat(z.ena,z.ena.maxent)
		colnames(niche.stat.ena)=paste("ena",colnames(niche.stat.ena),sep=".")
		re.t=data.frame(Genus=strsplit(j," ")[[1]][1],sp.niche[[j]][3],niche.stat.ea,niche.stat.ena,niche.ovlp.maxent)
		niche.suitable=rbind(niche.suitable,re.t)
	}
	write.csv(niche.suitable,"sp.niche2.maxent.csv")#add maxcent result into sp.niche2.Rdata	
	
	library(ggpubr)	
	library(gg.gap)	
	#install.packages("ggrepel")
	library(ggrepel)
	alien=read.csv("EA-ENA.splist.csv")
	overlap=na.omit(read.csv("sp.niche2.maxent.csv"))[-17,-1]
	
	my_col <- c("#E13A94", "#F588B8","#FEF201","brown","#8C52A2","#DEC3DE","#9EBC90","#2ABAA0","darkblue","green",
			"darkgray","lightgray","#FBAA72")
	my_col2 <-data.frame(Genus=unique(overlap$Genus), col=my_col)	
	p2=ggplot(overlap, aes(x=ea.niche.occupy*100, y=ena.niche.occupy*100,,fill=Genus))+
	xlab("Occupied suitalbe niches (EA,%)") +ylab("Occupied suitalbe niches (ENA,%)") +scale_fill_manual(values=my_col)+	
	geom_abline(intercept=0,slope=1, linetype = "twodash", col="black",size=1.5)+
	geom_point(size=5,shape=21,color="black",alpha=0.7)+ylim(0,100)+
	#geom_text_repel(aes(x=niche.size.change, y=range.change,label=Species,fontface = "italic"),size=3)+
	theme(axis.text = element_text(size=10,color='black',angle=0),		
		axis.title.x = element_text(size=15,color='black',angle=0),		
		axis.title.y = element_text(size=15,color='black',angle=90),
		panel.grid.minor.y=element_blank(),
		axis.line=element_line(linetype=1,color='black'),
		#axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank())
		
	p4=ggplot(overlap, aes(x=1-niche.overlap, y=1-niche.ovlp.maxent,fill=Genus))+
	xlab("Realized niche changes") +ylab("Suitable niche changes")+scale_fill_manual(values=my_col)+
	geom_abline(intercept=0,slope=1, linetype = "twodash", col="black",size=1.5)+
	geom_point(size=5,shape=21,color="black",alpha=0.8)+ylim(0,1)+
	#geom_text_repel(aes(x=niche.diff.ori, y=niche.diff.natur,label=Genus,fontface = "italic"),size=3)+
	theme(axis.text = element_text(size=10,color='black',angle=0),
		axis.title.x = element_text(size=15,color='black',angle=0),		
		axis.title.y = element_text(size=15,color='black',angle=90),
		panel.grid.minor.y=element_blank(),
		axis.line.y=element_line(linetype=1,color='black'),
		axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank(),
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))	
	
	ggarrange(p2,p4,nrow=2,ncol = 1,widths=c(1,1),heights=1,labels=c("a","b"),font.label = list(size = 25),
		hjust=-3.5,vjust=1.5,common.legend=TRUE,legend="right")
	
	
########################################
## plot niche overlap through time #####
########################################	
	library(data.table)
	library(magrittr)
	library(dplyr)
	library(sp)
	library(maptools)
	library(raster)
	library(rgeos)
	library(sf)
	var.list=c(paste("bio0",1:9,sep=""),paste("bio",10:19,sep=""))
	grid50=foreign::read.dbf("area/50km/grid_50km.dbf")				
	geo=read.csv("Geo-isrm.csv")
	adc.ea=geo[geo$continent=="Asia","ADCODE99"]
	adc.ena=geo[geo$continent=="NAmerica","ADCODE99"]
	ENA=subset(grid50,grid50$ADCODE99%in%adc.ena&grid50$Lon>=-105&grid50$Lon<=-45&grid50$Lat<60&grid50$Lat>10)
	EA=subset(grid50,grid50$ADCODE99%in%adc.ea&grid50$Lon>=100&grid50$Lat<60&grid50$Lat>18)
	clim1=na.omit(EA)[,c("Lon","Lat",var.list)]
	clim2=na.omit(ENA)[,c("Lon","Lat",var.list)]
	
	occ.clean=get(load("occ.clean2.Rdata"))
	alien=read.csv("EA-ENA.splist.csv")	
	genuslist=sort(unique(alien$Genus))
	splist.alien=sort(alien$Species_TPL)
	splist.native=sort(names(occ.clean)[!names(occ.clean)%in%splist.alien])
	timeline=c(seq(1830,2020,by=10),2022)
	gbif0=fread("occ_data.clean.csv")[,c("Acname","Lon","Lat","year2")]	%>% filter(!(is.na(year2)|is.na(Lon)|is.na(Lon))) 
		
	gbif0[,c("Lon","Lat")]=round(gbif0[,c("Lon","Lat")],0)	#remove points that are too close to each other, and the follow will 1 x 1 arc degree buffer
	gbif=gbif0  %>% filter(Acname%in%names(occ.clean))  %>% distinct()
	gbif.points = st_as_sf(gbif,coords=c("Lon","Lat")) %>% st_set_crs("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km")
	gbif_buffer = st_buffer(gbif.points, 1,endCapStyle="SQUARE")
	
	villages.ena = st_as_sf(ENA,coords=c("Lon","Lat")) %>% st_set_crs("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km")
	gbif_villages = st_intersection(gbif_buffer, villages.ena)	
	
	niche.cal3=function(genus,timeline,clim,gbif_villages,splist.native,splist.alien,occ.clean,var.list){	
		require(dplyr)				
		#clim=clim1;genus="Aralia";i=1970
		clim1=clim;clim2=clim
		# occ data		
		spname.native=splist.native[grep(genus,splist.native)]
		occ.native=do.call(rbind,occ.clean[spname.native])		
		occ.sp2=unique(na.omit(as.data.frame(occ.native)[,c("Lon","Lat",var.list)]))#ENA native
		range.size2=unique(as.data.frame(occ.native)[,c("Area","GRIDCODE")]) %>% sum(.[,"Area"])#area unit: km2
		spname.alien=splist.alien[grep(genus,splist.alien)]
		overlap=c()		
		for (i in timeline){
			occ.sp.natur= gbif_villages %>% filter(Acname%in%spname.alien)				
			a=st_coordinates(occ.sp.natur);colnames(a)=c("Lon","Lat")
			natur.t=as.data.frame(cbind(occ.sp.natur,a))%>% filter(year2<=i)%>% na.omit()
			if (nrow(natur.t)==0) next;
			occ.sp1=unique(natur.t[,c("Lon","Lat",var.list)])#ENA naturalizd
			range.size1=sum(unique(natur.t[,"Area"]))
			natur.all=unique(na.omit(as.data.frame(cbind(occ.sp.natur,a))[,c("Lon","Lat",var.list)]))
			get.niche=function(occ.sp1,clim1,clim2,occ.sp2){
				require(ade4)
				require(adehabitat)
				require(magrittr)
				source("niche.overlap.functions.R")
				source("occ.prep.functions.R")
		
				data.env.occ<-rbind(clim1,clim2,occ.sp1,occ.sp2)
				row.clim1<-1:nrow(clim1)
				row.clim2<-(nrow(clim1)+1):(nrow(clim1)+nrow(clim2))
				row.clim12<-1:(nrow(clim1)+nrow(clim2))
				row.sp1<-(nrow(clim1)+nrow(clim2)+1):(nrow(clim1)+nrow(clim2)+nrow(occ.sp1))
				row.sp2<-(nrow(clim1)+nrow(clim2)+nrow(occ.sp1)+1):(nrow(clim1)+nrow(clim2)+nrow(occ.sp1)+nrow(occ.sp2))
					
				# measures niche overlap along the two first axes of a PCA calibrated on occurence data
				row.w.1.env<-1-(nrow(clim1)/(nrow(clim1)+nrow(clim2)))  # prevalence of clim1
				row.w.2.env<-1-(nrow(clim2)/(nrow(clim1)+nrow(clim2)))  # prevalence of clim2
				row.w.env<-c(rep(row.w.1.env, nrow(clim1)),rep(row.w.2.env, nrow(clim2)),rep(0, nrow(occ.sp1)),rep(0, nrow(occ.sp2)))			
				pca.cal <-dudi.pca(data.env.occ[,-c(1:2)], row.w = row.w.env, center = T, scale = T, scannf = F, nf = 2)
					
				# predict the scores on the axes
				scores.clim12<- pca.cal$li[row.clim12,]
				scores.clim1<- pca.cal$li[row.clim1,]
				scores.clim2<- pca.cal$li[row.clim2,]
				scores.sp1<- pca.cal$li[row.sp1,]
				scores.sp2<- pca.cal$li[row.sp2,]
				# calculation of occurence density and test of niche similarity 
				
				z1<- grid.clim(scores.clim12,scores.clim1,scores.sp1,100)
				p1<-z1$z.cor/sum(z1$z.cor)	# rescale occurence densities so that the sum of densities is the same for both species
				niche.size1=length(p1[p1>0])				
				overlap.t=data.frame(niche.size1=niche.size1)				
				return(overlap.t)
			}
			
			overlap.t=get.niche(occ.sp1,clim1,clim2,occ.sp2)				
			if (nrow(occ.sp1)==nrow(natur.all)){
			overlap.t=cbind(Genus=genus,time=i,range.size1=range.size1,overlap.t,niche.rm.mean=overlap.t$niche.size1,
				niche.rm.l=overlap.t$niche.size1,niche.rm.h=overlap.t$niche.size1,niche.rm.se=0)
			} else{
				#random sample all known occurences at the same number of realized occurences at each time and caculate niche sizes for 100 times 
				get.rm=function(n,natur.all,occ.sp1){
					occ.sp1.rm.t = natur.all[sample(nrow(natur.all), nrow(occ.sp1)), ]
					return(occ.sp1.rm.t)
				}
				occ.sp1.rm=lapply(1:100,get.rm,natur.all,occ.sp1)			
				overlap.rm=do.call(rbind,parLapply(cl=mycl, X=occ.sp1.rm,get.niche,clim1,clim2,occ.sp2))
				if (nrow(unique(overlap.rm))==1){
					overlap.t=cbind(Genus=genus,time=i,range.size1=range.size1,overlap.t,niche.rm.mean=as.numeric(unique(overlap.rm)),
						niche.rm.l=as.numeric(unique(overlap.rm)),niche.rm.h=as.numeric(unique(overlap.rm)),niche.rm.se=0)
				} else {
					tmp=t.test(overlap.rm$niche.size1)
					overlap.t=cbind(Genus=genus,time=i,range.size1=range.size1,overlap.t,niche.rm.mean=tmp$est,
						niche.rm.l=tmp$conf[1],niche.rm.h=tmp$conf[2],niche.rm.se=tmp$std)
				}
			}		
			overlap=rbind(overlap,overlap.t)
		}				
		return(overlap)	
	}
	
	library(parallel)
	no_cores <- detectCores() - 1
	mycl <- makePSOCKcluster(no_cores);	
	re=c()
	for (j in genuslist){
	tmp=niche.cal3(j,timeline,clim1,gbif_villages,splist.native,splist.alien,occ.clean,var.list)
	re=rbind(re,tmp)	
	}
	write.csv(re,"time.nicheovlp.csv")	
	stopCluster(mycl)
	#changes in range size through time	
	library(dplyr)
	re=read.csv("time.nicheovlp.csv")[,-1]%>%filter(niche.rm.se>0&!Genus%in%"Osmanthus")
	library(ggpubr)
	my_col <- c("#E13A94", "#F588B8","#FEF201","brown","#8C52A2","#DEC3DE","#9EBC90","#2ABAA0","darkblue","green",
			"darkgray","lightgray","#FBAA72")
	scales::show_col(my_col)
	ggplot(data = re, mapping = aes(x = time, y = niche.size1,group=Genus,fill=Genus)) + 
	#geom_point(shape=21,size=5,color="black",alpha=0.6) +	
	xlab('Year of records')+ylab('Niche size')+scale_fill_manual(values=my_col)+#ylim(0,max(re$niche.size1))+
	geom_smooth(aes(x = time, y = niche.size1,color=Genus),data=re,method = "loess",size=1.5,show.legend=FALSE,se =TRUE,span=0.8,linetype=1)+
	geom_line(aes(x=time,y=niche.rm.mean,colour=Genus),size=1.5,linetype=2,show.legend=FALSE)+
	geom_ribbon(aes(x=time,ymin=niche.rm.l, ymax=niche.rm.h,fill=Genus),alpha=0.4,show.legend=FALSE)+
	scale_color_manual(values=my_col)+
	facet_wrap(~ Genus,scales="free")+
	theme(axis.text = element_text(size=10,color='black',angle=30,hjust=1),
		axis.title = element_text(size=15,color='black',angle=0),		
		panel.grid.minor.y=element_blank(),
		axis.line.y=element_line(linetype=1,color='black'),
		axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank(),
		strip.text=element_text(size=12,color='black'),
		strip.background = element_rect(fill = 'white', colour = 'black', size = rel(2), linetype = 2))
	
	#Earliest record year ~ niche/range size changes
	library(ggrepel)
	library(gg.gap)
	overlap=na.omit(read.csv("sp.niche2.maxent.csv"))[-17,-1]
	overlap=cbind(overlap,EarlyRecordyear=alien[match(overlap$Species,alien$Species_TPL),"EarlyRecordyear"])	#plot idexes of niche overlap
	
	p1t=ggplot(overlap, aes(x=EarlyRecordyear, y=niche.size.change,fill=Genus))+
	geom_point(size=5,shape=21,color="black",alpha=0.8)+
	scale_fill_manual(values=my_col)+
	#geom_text_repel(aes(x=EarlyRecordyear, y=range.change,label=Species,fontface = "italic"),size=3)+
	xlab("Year of The Earliest Record") +ylab("Proportion of \nniche size changes (%)") +
	theme(axis.text = element_text(size=12,color='black',angle=0,hjust=1),
		axis.title = element_text(size=15,color='black'),
		panel.grid.minor.y=element_blank(),
		axis.line.y=element_line(linetype=1,color='black'),
		axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank(),
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))	
		
	p2t=ggplot(overlap, aes(x=EarlyRecordyear, y=ena.niche.occupy,fill=Genus))+
	geom_point(size=5,shape=21,color="black",alpha=0.8)+
	scale_fill_manual(values=my_col)+
	#geom_text_repel(aes(x=EarlyRecordyear, y=range.change,label=Species,fontface = "italic"),size=3)+
	xlab("Year of The Earliest Record") +ylab("Occupied suitable \nniches in ENA(%)") +
	theme(axis.text = element_text(size=12,color='black',angle=0,hjust=1),
		axis.title = element_text(size=15,color='black'),	
		panel.grid.minor.y=element_blank(),
		axis.line.y=element_line(linetype=1,color='black'),
		axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank(),
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))	
	
	p3t=ggplot(data = re, mapping = aes(x = time, y = range.size1/10000,group=Genus)) + 
	geom_point(aes(fill=Genus),shape=21,size=3,color="black",alpha=0.6) +	
	xlab('Year of records')+ylab('Range size \n(10,000 km2)')+scale_fill_manual(values=my_col)+#ylim(0,max(re$niche.size1))+
	geom_smooth(aes(x = time, y = range.size1/10000,color=Genus),data=re,method = "loess",size=1.2,show.legend=FALSE,se =FALSE,span=0.8,linetype=2)+
	theme(axis.text = element_text(size=12,color='black',angle=0,hjust=1),
		axis.title = element_text(size=15,color='black',angle=0),	
		panel.grid.minor.y=element_blank(),
		axis.line.y=element_line(linetype=1,color='black'),
		axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank(),
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))		
	ggarrange(p1t,p2t,p3t,nrow=3,ncol =1,widths=c(1,1,1),heights=1,labels=c("a","b","c"),font.label = list(size = 25),
		hjust=0,vjust=1.2,common.legend=TRUE,legend="right")
	
	
####################################################
## link niche overlap with phylogenetic distances ##
####################################################		
#find the phylogeny for each genus
#using different color to illus different genus in the scanter plot
#draw PGLS regression for each genus and for overall dataset, seperatly

#phylogenetic tree
# ref:Smith, S. A., and J. W. Brown. 2018. Constructing a broadly inclusive seed plant phylogeny. American Journal of Botany 105(3): 1�?13.
# download from: https://github.com/FePhyFoFum/big_seed_plant_trees
library(ape)
text="(Catalpa_purpurea:22.35,(Catalpa_ovata:11.68,(Catalpa_speciosa:0.584,Catalpa_bignonioides:0.584):11.096):10.67);"
tre_Catalpa=read.tree(text=text)#Catalpa phylogeny ref:Dong et al. 2022 Molecular Phylogenetics and Evolution 166
tre_Cornus0=read.nexus("Cornus-beast.tre")#Cornus phylogeny ref:Zhiyuan Du
tip.Cornus=na.omit(read.csv("Cornus.tip.csv"))
tre_Cornus=keep.tip(tre_Cornus0, tip=tip.Cornus$tip)
tre_Cornus$tip.label=tip.Cornus[match(tre_Cornus$tip.label,tip.Cornus$tip),"tip1"]
tre_Mag0=read.tree("Magsnp_min50_50taxa.trees.biogeo.out.newick")#Magnolia phylogeny ref:Dong et al. (2021, Journal of Systematics and Evolution) 
tre_Mag=drop.tip(tre_Mag0,tip=c("Liriodendron_tulipifera","Liriodendron_chinense"))
tre_Mag$tip.label=gsub("M.", "Magnolia_", tre_Mag$tip.label)
tre0=read.tree("BigTreeSmithBrown2018v0.1/ALLOTB.tre")#356305 tips
gbif.clean=get(load("occ.clean3.Rdata"))	
genuslist=unique(do.call(c,lapply(strsplit(names(gbif.clean), " "),function(x) x[[1]])))
#genuslist=genuslist[-10]
tip.keep=do.call(c,lapply(genuslist[-c(4,5,8)],function(x,tip.list) tip.list[grep(x,tip.list)],tre0$tip.label))
tre=keep.tip(tre0, tip=tip.keep)
tip.new=c(tre_Mag$tip.label,tre_Cornus$tip.label,tre_Catalpa$tip.label,tre$tip.label)
write.csv(tip.new,"phy2dis.csv")#then correct names in TNRS

phy2dis=read.csv("phy2dis.csv")#the matched tip of trees and spname
alien0=read.csv("EA-ENA.splist.csv")[-18,]	
splist.alien=sort(alien0$Species_TPL)
splist.native=sort(names(gbif.clean)[!names(gbif.clean)%in%splist.alien])
	
source("read_Phylogenetic_Tree.R")
#plot the tree for each genus
alien=phy2dis[phy2dis$Accepted_name%in%splist.alien,"tip"]
layout(matrix(1:15, 3, 5, byrow = TRUE))
par(mar=c(2.1,1,2,0.15), cex=1, cex.axis=0.8, cex.lab=1.2, mgp=c(2.4,0.3,0), tck=-0.04)
for (i in genuslist){
sp.t=names(gbif.clean)[grep(i,names(gbif.clean))]
if (i%in%"Catalpa"){tre1=keep.tip(tre_Catalpa, tip=unique(phy2dis[phy2dis$Accepted_name%in%sp.t,"tip"]))}
if (i%in%"Cornus"){tre1=keep.tip(tre_Cornus, tip=unique(phy2dis[phy2dis$Accepted_name%in%sp.t,"tip"]))}
if (i%in%"Magnolia"){tre1=keep.tip(tre_Mag, tip=unique(phy2dis[phy2dis$Accepted_name%in%sp.t,"tip"]))}
if (!i%in%c("Cornus","Catalpa","Magnolia")){tre1=keep.tip(tre, tip=unique(phy2dis[phy2dis$Accepted_name%in%sp.t,"tip"]))}
plot(tre1,show.tip.label=F,edge.width=rep(3,Ntip(tre1)),main=i,cex.main = 2, label.offset = 4, no.margin = F, cex = 3)
tiplabels(text=rep("",Ntip(tre1)),tip=seq(1,Ntip(tre1),1),cex=1,frame="circle",bg=ifelse(tre1$tip.label %in% alien,'sandybrown','#619CFF'))
axisPhylo(cex.axis=2,lwd=1.2,padj=0.8)
}	

#phylodist matrix
phy.matrix=list();
for (i in genuslist){
sp.t=names(gbif.clean)[grep(i,names(gbif.clean))]
if (i%in%"Catalpa"){tre1=keep.tip(tre_Catalpa, tip=unique(phy2dis[phy2dis$Accepted_name%in%sp.t,"tip"]))}
if (i%in%"Cornus"){tre1=keep.tip(tre_Cornus, tip=unique(phy2dis[phy2dis$Accepted_name%in%sp.t,"tip"]))}
if (i%in%"Magnolia"){tre1=keep.tip(tre_Mag, tip=unique(phy2dis[phy2dis$Accepted_name%in%sp.t,"tip"]))}
if (!i%in%c("Cornus","Catalpa","Magnolia")){tre1=keep.tip(tre, tip=unique(phy2dis[phy2dis$Accepted_name%in%sp.t,"tip"]))}
phy.matrix[[i]] <- phylo.dist(tre1, tip1=tre1$tip.label,method = "branch.length")
}	

#niche overlap matrix
library(data.table)
vlist=c(paste("bio0",1:9,sep=""),paste("bio",10:19,sep=""))
grid50=foreign::read.dbf("area/50km/grid_50km.dbf")				
geo=read.csv("Geo-isrm.csv")
adc.ea=geo[geo$continent=="Asia","ADCODE99"]
adc.ena=geo[geo$continent=="NAmerica","ADCODE99"]
ENA=subset(grid50,grid50$ADCODE99%in%adc.ena&grid50$Lon>=-105&grid50$Lon<=-45&grid50$Lat<60&grid50$Lat>10)
EA=subset(grid50,grid50$ADCODE99%in%adc.ea&grid50$Lon>=100&grid50$Lat<60&grid50$Lat>18)
clim1=na.omit(EA)[,c("Lon","Lat",vlist)]
clim2=na.omit(ENA)[,c("Lon","Lat",vlist)]

get.overlap.matrix=function(genus,phy2dis,clim1,clim2,gbif.clean,vlist,R=100,splist.alien,typ=c("natur","ori")){
	niche.cal4=function(tip.sp1,tip.sp2,clim1,clim2,gbif.clean,vlist,R,phy2dis,splist.alien,typ=c("natur","ori")){
		# for (i in 1:length(splist)){
		# sp1="Ampelopsis aconitifolia";sp2="Ampelopsis cordata"
		require(ade4)
		require(adehabitat)	
		source("niche.overlap.functions.R")
		source("occ.prep.functions.R")
		sp1=unique(phy2dis[phy2dis$tip%in%tip.sp1,"Accepted_name"])
		sp2=unique(phy2dis[phy2dis$tip%in%tip.sp2,"Accepted_name"])
		
		get.occ.clim=function(gbif.clean,sp,splist.alien,typ){
			occ.sp=unique(na.omit(as.data.frame(gbif.clean[[sp]])[,c("Lat","Lon",vlist)]))
			if (sp%in%splist.alien){
				if (typ=="natur"){
					#occ.sp=subset(occ.sp,occ.sp$Lon<0)
					clim=clim2
				}
				if(typ=="ori"){
					#occ.sp=subset(occ.sp,occ.sp$Lon>0)
					clim=clim1		
				}
			}else{
				clim=clim2
			}
			return(list(occ.sp,clim))		
		}
		sp.clim1=get.occ.clim(gbif.clean,sp1,splist.alien,typ)
		sp.clim2=get.occ.clim(gbif.clean,sp2,splist.alien,typ)
		
		data.env.occ<-rbind(sp.clim1[[2]],sp.clim2[[2]],sp.clim1[[1]],sp.clim2[[1]])
		row.clim1<-1:nrow(sp.clim1[[2]])
		row.clim2<-(nrow(sp.clim1[[2]])+1):(nrow(sp.clim1[[2]])+nrow(sp.clim2[[2]]))
		row.clim12<-1:(nrow(sp.clim1[[2]])+nrow(sp.clim2[[2]]))
		row.sp1<-(nrow(sp.clim1[[2]])+nrow(sp.clim2[[2]])+1):(nrow(sp.clim1[[2]])+nrow(sp.clim2[[2]])+nrow(sp.clim1[[1]]))
		row.sp2<-(nrow(sp.clim1[[2]])+nrow(sp.clim2[[2]])+nrow(sp.clim1[[1]])+1):(nrow(sp.clim1[[2]])+nrow(sp.clim2[[2]])+nrow(sp.clim1[[1]])+nrow(sp.clim2[[1]]))
		
		# measures niche overlap along the two first axes of a PCA calibrated on occurence data
		row.w.1.env<-1-(nrow(sp.clim1[[2]])/(nrow(sp.clim1[[2]])+nrow(sp.clim2[[2]])))  # prevalence of clim1
		row.w.2.env<-1-(nrow(sp.clim2[[2]])/(nrow(sp.clim1[[2]])+nrow(sp.clim2[[2]])))  # prevalence of clim2
		row.w.env<-c(rep(row.w.1.env, nrow(sp.clim1[[2]])),rep(row.w.2.env, nrow(sp.clim2[[2]])),rep(0, nrow(sp.clim1[[1]])),rep(0, nrow(sp.clim2[[1]])))			
		pca.cal <-dudi.pca(data.env.occ[,-c(1:2)],row.w = row.w.env, center = T, scale = T, scannf = F, nf = 2)
		
		# predict the scores on the axes
		scores.clim12<- pca.cal$li[row.clim12,]
		scores.clim1<- pca.cal$li[row.clim1,]
		scores.clim2<- pca.cal$li[row.clim2,]
		scores.sp1<- pca.cal$li[row.sp1,]
		scores.sp2<- pca.cal$li[row.sp2,]
		# calculation of occurence density and test of niche similarity 
		z1<- grid.clim(scores.clim12,scores.clim1,scores.sp1,R)
		z2<- grid.clim(scores.clim12,scores.clim2,scores.sp2,R)
		
		p1<-z1$z.cor/sum(z1$z.cor)	# rescale occurence densities so that the sum of densities is the same for both species
		p2<-z2$z.cor/sum(z2$z.cor)	# rescale occurence densities so that the sum of densities is the same for both species				
		
		niche.ovlp=1-(0.5*(sum(abs(p1-p2))))			
		return(niche.ovlp)		
	}	
	
	sp.t=names(gbif.clean)[grep(genus,names(gbif.clean))]
	sp <- unique(phy2dis[phy2dis$Accepted_name%in%sp.t,"tip"])
	d=matrix(nrow=length(sp),ncol=length(sp),NA)
	rownames(d) <- sp;colnames(d) <- sp

	for (m in 1:(nrow(d)-1)) {
		for (n in (m+1):nrow(d)) {
			d[m,n]=niche.cal4(sp[m],sp[n],clim1,clim2,gbif.clean,vlist,R,phy2dis,splist.alien,typ)
			}
		print(m)
		}
	return(d)
}
niche.matrix.ori=list();
niche.matrix.natur=list();
require(parallel)
no_cores <- detectCores() - 1
mycl <- makePSOCKcluster(no_cores); 
date()
niche.matrix.ori=parLapply(cl=mycl,genuslist,get.overlap.matrix,phy2dis,clim1,clim2,gbif.clean,vlist,R=100,splist.alien,typ="ori")
niche.matrix.nat=parLapply(cl=mycl,genuslist,get.overlap.matrix,phy2dis,clim1,clim2,gbif.clean,vlist,R=100,splist.alien,typ="natur")
names(niche.matrix.ori)=genuslist;names(niche.matrix.nat)=genuslist
stopCluster(mycl)	

#arrange the matrix into data frame
get.mat2datfrm=function(ge,phy.matrix,niche.matrix,alien){
	phy=phy.matrix[[ge]];
	get.niche.d=function(niche,phy,alien){
		niche[is.na(niche)]=0
		niche=niche+t(niche)
		diag(niche)=NA
		niche=niche[rownames(phy),colnames(phy)]
		niche[lower.tri(niche)]=NA
		#plot(phy[upper.tri(phy)],niche[upper.tri(niche)])	
		alien.alien.n=as.numeric(niche[which(rownames(niche)%in%alien),which(colnames(niche)%in%alien)])
		native.native.n=as.numeric(niche[which(!rownames(niche)%in%alien),which(!colnames(niche)%in%alien)])
		alien.native.n=c(as.numeric(niche[which(rownames(niche)%in%alien),which(!colnames(niche)%in%alien)]),
			as.numeric(niche[which(!rownames(niche)%in%alien),which(colnames(niche)%in%alien)]))
		re.n=rbind(data.frame(type="alien.alien",niche.overlap=alien.alien.n),
				data.frame(type="alien.native",niche.overlap=alien.native.n),
				data.frame(type="native.native",niche.overlap=native.native.n))
		return(re.n)
	}
	re.n.ori=get.niche.d(niche.matrix.ori[[ge]],phy,alien)
	re.n.natur=get.niche.d(niche.matrix.nat[[ge]],phy,alien)
	re.p=get.niche.d(phy,phy,alien)
				
	re=na.omit(data.frame(Genus=ge,type=re.n.ori[,1],niche.overlap.ori=re.n.ori[,2],
		niche.overlap.natur=re.n.natur[,2],phylo.dis=re.p[,2]))
	return(re)	
}
mat2datfrm=do.call(rbind,lapply(genuslist,get.mat2datfrm,phy.matrix,niche.matrix,alien))
#write.csv(mat2datfrm,"mat2datfrm2.csv")
write.csv(mat2datfrm,"mat2datfrm3.csv")

require(dplyr)
mat2datfrm=read.csv("mat2datfrm3.csv")[,-1]#%>%filter(!Genus%in%"Osmanthus")
#plot nicheoverlap~phylodis
library(ggpubr)
library(scales)
mat2datfrm$type=factor(mat2datfrm$type,levels=c("alien.alien","alien.native","native.native"))
my_col <- data.frame(Genus=genuslist,col=c("#E13A94", "#F588B8","#FEF201","brown","#8C52A2","#DEC3DE","#9EBC90","#2ABAA0","darkblue","green",
			"darkgray","lightgray","#FBAA72"))
	
#Fig.a
get.scant=function(pdat,x,my_col,leg,y.title){
	ss.glm <- function(r.glm)
                {
                r.ss <- summary(r.glm)
                rsq <- 100*(r.ss$null.deviance-r.ss$deviance)/r.ss$null.deviance
                adj.rsq <- 100*(1-(r.ss$deviance/r.ss$df.residual)/(r.ss$null.deviance/r.ss$df.null))
                f.stat <- ((r.ss$null.deviance-r.ss$deviance)/(r.ss$df.null-
                        r.ss$df.residual))/(r.ss$deviance/r.ss$df.residual)
                P <- pf(f.stat, r.ss$df.null-r.ss$df.residual, r.ss$df.residual, lower.tail=FALSE)
				Px=ifelse(P <= 0.0001,"****",
					ifelse(P>0.0001&P<=0.001,"***",
					ifelse(P>0.001&P<=0.01,"**",
					ifelse(P>0.01&P<=0.05,"*",
					#ifelse(P>0.05&P<0.1,"",
					ifelse(P>0.05,"ns",P)))))	
                return(data.frame(r2=rsq,adj.r2=adj.rsq,p=P,px=Px,s=r.glm$coef[[2]]))
                }
	
	pdat$Genus=factor(pdat$Genus,levels=sort(unique(pdat$Genus)))
	p1=ggplot(pdat, aes(x=phylo.dis, y=niche.overlap.natur))+
		scale_x_continuous(trans = log_trans(),breaks = trans_breaks("log", function(x) exp(x)),labels = trans_format("log", math_format(e^.x)))+
		xlab("Phylogenetic distance") +ylab("Niche overlap") +	
		geom_point(aes(fill=Genus),size=5,shape=21,color="black",alpha=0.8)+
		scale_fill_manual(values=my_col[match(unique(pdat$Genus),my_col$Genus),"col"])+
		geom_smooth(aes(x=phylo.dis,y=niche.overlap.natur),data=pdat,colour="black",method = "glm",size=2,show.legend=FALSE,se = TRUE)+	
		geom_smooth(aes(x=phylo.dis,y=niche.overlap.natur,colour=Genus),data=pdat,method = "glm",size=1,show.legend=FALSE,se =FALSE,linetype=2,alpha=0.8)
	
	count=tapply(pdat$niche.overlap.natur,pdat$Genus,length)
	genus.remain=sort(names(count[count>1]))
	pdat[pdat$niche.overlap.natur==0,"niche.overlap.natur"]=min(pdat[pdat$niche.overlap.natur!=0,"niche.overlap.natur"])
	d=glm(log(niche.overlap.natur)~log(phylo.dis),data = pdat)
	p1=p1+scale_color_manual(values=my_col[match(genus.remain,my_col$Genus),"col"])+	
	annotate("text", x=x , y= 1,size=6,label=paste("R2 = ",round(ss.glm(d)[1],2),ss.glm(d)[4],sep="") )+
		theme(axis.text = element_text(size=12,color='black',angle=0),						
			panel.grid.minor.y=element_blank(),
			axis.line=element_line(linetype=1,color='black'),
			axis.ticks = element_line(linetype=2,color='black'),
			panel.grid=element_line(linetype=2,color='grey'),
			panel.background = element_blank())	
	if (leg==TRUE){
	p1=p1+theme(axis.title.x =element_blank(),		
			axis.title.y = element_text(size=15,color='black',angle=90),
			legend.background = element_rect(fill = NA),
			legend.text=element_text(face="bold.italic",size=10),
			legend.title=element_text(face="bold",size=12))
	}else{
	p1=p1+theme(axis.title.x =element_text(size=15,color='black'),		
			axis.title.y = element_text(size=15,color='black',angle=90),
			legend.position="none")
	}
	
return(p1)			
}
pdat1=mat2datfrm[mat2datfrm$type=="alien.native",]
pdat2=mat2datfrm[mat2datfrm$type=="alien.alien",]
pdat3=mat2datfrm[mat2datfrm$type=="native.native",]
p1=get.scant(pdat1,8,my_col,TRUE)
p2=get.scant(pdat2,8,my_col,FALSE)
p3=get.scant(pdat3,4,my_col,FALSE)
p23=ggarrange(p2,p3,nrow=1,ncol = 2,widths=c(1,1),heights=1,labels=c("b","c"),font.label = list(size = 20),hjust=0)
ggarrange(p1,p23,nrow=2,ncol = 1,widths=c(1,1),heights=1,labels=c("a",""),font.label = list(size = 20),hjust=0)

#compare niche overlap (Natur-native vs Ori-native)			
p.dat=rbind(data.frame(type="Original",Rmisc::summarySE(data.frame(Genus=pdat1[,1],overlap=pdat1[,3]),
		measurevar="overlap", groupvars="Genus")),
	data.frame(type="Naturalized",Rmisc::summarySE(data.frame(Genus=pdat1[,1],overlap=pdat1[,4]),
		measurevar="overlap", groupvars="Genus")))	
p.dat[is.na(p.dat)]=0

p.overlap=ggplot(p.dat, aes(x=Genus, y=overlap,fill=type)) + 	
	geom_errorbar(aes(ymin=overlap-se, ymax=overlap+se,color=type),size=0.8,width=0.1,show.legend=FALSE,alpha=0.6)+
	geom_point(size=4,shape=21,color="black") +		
	#scale_fill_manual(values=c("red","blue"))+	
	labs(y="Niche overlap with \nENA native species",fill="Niche type of naturalizd species") +theme_bw()+
	theme(axis.text = element_text(size=12,color='black'),
		axis.title.y = element_text(size=15,color='black'),
		axis.title.x= element_blank(),
		axis.text.x = element_text(face="italic",angle=15),
		legend.background=element_rect(fill='transparent'),
		legend.text=element_text(size=12),
		legend.title=element_text(face="bold",size=12),
		legend.position = c(0.2,0.8))

# plot niche expansion/unfilling ~ niche changes	
	library(ggpubr)	
	library(gg.gap)	
	#install.packages("ggrepel")
	library(ggrepel)
	alien=read.csv("EA-ENA.splist.csv")
	overlap=na.omit(read.csv("sp.niche2.maxent.csv"))[-17,-1]
	
	my_col <- c("#E13A94", "#F588B8","#FEF201","brown","#8C52A2","#DEC3DE","#9EBC90","#2ABAA0","darkblue","green",
			"darkgray","lightgray","#FBAA72")
	my_col2 <-data.frame(Genus=unique(overlap$Genus), col=my_col)	
	p1=ggplot(overlap, aes(x=niche.loss*100, y=niche.expansion*100,fill=Genus))+
	geom_abline(intercept=0,slope=1, linetype = "twodash", col="black",size=1.5)+
	geom_point(size=5,shape=21,color="black",alpha=0.8)+
	scale_fill_manual(values=my_col)+	
	#geom_text_repel(aes(x=niche.size.change, y=niche.expansion,label=Species,fontface = "italic"),size=3)+
	xlab("Niche unfilling (%)") +ylab("Niche expansion (%)") +	ylim(0,100)+
	theme(axis.text = element_text(size=12,color='black',angle=0),
		axis.title.x = element_text(size=15,color='black',angle=0),		
		axis.title.y = element_text(size=15,color='black',angle=90),
		panel.grid.minor.y=element_blank(),
		axis.line.y=element_line(linetype=1,color='black'),
		axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank(),		
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))			
	
	#niche.size.change/niche.shift ~ niche.loss + niche.expansion
	#hp
	hp1=hier.part::hier.part(overlap$niche.overlap,overlap[,c("niche.expansion","niche.loss")],gof = "Rsqu",barplot = FALSE)$IJ
	hp.p1=rbind(data.frame(vars=rownames(hp1),rsq=hp1$I*100,Rsq.type="Independent"),data.frame(vars=rownames(hp1),rsq=hp1$J*100,Rsq.type="Joint"))		
	p1.hp=ggplot(hp.p1,aes(x=vars,y=rsq,fill=Rsq.type))+ geom_bar(stat="identity",position = "stack")+ 
	geom_hline(yintercept=0,col="black",size=1,linetype="longdash")+
	ylab("Explained variation (R2, %)") +
	scale_x_discrete("Drivers on niche change",labels=c("Expansion","Unfilling"))+	
	theme(axis.text = element_text(size=12,color='black',angle=0),
			panel.grid.minor.y=element_blank(),
			axis.line.y=element_line(linetype=1,color='black'),
			axis.line.x=element_line(linetype=1,color='black'),
			axis.ticks = element_line(linetype=2,color='black'),
			panel.grid=element_line(linetype=2,color='grey'),
			panel.background = element_blank(),			
			axis.title.x = element_text(size=15,color='black',angle=0),
			axis.title.y = element_text(size=15,color='black',angle=90),
			legend.background = element_rect(fill = NA),
			legend.text=element_text(face="bold.italic",size=12),
			legend.title=element_text(face="bold",size=12))
	
	part2=ggarrange(p1,p1.hp,nrow=1,ncol = 2,widths=c(2,1),heights=1,labels=c("b","c"),font.label = list(size = 25),
		hjust=-0.6,vjust=0.8,common.legend=FALSE)		
	ggarrange(p.overlap,part2,nrow=2,ncol = 1,widths=c(1,1),heights=1,labels=c("a",""),font.label = list(size = 25))
	
	
	ss.glm <- function(r.glm)
                {
                r.ss <- summary(r.glm)
                rsq <- 100*(r.ss$null.deviance-r.ss$deviance)/r.ss$null.deviance
                adj.rsq <- 100*(1-(r.ss$deviance/r.ss$df.residual)/(r.ss$null.deviance/r.ss$df.null))
                f.stat <- ((r.ss$null.deviance-r.ss$deviance)/(r.ss$df.null-
                        r.ss$df.residual))/(r.ss$deviance/r.ss$df.residual)
                P <- pf(f.stat, r.ss$df.null-r.ss$df.residual, r.ss$df.residual, lower.tail=FALSE)
				Px=ifelse(P <= 0.0001,"****",
					ifelse(P>0.0001&P<=0.001,"***",
					ifelse(P>0.001&P<=0.01,"**",
					ifelse(P>0.01&P<=0.05,"*",
					#ifelse(P>0.05&P<0.1,"",
					ifelse(P>0.05,"ns",P)))))	
                return(data.frame(r2=rsq,adj.r2=adj.rsq,p=P,px=Px,s=r.glm$coef[[2]]))
                }
	them=theme(axis.text.x = element_text(size=10,color='black',angle=0),
			panel.grid.minor.y=element_blank(),
			axis.line.y=element_line(linetype=1,color='black'),
			axis.line.x=element_line(linetype=1,color='black'),
			axis.ticks = element_line(linetype=2,color='black'),
			panel.grid=element_line(linetype=2,color='grey'),
			panel.background = element_blank(),			
			axis.text=element_text(face="bold",size=11.5),				
			axis.title.x = element_text(size=15,color='black',angle=0),
			axis.title.y = element_text(size=15,color='black',angle=90),
			axis.title=element_text(size=11.5),
			legend.background = element_rect(fill = NA),
			legend.text=element_text(face="bold.italic",size=12),
			legend.title=element_text(face="bold",size=12))	
	d1=glm(niche.overlap~niche.loss,data = overlap)
	p1.over=ggplot(overlap, aes(x=niche.loss*100, y=niche.overlap))+
		xlab("Niche unfilling") +ylab("Niche changes") +
		geom_point(aes(fill=Genus),size=5,shape=21,color="black",alpha=0.8)+
		scale_fill_manual(values=my_col2[match(unique(overlap$Genus),my_col2$Genus),"col"])+
		#scale_y_continuous(limits=c(0,1),breaks=round(seq(0,1,length.out=6),1))+
		geom_smooth(aes(x=niche.loss*100, y=niche.overlap),data=overlap,colour="black",method = "glm",size=2,show.legend=FALSE,se = TRUE)+
		annotate("text", x=80 , y= 0.5,size=5,label=paste("Slope = ",round(ss.glm(d1)[5],2),"\n","R2 = ",round(ss.glm(d1)[1],2),ss.glm(d1)[4],sep="") )+
		them
	
	d2=glm(niche.overlap~niche.expansion,data = overlap)
	p2.over=ggplot(overlap, aes(x=niche.expansion*100, y=niche.overlap))+
		xlab("Niche expansion") +ylab("Niche changes") +
		geom_point(aes(fill=Genus),size=5,shape=21,color="black",alpha=0.8)+
		scale_fill_manual(values=my_col2[match(unique(overlap$Genus),my_col2$Genus),"col"])+
		#scale_y_continuous(limits=c(0,1),breaks=round(seq(0,1,length.out=6),1))+		
		geom_smooth(aes(x=niche.expansion*100, y=niche.overlap),data=overlap,colour="black",method = "glm",size=2,show.legend=FALSE,se = TRUE)+
		annotate("text", x=25 , y= 0.5,size=5,label=paste("Slope = ",round(ss.glm(d2)[5],2),"\n","R2 = ",round(ss.glm(d2)[1],2),ss.glm(d2)[4],sep="") )+
		them
	ggarrange(p1.over,p2.over,nrow=2,ncol = 1,labels=c("a","b"),common.legend=TRUE,legend="right")
		
####  code graveyard  ------

##STEP2 niche diff between alien and naturalized species
	library(data.table)		
	gbif.clean=get(load("occ.clean3.Rdata"))	
	alien=read.csv("EA-ENA.splist.csv")[-18,]	
	genuslist=sort(unique(alien$Genus))
	splist.alien=sort(alien$Species_TPL)
	splist.native=sort(names(gbif.clean)[!names(gbif.clean)%in%splist.alien])
	var.list=c(paste("bio0",1:9,sep=""),paste("bio",10:19,sep=""))
	grid50=maptools::readShapeSpatial("area/50km/grid_50km.shp")				
	geo=read.csv("Geo-isrm.csv")
	adc.ea=geo[geo$continent=="Asia","ADCODE99"]
	adc.ena=geo[geo$continent=="NAmerica","ADCODE99"]
	ENA=subset(grid50,grid50@data$ADCODE99%in%adc.ena&grid50@data$Lon>=-105&grid50@data$Lon<=-45&grid50@data$Lat<60&grid50@data$Lat>10)
	EA=subset(grid50,grid50@data$ADCODE99%in%adc.ea&grid50@data$Lon>=100&grid50@data$Lat<60&grid50@data$Lat>18)
	clim1=na.omit(EA@data)[,c("Lat","Lon",var.list)]
	clim2=na.omit(ENA@data)[,c("Lat","Lon",var.list)]
	R=100
	
	niche.cal2=function(genus,splist.alien,splist.native,clim1,clim2,gbif.clean,adc.ea,adc.ena,vlist,R){
		# for (i in 1:length(splist)){
		# genus=genuslist[i]
		require(ade4)
		require(adehabitat)	
		source("niche.overlap.functions.R")
		source("occ.prep.functions.R")
		# occ data
		spname.alien=splist.alien[grep(genus,splist.alien)]
		spname.native=splist.native[grep(genus,splist.native)]
		occ.native=do.call(rbind,gbif.clean[spname.native])
		occ.alien=do.call(rbind,gbif.clean[spname.alien])
		occ.original<-subset(occ.alien,occ.alien$ADCODE99%in%adc.ea)#original distribution in EA
		occ.naturalizd<-subset(occ.alien,occ.alien$ADCODE99%in%adc.ena)#naturalized distribution of aliens in ENA
		
		occ.sp.native=unique(na.omit(as.data.frame(occ.native)[,c("Lat","Lon",vlist)]))#ENA native
		occ.sp.natur=unique(na.omit(as.data.frame(occ.naturalizd)[,c("Lat","Lon",vlist)]))#ENA naturalizd
		occ.sp.ori=unique(na.omit(as.data.frame(occ.original)[,c("Lat","Lon",vlist)]))#EA original
				
		get.z=function(genus,clim1,clim2,occ.sp1,occ.sp2,occ.sp1.sf,occ.sp2.sf){
			# global dataset for the analysis and rows for each sub dataset
			data.env.occ<-rbind(clim1,clim2,occ.sp1,occ.sp2)
			row.clim1<-1:nrow(clim1)
			row.clim2<-(nrow(clim1)+1):(nrow(clim1)+nrow(clim2))
			row.clim12<-1:(nrow(clim1)+nrow(clim2))
			row.sp1<-(nrow(clim1)+nrow(clim2)+1):(nrow(clim1)+nrow(clim2)+nrow(occ.sp1))
			row.sp2<-(nrow(clim1)+nrow(clim2)+nrow(occ.sp1)+1):(nrow(clim1)+nrow(clim2)+nrow(occ.sp1)+nrow(occ.sp2))
			
			# measures niche overlap along the two first axes of a PCA calibrated on occurence data
			row.w.1.env<-1-(nrow(clim1)/(nrow(clim1)+nrow(clim2)))  # prevalence of clim1
			row.w.2.env<-1-(nrow(clim2)/(nrow(clim1)+nrow(clim2)))  # prevalence of clim2
			row.w.env<-c(rep(row.w.1.env, nrow(clim1)),rep(row.w.2.env, nrow(clim2)),rep(0, nrow(occ.sp1)),rep(0, nrow(occ.sp2)))			
			pca.cal <-dudi.pca(data.env.occ[,-c(1:2)], row.w = row.w.env, center = T, scale = T, scannf = F, nf = 2)
			
			# predict the scores on the axes
			scores.clim12<- pca.cal$li[row.clim12,]
			scores.clim1<- pca.cal$li[row.clim1,]
			scores.clim2<- pca.cal$li[row.clim2,]
			scores.sp1<- pca.cal$li[row.sp1,]
			scores.sp2<- pca.cal$li[row.sp2,]
			# calculation of occurence density and test of niche similarity 
			z1<- grid.clim(scores.clim12,scores.clim1,scores.sp1,R)
			z2<- grid.clim(scores.clim12,scores.clim2,scores.sp2,R)
			
			p1<-z1$z.cor/sum(z1$z.cor)	# rescale occurence densities so that the sum of densities is the same for both species
			p2<-z2$z.cor/sum(z2$z.cor)	# rescale occurence densities so that the sum of densities is the same for both species				
			
			niche.ovlp=1-(0.5*(sum(abs(p1-p2))))		
			niche.size1=length(p1[p1>0]);niche.size2=length(p2[p2>0])	
			## runs niche similarity test(see Warren et al 2008) based on two species occurrence density grids		
			a<-niche.equivalency.test(z1,z2,rep=100)
			P = a[[3]]
			Px=ifelse(P <= 0.0001,"****",
								ifelse(P>0.0001&P<=0.001,"***",
								ifelse(P>0.001&P<=0.01,"**",
								ifelse(P>0.01&P<=0.05,"*",
								#ifelse(P>0.05&P<0.1,"",
								ifelse(P>0.05,"ns",P)))))	
			rangesize1=unique(as.data.frame(occ.sp1.sf)[,c("Area","GRIDCODE")])#area unit: km2
			rangesize2=unique(as.data.frame(occ.sp2.sf)[,c("Area","GRIDCODE")])#area unit: km2		
			range.size1=sum(rangesize1$Area);	range.size2=sum(rangesize2$Area)
			overlap.t=data.frame(Genus=genus,
				niche.overlap=niche.ovlp,niche.overlap.p1 = paste(round(niche.ovlp,3),Px,sep=""),niche.overlap.p2 = P,
				niche.size1=niche.size1,niche.size2=niche.size2,				
				range.size1=range.size1,range.size2=range.size2
			)			
			result.sp=list(z1,z2,overlap.t)
			return(result.sp)
		}
		
		natur.native=get.z(genus,clim2,clim2,occ.sp.natur,occ.sp.native,occ.naturalizd,occ.native)
		ori.native=get.z(genus,clim1,clim2,occ.sp.ori,occ.sp.native,occ.original,occ.native)
		
		return(list(natur.native,ori.native))
	}	
	
	require(parallel)
	no_cores <- detectCores() - 1
	mycl <- makePSOCKcluster(no_cores); 
	date()
	sp.niche.genus=parLapply(cl=mycl,genuslist,niche.cal2,splist.alien,splist.native,clim1,clim2,gbif.clean,adc.ea,adc.ena,var.list,R)
	names(sp.niche.genus)=genuslist	
	stopCluster(mycl)	
	date()#20 min	
	#save(sp.niche.genus,file="sp.niche.genus2.Rdata")
	save(sp.niche.genus,file="sp.niche.genus3.Rdata")
	
	sp.niche.genus=get(load("sp.niche.genus3.Rdata"))
	overlap.natur.native=c();overlap.ori.native=c()
	for (i in 1:length(genuslist)){
		overlap.natur.native=rbind(overlap.natur.native,sp.niche.genus[[i]][[1]][[3]]);
		overlap.ori.native=rbind(overlap.ori.native,sp.niche.genus[[i]][[2]][[3]])
	} 		
	
	#plot niche overlap			
	layout(matrix(1:15, 3, 5, byrow = TRUE))
	par(mar=c(1.2,1,0.02,0.17), cex=1, cex.axis=0.8, cex.lab=1.2, tck=-0.04,oma=c(1.5,1.5,0,0.15))
	seq=1
	for (i in sort(genuslist)){
		# the plot region color
		plot.new()
		rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col = "lightblue")
		# adding the new plot
		par(new = TRUE)
		z1=sp.niche.genus[[i]][[1]][[1]];z2=sp.niche.genus[[i]][[1]][[2]]			
		if (seq<=8){
		if (seq%in%c(1,6)){
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="darkred",xaxt='n',cex.axis=2,tck=-0.01,mgp = c(3, 1, 0))#naturalized
		}else{
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="darkred",xaxt='n',yaxt='n')
		}		
		}else{
		if (seq==11){
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="darkred",cex.axis=2,tck=-0.01,mgp = c(3, 1, 0))
		}else{
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="darkred",yaxt='n',cex.axis=2,tck=-0.01,mgp = c(3, 1, 0))
		}
		}		
		contour(z2$x,z2$y,z2$z.cor,add=T,levels=quantile(z2$z.cor[z2$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="darkblue")#native
		nichove=overlap.natur.native[overlap.natur.native$Genus==i,"niche.overlap.p1"]
		niche.p=overlap.natur.native[overlap.natur.native$Genus==i,"niche.overlap"]
		mtext(paste(i,"\n","niche overlap:","D=",nichove),side=1,adj=0.1,line=-1.5,cex=1.4,col="black",font=3)
		seq=seq+1
	}
	layout(matrix(1:15, 3, 5, byrow = TRUE))
	par(mar=c(1.2,1,0.02,0.17), cex=1, cex.axis=0.8, cex.lab=1.2, tck=-0.04,oma=c(1.5,1.5,0,0.15))
	seq=1
	for (i in sort(genuslist)){
		# the plot region color
		plot.new()
		rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col = "lightblue")
		# adding the new plot
		par(new = TRUE)
		z1=sp.niche.genus[[i]][[2]][[1]]#ori		
		z2=sp.niche.genus[[i]][[2]][[2]]#native			
		if (seq<=8){
		if (seq%in%c(1,6)){
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="#F4A460",xaxt='n',cex.axis=2,tck=-0.01,mgp = c(3, 1.05, 0))#original
		}else{
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="#F4A460",xaxt='n',yaxt='n')
		}		
		}else{
		if (seq==11){
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="#F4A460",cex.axis=2,tck=-0.01,mgp = c(3, 1.05, 0))
		}else{
		contour(z1$x,z1$y,z1$z.cor,levels=quantile(z1$z.cor[z1$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="#F4A460",yaxt='n',cex.axis=2,tck=-0.01,mgp = c(3, 1.05, 0))
		}
		}		
		contour(z2$x,z2$y,z2$z.cor,add=T,levels=quantile(z2$z.cor[z2$z.cor>0],c(0,0.25,0.5,0.75)),drawlabels=F,lty=c(2,2,2,1),lwd=c(1,2,3,4),col="darkblue")#native
		nichove=overlap.ori.native[overlap.ori.native$Genus==i,"niche.overlap.p1"]
		niche.p=overlap.ori.native[overlap.ori.native$Genus==i,"niche.overlap"]
		mtext(paste(i,"\n","niche overlap:","D=",nichove),side=1,adj=0.1,line=-1.5,cex=1.4,col="black",font=3)
		seq=seq+1
	}
	
	# plot scannter plot	
	library(ggpubr)	
	library(gg.gap)	
	#install.packages("ggrepel")
	library(ggrepel)
	alien=read.csv("EA-ENA.splist.csv")
	overlap=na.omit(read.csv("sp.niche2.maxent.csv"))[,-1]
	overlap=cbind(overlap,EarlyRecordyear=alien[match(overlap$Species,alien$Species_TPL),"EarlyRecordyear"])	#plot idexes of niche overlap
	sp.niche.genus=get(load("sp.niche.genus2.Rdata"))
	genuslist=names(sp.niche.genus)
	overlap.genus=c()	
	for (i in sort(genuslist)){
		tmp=data.frame(Genus=i,overlap.natur=sp.niche.genus[[i]][[1]][[3]][,"niche.overlap"],overlap.ori=sp.niche.genus[[i]][[2]][[3]][,"niche.overlap"],
		niche.diff.natur=sp.niche.genus[[i]][[1]][[3]][,"niche.size1"]-sp.niche.genus[[i]][[1]][[3]][,"niche.size2"],#natur - native
		niche.diff.ori=sp.niche.genus[[i]][[2]][[3]][,"niche.size1"]-sp.niche.genus[[i]][[2]][[3]][,"niche.size2"]);
		overlap.genus=rbind(overlap.genus,tmp)
	} 		
		
	my_col <- c("#E13A94", "#F588B8","#FEF201","brown","#8C52A2","#DEC3DE","#9EBC90","#2ABAA0","darkblue","black","green",
			"darkgray","lightgray","#FBAA72")
	my_col2 <-data.frame(Genus=unique(overlap$Genus), col=my_col)	
	p1=ggplot(overlap, aes(x=niche.loss*100, y=niche.expansion*100,fill=Genus))+
	geom_abline(intercept=0,slope=1, linetype = "twodash", col="black",size=1.5)+
	geom_point(size=5,shape=21,color="black",alpha=0.8)+
	scale_fill_manual(values=my_col)+	
	#geom_text_repel(aes(x=niche.size.change, y=niche.expansion,label=Species,fontface = "italic"),size=3)+
	xlab("Niche unfilling(%)") +ylab("Niche expansion(%)") +	ylim(0,100)+
	theme(axis.text = element_text(size=10,color='black',angle=0),
		axis.title.x = element_text(size=15,color='black',angle=0),		
		axis.title.y = element_text(size=15,color='black',angle=90),
		panel.grid.minor.y=element_blank(),
		axis.line.y=element_line(linetype=1,color='black'),
		axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank(),		
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))			
	
	p2=ggplot(overlap, aes(x=ea.niche.occupy*100, y=ena.niche.occupy*100,,fill=Genus))+
	xlab("Occupied suitalbe niches (EA,%)") +ylab("Occupied suitalbe niches (ENA,%)") +scale_fill_manual(values=my_col)+	
	geom_abline(intercept=0,slope=1, linetype = "twodash", col="black",size=1.5)+
	geom_point(size=5,shape=21,color="black",alpha=0.7)+ylim(0,100)+
	#geom_text_repel(aes(x=niche.size.change, y=range.change,label=Species,fontface = "italic"),size=3)+
	theme(axis.text = element_text(size=10,color='black',angle=0),		
		axis.title.x = element_text(size=15,color='black',angle=0),		
		axis.title.y = element_text(size=15,color='black',angle=90),
		panel.grid.minor.y=element_blank(),
		axis.line=element_line(linetype=1,color='black'),
		#axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank())
	
	p3=ggplot(overlap.genus, aes(x=overlap.ori, y=overlap.natur,fill=Genus))+
	xlab("Niche overlap (Ori.-Native)") +ylab("Niche overlap (Natur.-Native)")+scale_fill_manual(values=my_col)+	
	geom_abline(intercept=0,slope=1, linetype = "twodash", col="black",size=1.5)+
	geom_point(size=5,shape=21,color="black",alpha=0.8)+ylim(0,1)+
	#geom_text_repel(aes(x=overlap.ori, y=overlap.natur,label=Genus,fontface = "italic"),size=3)+
	theme(axis.text = element_text(size=10,color='black',angle=0),
		axis.title.x = element_text(size=15,color='black',angle=0),		
		axis.title.y = element_text(size=15,color='black',angle=90),
		panel.grid.minor.y=element_blank(),
		axis.line.y=element_line(linetype=1,color='black'),
		axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank(),		
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))	
	
	p4=ggplot(overlap, aes(x=1-niche.overlap, y=1-niche.ovlp.maxent,fill=Genus))+
	xlab("Realized niche changes") +ylab("Suitable niche changes")+scale_fill_manual(values=my_col)+
	geom_abline(intercept=0,slope=1, linetype = "twodash", col="black",size=1.5)+
	geom_point(size=5,shape=21,color="black",alpha=0.8)+ylim(0,1)+
	#geom_text_repel(aes(x=niche.diff.ori, y=niche.diff.natur,label=Genus,fontface = "italic"),size=3)+
	theme(axis.text = element_text(size=10,color='black',angle=0),
		axis.title.x = element_text(size=15,color='black',angle=0),		
		axis.title.y = element_text(size=15,color='black',angle=90),
		panel.grid.minor.y=element_blank(),
		axis.line.y=element_line(linetype=1,color='black'),
		axis.line.x=element_line(linetype=1,color='black'),
		axis.ticks = element_line(linetype=2,color='black'),
		panel.grid=element_line(linetype=2,color='grey'),
		panel.background = element_blank(),
		legend.background = element_rect(fill = NA),
		legend.text=element_text(face="bold.italic",size=10),
		legend.title=element_text(face="bold",size=12))	
	
	ggarrange(p2,p4,nrow=2,ncol = 1,widths=c(1,1),heights=1,labels=c("a","b"),font.label = list(size = 25),
		hjust=-3.5,vjust=1.5,common.legend=TRUE,legend="right")	
	
	#plot niche overlap/niche size change ~ niche.expansion/loss
	ss.glm <- function(r.glm)
                {
                r.ss <- summary(r.glm)
                rsq <- 100*(r.ss$null.deviance-r.ss$deviance)/r.ss$null.deviance
                adj.rsq <- 100*(1-(r.ss$deviance/r.ss$df.residual)/(r.ss$null.deviance/r.ss$df.null))
                f.stat <- ((r.ss$null.deviance-r.ss$deviance)/(r.ss$df.null-
                        r.ss$df.residual))/(r.ss$deviance/r.ss$df.residual)
                P <- pf(f.stat, r.ss$df.null-r.ss$df.residual, r.ss$df.residual, lower.tail=FALSE)
				Px=ifelse(P <= 0.0001,"****",
					ifelse(P>0.0001&P<=0.001,"***",
					ifelse(P>0.001&P<=0.01,"**",
					ifelse(P>0.01&P<=0.05,"*",
					#ifelse(P>0.05&P<0.1,"",
					ifelse(P>0.05,"ns",P)))))	
                return(data.frame(r2=rsq,adj.r2=adj.rsq,p=P,px=Px,s=r.glm$coef[[2]]))
                }
	d1=glm(niche.overlap~niche.loss,data = overlap)
	p1.over=ggplot(overlap, aes(x=niche.loss*100, y=niche.overlap))+
		xlab("Niche unfilling") +ylab("Niche changes") +
		geom_point(aes(fill=Genus),size=5,shape=21,color="black",alpha=0.8)+
		scale_fill_manual(values=my_col2[match(unique(overlap$Genus),my_col2$Genus),"col"])+
		#scale_y_continuous(limits=c(0,1),breaks=round(seq(0,1,length.out=6),1))+
		geom_smooth(aes(x=niche.loss*100, y=niche.overlap),data=overlap,colour="black",method = "glm",size=2,show.legend=FALSE,se = TRUE)+
		annotate("text", x=80 , y= 0.5,size=5,label=paste("Slope = ",round(ss.glm(d1)[5],2),"\n","R2 = ",round(ss.glm(d1)[1],2),ss.glm(d1)[4],sep="") )+
		theme(axis.text.x = element_text(size=10,color='black',angle=0),
			panel.grid.minor.y=element_blank(),
			axis.line.y=element_line(linetype=1,color='black'),
			axis.line.x=element_line(linetype=1,color='black'),
			axis.ticks = element_line(linetype=2,color='black'),
			panel.grid=element_line(linetype=2,color='grey'),
			panel.background = element_blank(),			
			axis.text=element_text(face="bold",size=11.5),				
			axis.title.x = element_text(size=15,color='black',angle=0),
			axis.title.y = element_text(size=15,color='black',angle=90),
			axis.title=element_text(size=11.5),
			legend.position="none")	
	
	d2=glm(niche.overlap~niche.expansion,data = overlap)
	p2.over=ggplot(overlap, aes(x=niche.expansion*100, y=niche.overlap))+
		xlab("Niche expansion") +ylab("Niche changes") +
		geom_point(aes(fill=Genus),size=5,shape=21,color="black",alpha=0.8)+
		scale_fill_manual(values=my_col2[match(unique(overlap$Genus),my_col2$Genus),"col"])+
		#scale_y_continuous(limits=c(0,1),breaks=round(seq(0,1,length.out=6),1))+		
		geom_smooth(aes(x=niche.expansion*100, y=niche.overlap),data=overlap,colour="black",method = "glm",size=2,show.legend=FALSE,se = TRUE)+
		annotate("text", x=25 , y= 0.5,size=5,label=paste("Slope = ",round(ss.glm(d2)[5],2),"\n","R2 = ",round(ss.glm(d2)[1],2),ss.glm(d2)[4],sep="") )+
		theme(axis.text.x = element_text(size=10,color='black',angle=0),
			axis.text.y = element_blank(),			
			panel.grid.minor.y=element_blank(),
			axis.line.y=element_line(linetype=1,color='black'),
			axis.line.x=element_line(linetype=1,color='black'),
			axis.ticks = element_line(linetype=2,color='black'),
			panel.grid=element_line(linetype=2,color='grey'),
			panel.background = element_blank(),			
			axis.text=element_text(face="bold",size=11.5),
			axis.title.x = element_text(size=15,color='black',angle=0),		
			axis.title.y = element_blank(),
			axis.title=element_text(size=11.5),
			legend.position="none")	
		
	#niche.size.change/niche.shift ~ niche.loss + niche.expansion
	#hp
	hp1=hier.part::hier.part(overlap$niche.overlap,overlap[,c("niche.expansion","niche.loss")],gof = "Rsqu",barplot = FALSE)$IJ
	hp.p1=rbind(data.frame(vars=rownames(hp1),rsq=hp1$I*100,Rsq.type="Independent"),data.frame(vars=rownames(hp1),rsq=hp1$J*100,Rsq.type="Joint"))		
	p1.hp=ggplot(hp.p1,aes(x=vars,y=rsq,fill=Rsq.type))+ geom_bar(stat="identity",position = "stack")+ 
	geom_hline(yintercept=0,col="black",size=1,linetype="longdash")+
	ylab("Explained variation (R2, %)") +
	scale_x_discrete("Drivers on niche change",labels=c("Expansion","Unfilling"))+	
	theme(axis.text = element_text(size=10,color='black',angle=0),
			panel.grid.minor.y=element_blank(),
			axis.line.y=element_line(linetype=1,color='black'),
			axis.line.x=element_line(linetype=1,color='black'),
			axis.ticks = element_line(linetype=2,color='black'),
			panel.grid=element_line(linetype=2,color='grey'),
			panel.background = element_blank(),			
			axis.title.x = element_text(size=15,color='black',angle=0),
			axis.title.y = element_text(size=15,color='black',angle=90),
			legend.background = element_rect(fill = NA),
			legend.text=element_text(face="bold.italic",size=10),
			legend.title=element_text(face="bold",size=12))
	
	part2=ggarrange(p1.over,p2.over,p1.hp,nrow=1,ncol = 3,widths=1,heights=c(1,1,1),labels=c("c","d","e"),font.label = list(size = 25),
		hjust=-0.6,vjust=0.8,common.legend=FALSE)	
	part1=ggarrange(p3,p1,nrow=1,ncol = 2,widths=1,heights=c(1,1),labels=c("a","b"),font.label = list(size = 25),
		hjust=-0.6,vjust=1,common.legend=TRUE,legend="right")	
	ggarrange(part1,part2,nrow=2,ncol = 1,widths=c(1,1),heights=1,common.legend=TRUE,legend="right")
#compare niche overlap
my_compar=list(c("native.native","alien.native"),c("alien.alien","alien.native"),c("native.native","alien.alien"))
p5=ggviolin(mat2datfrm,x="type", y="niche.overlap",fill="type",add="boxplot",add.params=list(fill="white"))+
	stat_compare_means(comparisons=my_compar,label="p.signif",bracket.size=1.2)+theme_bw()+	
	theme(axis.text.x = element_blank(),
		axis.text.y = element_text(size=15,color='black',angle=30,vjust=0.2),
		axis.title.x = element_text(size=20),
		axis.title.y = element_text(size=18,color='black',angle=90),
		legend.position="none")+xlab(" ")#ns: p > 0.05,*: p <= 0.05,**: p <= 0.01
p6=ggviolin(mat2datfrm,x="type", y="phylo.dis",fill="type",add="boxplot",add.params=list(fill="white"))+
	stat_compare_means(comparisons=my_compar,label="p.signif",bracket.size=1.2)+theme_bw()+	
	theme(axis.text.x = element_text(size=18,color='black',angle=0),
		axis.text.y = element_text(size=15,color='black',angle=30,vjust=0.2),
		axis.title.x = element_blank(),
		axis.title.y = element_text(size=18,color='black',angle=90),
		legend.position="none")#ns: p > 0.05,*: p <= 0.05,**: p <= 0.01	
ggarrange(p5,p6,nrow=2,ncol = 1,widths=c(1,1),heights=1,labels=c("a","b"),font.label = list(size = 20))	

####################################################
## link niche overlap with traits distances ########
####################################################			
dis.all=unique(read.csv("ENA.alien.native.csv")[,-c(1:2)])
###### obtain data from BIEN: returns no records for alien species in ENA so disgard ####
# genuslist=sort(unique(dis.all$Genus_E))
# #install.packages("BIEN")
# library(BIEN)
# tre <- BIEN_phylogeny_complete()#contain less species than Simth and Brown (2018) and the dating is very different, thus did not use
# bien=BIEN_trait_genus(genuslist,all.taxonomy=TRUE,political.boundaries=TRUE)
# bien2=subset(bien,!(is.na(bien$latitude)&is.na(bien$longitude)&is.na(bien$country)&is.na(bien$state_province)))
# bien2=subset(bien2,!bien2$country%in%c("Germany","Argentina","United Kingdom","Slovakia","Italy","South Africa","Spain","Brazil","Estonia","Norway","Netherlands","Ecuador","Peru","Cuba" ))
# bien2=subset(bien2,is.na(bien2$latitude)|is.na(bien2$longitude)|(bien2$longitude>=-180&bien2$longitude<=-52&bien2$latitude>7.2)|(bien2$longitude>=26&bien2$longitude<=180&bien2$latitude>-11))
# re=c()
# trait.list=unique(bien2$trait_name)
# for (i in 1:length(trait.list)) {
# tmp=subset(bien2,bien2$trait_name==trait.list[i])
# tmp2=data.frame(trait=trait.list[i],name1=length(unique(tmp$verbatim_scientific_name)),name2=length(unique(tmp$scrubbed_species_binomial)))
# re=rbind(re,tmp2)
# }
# re=re[order(re$name1,decreasing=T),]
# bien.t1=subset(bien2[,c("id","trait_value")],bien2$trait_name%in%re$trait[1])#diameter at breast height (1.3 m),cm
# bien.t2=subset(bien2[,c("id","trait_value")],bien2$trait_name%in%re$trait[3])#whole plant height (1.3 m),m
# bien3=cbind(bien2[,c("id","scrubbed_species_binomial","latitude","longitude" ,"elevation_m","country","state_province")],
	# DBH=bien.t1[match(bien2$id,bien.t1$id),-1],	height=bien.t2[match(bien2$id,bien.t2$id),-1])
# bien3=subset(bien3,!(is.na(bien3$DBH)&is.na(bien3$height)))
# splist=unique(dis.all[,c("Genus_E","Acname")])
# bien4=cbind(splist[match(bien3$scrubbed_species_binomial,splist$Acname),],bien3)
# umat.sp.bien=sort(unique(bien4[is.na(bien4$Acname),"scrubbed_species_binomial"]))
# write.csv(umat.sp.bien,"umat.sp.bien.csv")
# umat.sp.bien=read.csv("umat.sp.bien.csv")
# bien.unmat=bien4[is.na(bien4$Acname),-c(1:2)]
# bien.mat=cbind(umat.sp.bien[match(bien.unmat$scrubbed_species_binomial,umat.sp.bien$bien),-1],bien.unmat)
# bien5=rbind(bien4[!is.na(bien4$Acname),],bien.mat[!is.na(bien.mat$Acname),])
# write.csv(bien5,"trait.bien.csv")

# library(data.table)
# library(Rmisc)
# bien=as.data.frame(fread("trait.bien.csv"))
# DBH.na=summarySE(bien[bien$Continent=="Namerica"&!is.na(bien$height),], measurevar="height", groupvars="Acname")
# DBH.ea=summarySE(bien[bien$Continent=="Asia"&!is.na(bien$height),], measurevar="height", groupvars="Acname")
# dis.all=unique(read.csv("ENA.alien.native.csv")[,-c(1:2)])
# dis.na=subset(dis.all,!dis.all$status%in%"alien.original")
# DBH.na2=cbind(DBH.na,status=dis.na[match(DBH.na$Acname,dis.na$Acname),"status"])#no trait data for alien sp

###### obtain data from TRY database ####
# ref:Kattge, J, Boenisch, G, Diaz, S, et al. TRY plant trait database - enhanced coverage and open access. Glob Change Biol. 2020; 26: 119-188. https://doi.org/10.1111/gcb.14904
# Obtain the splist
Try.splist=as.data.frame(fread("TRY.splist.csv"))
sp=unique(dis.all[dis.all$status!="alien.original",])
re=cbind(sp,Try.splist[match(sp$Acname,Try.splist$AccSpeciesName),])
write.csv(re,"TRY.EA-ENA.csv")
re=na.omit(read.csv("TRY.EA-ENA.csv"))
paste(re$AccSpeciesID,collapse=",")
3274,3275,4400,4401,4403,4404,4405,4407,4408,4411,5801,11183,11187,11188,11189,14553,14555,14556,14558,14563,14569,14571,14572,14574,14575,14576,14578,14580,14584,14590,14593,14594,14595,14597,14598,14602,14789,14790,29728,29735,29737,29738,29739,29740,35014,35016,35028,35029,35032,35037,35039,35042,35043,35049,35050,35053,35055,35057,35062,35063,35065,35067,37901,37902,39400,39402,39403,39760,40427,40428,40430,41980,41982,57061,57063,64213,64214,64960,65634,68594,69380,69381,74602,75767,79734,89509,89511,89513,89517,89519,89527,89529,89531,89532,89540,92091,92254,202645,202646,202647,204088,204090,204098,204119,204124,204125,204127,205506,205522,216555,216571,216687,234060,234061,234062,239416,239419,239449,239466,239467,239473,239497,239498,245769,247118,250682,294906,294907,294908,301849,331849,331852,344498,344525,344532,344537,344538,344542,344556,344557,344572,344606,344639
# manage the try data
# extract the geo coords of EA-ENA species
require(data.table)
TRYdata <- fread("TRY/21125.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
trait.list=unique(TRYdata$TraitName)
lat=TRYdata[TRYdata$DataID==59,c("AccSpeciesID","ObservationID","OrigValueStr","StdValue")]
lon=TRYdata[TRYdata$DataID==60,c("ObservationID","OrigValueStr","StdValue")]
#ref=TRYdata[TRYdata$DataID==113,c("ObservationID","OrigValueStr")]
colnames(lat)[4]="Lat";colnames(lon)[3]="Lon"#;colnames(ref)[2]="ref"
coord=cbind(lat,lon[match(lat$ObservationID,lon$ObservationID),-1])
coord2=cbind(re[match(coord$AccSpeciesID,re$AccSpeciesID),c("Family_E","Genus_E","Acname","status")],coord)
write.csv(coord2,"corrd.try.csv")#update NA stdvalue based on origvaluestr, and remove records with na lon

coord=read.csv("corrd.try.csv")	
coord.ena=subset(coord,coord$Lon>=-180&coord$Lon<=-52&coord$Lat>7.2)
coord.ea=subset(coord,coord$Lon>=26&coord$Lon<=180&coord$Lat>-11&coord$status%in%"alien")
coord.ea$status="alien.original"
coord2=rbind(data.frame(continent="NAma",coord.ena),data.frame(continent="Asia",coord.ea))

#extract traits
trait=TRYdata[TRYdata$ObservationID%in%unique(coord2$ObservationID)&!is.na(TraitID),
	c("AccSpeciesID","ObservationID","TraitID","TraitName","OrigValueStr","OrigUnitStr","StdValue")]
trait.stat=unique(trait[,c("AccSpeciesID","TraitID","TraitName")])
traitlist=names(sort(tapply(trait.stat$AccSpeciesID,trait.stat$TraitName,length),decreasing=T))[c(1:5,7:8)]
try.coord=coord2
for (i in traitlist){
tmp=TRYdata[TRYdata$TraitName==i,c("ObservationID","OrigValueStr")]
try.coord=cbind(try.coord,tmp[match(try.coord$ObservationID,tmp$ObservationID),"OrigValueStr"])
}
colnames(try.coord)=c(colnames(coord2),c("Leaf_phenology_type","LMA","Leaf_N","height","Leaf_area","climate_type","Leaf_N.area"))
write.csv(try.coord,"coord.try.trait.csv")#still not good enough for analysis

#可否通过计算机模拟，在群落建立初期，系统发育相似的一些物种（如间断分布属下的各类物种）定殖的概率是相同的，一旦某些成为本地种，其外来种姊妹群很难侵入
#对于不同类群的间断分布种，外来种姊妹群侵入的难易程度可能与该类群的生态位进化速率有关
#与北美相比，中国只有一种入侵种，是否意味着中国的生态位已经被本地种充分占据�?
# 其他间断分布类型的结果如何？比如欧洲-北美分布，南�?-北美分布
旭日�?
options("install.lock"=FALSE)
install.packages("webr")
library(webr)
library(data.table)
library(ggplot2)
other=function(re,limit,lev="Genus"){
		thes=unique(re[,c(lev,paste(lev,"rich",sep="."))])
		thes=thes[order(thes[,2],decreasing=TRUE),]
		re2=subset(thes,thes[,2]>=thes[,2][limit])
		if (sum(thes[,2])==sum(re2[,2])) return(re2) else {
			re3=data.frame("other",sum(thes[,2])-sum(re2[,2]))
			colnames(re3)=colnames(thes)
			fin=rbind(re2,re3)
			return(fin)
		}
		
	}
alien.stat=function(splist0,limit.taxa=TRUE,limit.fam=7,limit.other=5){
	splist1=subset(splist0,is.na(splist0$Accepted_ID))[,c("Family","Genus_ori","Species_ori","alien.reg","country")]
	colnames(splist1)=c("Family","Genus","Species","alien.reg","country")
	splist2=subset(splist0,!is.na(splist0$Accepted_ID))[,c("Family","Genus","Species_TPL","alien.reg","country")]
	colnames(splist2)=c("Family","Genus","Species","alien.reg","country")
	splist=na.omit(rbind(unique(splist1),unique(splist2)))
	gen=tapply(splist$Species,splist$Genus,length)
	fam=tapply(splist$Species,splist$Family,length)
	taxon=unique(splist[,c("Family","Genus")])
	re=cbind(taxon,Genus.rich=gen[match(taxon$Genus,names(gen))],Family.rich=fam[match(taxon$Family,names(fam))])
	re=re[order(re$Genus.rich,decreasing=TRUE),]	
	re=re[order(re$Family.rich,decreasing=TRUE),]	
	if (limit.taxa==TRUE){
		re.fam=other(re,limit.fam,"Family")#因为存在并列，所以行数可能大于limit.fam	
		richest.gen=c()	
		for (i in 1:(dim(re.fam)[1]-1)){
			re.t=subset(re,re$Family==re.fam[i,"Family"])
			limit.gen=limit.fam-i+1
			if (dim(re.t)[1]<=limit.gen) richest.gen=rbind(richest.gen,re.t) else{
				richest.gen=rbind(richest.gen,data.frame(Family=re.fam[i,"Family"],other(re.t,limit.gen,"Genus"),Family.rich=re.fam[i,"Family.rich"]))
			}		
		}
		re.other=other(re[!re$Family%in%re.fam[,"Family"],],limit.other,"Genus")
		richest.alien=rbind(richest.gen,data.frame(Family=re.fam[dim(re.fam)[1],"Family"],re.other,Family.rich=re.fam[dim(re.fam)[1],"Family.rich"]))
	}else{
		richest.alien=re
	}		
	return(richest.alien)
}

sp.all=as.data.frame(fread("alien.EA-ENA.csv",header=T))[,-1]
sp.disjuct=as.data.frame(fread("alien.EA-ENA.disjuct.csv",header=T))[,-1]
dat=list(sp.all,
sp.ea=subset(sp.all,sp.all$alien.reg=="EAsia"),
sp.ena=subset(sp.all,sp.all$alien.reg=="NAma"),
sp.disjuct=sp.disjuct,
sp.disjuct.ea=subset(sp.disjuct,sp.disjuct$alien.reg=="EAsia"),
sp.disjuct.ena=subset(sp.disjuct,sp.disjuct$alien.reg=="NAma"))
names(dat)=c("EA-NA Alien","EA-NA Alien (EA)","EA-NA Alien (NA)","EA-ENA Disjuct Alien","EA-ENA Disjuct Alien (EA)","EA-ENA Disjuct Alien (ENA)")

for (i in 1:3){
	alien=alien.stat(dat[[i]],limit.fam=15,limit.other=3)
	windows()
	PieDonut(alien, aes(Family,Genus, count=Genus.rich),
			 title = names(dat)[i],
			 ratioByGroup =FALSE,pieLabelSize = 3,  donutLabelSize =2.5,showRatioThreshold =0.01)
}

disjuct=alien.stat(dat[[4]],limit.fam=9,limit.other=3)
PieDonut(disjuct, aes(Family,Genus, count=Genus.rich),
         title = names(dat)[4],
         ratioByGroup =FALSE,pieLabelSize = 4,  donutLabelSize =3,showRatioThreshold =0.005)
		 
disjuct.ea=alien.stat(dat[[5]],limit.taxa=FALSE)
PieDonut(disjuct.ea, aes(Family,Genus, count=Genus.rich),
         title = names(dat)[5],
         ratioByGroup =FALSE,pieLabelSize = 4,  donutLabelSize =3,showRatioThreshold =0.005)
		 
disjuct.ena=alien.stat(dat[[6]],limit.fam=8,limit.other=3)
PieDonut(disjuct.ena, aes(Family,Genus, count=Genus.rich),
         title = names(dat)[6],
         ratioByGroup =FALSE,pieLabelSize = 4,  donutLabelSize =3,showRatioThreshold =0.005)
#stat
stat.taxa=c()
for (i in 2:3){
	stat=alien.stat(dat[[i]],limit.taxa=FALSE)
	tmp=rbind(data.frame(Type=names(dat)[i],taxon="Family",rich=length(unique(stat$Family))),
		data.frame(Type=names(dat)[i],taxon="Genus",rich=length(unique(stat$Genus))),
		data.frame(Type=names(dat)[i],taxon="Species",rich=sum(stat$Genus.rich)))	
	stat.taxa=rbind(stat.taxa,tmp)
}
taxon=unique(stat.taxa$taxon)
stat.taxa2=c()
for (i in 1:3){
	tmp=subset(stat.taxa,stat.taxa$taxon==taxon[i])
	por1=tmp[1,3]/sum(tmp[,3])
	por2=tmp[2,3]/sum(tmp[,3])
	tmp2=cbind(tmp,por=c(por1,por2))
	stat.taxa2=rbind(stat.taxa2,tmp2)
}
stat.taxa2$Type=factor(stat.taxa2$Type,levels=c("EA-ENA Disjuct Alien (ENA)","EA-ENA Disjuct Alien (EA)"))
stat.taxa2$Type=factor(stat.taxa2$Type,levels=c("EA-NA Alien (NA)","EA-NA Alien (EA)"))
ggplot(stat.taxa2,aes(x=taxon,y=por,fill=Type))+
  geom_bar(stat="identity",position = "stack")+ 
	geom_text(aes(label=rich), 
            position = position_dodge2(width = 0.9, preserve = 'single'), 
            vjust = 2, hjust =1)+ 
  theme_bw()	

##combine neon data (https://data.neonscience.org/documents)  
#ref:National Ecological Observatory Network. 2022. Data Product DP1.10058.001, Plant presence and percent cover. Provisional data downloaded from https://data.neonscience.org on January 11, 2022. Battelle, Boulder, CO, USA NEON. 2022.
library(data.table)
splist=as.data.frame(fread("NEON_presence-cover-plant/NEON_pla_nativeStatusCodes/NEON_pla_taxonomy.csv",header=T))
myfiles0 <- Sys.glob("NEON_presence-cover-plant/NEON.*/*div_1m2Data*.csv")
sp.in.plot= do.call(rbind,lapply(myfiles0, read.csv))
sp.in.plot2=subset(sp.in.plot,sp.in.plot$divDataType%in%"plantSpecies")
sp.in.plot3=cbind(splist[match(sp.in.plot2$taxonID,splist$taxonID),c("order","family","genus","scientificName0","acceptedTaxonID")],
	sp.in.plot2[,c("uid", # Unique ID within NEON database; an identifier for the record
	"taxonID","taxonRank", # The lowest level taxonomic rank that can be determined for the individual or specimen
	"domainID", # Unique identifier of the NEON domain
	"decimalLatitude", # The geographic latitude (in decimal degrees, WGS84) of the geographic center of the reference area
	"decimalLongitude",	"elevation",
	"nlcdClass", # National Land Cover Database Vegetation Type Name
	"plotID",  # Plot identifier (NEON site code_XXX)
	"subplotID", # Identifier for the NEON subplot
	"endDate", # The end date-time or interval during which an event occurred
	"nativeStatusCode",
	"taxonIDRemarks", # Technician notes about the specific taxon; free text comments accompanying the record, e.g., seedling
	"percentCover", # Ocular estimate of cover of the index (e.g., species) as a percent
	"heightPlantOver300cm", # Indicator of whether individuals of the species in the sample are taller than 300 cm
	"heightPlantSpecies", # Ocular estimate of the height of the plant species, cm, lower than 300 cm 
	"remarks" # Technician notes; free text comments accompanying the record	
	)])
	
#correct sp names using TPL (Date 2015)
library(data.table)
tpl.ac=unique(as.data.frame(fread("TPL14Ac.csv",header=T)))
tpl.sy=unique(as.data.frame(fread("TPL14Sy.csv",header=T)))
corrt.1=cbind(sp.in.plot3,Accepted_ID=tpl.ac[match(sp.in.plot3$scientificName0,tpl.ac$Acname),"ID"])
unmat=subset(corrt.1,is.na(corrt.1$Accepted_ID))
corrt.2=cbind(unmat[,-23],Accepted_ID=tpl.sy[match(unmat$scientificName0,tpl.sy$Syname),"Accepted ID"])
corrt.3=rbind(subset(corrt.1,!is.na(corrt.1$Accepted_ID)),corrt.2)	
corrt.4=cbind(corrt.3,tpl.ac[match(corrt.3$Accepted_ID,tpl.ac$ID),c("Major group","Family","Genus","Acname")])

# match EA-ENA disjuct
disjuct=read.csv("Wen.annurev.ecolsys.EA-ENAchecklist.csv")
sp.disj=cbind(corrt.4[,-c(2:3)],DisjuctDataScources=disjuct[match(corrt.4$Genus,disjuct$Genus),"DisjuctDataScources"])

#match EA-ENA disjuct alien data
alien.all=read.csv("alien.EA-ENA.disjuct.csv",header=T)[,-1]
alien.us=subset(alien.all,alien.all$country%in%"United States of America (the)")
sp.disj.alien=cbind(sp.disj,alien.us[match(sp.disj$Accepted_ID,alien.us$Accepted_ID),c("status","datascource","invader.datascource")])
colnames(sp.disj.alien)[28]="naturalized.datascource"
write.csv(sp.disj.alien,"neon.disj.alien.csv")
alien.disj.sp=subset(sp.disj.alien,!is.na(sp.disj.alien$naturalized.datascource)&(!is.na(sp.disj.alien$DisjuctDataScources)))
length(unique(alien.disj.sp$Acname))#25 species, while all of them are recorded as native in NEON
# [1] "Hamamelis virginiana"        "Parthenocissus quinquefolia"
 # [3] "Carya ovata"                 "Carya glabra"               
 # [5] "Brachyelytrum aristosum"     "Cornus racemosa"            
 # [7] "Liriodendron tulipifera"     "Phryma leptostachya"        
 # [9] "Gleditsia triacanthos"       "Podophyllum peltatum"       
# [11] "Campsis radicans"            "Aralia spinosa"             
# [13] "Magnolia virginiana"         "Magnolia fraseri"           
# [15] "Hydrangea arborescens"       "Magnolia tripetala"         
# [17] "Magnolia acuminata"          "Carya laciniosa"            
# [19] "Veronicastrum virginicum"    "Decumaria barbara"          
# [21] "Ampelopsis cordata"          "Penthorum sedoides"         
# [23] "Carya illinoinensis"         "Hydrangea cinerea"          
# [25] "Magnolia macrophylla"       

