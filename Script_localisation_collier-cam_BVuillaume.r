# LIER LES VIDEOS DES COLLIERS AUX LOCALISATIONS GPS


# fichier date et heure associé à chaque video du collier
DH <- read.table("C:/Users/vuill/Documents/Ulaval2019-A/Chap2/Colliers_Cameras/2016/21226_2016_Yann-AFINIR/Collier1.21226.txt", sep="\t", header=TRUE)

# fichier contenant les locations des points GPS
# 2 temps UTC et LMT (Local Mean Time) : utiliser LMT
Loc <- read.table("F:/Barbara/Collier camera 2016/colliers caméra - 2016/prets/GPS_Collar21226_1.txt", sep="\t", header=TRUE)

# Dans excel (plus rapide) on crée une colonne jour julien
# on convertit les colonnes dates et temps en 3 colonnes chaque (Jr,Mo,An et H,Mi,Se)
# Julian = DATE(year,month,day) + TIME(hour,min,sec)

# peut aussi s'écrire dans R si on veut

Loc$DaTim <- paste(Loc$LMT_Date,Loc$LMT_Time,sep=" ")
Loc$JJLMT <- NA


for (i in 1:nrow(Loc))
{
  k=as.POSIXct(Loc[i,16], format = "%d/%m/%Y %H:%M:%S") - as.POSIXct("01/01/1900 00:00:00", format = "%d/%m/%Y %H:%M:%S")
  Loc[i,17] = as.numeric(unclass(k)[1])   # permet d'aller chercher juste la valeur car sortie par défaut en phrase
}

# rechargement du fichier avec la nouvelle colonne si fait dans excel
#Loc <- read.table("F:/Barbara/Collier camera 2016/colliers caméra - 2016/prets/GPS_Collar21226_1b.txt", sep="\t", header=TRUE)

# comme les videos ne sont que du 1 juin au 1er septembre on extrait la partie du jeu de données qui nous intéresse
Loc$Mois <- as.numeric(substr(Loc$LMT_Date,4,5))
Loc2 <- Loc[Loc$Mois>5,]


# on crée aussi une colonne Julian day dans le fichier DH

DH$JJLMT <- NA


for (i in 1:nrow(DH))
{
  dh <- paste(DH[i,4],"/",DH[i,5],"/",DH[i,6]," ",DH[i,7],":",DH[i,8],":00",sep="")     # jour mois an heure min
  k=as.POSIXct(dh, format = "%d/%m/%Y %H:%M:%S") - as.POSIXct("01/01/1900 00:00:00", format = "%d/%m/%Y %H:%M:%S")
  DH$JJLMT[i] = as.numeric(unclass(k)[1])   # permet d'aller chercher juste la valeur car sortie par défaut en phrase
}


# on crée 3 nouvelles colonnes : Locdate, Lon, Lat 
# Locdate permettra de savoir la date de la localisation qui lui est associée car il y aura des légères différences

DH$Locdate <- NA
DH$Lon <- NA
DH$Lat <- NA

# les localisations ne tombent pas toujours à heures fixes.
# j'ai arbitrairement choisi de procéder ainsi :
# Supposons que les 3 vidéos ont été prises le 6 juin 2016 à 8h, 8h20 et 8h40
# Pour être associé à la 1e video, le point GPS devra tomber le 6 juin entre 7h50 et 8h10
# Pour être associé à la 2e video, le point GPS devra tomber le 6 juin entre 8h10 et 8h30
# Pour être associé à la 3e video, le point GPS devra tomber le 6 juin entre 8h30 et 8h50

for (i in 1:nrow(DH))
{
  print (paste(i," / ",nrow(DH)))
  absDH <- trunc(DH[i,9])
  Loc3 <- Loc2[trunc(Loc2$JJLMT)==absDH,]
  
  if (DH$Minute[i]==0){
    b <-which(as.numeric(substr(Loc3$LMT_Time,1,2))==(DH[i,7]-1)& as.numeric(substr(Loc3$LMT_Time,4,5))>49 | as.numeric(substr(Loc3$LMT_Time,1,2))==DH[i,7]& as.numeric(substr(Loc3$LMT_Time,4,5))<11)
  } else if (DH$Minute[i]== 20) {
    b <-which(as.numeric(substr(Loc3$LMT_Time,1,2))==(DH[i,7])& as.numeric(substr(Loc3$LMT_Time,4,5))>9 & as.numeric(substr(Loc3$LMT_Time,4,5))<31)
  } else if (DH$Minute[i]== 40) {
    b <-which(as.numeric(substr(Loc3$LMT_Time,1,2))==(DH[i,7])& as.numeric(substr(Loc3$LMT_Time,4,5))>29 & as.numeric(substr(Loc3$LMT_Time,4,5))<51)
  } else {
    b <- c()
  }
  
  if (length(b)>1){
    print(paste(i," / ",length(b),sep=""))
    b <- b[1]
  }
  Loc4 <- Loc3[b, ]
  
  if (nrow(Loc4)==1) {
    DH$Locdate[i] <- Loc3[b,16]   # DaTime
    DH$Lon[i] <- Loc3[b,14]       # Longitude
    DH$Lat[i] <- Loc3[b,13]       # Latitude
  } else {
    DH$Locdate[i] <- NA   # DaTime
    DH$Lon[i] <- NA       # Longitude
    DH$Lat[i] <- NA       # Latitude
  }

}

# on sauve le fichier créé
write.table(DH,"F:/Barbara/Ulaval2018-A/Colliers_Cameras/2016/21226_2016_Yann-AFINIR/LocVid_21226_2016.txt", sep="\t", row.names=F, col.names=T, quote=F)

DH <- read.table("F:/Barbara/Ulaval2018-A/Colliers_Cameras/2016/21226_2016_Yann-AFINIR/LocVid_21226_2016.txt", sep="\t", header=T)

# que faire maintenant pour les vidéos sans coordonnées
# a moins que la location existe sur pour les videos a 20 et 40 min
# on cherche à établir la localisation.
# quand elle n'existe pas on prend la localisation aux heures pleines H et H+1
# et on calcule une localisation hypothétique en supposant 
# que le déplacement est rectiligne pendant l'heure et donc que les localisations aux 20 et 40 min se trouvent donc 
# à 1/3 et 2/3 respectivement entre les deux points

# pour cela ON CONVERTIT Lon et Lat EN CM !!!!

# on travaille uniquement sur DH

library(raster)
library(sp)
library(rgdal)

SPtc <- as.data.frame(matrix(nrow=nrow(DH),ncol=2),NA)

for (i in 1:nrow(DH))
{
  if (is.na(DH$Lon[i])==F)
  {
    #projeter les données en lambert conique
    options(digits=12)
    SP<-SpatialPoints(cbind(DH$Lon[i],DH$Lat[i]),proj4string=CRS("+proj=longlat"))
    SPt<-spTransform(SP,CRS("+init=epsg:32198")) #### projection en Québec Lambert
    SPt<-as.data.frame(SPt)
    colnames(SPt)<-c("X","Y")
    SPtc[i,] <- SPt[1,]
  }
}
colnames(SPtc)<-c("X","Y")
DH<-cbind(DH,SPtc) # on ajoute à tab, 2 colonnes x et y contenant les DHalisation en Lambert
#write.table(DH,"F:/Barbara/Ulaval2018-A/Colliers_Cameras/2016/21226_2016_Yann-AFINIR/LocVid_21226_2016int.txt",sep="\t",col.names=T,row.names=F,quote=F) # on exporte le fichier avec les DHalisations transformées



for (i in 1:nrow(DH)) {
  if(is.na(DH[i,14])==T) {
    hour <- DH[i,7]
    minu <- DH[i,8]

      key <- which(is.na(DH$X)==F)
      M <- max(which(key < i))    # numero de ligne de la derniere ligne avec localisation avant i
      m <- min(which(key > i))    # numero de ligne de la 1e ligne avec localisation après i

      DH$X[i] <- DH[key[M],13] + (i - key[M]) * ((DH[key[m],13] - DH[key[M],13])/ (key[m] - key[M]))
      DH$Y[i] <- DH[key[M],14] + (i - key[M]) * ((DH[key[m],14] - DH[key[M],14])/ (key[m] - key[M]))

  }
}

write.table(DH,"F:/Barbara/Ulaval2018-A/Colliers_Cameras/2016/21226_2016_Yann-AFINIR/LocVid_21226_2016cplt.txt", row.names=F, col.names=T, quote=F)

# FINI !!!!
