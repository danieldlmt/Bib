#library(dplyr)
######################################### Import
#import data 
data <- readLines('data/log_SDL.csv')
data <- gsub(pattern="\t", replacement=";", data, fixed = TRUE)
datatemp <- tempfile() 
writeLines(data, con = datatemp) 
data <- read.table(datatemp, sep=";" , header=F, col.names=c('rev', 'author', 'date','n_line_add' ,'n_line_del' , 'path'))
#data <- read.csv('data/log_SDL.csv', sep=";" , header=F, col.names=c('rev', 'author', 'date', 'path'))

MakefileLinux <- readLines('data/make_SDL/MakefileLinux')
MakefileLinux <- data.frame(Path=grep('^[$].*: [^$]', MakefileLinux, value=TRUE))
MakefileLinux$Path<-  gsub('^[$].*: [^$]','',MakefileLinux$Path)
MakefileLinux$Path<-  gsub('home/vilinux/Documents/SDL-mirror/','',MakefileLinux$Path)

MakefilemacOS <- readLines('data/make_SDL/MakefilemacOS')
MakefilemacOS <- data.frame(Path=grep('^[$].*: [^$]', MakefilemacOS, value=TRUE))
MakefilemacOS$Path<-  gsub('^[$].*: [^$]','',MakefilemacOS$Path)
MakefilemacOS$Path<-  gsub('home/vilinux/Documents/SDL-mirror/','',MakefilemacOS$Path)

MakefileWindows <- readLines('data/make_SDL/MakefileWindows')
MakefileWindows <- data.frame(Path=grep('^[$].*: [^$]', MakefileWindows, value=TRUE))
MakefileWindows$Path<-  gsub('^[$].*: [^$]','',MakefileWindows$Path)
MakefileWindows$Path<-  gsub('home/vilinux/Documents/SDL-mirror/','',MakefileWindows$Path)

MakefileiOS <- readLines('data/make_SDL/MakefileiOS')
MakefileiOS <- data.frame(Path=grep('^[$].*: [^$]', MakefileiOS, value=TRUE))
MakefileiOS$Path<-  gsub('^[$].*: [^$]','',MakefileiOS$Path)
MakefileiOS$Path<-  gsub('home/vilinux/Documents/SDL-mirror/','',MakefileiOS$Path)

MakefileAndroid <- readLines('data/make_SDL/MakefileAndroid')
MakefileAndroid <- data.frame(Path=grep('^[$].*: [^$]', MakefileAndroid, value=TRUE))
MakefileAndroid$Path<-  gsub('^[$].*: [^$]','',MakefileAndroid$Path)
MakefileAndroid$Path<-  gsub('home/vilinux/Documents/SDL-mirror/','',MakefileAndroid$Path)

max.len <- max (nrow(MakefileiOS),nrow(MakefileAndroid),nrow(MakefileWindows),nrow(MakefileLinux),nrow(MakefilemacOS))


Makefiles <-merge(MakefileAndroid,MakefileiOS,by="Path", all.x = TRUE, all=TRUE)
Makefiles <-merge(Makefiles,MakefileWindows,by="Path", all.x = TRUE, all=TRUE)
Makefiles <-merge(Makefiles,MakefilemacOS,by="Path", all.x = TRUE, all=TRUE)
Makefiles <-merge(Makefiles,MakefileLinux,by="Path", all.x = TRUE, all=TRUE)
Makefiles <- Makefiles%>% mutate(Linux=0,Windows=0,macOS=0,iPhone=0,Android=0)
Makefiles$Linux <- ifelse(Makefiles$Path %in% MakefileLinux$Path,1,0)
Makefiles$Windows <- ifelse(Makefiles$Path %in% MakefileWindows$Path,1,0)
Makefiles$macOS <- ifelse(Makefiles$Path %in% MakefilemacOS$Path,1,0)
Makefiles$iPhone <- ifelse(Makefiles$Path %in% MakefileiOS$Path,1,0)
Makefiles$Android <- ifelse(Makefiles$Path %in% MakefileAndroid$Path,1,0)

Makefiles<- Makefiles %>% mutate(Plataformas=Linux+Windows+macOS+iPhone+Android)
Makefiles<- Makefiles %>% mutate(Codigo = ifelse(Plataformas!=1,"Independente", ifelse(Linux==1, "Linux",ifelse(Windows==1,"Windows",ifelse(macOS==1,"macOS",ifelse(iPhone==1,"iPhone",ifelse(Android==1,"Android",""))))) ) )

#plats<- data.frame (diretorio = c("windows","linux","macosx","macos","android","iphoneos","x11", "alsa", "haiku", "cocoa", "raspberry", "emscripten" ,"wayland","uikit","dummy","winrt", "nacl", "directsound","psp", "qsa","pandora" ,"vivante","darwin","arts", "fusionsound","pulseaudio", "sndio", "winmm","mir", "coreaudio", "esd","nas", "paudio", "sun","xaudio2","bsd","directfb","ataricommon","opengles2","gem", "mx6","xbios","direct3d11","nds","beos", "bwindow","win32","baudio","dart","macrom","mint","symbian", "windib","windx5","aix", "os2","dynapi", "hermes", "haptic", "loadso","direct3d", "opengles", "opengl","irix","wince","picogui","wscons", "wincommon", "svga", "riscos", "photon", "ps2gs", "qtopia", "nanox", "maccommon", "macdsp","ipod","ggi", "gapi", "fbcon", "Xext", "events", "aalib", "libm", "quartz","ps3", "dc/", "qnxgf", "qnx","pth","pthread", "cdrom", "ums", "stdcpp", "thread/", "render/", "dga", "vgl", "video/", "timer/", "input/","nto","mme","dma", "audio/", "joystick/","src/"), plataforma= c("Windows", "Linux","macOS", "macOS", "Android", "iPhone", "Linux","Linux", "Haiku", "Independente","Raspberry", "3rdpart","Linux", "iPhone", "Dummy", "Windows", "3rdpart" ,"Windows", "PSP", "3rdpart", "Pandora", "3rdpart" , "Darwin", "3rdpart","3rdpart", "Independente", "Independente", "Windows", "Linux" , "Linux" , "3rdpart", "3rdpart", "3rdpart", "Sun" , "Windows", "BSD", "Independente", "Atari","Independente", "Ruby", "3rdpart","3rdpart", "Windows", "NSD","BeOS","Haiku","Windows","Independente", "Independente","Macrom", "Linux", "Symbian", "Windows", "Windows", "Linux", "OS2", "3rdpart", "3rdpart", "Independente", "3rdpart", "Windows", "Independente", "Independente", "Irix", "WinCE", "Independente", "Independente", "Windows", "Independente", "RiscOS", "Photon", "PSP", "Linux", "nanoX", "macOS", "macOS", "iPod", "Independente", "WinCE", "Linux", "Linux", "Independente", "Independente", "Independente", "macOS","PSP", "DC/OS","QNX","QNX", "Independente", "Independente", "Independente", "Independente", "Independente", "Independente","Independente", "Independente","Independente", "Independente", "Independente", "Independente", "Independente", "Independente", "Independente", "Independente", "Independente","Independente"))

#write.csv(plats, "plats.csv")
plats<- read.csv("data/plats.csv")
plats<- plats %>% select(diretorio,plataforma)

##Suporte
##Android foi a ultima plataforma jun/2010

###################################### Filter 1
#filtro de data
data <- data %>%
  mutate(module = gsub('/[^/]+$', '', path)) %>%
  filter(grepl('^src.*', module)  )%>%
  filter(as.Date(date)>as.Date("2010-06-10 00:00:2018-11-05 -0000")) %>%
  filter(n_line_add>=3)


data <- data %>% mutate(platform = ifelse(path%in%Makefiles$Path, Makefiles$Codigo[match(path, Makefiles$Path)], "Outros"))

for (i in 1:dim(plats)[1]){
  #print(plats[i,1]) 
  data$platform <- ifelse(data$platform=="Outros" & grepl(plats[i,1],data$path),paste("", plats[i,2], sep=""),data$platform)
}

data <- data %>% filter(platform=="Independente" |platform=="Windows"|platform=="Linux"|platform=="macOS"|platform=="Android"|platform=="iPhone" )

remove(MakefileAndroid,MakefileiOS,MakefileLinux,MakefilemacOS, Makefiles, MakefileWindows,plats, max.len, i)
