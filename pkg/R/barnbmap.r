barnbmap <- function(sp.obj, nb.obj,
criteria=NULL, carte=NULL, identify=FALSE, cex.lab=0.8, pch=16, col="lightblue3",
xlab="", ylab="", axes=FALSE, lablong="", lablat="")
{
# Verification of the Spatial Object sp.obj
class.obj<-class(sp.obj)[1]

if(substr(class.obj,1,7)!="Spatial") stop("sp.obj may be a Spatial object")

# Is there a Tk window already open ?
if(interactive())
{
 if(!exists("GeoXp.open",envir = baseenv())) # new environment
 {
  assign("GeoXp.open", TRUE, envir = baseenv())
 }
 else
 {if(get("GeoXp.open",envir= baseenv()))
   {stop("Warning : a GeoXp function is already open. Please, close Tk window before calling a new GeoXp function to avoid conflict between graphics")}
  else
  {assign("GeoXp.open", TRUE, envir = baseenv())}
 }
}

# we propose to refind the same arguments used in first version of GeoXp
coords<-coordinates(sp.obj)

if(substr(class.obj,nchar(class.obj)-8,nchar(class.obj))=="DataFrame")
{listvar<-sp.obj@data
listnomvar<-names(sp.obj@data)
}
else
{listvar<-NULL
listnomvar<-NULL
}

# Code which was necessary in the previous version
object<-nb.obj

 if(is.null(carte) & substr(class.obj,1,15)=="SpatialPolygons") carte<-spdf2list(sp.obj)$poly

 # for identifyng the selected sites
ifelse(identify, label<-row.names(listvar),label<-"")

# initialisation
  nointer<-FALSE
  nocart<-FALSE
  buble<-FALSE
  z<-NULL
  legmap<-NULL
  legends<-list(FALSE,FALSE,"","")
  labvar=c(xlab,ylab)

# Transformation data.frame en matrix

if((length(listvar)>0) && (dim(as.matrix(listvar))[2]==1)) listvar<-as.matrix(listvar)

# Initialisation des objets de spdep
  nb <- nb.obj
  W<-nb2mat(nb)
  if (!inherits(nb, "nb")) stop("Not a neighbours list")
 
  c.nb <- card(nb)
  n.nb <- length(nb)
  regids <- attr(nb, "region.id")
  
  if (is.null(regids)) regids <- as.character(1:n.nb)

  long <- coords[, 1]
  lat <- coords[, 2]
    
  obs <- matrix(FALSE, nrow=n.nb, ncol=n.nb)
    
  graf<-"Neighbourplot1"

  graphics.off()

  # Ouverture des fenêtres graphiques
  dev.new()
  dev.new()

  fin <- tclVar(FALSE)

 # for colors in map and new grahics
 ifelse(length(col)==1, col2<-"blue", col2<-col)
 col3<-"lightblue3"
 
####################################################
# sélection d'un point sur la carte
####################################################

pointfunc<-function()
{
   if (graf=="Neighbourplot2")
   {
    SGfunc();
   }
    quit <- FALSE;
    graf<<-"Neighbourplot1"
    
    dev.set(2)
    title("ACTIVE DEVICE", cex.main = 0.8, font.main = 3, col.main='red')
    title(sub = "To stop selection, click on the right button of the mouse and stop (for MAC, ESC)", cex.sub = 0.8, font.sub = 3,col.sub='red')
        
    while(!quit)
    {
        dev.set(2)
        loc<-locator(1)
        if (is.null(loc))
        {
            quit<-TRUE
            carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=pch,
            method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,buble=buble,criteria=criteria,
            nointer=nointer,cbuble=z,carte=carte,nocart=nocart,couleurs=col2,classe=card(object),cex.lab=cex.lab)
            next
        }
        obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point");

        # graphiques
        carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=pch,
        method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,buble=buble,criteria=criteria,
        nointer=nointer,cbuble=z,carte=carte,nocart=nocart,couleurs=col2,classe=card(object),cex.lab=cex.lab)

        title("ACTIVE DEVICE", cex.main = 0.8, font.main = 3, col.main='red')
        title(sub = "To stop selection, click on the right button of the mouse and stop (for MAC, ESC)", cex.sub = 0.8, font.sub = 3,col.sub='red')
    
        graphique(var1=nb, obs=obs, num=3, graph="bar.nb", W=W,
        labvar=labvar, symbol=pch,couleurs=col);

  #  obs <<- matrix(FALSE, nrow=length(long), ncol=length(long));

    }
  }
  

####################################################
# sélection d'un polygone
####################################################

polyfunc<-function()
{
   if (graf=="Neighbourplot2")
   {
    SGfunc()
   }
    graf<<-"Neighbourplot1"
    polyX <- NULL
    polyY <- NULL
    quit <- FALSE
  
    dev.set(2)
    title("ACTIVE DEVICE", cex.main = 0.8, font.main = 3, col.main='red')
    title(sub = "To stop selection, click on the right button of the mouse and stop (for MAC, ESC)", cex.sub = 0.8, font.sub = 3,col.sub='red')

    
    while(!quit)
    {
        dev.set(2)
        loc<-locator(1)
        if(is.null(loc))
        {
           quit<-TRUE
           carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=pch,
           method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,buble=buble,criteria=criteria,
           nointer=nointer,cbuble=z,carte=carte,nocart=nocart,couleurs=col2,classe=card(object),cex.lab=cex.lab)
           next
        }

        polyX <- c(polyX, loc[1])
        polyY <- c(polyY, loc[2])
        lines(polyX,polyY);
   }

    polyX <- c(polyX, polyX[1])
    polyY <- c(polyY, polyY[1])
if (length(polyX)>0)
{
    lines(polyX,polyY)

    obs <<- selectmap(var1=long, var2=lat, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly")

    # graphiques
    carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=pch,
    method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,buble=buble,criteria=criteria,
    nointer=nointer,cbuble=z,carte=carte,nocart=nocart,couleurs=col2,classe=card(object),cex.lab=cex.lab)
    
    graphique(var1=nb, obs=obs, num=3, graph="bar.nb", W=W, labvar=labvar,
    symbol=pch,couleurs=col)
    
 #   obs <<- matrix(FALSE, nrow=length(long), ncol=length(long));

}
  }




####################################################
# sélection d'une barre de l'histogramme
####################################################

barfunc<-function()
{
   if (graf=="Neighbourplot1") SGfunc()
   graf<<-"Neighbourplot2"
    
   quit <- FALSE
   dev.set(3)
   title("ACTIVE DEVICE", cex.main = 0.8, font.main = 3, col.main='red')
   title(sub = "To stop selection, click on the right button of the mouse and stop (for MAC, ESC)", cex.sub = 0.8, font.sub = 3,col.sub='red')

    while(!quit)
    {
        dev.set(3)
        loc<-locator(1)
        if(is.null(loc))
        {
         quit<-TRUE
        graphique(var1=nb, obs=obs, num=3, graph="bar.nb", 
        labvar=labvar, symbol=pch,couleurs=col)
          next
        }
        obs<<-selectstat(var1=nb,obs=obs,Xpoly=loc[1], Ypoly=loc[2],method="barnb")

      # graphiques

        graphique(var1=nb, obs=obs, num=3, graph="bar.nb", 
        labvar=labvar, symbol=pch,couleurs=col)
        title("ACTIVE DEVICE", cex.main = 0.8, font.main = 3, col.main='red')
        title(sub = "To stop selection, click on the right button of the mouse and stop (for MAC, ESC)", cex.sub = 0.8, font.sub = 3,col.sub='red')

        carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label,
        symbol=pch, method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,
        buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart,
        couleurs=col2,classe=card(object),cex.lab=cex.lab)
    }
  }

        
####################################################
# contour des unités spatiales
####################################################
cartfunc <- function()
{ 
 if (length(carte) != 0)
   { ifelse(!nocart,nocart<<-TRUE,nocart<<-FALSE)
     carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label,
     symbol=pch, method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,
     buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart,
     couleurs=col2,classe=card(object),cex.lab=cex.lab)
   }
   else
   {
    tkmessageBox(message="Spatial contours have not been given",icon="warning",type="ok")    
   }
}

####################################################
# Open a no interactive selection
####################################################

fnointer<-function() 
{
 if (length(criteria) != 0)
  {  
     ifelse(!nointer,nointer<<-TRUE,nointer<<-FALSE)
     carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label,
     symbol=pch, method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,
     buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart,
     couleurs=col2,classe=card(object),cex.lab=cex.lab)
  }
 else
  {
  tkmessageBox(message="Criteria has not been given",icon="warning",type="ok")
  }
 
}

  
####################################################
# Bubble
####################################################

fbubble<-function()
{
  res2<-choix.bubble(buble,listvar,listnomvar,legends)
  
  buble <<- res2$buble
  legends <<- res2$legends
  z <<- res2$z
  legmap <<- res2$legmap
  
  carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label,
  symbol=pch, method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,
  buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart,
  couleurs=col2,classe=card(object),cex.lab=cex.lab)
}


####################################################
# rafraichissement des graphiques
####################################################

SGfunc<-function()
{
    obs <<- matrix(FALSE, nrow=length(long), ncol=length(long));

    # graphiques
        carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label,
        symbol=pch, method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,
        buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart,
        couleurs=col2,classe=card(object),cex.lab=cex.lab)
        
        graphique(var1=nb, obs=obs, num=3, graph="bar.nb", W=W,
        labvar=labvar, symbol=pch,couleurs=col)
  }
  
####################################################
# quitter l'application
####################################################

quitfunc<-function()
{
    assign("GeoXp.open", FALSE, envir = baseenv())
    tkdestroy(tt)
}

####################################################
# Représentation des graphiques
####################################################

carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label,
symbol=pch, method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,
buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,nocart=nocart,
couleurs=col2,classe=card(object),cex.lab=cex.lab)

graphique(var1=nb, obs=obs, num=3, graph="bar.nb", W=W,labvar=labvar,
symbol=pch,couleurs=col)

####################################################
# Boîte de Dialogue
####################################################

if(interactive())
{
tt <- tktoplevel()


labelText1 <- tclVar("Work on the map")
label1 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)

point.but <- tkbutton(tt, text="  Point  ", command=pointfunc);
poly.but <- tkbutton(tt, text="  Polygon  ", command=polyfunc);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))

labelText2 <- tclVar("Work on the bar plot")
label2 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText2))
tkconfigure(label2, textvariable=labelText2)
tkgrid(label2,columnspan=2)

barre.but <- tkbutton(tt, text="   Cell   ", command=barfunc);
tkgrid(barre.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


label1 <- tclVar("To stop selection, let the cursor on the active graph, click on the right button of the mouse and stop")
label11 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(label1))
tkconfigure(label11, textvariable=label1)
tkgrid(label11,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText7 <- tclVar("Preselected sites")
label7 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText7))
tkconfigure(label7, textvariable=labelText7)
tkgrid(label7,columnspan=2)

noint1.but <- tkbutton(tt, text="  On/Off  ", command=fnointer);
tkgrid(noint1.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

labelText6 <- tclVar("Draw spatial contours")
label6 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText6))
tkconfigure(label6, textvariable=labelText6)
tkgrid(label6,columnspan=2)

nocou1.but <- tkbutton(tt, text="  On/Off  ", command=cartfunc);
tkgrid(nocou1.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText3 <- tclVar("Restore graph")
label3 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText3))
tkconfigure(label3, textvariable=labelText3)
tkgrid(label3,columnspan=2)

nettoy.but <- tkbutton(tt, text="     OK     " , command=SGfunc);
tkgrid(nettoy.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

labelText9 <- tclVar("Bubbles")
label9 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText9))
tkconfigure(label9, textvariable=labelText9)
tkgrid(label9,columnspan=2)

bubble.but <- tkbutton(tt, text="  On/Off  ", command=fbubble);
tkgrid(bubble.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText5 <- tclVar("Exit")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)

quit.but <- tkbutton(tt, text="     OK     ", command=quitfunc);
tkgrid(quit.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))
}
####################################################


return(invisible())

}
