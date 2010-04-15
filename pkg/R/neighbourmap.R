`neighbourmap` <-function(sp.obj, name.var, nb.obj, lin.reg=TRUE,
names.attr=names(sp.obj), criteria=NULL, carte=NULL, identify=FALSE, cex.lab=0.8, pch=16, col="lightblue3",
xlab="", ylab="", axes=FALSE, lablong="", lablat="")
{
# Verification of the Spatial Object sp.obj
class.obj<-class(sp.obj)[1]

if(substr(class.obj,1,7)!="Spatial") stop("sp.obj may be a Spatial object")
if(substr(class.obj,nchar(class.obj)-8,nchar(class.obj))!="DataFrame") stop("sp.obj should contain a data.frame")
if(!is.numeric(name.var) & is.na(match(as.character(name.var),names(sp.obj)))) stop("name.var is not included in the data.frame of sp.obj")
if(length(names.attr)!=length(names(sp.obj))) stop("names.attr should be a vector of character with a length equal to the number of variable")

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
long<-coordinates(sp.obj)[,1]
lat<-coordinates(sp.obj)[,2]

var<-sp.obj@data[,name.var]

# verify the type of the main variable
if(!(is.integer(var) || is.double(var))) stop("the variable name.var should be a numeric variable")


listvar<-sp.obj@data
listnomvar<-names.attr

# Code which was necessary in the previous version
 if(is.null(carte) & class.obj=="SpatialPolygonsDataFrame") carte<-spdf2list(sp.obj)$poly

 # for identifyng the selected sites
ifelse(identify, label<-row.names(listvar),label<-"")

# spatial weight matrix
W<-nb2mat(nb.obj)

#initialisation
 obs <- matrix(FALSE, nrow=length(long), ncol=length(long))
 obs2 <- matrix(FALSE, nrow=length(long), ncol=length(long));
 nointer<-FALSE
 nocart<-FALSE
 buble<-FALSE
 legends<-list(FALSE,FALSE,"","")
 z<-NULL
 legmap<-NULL
 inout<-NULL
 graf<-"Neighbourplot1"
 labvar<-c(xlab,ylab)
 classe<-rep(1,length(long))

# Transformation data.frame en matrix
if((length(listvar)>0) && (dim(as.matrix(listvar))[2]==1)) listvar<-as.matrix(listvar)

# Ouverture des fenêtres graphiques
graphics.off()

dev.new()
dev.new()

fin <- tclVar(FALSE)

####################################################
# sélection d'un point sur la carte
####################################################

pointfunc<-function() 
{
  if (graf=="Neighbourplot2") SGfunc()
   
   quit <- FALSE
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
         carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=classe,
         symbol=c(pch[1],16),W=W, method="Neighbourplot1", buble=buble, cbuble=z, criteria=criteria,
         nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
         label=label, cex.lab=cex.lab)    
         next
       }           
   
      obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point")

        # graphiques
        
      carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=classe,
      symbol=c(pch[1],16),W=W, method="Neighbourplot1", buble=buble, cbuble=z, criteria=criteria,
      nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
      label=label, cex.lab=cex.lab)      
      
      title("ACTIVE DEVICE", cex.main = 0.8, font.main = 3, col.main='red')
      title(sub = "To stop selection, click on the right button of the mouse and stop (for MAC, ESC)", cex.sub = 0.8, font.sub = 3,col.sub='red')

      graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", labvar=labvar,
      couleurs=col, symbol=pch, opt1=lin.reg, W=W)
    }
}
     
####################################################
# sélection d'un polygone
####################################################

polyfunc<-function() 
{
   if (graf=="Neighbourplot2") SGfunc()
   
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
            next
        }
        
        polyX <- c(polyX, loc[1])
        polyY <- c(polyY, loc[2])
        lines(polyX,polyY)
    }

    polyX <- c(polyX, polyX[1])
    polyY <- c(polyY, polyY[1])
if (length(polyX)>0)
{
    lines(polyX,polyY)

    obs <<- selectmap(var1=long, var2=lat, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly")
    
    # graphiques
      carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart,  classe=classe,
      symbol=c(pch[1],16),W=W, method="Neighbourplot1", buble=buble, cbuble=z, criteria=criteria,
      nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
      label=label, cex.lab=cex.lab)      
      
      graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", labvar=labvar,
      couleurs=col, symbol=pch, opt1=lin.reg, W=W)
 #   obs <<- matrix(FALSE, nrow=length(long), ncol=length(long));

}
  }

####################################################
# sélection d'un point sur le scatterplot
####################################################

voisfunc <- function()
{
   if (graf=="Neighbourplot1")  SGfunc()
   
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
            graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", labvar=labvar,
            couleurs=col, symbol=pch, opt1=lin.reg , W=W)
            next
        }
 
 
        obs <<- selectstat(var1=var,obs=obs, Xpoly=loc[1], Ypoly=loc[2], method="Neighbourplot", W=W)

        # graphiques
      carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=classe,
      symbol=c(pch[1],16),W=W, method="Neighbourplot2", buble=buble, cbuble=z, criteria=criteria,
      nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
      label=label, cex.lab=cex.lab)      
      
      graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", labvar=labvar,
      couleurs=col, symbol=pch, opt1=lin.reg , W=W)
      title("ACTIVE DEVICE", cex.main = 0.8, font.main = 3, col.main='red')
      title(sub = "To stop selection, click on the right button of the mouse and stop (for MAC, ESC)", cex.sub = 0.8, font.sub = 3,col.sub='red')

    }
  }



####################################################
# sélection d'un polygone sur le scattermap
####################################################


polyscatfunc <- function() 
{ 
  obs2 <<- matrix(FALSE, nrow=length(long), ncol=length(long))
  
  if (graf=="Neighbourplot1")  SGfunc()

  graf<<-"Neighbourplot2"
  quit <- FALSE
  polyX <- NULL
  polyY <- NULL
 
    dev.set(3)
    title("ACTIVE DEVICE", cex.main = 0.8, font.main = 3, col.main='red')
    title(sub = "To stop selection, click on the right button of the mouse and stop (for MAC, ESC)", cex.sub = 0.8, font.sub = 3,col.sub='red')

   while (!quit) 
    {
     dev.set(3)
     loc <- locator(1)
       if (is.null(loc)) 
         {
           quit <- TRUE
           next
         }
      polyX <- c(polyX, loc[1])
      polyY <- c(polyY, loc[2])
      lines(polyX, polyY)
    }

  polyX <- c(polyX, polyX[1])
  polyY <- c(polyY, polyY[1])

 if (length(polyX)>0)
 {
  lines(polyX, polyY)

  
  obs2[which(W!=0,arr.ind=TRUE)] <<- inout(cbind(var[which(W!=0,arr.ind=TRUE)[,1]],var[which(W!=0,arr.ind=TRUE)[,2]]),cbind(polyX, polyY), bound = TRUE)

  obs3<-obs+obs2
  obs[which(obs3==1,arr.ind=TRUE)]<<-TRUE
  obs[which(obs3!=1,arr.ind=TRUE)]<<-FALSE

  carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=classe,
  symbol=c(pch[1],16),W=W, method="Neighbourplot2", buble=buble, cbuble=z, criteria=criteria,
  nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
  label=label, cex.lab=cex.lab)      
      
  graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", labvar=labvar,
  couleurs=col, symbol=pch, opt1=lin.reg , W=W)
  }    
}



####################################################
# contour des unités spatiales
####################################################

cartfunc <- function()
{  
  if (length(carte) != 0)
   {
   ifelse(!nocart,nocart<<-TRUE,nocart<<-FALSE)
   carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=classe,
   symbol=c(pch[1],16),W=W, method=graf, buble=buble, cbuble=z, criteria=criteria,
   nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
   label=label, cex.lab=cex.lab)       
   }
   else
   {
    tkmessageBox(message="Spatial contours have not been given",icon="warning",type="ok")    
   }
}


####################################################
# rafraichissement des graphiques
####################################################

SGfunc<-function() 
{
    obs <<- matrix(FALSE, nrow=length(long), ncol=length(long))

  # graphiques
  carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=classe,
  symbol=c(pch[1],16),W=W, method=graf, buble=buble, cbuble=z, criteria=criteria,
  nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
  label=label, cex.lab=cex.lab)  
  
  graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", labvar=labvar,
  couleurs=col, symbol=pch, opt1=lin.reg , W=W)
}

####################################################
# Open a no interactive selection
####################################################

fnointer<-function() 
{
 if (length(criteria) != 0)
 {
  ifelse(!nointer,nointer<<-TRUE,nointer<<-FALSE)
  carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart,  classe=classe,
  symbol=c(pch[1],16),W=W, method=graf, buble=buble, cbuble=z, criteria=criteria,
  nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
  label=label, cex.lab=cex.lab)  
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
  
  carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart,  classe=classe,
  symbol=c(pch[1],16),W=W, method=graf, buble=buble, cbuble=z, criteria=criteria,
  nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
  label=label, cex.lab=cex.lab)  
 }


####################################################
# quitter l'application
####################################################

quitfunc<-function()
{
    #tclvalue(fin)<<-TRUE
    tkdestroy(tt)
    assign("GeoXp.open", FALSE, envir = baseenv())
   # assign("obs", row.names(sp.obj)[obs], envir = .GlobalEnv)
}

quitfunc2<-function()
{
    #tclvalue(fin)<<-TRUE
    tkdestroy(tt)
    assign("GeoXp.open", FALSE, envir = baseenv())
    print("Results have been saved in last.select object")
    if(graf=="Neighbourplot1")
    {obs<-unique(which(obs,arr.ind=TRUE)[,1])
     p<-length(obs)
     res<-NULL
     for(k in 1:p)
     {res<-rbind(res,cbind(obs[k],nb.obj[[obs[k]]]))
     }
    }
    else
    {res<-which(obs,arr.ind=TRUE)}
    assign("last.select", res, envir = .GlobalEnv)
}

####################################################
# Représentation des graphiques
####################################################

   carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=classe,
   symbol=c(pch[1],16),W=W, method="Neighbourplot1", buble=buble, cbuble=z, criteria=criteria,
   nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
   label=label, cex.lab=cex.lab)      
   
   graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", labvar=labvar,
   couleurs=col, symbol=pch, opt1=lin.reg, W=W)
   
####################################################
# création de la boite de dialogue
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

labelText71 <- tclVar("Work on the Graph")
label71 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText71))
tkconfigure(label71, textvariable=labelText71)
tkgrid(label71,columnspan=2)

vari.but <- tkbutton(tt, text="  Points  ", command=voisfunc);
vari2.but <- tkbutton(tt, text="  Polygon  ", command=polyscatfunc);
tkgrid(vari.but,vari2.but)
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


labelText5 <- tclVar("  Exit  ")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)


quit.but <- tkbutton(tt, text="Save results", command=quitfunc2);
quit.but2 <- tkbutton(tt, text="Don't save results", command=quitfunc);
tkgrid(quit.but2,quit.but)
tkgrid(tklabel(tt,text="    "))
}
####################################################
return(invisible())
}

