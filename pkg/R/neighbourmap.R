`neighbourmap` <-
function(long, lat, var, W, id=FALSE, listvar=NULL, listnomvar=NULL, carte=NULL,
criteria=NULL, label="", cex.lab=1, pch=16, col="blue", xlab="",ylab="",
axes=FALSE, lablong="", lablat="") 
{
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
   
    while(!quit)
    {
     dev.set(2)
     loc<-locator(1)
    
      if (is.null(loc)) 
       {
         quit<-TRUE
         next
       }           
   
      obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point")

        # graphiques
        
      carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=classe,
      symbol=c(pch[1],16),W=W, method="Neighbourplot1", buble=buble, cbuble=z, criteria=criteria,
      nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
      label=label, cex.lab=cex.lab)      
      
      graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", labvar=labvar,
      couleurs=col, symbol=pch, opt1=id, W=W)  
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
      couleurs=col, symbol=pch, opt1=id, W=W)  
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

    while(!quit)
    {
        dev.set(3)
        loc<-locator(1)
        if(is.null(loc)) 
        {
            quit<-TRUE
            next
        }
 
 
        obs <<- selectstat(var1=var,obs=obs, Xpoly=loc[1], Ypoly=loc[2], method="Neighbourplot", W=W)

        # graphiques
      carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=classe,
      symbol=c(pch[1],16),W=W, method="Neighbourplot2", buble=buble, cbuble=z, criteria=criteria,
      nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
      label=label, cex.lab=cex.lab)      
      
      graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", labvar=labvar,
      couleurs=col, symbol=pch, opt1=id , W=W) 
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
  couleurs=col, symbol=pch, opt1=id , W=W)  
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
  couleurs=col, symbol=pch, opt1=id , W=W)  
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
  tclvalue(fin)<<-TRUE
  tkdestroy(tt)
}

####################################################
# Représentation des graphiques
####################################################

   carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=classe,
   symbol=c(pch[1],16),W=W, method="Neighbourplot1", buble=buble, cbuble=z, criteria=criteria,
   nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
   label=label, cex.lab=cex.lab)      
   
   graphique(var1=var, obs=obs, num=3, graph="Neighbourplot", labvar=labvar,
   couleurs=col, symbol=pch, opt1=id, W=W) 
   
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


labelText5 <- tclVar("Exit")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)

quit.but <- tkbutton(tt, text="     OK     ", command=quitfunc);
tkgrid(quit.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))
tkwait.variable(fin)
}
####################################################


return(obs)
  }

