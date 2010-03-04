`moranplotmap` <-
function (long, lat, var, W, flower=FALSE, locmoran=FALSE, listvar=NULL, listnomvar=NULL,carte=NULL,
criteria=NULL, label="", cex.lab=1, col="blue",pch=16, xlab=expression((X-bar(X))),ylab=expression(W(X-bar(X))),
axes=FALSE, lablong="", lablat="",names.arg=c("H.-H.","L.-H.","L.-L.","H.-L."))
{

#initialisation
  obs<-vector(mode = "logical", length = length(long))
  obsq<-rep(0,length(long))
  nointer<-FALSE
  nocart<-FALSE
  buble<-FALSE
  buble2<-FALSE
  maptest<-FALSE
  classe<-rep(1,length(long))

  legends<-list(FALSE,FALSE,"","")
  legends2<-list(FALSE,FALSE,"","")

  z<-NULL
  z2<-NULL
  legmap<-NULL
  legmap2<-NULL
  num<-NULL
  labvar=c(xlab,ylab)

  graphChoice <- ""
  varChoice1 <- ""
  varChoice2 <- ""
  choix <- ""
  listgraph <- c("Histogram","Barplot","Scatterplot")

#Ouverture des fenêtres graphiques
graphics.off()
dev.new()
dev.new()

fin <- tclVar(FALSE)

quad <- FALSE

# Transformation data.frame en matrix
if((length(listvar)>0)&&(dim(as.matrix(listvar))[2]==1)) listvar<-as.matrix(listvar)

# Option sur le moran
ifelse(flower,method <- "Neighbourplot1", method <- "")

choix.col<-FALSE

graph <- "Moran"
col2 <- rep(col[1],4)
pch2 <- rep(pch[1],4)


#Normalize w
#W<-normw(W)
ifelse(apply(W,1,sum)==rep(1,length(long)),is.norm<-TRUE,is.norm<-FALSE)
# calcul du I de Moran
var <- var - mean(var)
wvar <- W%*%var
stdvar <- var/sd(var)
uns <- rep(1, length(var))
result <- nonormmoran(stdvar,uns,W)
MORAN <- result$morani
prob.I <- pnorm(result$istat)
rvar <- qr(var)
beta.I <- qr.coef(rvar,wvar)

# calcul de la variable obsq (pour les quadrants)

obsq[which((var > 0) & (wvar >= 0))] <- 1
obsq[which((var <= 0) & (wvar > 0))] <- 2
obsq[which((var < 0) & (wvar <= 0))] <- 3
obsq[which((var >= 0) & (wvar < 0))] <- 4
 
  
####################################################
# sélection d'un point
####################################################

pointfunc<-function() 
{
    quit <- FALSE
    quad <<- FALSE

    while(!quit)
    {
        if(maptest) 
        {
            dev.set(2)
            loc<-locator(1)
            if(is.null(loc)) 
            {
              quit<-TRUE
              next
            }           
            obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point") 
        }
        else
        {
            dev.set(3)
            loc<-locator(1)
            if(is.null(loc)) 
            {
              quit<-TRUE
              next
            }
            obs<<-selectmap(var1=var,var2=wvar,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point")
        }
        
        #graphiques
        carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=obsq,
        couleurs=col2, symbol=pch2, W=W, method=method, buble=buble, cbuble=z, criteria=criteria,
        nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
        label=label, cex.lab=cex.lab, labmod=names.arg)      

        graphique(var1=var, var2=wvar, obs=obs, num=3, graph=graph, labvar=labvar, couleurs=col2, 
        symbol=pch2, locmoran=locmoran,obsq=obsq, cex.lab=cex.lab,buble=buble2, cbuble=z2, 
        legmap=legmap2, legends=legends2 ) 
           
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], 
            obs=obs, num=4, graph=graphChoice, couleurs=col[1], symbol=pch[1], labvar=c(varChoice1,varChoice2))
        }
    }
  }

####################################################
# sélection d'un point sur la carte
####################################################

pt1func <- function()
{
  ifelse(flower,method <<- "Neighbourplot1", method <<- "")
  graph <<- "Moran" 
  maptest <<- TRUE  
  pointfunc()
}

####################################################
# sélection d'un point sur le graphique
####################################################

pt2func <- function()
{    
  ifelse(flower,method <<- "Neighbourplot1", method <<- "")
  graph <<- "Moran" 
  maptest <<- FALSE  
  pointfunc()
  }

####################################################
# sélection d'un polygone
####################################################

polyfunc<-function() 
{
   polyX <- NULL
   polyY <- NULL
   quit <- FALSE
   quad <<- FALSE

   while(!quit)
    {
        if(maptest)
        {
          dev.set(2)
          loc<-locator(1)
          if(is.null(loc)) 
            {
              quit<-TRUE
              next
            }
        }
        else
        {
            dev.set(3);
            loc<-locator(1)
            if(is.null(loc)) 
            {
                quit<-TRUE
                next
            }   
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

    if (maptest)
    {
        obs <<- selectmap(var1=long, var2=lat, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly")
    }
    else
    {
        obs <<- selectmap(var1=var, var2=wvar, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly")
    }

    #graphiques
        carte(long=long, lat=lat, obs=obs,  carte=carte,nocart=nocart, classe=obsq,
        couleurs=col2, symbol=pch2, W=W, method=method, buble=buble, cbuble=z, criteria=criteria,
        nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
        label=label, cex.lab=cex.lab, labmod=names.arg)        

        graphique(var1=var, var2=wvar, obs=obs, num=3, graph=graph, labvar=labvar, couleurs=col2, 
        symbol=pch2, locmoran=locmoran,obsq=obsq, cex.lab=cex.lab,buble=buble2, cbuble=z2, 
        legmap=legmap2, legends=legends2 )   
        
     if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
          graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], 
          obs=obs, num=4, graph=graphChoice, couleurs=col[1], symbol=pch[1], labvar=c(varChoice1,varChoice2))
        }
  }
}

####################################################
# sélection d'un polygone sur la carte
####################################################

poly1func <- function()
{
  ifelse(flower,method <<- "Neighbourplot1", method <<- "")
  graph <<- "Moran" 
       
    if (quad == TRUE)
    {
      SGfunc()
      quad <<- FALSE
    }
    
    maptest <<- TRUE
    polyfunc()
  }

####################################################
# sélection d'un polygone sur le graphique
####################################################

poly2func <- function()
{
 
  ifelse(flower,method <<- "Neighbourplot1", method <<- "")
  graph <<- "Moran" 
       
    if (quad == TRUE)
    {
      SGfunc()
      quad <<- FALSE
    }
    maptest <<- FALSE
    polyfunc()
  }

####################################################
# sélection d'un quadrant
####################################################

quadfunc <- function()
{
    if (quad == FALSE)
    {
      SGfunc()
      quad <<- TRUE
    }

    obs[which(obsq == num)] <<- !obs[which(obsq == num)]

    carte(long=long, lat=lat, obs=obs, classe=obsq,  carte=carte,nocart=nocart, labmod=names.arg,
    couleurs=col2,symbol=pch2, method="Quadrant",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,
    legends=legends,axis=axes,lablong=lablong, lablat=lablat, label=label, cex.lab=cex.lab) 

    graphique(var1=var, var2=wvar, obs=obs, num=3, graph=graph, labvar=labvar, couleurs=col2, 
    symbol=pch2, locmoran=locmoran,obsq=obsq, cex.lab=cex.lab,buble=buble2, cbuble=z2, 
    legmap=legmap2, legends=legends2 )   
    
    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
     {
      graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
      obs=obs, num=4, graph=graphChoice, symbol=pch[1], couleurs=col[1], labvar=c(varChoice1,varChoice2))
     }
}

####################################################
# sélection d'un des 4 quadrants 
####################################################

quad1func <- function()
{
  num <<- 1 
  method <<- "Quadrant" 
  graph <<- "Quadrant"  
  quadfunc()
}

quad2func <- function()
{
 num <<- 2  
 method <<- "Quadrant"
 graph <<- "Quadrant"      
 quadfunc()
}

quad3func <- function()
{
 num <<- 3
 method <<- "Quadrant"
 graph <<- "Quadrant"    
 quadfunc()
}

quad4func <- function()
{
 num <<- 4
 method <<- "Quadrant"
 graph <<- "Quadrant"    
 quadfunc()
}

####################################################
# Différentes couleurs selon le quadrant
####################################################

 colfunc <- function()
{
    if(!choix.col)
    {choix.col<<-TRUE
     res1<-choix.couleur("Moran",col=col[1],pch=pch[1],legends=legends)     
     ifelse(length(res1$col2)==4,col2 <<- res1$col2,col2 <<- rep(col[1],4))
     ifelse(length(res1$pch2)==4,pch2 <<- res1$pch2,pch2 <<- rep(pch[1],4))
     legends <<- res1$legends
     }
    else
    {choix.col<<-FALSE
     col2 <<- rep(col[1],4)
     pch2 <<- rep(pch[1],4)
     legends <<- list(legends[[1]],FALSE,legends[[3]],"")
    }
     
carte(long=long, lat=lat, obs=obs, carte=carte,nocart=nocart, classe=obsq,
couleurs=col2, symbol=pch2, W=W, method=method, buble=buble, cbuble=z, criteria=criteria,
nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
label=label, cex.lab=cex.lab,labmod=names.arg)     

graphique(var1=var, var2=wvar, obs=obs, num=3, graph=graph, labvar=labvar, couleurs=col2, 
symbol=pch2, locmoran=locmoran,obsq=obsq, cex.lab=cex.lab,buble=buble2, cbuble=z2, 
legmap=legmap2, legends=legends2 )   
        
  }

####################################################
# choix d'un autre graphique
####################################################

graphfunc <- function()
{
    if ((length(listvar) != 0) && (length(listnomvar) != 0))
    {
        dev.off(4)
        choix <<- selectgraph(listnomvar,listgraph)
        varChoice1 <<- choix$varChoice1
        varChoice2 <<- choix$varChoice2
        graphChoice <<- choix$graphChoice
            
        if ((graphChoice != "") && (varChoice1 != ""))
        {
            dev.new()
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, couleurs=col[1], symbol=pch[1], labvar=c(varChoice1,varChoice2))
        }
    }
    else
    {
        tkmessageBox(message="List of Variables and list of variables names must have been given",icon="warning",type="ok")
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
    carte(long=long, lat=lat, obs=obs, carte=carte,nocart=nocart, classe=obsq,
    couleurs=col2, symbol=pch2, W=W, method=method, buble=buble, cbuble=z, criteria=criteria,
    nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
    label=label, cex.lab=cex.lab,labmod=names.arg)        
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
    obs<<-vector(mode = "logical", length = length(long));

carte(long=long, lat=lat, obs=obs, carte=carte,nocart=nocart, classe=obsq,
couleurs=col2, symbol=pch2, W=W, method=method, buble=buble, cbuble=z, criteria=criteria,
nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
label=label, cex.lab=cex.lab,labmod=names.arg)     

graphique(var1=var, var2=wvar, obs=obs, num=3, graph=graph, labvar=labvar, couleurs=col2, 
symbol=pch2, locmoran=locmoran,obsq=obsq, cex.lab=cex.lab,buble=buble2, cbuble=z2, 
legmap=legmap2, legends=legends2 )   
    
   if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }

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
# Open a no interactive selection
####################################################

fnointer<-function() 
{
 if (length(criteria) != 0)
 {
 ifelse(!nointer,nointer<<-TRUE,nointer<<-FALSE)
 carte(long=long, lat=lat, obs=obs, carte=carte,nocart=nocart, classe=obsq,
 couleurs=col2, symbol=pch2, W=W, method=method, buble=buble, cbuble=z, criteria=criteria,
 nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
 label=label, cex.lab=cex.lab,labmod=names.arg)       
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
  
carte(long=long, lat=lat, obs=obs, carte=carte,nocart=nocart, classe=obsq,
couleurs=col2, symbol=pch2, W=W, method=method, buble=buble, cbuble=z, criteria=criteria,
nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
label=label, cex.lab=cex.lab,labmod=names.arg)    
 
}

####################################################
# Bubble  of Lisa
####################################################

lisa<-function()
{
  ilocal <- (var/sd(var)) * (wvar/sd(var))          
  res3<-choix.bubble(buble2,abs(ilocal),"ilocal",legends2)
  
  buble2 <<- res3$buble
  legends2 <<- res3$legends
  z2 <<- res3$z
  legmap2 <<- res3$legmap
  
graphique(var1=var, var2=wvar, obs=obs, num=3, graph=graph, labvar=labvar, couleurs=col2, 
symbol=pch2, locmoran=locmoran,obsq=obsq, cex.lab=cex.lab,buble=buble2, cbuble=z2, 
legmap=legmap2, legends=legends2 )   
 
}

####################################################
# Permutation
####################################################
permutation<-function()
{
  tt1<-tktoplevel()
  Name <- tclVar("n")
  entry.Name <-tkentry(tt1,width="5",textvariable=Name)
  tkgrid(tklabel(tt1,text="Number of simulations"),entry.Name)

  OnOK <- function()
  { 
	 value1 <- tclvalue(Name)
	 tkdestroy(tt1)
       if(is.na(as.integer(value1)))
      {
        tkmessageBox(message="Sorry, but you have to choose decimal values",icon="warning",type="ok");
      }
      else
      {
       n=as.integer(value1)
       perm<-NULL
      for (i in 1:n)
       {
       sam<-sample(var,length(var))
       sam <- sam - mean(sam);

       epe <- sam %*% sam
       mi <- (sam %*% W %*% sam)/epe
       morani <- round(mi,4);
       perm <- c(perm,morani);
      }
     msg <- paste("The p-value of the permutation test is :",1-length(which(perm<rep(MORAN,n)))/n)
     tkmessageBox(message=msg,icon="info",type="ok") 
    }   
}


  OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt1)
}

carte(long=long, lat=lat, obs=obs, carte=carte,nocart=nocart, classe=classe,
couleurs=col2, symbol=pch2, W=W, method=method, buble=buble, cbuble=z, criteria=criteria,
nointer=nointer, legmap=legmap, legends=legends,axis=axes,lablong=lablong, lablat=lablat,
label=label, cex.lab=cex.lab)     

graphique(var1=var, var2=wvar, obs=obs, num=3, graph="Moran", labvar=labvar, couleurs=col2, symbol=pch2, 
locmoran=locmoran,obsq=obsq,buble=buble2, cbuble=z2, legmap=legmap2, legends=legends2 )
        

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

point.but <- tkbutton(tt, text="  Points  ", command=pt1func);
poly.but <- tkbutton(tt, text=" Polygon ", command=poly1func);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))


labelText1 <- tclVar("Work on the moranplot")
label1 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)

point1.but <- tkbutton(tt, text="  Points  ", command=pt2func);
poly1.but <- tkbutton(tt, text=" Polygon ", command=poly2func);
tkgrid(point1.but, poly1.but)
tkgrid(tklabel(tt,text="    "))


labelText1 <- tclVar("Choose a quadrant")
label1 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)

point2.but <- tkbutton(tt, text="4th Quadrant", command=quad2func);
poly2.but <- tkbutton(tt, text="1st Quadrant", command=quad1func);
tkgrid(point2.but, poly2.but)
point3.but <- tkbutton(tt, text="3rd Quadrant", command=quad3func);
poly3.but <- tkbutton(tt, text="2nd Quadrant", command=quad4func);
tkgrid(point3.but, poly3.but)

tkgrid(tklabel(tt,text="    "))



label1 <- tclVar("To stop selection, leave the cursor on the active graph, click on the right button of the mouse and stop")
label11 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(label1))
tkconfigure(label11, textvariable=label1)
tkgrid(label11,columnspan=2)
tkgrid(tklabel(tt,text="    "))


msg <- paste("Moran index ",ifelse(is.norm,"(W normalized)","(W not normalized)"),": ", MORAN, " - ","p-value (Gaussian Test) : ", ifelse(round(1-prob.I,4)<0.0001,"<0.0001",round(1-prob.I,4)))
tkgrid(tklabel(tt,text=msg),columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText10 <- tclVar("Permutation Test")
label10 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText10))
tkconfigure(label10, textvariable=labelText10)
tkgrid(label10,columnspan=2)

noint10.but <- tkbutton(tt, text="     OK     ", command=permutation);
tkgrid(noint10.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText7 <- tclVar("Preselected sites  /  Draw Spatial contours ")
label7 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText7))
tkconfigure(label7, textvariable=labelText7)
tkgrid(label7,columnspan=2)

noint1.but <- tkbutton(tt, text="  On/Off  ", command=fnointer);
nocou1.but <- tkbutton(tt, text="  On/Off  ", command=cartfunc);
tkgrid(noint1.but , nocou1.but)
tkgrid(tklabel(tt,text="    "))



labelText9 <- tclVar("Print different colors by quadrant / Bubbles")
label9 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText9))
tkconfigure(label9, textvariable=labelText9)
tkgrid(label9,columnspan=2)

coul.but <- tkbutton(tt, text="  On/Off  ", command=colfunc);
bubble.but <- tkbutton(tt, text="  On/Off  ", command=fbubble);
tkgrid(coul.but,bubble.but)
tkgrid(tklabel(tt,text="    "))

labelText3 <- tclVar("Restore graph")
label3 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText3))
tkconfigure(label3, textvariable=labelText3)
tkgrid(label3,columnspan=2)

nettoy.but <- tkbutton(tt, text="     OK     " , command=SGfunc);
tkgrid(nettoy.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText4 <- tclVar("Bubbles of LISA / Additional graph")
label4 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText4))
tkconfigure(label4, textvariable=labelText4)
tkgrid(label4,columnspan=2)

lisa.but <- tkbutton(tt, text="  On/Off  ", command=lisa);
autre.but <- tkbutton(tt, text="     OK     " , command=graphfunc);
tkgrid(lisa.but,autre.but)
tkgrid(tklabel(tt,text="    "))


labelText5 <- tclVar(" Exit ")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)

quit.but <- tkbutton(tt, text="     OK     ", command=quitfunc);
tkgrid(quit.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

tkwait.variable(fin)
}
####################################################


return(list(obs=obs,MORAN=MORAN))
  }

