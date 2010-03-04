`clustermap` <-
function(long, lat, dataset, clustnum, method="kmeans", type=NULL, centers=NULL, scale=FALSE,
listvar=NULL, listnomvar=NULL, carte=NULL, criteria=NULL, xlab="Cluster", ylab="Count", label="",
cex.lab=1, pch=16, col="blue", names.arg="", axes=FALSE, lablong="", lablat="")
{

  # initialisation
  nointer<-FALSE
  nocart<-FALSE
  buble<-FALSE
  z<-NULL
  legmap<-NULL
  legends<-list(FALSE,FALSE,"","")
  labvar=c(xlab,ylab)
  fin <- tclVar(FALSE);
  graphChoice <- ""
  varChoice1 <- ""
  varChoice2 <- ""
  choix <- ""
  listgraph <- c("Histogram","Barplot","Scatterplot")
  if(names.arg[1]=="") names.arg <- 1:clustnum
  obs<-vector(mode = "logical", length = length(long))
  graphics.off()

  # Méthodes de classification
  if(length(type)==0)
  {ifelse(method=="kmeans",type<-"Hartigan-Wong", type <-"ward")}

  # Réduction de la matrice des données
  if(scale && class(dataset)!="dist") dataset<-scale(dataset)

  # Etude des différentes possibilités
  if(class(dataset)!="dist")
  {if(method=="hclust") dataset<-dist(dataset)}
  else
  {if(method=="kmeans") dataset<-as.matrix(dataset)}

  # classification
  if(method=="kmeans")
  {
    num.graph <- 4 
    ifelse(length(centers)==0,res <- kmeans(dataset,clustnum,algorithm=type),
    res <- kmeans(dataset,centers,algorithm=type))
    vectclass <- res$cluster
  }
  else
  {
    num.graph <- 5
    res <- hclust(dataset,method=type)
    vectclass <- as.vector(cutree(res,k=clustnum))
  }


  # Transformation de data.frame en matrix
  if((length(listvar)>0) && (dim(as.matrix(listvar))[2]==1)) listvar<-as.matrix(listvar)

  # Ouverture des fenêtres graphiques
  dev.new()
  dev.new()

  if(method=="hclust") dev.new()


####################################################
# sélection d'un point
####################################################

pointfunc<-function() 
{
    quit <- FALSE;

    while(!quit)
    {
        #sélection des points
        dev.set(2)
        loc<-locator(1)
        if(is.null(loc)) 
        {
            quit<-TRUE;
            next;
        }   
        obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point")         

        # graphiques

        carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
        lablong=lablong, lablat=lablat, label=label, symbol=pch,carte=carte,nocart=nocart,
        method="Cluster",classe=vectclass,couleurs=col,legmap=legmap,legends=legends,labmod=names.arg,
        cex.lab=cex.lab,axis=axes)
        
        graphique(var1=vectclass, obs=obs, num=3,graph="Barplot", labvar=labvar, symbol=pch,labmod=names.arg,couleurs=col);
        
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
         graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
         obs=obs, num=num.graph, graph=graphChoice, couleurs=col[1], labvar=c(varChoice1,varChoice2))       
        }


    }
  }

####################################################
# sélection d'un polygone
####################################################

polyfunc<-function() 
{
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

    #graphiques
   carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
   lablong=lablong, lablat=lablat, label=label, symbol=pch,couleurs=col,carte=carte,nocart=nocart,
   method="Cluster",classe=vectclass,legmap=legmap,legends=legends,labmod=names.arg,axis=axes,cex.lab=cex.lab)
   
   graphique(var1=vectclass, obs=obs, num=3,graph="Barplot", labvar=labvar, symbol=pch,labmod=names.arg,
   couleurs=col)
   
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
         graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
         obs=obs, num=num.graph, graph=graphChoice, couleurs=col[1], labvar=c(varChoice1,varChoice2))       
        }

}

  }

####################################################
# sélection d'une barre sur le diagramme
####################################################

barfunc<-function()
{
    SGfunc()
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
        obs<<-selectstat(var1=as.vector(vectclass),obs=obs,Xpoly=loc[1], Ypoly=loc[2],method="Barplot");   

        # graphiques
        graphique(var1=vectclass, obs=obs, num=3,graph="Barplot", labvar=labvar, symbol=pch,labmod=names.arg,
        couleurs=col)
        
        carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
        lablong=lablong, lablat=lablat, label=label,cex.lab=cex.lab, symbol=pch,carte=carte,nocart=nocart,couleurs=col,
        method="Cluster",classe=vectclass,legmap=legmap,legends=legends,labmod=names.arg,axis=axes)

        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
         graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
         obs=obs, num=num.graph, graph=graphChoice, couleurs=col[1], labvar=c(varChoice1,varChoice2))       
        }      
    }
  }

####################################################
# choix d'un autre graphique
####################################################

graphfunc <- function()
{
    if ((length(listvar) != 0) && (length(listnomvar) != 0))
    {
       ifelse(method=="kmeans",dev.off(4),dev.off(5))
        
        choix <<- selectgraph(listnomvar,listgraph)
        varChoice1 <<- choix$varChoice1
        varChoice2 <<- choix$varChoice2
        graphChoice <<- choix$graphChoice
            
        if ((graphChoice != "") && (varChoice1 != ""))
        {
         dev.new()
         graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
         obs=obs, num=num.graph, graph=graphChoice, couleurs=col[1], labvar=c(varChoice1,varChoice2))       
        }
        else
        {
        tkmessageBox(message="You must choose a variable",icon="warning",type="ok")
        }
        
    }
    else
    {
        tkmessageBox(message="You must give listvar and listnomvar in input",icon="warning",type="ok")
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
   carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
   lablong=lablong, lablat=lablat, label=label, cex.lab=cex.lab, symbol=pch,couleurs=col,carte=carte,
   nocart=nocart, method="Cluster",classe=vectclass,legmap=legmap,legends=legends,labmod=names.arg,axis=axes)    
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
   
    carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
    lablong=lablong, lablat=lablat, label=label,cex.lab=cex.lab, symbol=pch,carte=carte,nocart=nocart,
    couleurs=col,method="Cluster",classe=vectclass,legmap=legmap,legends=legends,labmod=names.arg,axis=axes)
    
    graphique(var1=vectclass, obs=obs, num=3, graph="Barplot", labvar=labvar, symbol=pch,labmod=names.arg,couleurs=col)
    
     if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
         graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
         obs=obs, num=num.graph, graph=graphChoice, couleurs=col[1], labvar=c(varChoice1,varChoice2))       
        }      
  }

####################################################
# quitter l'application
####################################################

quitfunc<-function() 
{
    tclvalue(fin)<<-TRUE
  #  graphics.off();
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
   carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
   lablong=lablong, lablat=lablat, label=label,cex.lab=cex.lab, symbol=pch,carte=carte,nocart=nocart,
   couleurs=col,method="Cluster",classe=vectclass,legmap=legmap,legends=legends,labmod=names.arg,axis=axes)   
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
  
  carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
  lablong=lablong, lablat=lablat, label=label,cex.lab=cex.lab, symbol=pch,carte=carte,nocart=nocart,
  couleurs=col,method="Cluster",classe=vectclass,legmap=legmap,legends=legends,labmod=names.arg,axis=axes) 
 
}

####################################################
# Représentation graphique
####################################################

carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
lablong=lablong, lablat=lablat, label=label,cex.lab=cex.lab, symbol=pch,carte=carte,nocart=nocart,
couleurs=col,method="Cluster",classe=vectclass,legmap=legmap,legends=legends,labmod=names.arg,axis=axes)
    
graphique(var1=as.vector(vectclass), obs=obs, num=3, graph="Barplot", labvar=labvar, symbol=pch,
labmod=names.arg,couleurs=col)

if(method=="hclust") 
{dev.set(4)
plot(res)
}


####################################################
# création de la boite de dialogue to create legens
####################################################

if(interactive())
{
 OnOK <- function()
 { 
  tkdestroy(tt1)	
  msg <- paste("Click on the map to indicate the location of the upper left corner of the legend box")
	tkmessageBox(message=msg)

  dev.set(2)
  loc <- locator(1)
  legends<<-list(legends[[1]],TRUE,legends[[3]],loc)

  carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
  lablong=lablong, lablat=lablat, label=label,cex.lab=cex.lab, symbol=pch,carte=carte,nocart=nocart,
  couleurs=col,method="Cluster",classe=vectclass,legmap=legmap,legends=legends,labmod=names.arg,axis=axes)

  graphique(var1=as.vector(vectclass), obs=obs, num=3, graph="Barplot", labvar=labvar, symbol=pch,
  labmod=names.arg,couleurs=col)
 }

OnOK2 <- function()
 { 
  legends<<-list(legends[[1]],FALSE,legends[[3]],"")
  tkdestroy(tt1)	

  carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
  lablong=lablong, lablat=lablat, label=label,cex.lab=cex.lab, symbol=pch,carte=carte,nocart=nocart,
  couleurs=col,method="Cluster",classe=vectclass,legmap=legmap,legends=legends,labmod=names.arg,axis=axes)
    
  graphique(var1=as.vector(vectclass), obs=obs, num=3, graph="Barplot", labvar=labvar, 
  symbol=pch,labmod=names.arg,couleurs=col)
  }


if(length(col)==length(levels(as.factor(vectclass)))||length(pch)==length(levels(as.factor(vectclass))))
 {
  tt1<-tktoplevel()

  labelText12 <- tclVar("Do you want a legend for factors")
  label12 <- tklabel(tt1,justify = "center", wraplength = "3i", text=tclvalue(labelText12))
  tkconfigure(label12, textvariable=labelText12)
  tkgrid(label12,columnspan=2)

  point.but <- tkbutton(tt1, text="  Yes  ", command=OnOK)
  poly.but <- tkbutton(tt1, text=" No ", command=OnOK2)
  tkgrid(point.but, poly.but)
  tkgrid(tklabel(tt1,text="    "))

  tkfocus(tt1)
 }
}
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

point.but <- tkbutton(tt, text="  Points  ", command=pointfunc);
poly.but <- tkbutton(tt, text=" Polygon ", command=polyfunc);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))


labelText2 <- tclVar("Work on the bar plot")
label2 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText2))
tkconfigure(label2, textvariable=labelText2)
tkgrid(label2,columnspan=2)

barre.but <- tkbutton(tt, text="   Bar   ", command=barfunc);
tkgrid(barre.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))



label1 <- tclVar("To stop selection, leave the cursor on the active graph, click on the right button of the mouse and stop")
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


labelText9 <- tclVar("Bubbles")
label9 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText9))
tkconfigure(label9, textvariable=labelText9)
tkgrid(label9,columnspan=2)

bubble.but <- tkbutton(tt, text="  On/Off  ", command=fbubble);
tkgrid(bubble.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

labelText3 <- tclVar("Restore graph")
label3 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText3))
tkconfigure(label3, textvariable=labelText3)
tkgrid(label3,columnspan=2)

nettoy.but <- tkbutton(tt, text="     OK     " , command=SGfunc);
tkgrid(nettoy.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText4 <- tclVar("Additional graph")
label4 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText4))
tkconfigure(label4, textvariable=labelText4)
tkgrid(label4,columnspan=2)

autre.but <- tkbutton(tt, text="     OK     " , command=graphfunc);
tkgrid(autre.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText5 <- tclVar("Exit")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)

quit.but <- tkbutton(tt, text="     OK     ", command=quitfunc);
tkgrid(quit.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

tkwait.variable(fin);
}


####################################################
# Fin
####################################################


return(list(obs=obs,vectclass=vectclass));
  }

