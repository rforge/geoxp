`dbledensitymap` <-
function (long, lat, var1,var2,kernel='triweight',listvar=NULL, listnomvar=NULL,carte=NULL,
criteria=NULL, label="", cex.lab=1, pch=16, col=c("grey","blue"), xlab=c("",""),
ylab=c("density","density"),axes=FALSE, lablong="", lablat="")
{
  # initialisation
  obs<-vector(mode = "logical", length = length(long))
  graph1<-"Densityplot1" 
  graph2<-"Densityplot1"
  nointer<-FALSE
  nocart<-FALSE
  z<-NULL
  legmap<-NULL
  interv<-NULL
  buble<-FALSE
  legends<-list(FALSE,FALSE,"","")
  graphics.off()

  alpha11<-20 
  alpha21<-20 
  
  graphChoice <- ""
  varChoice1 <- ""
  varChoice2 <- ""
  choix<-""

  listgraph <- c("Histogram","Barplot","Scatterplot")

  # options for adding a graphic with colors
  polyX2 <- NULL
  method <- ""
  col2 <- "blue"
  col3 <- col[1]
  pch2<-pch[1]
  labmod<-""
  labvar1<-c(xlab[1],ylab[1])
  labvar2<-c(xlab[2],ylab[2])


# Transformation de data.frame en matrix
if((length(listvar)>0)&&(dim(as.matrix(listvar))[2]==1)) listvar<-as.matrix(listvar)

# Ouverture des fenêtres graphiques
dev.new()
dev.new()
dev.new()
fin <- tclVar(FALSE)

####################################################
# sélection d'un point
####################################################

pointfunc<-function() 
{
   if((graph1=="Densityplot2")||(graph2=="Densityplot2"))
    { 
     #SGfunc()
      graph1<<-"Densityplot1" 
      graph2<<-"Densityplot1" 
      
      graphique(var1=var1, obs=obs, alpha1=alpha11,  num=3, graph=graph1, labvar=labvar1,
      couleurs=col[1],symbol=pch,kernel=kernel)
     
      graphique(var1=var2, obs=obs, alpha1=alpha21,  num=4, graph=graph2, labvar=labvar2,
      couleurs=col[2],symbol=pch,kernel=kernel);    
      
    } 
    
    quit <- FALSE;
    loc <- NULL;

    while(!quit)
    {
        dev.set(2);
        loc<-locator(1);

    graph1<<-"Densityplot1"  
    graph2<<-"Densityplot1" 

        if(is.null(loc)) 
        {
          quit<-TRUE
          next
        }

        obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point");         
        
        # graphiques
      if (length(var1[obs]) >= 1)
        {
         graphique(var1=var1, obs=obs, alpha1=alpha11,  num=3, graph=graph1, labvar=labvar1,
         couleurs=col[1],symbol=pch,kernel=kernel)
     
         graphique(var1=var2, obs=obs, alpha1=alpha21,  num=4, graph=graph2, labvar=labvar2,
         couleurs=col[2],symbol=pch,kernel=kernel)
        }

        carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
        label=label,cex.lab=cex.lab, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
        lablong=lablong, lablat=lablat,symbol=pch2, couleurs=col2,method=method,
        classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
        

         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
          {graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=5, graph=graphChoice, symbol=pch, labvar=c(varChoice1,varChoice2),couleurs=col3)        
          }    
    }
  }

####################################################
# sélection d'un polygone
####################################################

polyfunc<-function() 
{

   if((graph1=="Densityplot2")||(graph2=="Densityplot2"))
    { 
     #SGfunc()
      graph1<<-"Densityplot1" 
      graph2<<-"Densityplot1" 
      
      graphique(var1=var1, obs=obs, alpha1=alpha11,  num=3, graph=graph1, labvar=labvar1,
      couleurs=col[1],symbol=pch,kernel=kernel)
     
      graphique(var1=var2, obs=obs, alpha1=alpha21,  num=4, graph=graph2, labvar=labvar2,
      couleurs=col[2],symbol=pch,kernel=kernel)    
      
    } 
    
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


    graphique(var1=var1, obs=obs, alpha1=alpha11,  num=3, graph=graph1, labvar=labvar1,
    couleurs=col[1],symbol=pch,kernel=kernel)
     
    graphique(var1=var2, obs=obs, alpha1=alpha21,  num=4, graph=graph2, labvar=labvar2,
    couleurs=col[2],symbol=pch,kernel=kernel)
        

    carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
    label=label,cex.lab=cex.lab, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
    lablong=lablong, lablat=lablat,symbol=pch2, couleurs=col2,method=method,
    classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
        

    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
     {graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
      obs=obs, num=5, graph=graphChoice, symbol=pch, labvar=c(varChoice1,varChoice2),couleurs=col3)        
     }    
}  

 }

####################################################
# sélection d'un intervalle sous la courbe de densité
####################################################

inter1func<-function()
{
  # SGfunc();

#    quit <- FALSE;

  if(graph1=="Densityplot1")
   { 
    SGfunc()
    graph1<<-"Densityplot2"  
    graph2<<-"Densityplot1" 
   }
   
    polyX <- NULL
    n.inter<-length(polyX2)
    
    while (length(polyX)<2)
    {
      dev.set(3)
      loc<-locator(1)
      polyX <- c(polyX, loc[1])
    }
    
    polyX2[[n.inter+1]]<<-polyX

    obs<<-selectstat(var1=var1,obs=obs,Xpoly=polyX[1], Ypoly=polyX[2],method="Densityplot") 
        
    # graphiques

         graphique(var1=var1, obs=obs, alpha1=alpha11,   num=3, graph=graph1, Xpoly=polyX2,
         labvar=labvar1, couleurs=col[1], kernel=kernel)
         
         graphique(var1=var2, obs=obs, alpha1=alpha21,  num=4, graph=graph2,
         labvar=labvar2, couleurs=col[2], kernel=kernel)
  
        carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
        label=label,cex.lab=cex.lab, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
        lablong=lablong, lablat=lablat,symbol=pch2, couleurs=col2,method=method,
        classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
        

    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
      {graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
       obs=obs, num=5, graph=graphChoice, symbol=pch, labvar=c(varChoice1,varChoice2),couleurs=col3)        
      }  
      
  }

####################################################
# sélection d'un intervalle sous la courbe de densité
####################################################

inter2func<-function()
{
  if(graph2=="Densityplot1")
   { 
    SGfunc()
    graph1<<-"Densityplot1"  
    graph2<<-"Densityplot2" 
   }

    polyX <- NULL
    n.inter<-length(polyX2)
    
    while (length(polyX)<2)
    {
      dev.set(4)
      loc<-locator(1)
      polyX <- c(polyX, loc[1])
    }
    
    polyX2[[n.inter+1]]<<-polyX
    
    obs<<-selectstat(var1=var2,obs=obs,Xpoly=polyX[1], Ypoly=polyX[2],method="Densityplot") 
        
    # graphiques
     graphique(var1=var2, obs=obs, alpha1=alpha21,  num=4, graph=graph2, Xpoly=polyX2,
     labvar=labvar2, couleurs=col[2], kernel=kernel)
     
     graphique(var1=var1, obs=obs, alpha1=alpha11,  num=3, graph=graph1,
     labvar=labvar1, couleurs=col[1],kernel=kernel)
  
     carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
     label=label,cex.lab=cex.lab, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
     lablong=lablong, lablat=lablat,symbol=pch2, couleurs=col2,method=method,
     classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
        
    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
      {graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
       obs=obs, num=5, graph=graphChoice, symbol=pch, labvar=c(varChoice1,varChoice2),couleurs=col3)        
      }   

 }


####################################################
# Choisir une valeur sur le 1er graphique
####################################################

choixvalue1 <- function() 
{
  if(graph1=="Densityplot1")
   { 
    SGfunc()
    graph1<<-"Densityplot2"  
    graph2<<-"Densityplot1" 
   }
   
  tt1<-tktoplevel()
  Name <- tclVar("1st value")
  Name2 <- tclVar("2nd value")
  entry.Name <-tkentry(tt1,width="8",textvariable=Name)
  entry.Name2 <-tkentry(tt1,width="8",textvariable=Name2)
  tkgrid(tklabel(tt1,text="Please enter values"),entry.Name,entry.Name2)

  OnOK <- function()
  { 
	 value1 <- tclvalue(Name)
	 value2 <- tclvalue(Name2)
	 n.inter<-length(polyX2)
	 tkdestroy(tt1)
	 
   if(is.na(as.numeric(value1))||is.na(as.numeric(value2)))
    {
        tkmessageBox(message="Sorry, but you have to choose decimal values",icon="warning",type="ok");
    }
      else
    {
    
    polyX2[[n.inter+1]]<<- c(as.numeric(value1),as.numeric(value2))
    
    obs<<-selectstat(var1=var1,obs=obs,Xpoly=as.numeric(value1), Ypoly=as.numeric(value2),method="Densityplot")
        
    # graphiques
    graphique(var1=var1, obs=obs, alpha1=alpha11,   num=3, graph=graph1, Xpoly=polyX2,
    labvar=labvar1, couleurs=col[1], kernel=kernel)
         
    graphique(var1=var2, obs=obs, alpha1=alpha21,  num=4, graph=graph2,
    labvar=labvar2, couleurs=col[2], kernel=kernel)
  
    carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
    label=label,cex.lab=cex.lab, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
    lablong=lablong, lablat=lablat,symbol=pch2, couleurs=col2,method=method,
    classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)     

    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
      {graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
       obs=obs, num=5, graph=graphChoice, symbol=pch, labvar=c(varChoice1,varChoice2),couleurs=col3)        
      }  
    }   
  }

  OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
  #tkbind(entry.Name, "<Return>",OnOK)
  tkgrid(OK.but)
  tkfocus(tt1)

}

####################################################
# Choisir une valeur sur le deuxième graphique
####################################################

choixvalue2 <- function() 
{
  if(graph2=="Densityplot1")
   { 
    SGfunc()
    graph1<<-"Densityplot1"  
    graph2<<-"Densityplot2" 
   }
     
  tt1<-tktoplevel()
  Name <- tclVar("1st value")
  Name2 <- tclVar("2nd value")
  entry.Name <-tkentry(tt1,width="8",textvariable=Name)
  entry.Name2 <-tkentry(tt1,width="8",textvariable=Name2)
  tkgrid(tklabel(tt1,text="Please enter values"),entry.Name,entry.Name2)


  OnOK <- function()
  { 
	 value1 <- tclvalue(Name)
	 value2 <- tclvalue(Name2)
	 n.inter<-length(polyX2)
	 tkdestroy(tt1)
	 
   if(is.na(as.numeric(value1))||is.na(as.numeric(value2)))
    {
        tkmessageBox(message="Sorry, but you have to choose decimal values",icon="warning",type="ok")
    }
   else
    {
    
    polyX2[[n.inter+1]]<<- c(as.numeric(value1),as.numeric(value2))
    
    obs<<-selectstat(var1=var2,obs=obs,Xpoly=as.numeric(value1), Ypoly=as.numeric(value2),method="Densityplot") 
        
    # graphiques
     graphique(var1=var2, obs=obs, alpha1=alpha21,  num=4, graph=graph2, Xpoly=polyX2,
     labvar=labvar2, couleurs=col[2], kernel=kernel)
     
     graphique(var1=var1, obs=obs, alpha1=alpha11,  num=3, graph=graph1,
     labvar=labvar1, couleurs=col[1],kernel=kernel)
  
     carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
     label=label,cex.lab=cex.lab, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
     lablong=lablong, lablat=lablat,symbol=pch2, couleurs=col2,method=method,
     classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
        
    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
      {graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
       obs=obs, num=5, graph=graphChoice, symbol=pch, labvar=c(varChoice1,varChoice2),couleurs=col3)        
      }   
    }   
  }

OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
#tkbind(entry.Name, "<Return>",OnOK)
tkgrid(OK.but)
tkfocus(tt1)

}
####################################################
# modification du alpha pour la courbe de densité
####################################################
  

refresh1.code<-function(...)
{
 alpha11<<-slider1(no=1) 
 if(graph1=="Densityplot1") 
 {graphique(var1=var1, obs=obs, alpha1=alpha11, num=3, graph=graph1, labvar=labvar1, couleurs=col[1],kernel=kernel)}
 else
 {graphique(var1=var1, obs=obs,alpha1=alpha11,  num=3, graph="Densityplot2", Xpoly=polyX2,
  labvar=labvar1, couleurs=col[1],kernel=kernel)}
 }


####################################################
# modification du alpha pour la courbe de densité
####################################################
  
refresh2.code<-function(...)
{
 alpha21<<-slider2(no=1) 
 if(graph2=="Densityplot1") 
  {graphique(var1=var2, obs=obs, alpha1=alpha21, num=4, graph=graph2, labvar=labvar2,
   couleurs=col[2],kernel=kernel)}
 else
  {graphique(var1=var2, obs=obs,alpha1=alpha21, num=4, graph="Densityplot2", Xpoly=polyX2,
   labvar=labvar2, couleurs=col[2],kernel=kernel)}
 }


####################################################
# rafraichissement des graphiques
####################################################

SGfunc<-function() 
{
    obs<<-vector(mode = "logical", length = length(long));   
    polyX2 <<- NULL 
     
    graphique(var1=var2, obs=obs, alpha1=alpha21,   num=4, graph="Densityplot1", 
    labvar=labvar2, couleurs=col[2], kernel=kernel)
    
    graphique(var1=var1, obs=obs, alpha1=alpha11,   num=3, graph="Densityplot1", labvar=labvar1,
    couleurs=col[1], kernel=kernel)

    carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
    label=label,cex.lab=cex.lab, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
    lablong=lablong, lablat=lablat,symbol=pch2, couleurs=col2,method=method,
    classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
        
    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
      {graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
       obs=obs, num=5, graph=graphChoice, symbol=pch, labvar=c(varChoice1,varChoice2),couleurs=col3)        
      }   
}


####################################################
# quitter l'application
####################################################

quitfunc<-function() 
{
  tclvalue(fin)<-TRUE
  graphics.off()
  tkdestroy(tt)
}

####################################################
# contour des unités spatiales
####################################################
cartfunc <- function()
{
 if (length(carte) != 0)
   {
    ifelse(!nocart,nocart<<-TRUE,nocart<<-FALSE)
    carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
    label=label,cex.lab=cex.lab, symbol=pch2, couleurs=col2, carte=carte,nocart=nocart,legmap=legmap,legends=legends,
    axis=axes,lablong=lablong, lablat=lablat,method=method,classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
    }
   else
   {
    tkmessageBox(message="Spatial contours have not been given",icon="warning",type="ok")    
   }
}



####################################################
# choix d'un autre graphique
####################################################

graphfunc <- function()
{ 
   if ((length(listvar) != 0) && (length(listnomvar) != 0))
    {
        dev.off(5)
        choix <<- selectgraph(listnomvar,listgraph)
        varChoice1 <<- choix$varChoice1
        varChoice2 <<- choix$varChoice2
        graphChoice <<- choix$graphChoice
           
        if ((graphChoice != "") && (varChoice1 != ""))
        {
          if (((graphChoice == "Histogram")&&(!is.numeric(listvar[,which(listnomvar == varChoice1)])))||((graphChoice == "Scatterplot")&&((!is.numeric(listvar[,which(listnomvar == varChoice1)]))||(!is.numeric(listvar[,which(listnomvar == varChoice2)]))))) 
           {
            tkmessageBox(message="Variables choosed are not in a good format",icon="warning",type="ok");
           }
          else
           {
            res1<-choix.couleur(graphChoice,listvar,listnomvar,varChoice1,legends,col,pch)
            
            method <<- res1$method
            col2 <<- res1$col2
            col3 <<- res1$col3
            pch2 <<- res1$pch2
            legends <<- res1$legends
            labmod <<- res1$labmod
                     
            dev.new()
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=5, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2));    
            
            carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
            label=label,cex.lab=cex.lab, symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
            lablong=lablong, lablat=lablat,method=method,classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
            }
       }   
   }
   else
   {
    tkmessageBox(message="Variables (listvar) and their names (listnomvar) must have been given",icon="warning",type="ok");
   }  
}

####################################################
# quitter l'application
####################################################

quitfunc<-function() 
{
  tclvalue(fin)<-TRUE
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
   carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
   label=label,cex.lab=cex.lab, symbol=pch2, couleurs=col2, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
   lablong=lablong, lablat=lablat,method=method,classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod) 
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
  
  carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
  label=label,cex.lab=cex.lab, symbol=pch2, couleurs=col2, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
  lablong=lablong, lablat=lablat,method=method,classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)  

}

####################################################
# Représentation graphique
####################################################
        
graphique(var1=var2, obs=obs, alpha1=alpha21, num=4, graph="Densityplot1", labvar=labvar2,
couleurs=col[2],kernel=kernel)

graphique(var1=var1, obs=obs, alpha1=alpha11, num=3, graph="Densityplot1", labvar=labvar1,
couleurs=col[1],kernel=kernel)

carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
label=label,cex.lab=cex.lab, symbol=pch2, couleurs=col2, carte=carte,nocart=nocart,legmap=legmap,legends=legends,
axis=axes, lablong=lablong, lablat=lablat,method=method,classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)


####################################################
# création de la boite de dialogue
####################################################

if(interactive())
{
tt <- tktoplevel()

labelText1 <- tclVar("Work on the map")
label1 <- tklabel(tt, text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)
point.but <- tkbutton(tt, text="    Points    ", command=pointfunc);
poly.but <- tkbutton(tt, text="   Polygon   ", command=polyfunc);
tkgrid(point.but, poly.but, tklabel(tt,text="         "))
tkgrid(tklabel(tt,text="    "))

labelText2 <- tclVar("Select an interval by selecting on graph")
label2 <- tklabel(tt, text=tclvalue(labelText2))
tkconfigure(label2, textvariable=labelText2)
tkgrid(label2,columnspan=2)
intervalle1.but <- tkbutton(tt, text="  1st graph  ", command=inter1func);
intervalle2.but <- tkbutton(tt, text="  2nd graph  ", command=inter2func);
tkgrid(intervalle1.but, intervalle2.but)
tkgrid(tklabel(tt,text="    "))

labelText2 <- tclVar("Select an interval by specifying bounds")
label2 <- tklabel(tt, text=tclvalue(labelText2))
tkconfigure(label2, textvariable=labelText2)
tkgrid(label2,columnspan=2)
intervalle11.but <- tkbutton(tt, text="1st variable", command=choixvalue1);
intervalle22.but <- tkbutton(tt, text="2nd variable", command=choixvalue2);
tkgrid(intervalle11.but,intervalle22.but, tklabel(tt,text="         "))
tkgrid(tklabel(tt,text="    "))



label1 <- tclVar("To stop selection, leave the cursor on the active graph, click on the right button of the mouse and stop")
label11 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(label1))
tkconfigure(label11, textvariable=label1)
tkgrid(label11,columnspan=2)
tkgrid(tklabel(tt,text="    "))


slider1(tt,refresh1.code,c("Alpha (1st graph)"),c(3),c(100),c(1),c(alpha11))

slider2(tt,refresh2.code,c("Alpha (2nd graph)"),c(3),c(100),c(1),c(alpha21))


labelText7 <- tclVar("Preselected sites / Draw Spatial Contours")
label7 <- tklabel(tt, text=tclvalue(labelText7))
tkconfigure(label7, textvariable=labelText7)
tkgrid(label7,columnspan=2)
noint1.but <- tkbutton(tt, text="  On/Off  ", command=fnointer);
nocou1.but <- tkbutton(tt, text="  On/Off  ", command=cartfunc);
tkgrid(noint1.but, nocou1.but)
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


labelText5 <- tclVar("  Exit  ")
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

