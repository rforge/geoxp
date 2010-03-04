`densitymap` <-
function (long, lat, var, kernel='triweight',listvar=NULL, listnomvar=NULL, carte=NULL,
criteria=NULL,label="",cex.lab=1, pch=16, col="blue", xlab="", ylab="", axes=FALSE, 
lablong="", lablat="")
{
#initialisation
  obs<-vector(mode = "logical", length = length(long))
  graph<-"Densityplot1" 
  nointer<-FALSE
  nocart<-FALSE
  buble<-FALSE
  legends<-list(FALSE,FALSE,"","")
  z<-NULL
  legmap<-NULL
  fin <- tclVar(FALSE)

  value1 <- ""
  value2 <- ""

  graphChoice <- ""
  varChoice1 <- ""
  varChoice2 <- ""
  choix<-""

  listgraph <- c("Histogram","Barplot","Scatterplot")

  alpha1<-20


# options for adding a graphic with colors
  polyX2 <- NULL
  method <- ""
  col2 <- "blue"
  col3 <- col[1]
  pch2 <- pch[1]
  labmod <- ""
  labvar <- c(xlab,ylab)

# transformation data.frame en matrix
if((length(listvar)>0)&&(dim(as.matrix(listvar))[2]==1))listvar<-as.matrix(listvar)

# Ouverture des fenêtres graphiques
graphics.off()
dev.new()
dev.new()


####################################################
# sélection d'un point
####################################################

pointfunc<-function() 
{  if(graph=="Densityplot2")
    { 
     graph<<-"Densityplot1" 
      
     graphique(var1=var, obs=obs, alpha1=alpha1,  num=3, graph=graph, labvar=labvar,
     symbol=pch, couleurs=col,kernel=kernel)
    }    
    quit <- FALSE
    loc <- NULL

    while(!quit)
    {
        dev.set(2)
        loc<-locator(1)

        if(is.null(loc)) 
        {
          quit<-TRUE
          next
        }

        obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point")         
        
        # graphiques

        if (length(var[obs]) >= 2)
        {
            graphique(var1=var, obs=obs, alpha1=alpha1,  num=3, graph=graph, labvar=labvar,
            symbol=pch, couleurs=col,kernel=kernel)   
         }     
        
        carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
        label=label,cex.lab=cex.lab, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
        lablong=lablong, lablat=lablat,symbol=pch2, couleurs=col2,method=method,
        classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
        
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
          {graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, symbol=pch, labvar=c(varChoice1,varChoice2),couleurs=col3)        
          }
    }
  }

####################################################
# sélection d'un polygone
####################################################

polyfunc<-function() 
{  if(graph=="Densityplot2")
    { 
     graph<<-"Densityplot1" 
     
     graphique(var1=var, obs=obs, alpha1=alpha1,  num=3, graph=graph, labvar=labvar,
     symbol=pch, couleurs=col,kernel=kernel)
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

    #graphiques
    
    graphique(var1=var, obs=obs, alpha1=alpha1,  num=3, graph=graph, labvar=labvar,
    symbol=pch, couleurs=col,kernel=kernel)
            
    carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
    label=label,cex.lab=cex.lab, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
    lablong=lablong, lablat=lablat,symbol=pch2, couleurs=col2,method=method,classe=listvar[,
    which(listnomvar == varChoice1)],labmod=labmod)
            
          if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, symbol=pch, couleurs=col3, labvar=c(varChoice1,varChoice2))
           }  
  }
}  

####################################################
# sélection d'une aire sous la courbe de densité
####################################################

interfunc<-function()
{
#   SGfunc();
#    quit <- FALSE;

    if(graph=="Densityplot1")
    { 
     SGfunc()
     graph<<-"Densityplot2" 
    } 
  
    n.inter<-length(polyX2)
    polyX<-NULL
    
    while (length(polyX)<2)
    {
      dev.set(3)
      loc<-locator(1)
      polyX <- c(polyX, loc[1])
    }
    
  polyX2[[n.inter+1]]<<-polyX
    
    
   obs<<-selectstat(var1=var,obs=obs,Xpoly=polyX[1], Ypoly=polyX[2],method="Densityplot") 
        
    # graphiques
    graphique(var1=var, obs=obs, alpha1=alpha1, num=3, graph=graph,
    Xpoly=polyX2, labvar=labvar, symbol=pch, couleurs=col, kernel=kernel)
    
    carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
    label=label,cex.lab=cex.lab, symbol=pch2, couleurs=col2, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
    lablong=lablong, lablat=lablat,method=method,classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
            
       if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
          graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
          obs=obs, num=4, graph=graphChoice, symbol=pch, couleurs=col3, labvar=c(varChoice1,varChoice2))
        }      
}



####################################################
# Choisir une valeur
####################################################

choixvalue <- function() 
{
    if(graph=="Densityplot1")
    { 
     SGfunc()
     graph<<-"Densityplot2" 
    }    
      
  tt1<-tktoplevel()
  Name <- tclVar("1st value")
  Name2 <- tclVar("2nd value")
  entry.Name <-tkentry(tt1,width="8",textvariable=Name)
  entry.Name2 <-tkentry(tt1,width="8",textvariable=Name2)
  tkgrid(tklabel(tt1,text="Please enter values"),entry.Name,entry.Name2)

  OnOK <- function()
   { 
	   value1 <<- tclvalue(Name)
	   value2 <<- tclvalue(Name2)
   	 n.inter<-length(polyX2)
	
	   tkdestroy(tt1)
       if(is.na(as.numeric(value1))||is.na(as.numeric(value2)))
       {
        tkmessageBox(message="Sorry, but you have to choose decimal values",icon="warning",type="ok");
       }
      else
      {   
        polyX2[[n.inter+1]]<<- c(as.numeric(value1),as.numeric(value2))       
        obs<<-selectstat(var1=var,obs=obs,Xpoly=as.numeric(value1), Ypoly=as.numeric(value2),method="Densityplot") 
        
         # graphiques
        graphique(var1=var, obs=obs, alpha1=alpha1,  num=3, graph=graph, Xpoly=polyX2,
        labvar=labvar, symbol=pch, couleurs=col, kernel=kernel)
 
        carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
        label=label,cex.lab=cex.lab, symbol=pch2, couleurs=col2, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
        lablong=lablong, lablat=lablat,method=method,classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
            
          if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, symbol=pch, couleurs=col3, labvar=c(varChoice1,varChoice2))
           }      
     }     
  }



OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
#tkbind(entry.Name, "<Return>",OnOK)
tkgrid(OK.but)
tkfocus(tt1)


}

####################################################
# modification du alpha pour l'estimateur de tous les points
####################################################
  
refresh.code<-function(...)
{
    alpha1<<-slider1(no=1) 
    if(graph=="Densityplot1") 
    {graphique(var1=var, obs=obs, alpha1=alpha1,  num=3, graph=graph, labvar=labvar,
     symbol=pch, couleurs=col, kernel=kernel)}
    else
    {graphique(var1=var, obs=obs, alpha1=alpha1,  num=3, graph="Densityplot2",
     Xpoly=polyX2, labvar=labvar, symbol=pch, couleurs=col, kernel=kernel)
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
    carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
    label=label,cex.lab=cex.lab, symbol=pch2, couleurs=col2, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
    lablong=lablong, lablat=lablat,method=method,classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
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
        dev.off(4);
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
            obs=obs, num=4, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2));    
            
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
# rafraichissement des graphiques
####################################################

SGfunc<-function() 
{
    obs<<-vector(mode = "logical", length = length(long))
    graph<<-"Densityplot1"
    
    polyX2 <<- NULL 
     
    graphique(var1=var, obs=obs, alpha1=alpha1,  num=3, graph="Densityplot1", labvar=labvar, symbol=pch,
    couleurs=col,kernel=kernel)

    carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
    label=label,cex.lab=cex.lab, symbol=pch2, couleurs=col2, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
    lablong=lablong, lablat=lablat,method=method,classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)
  
  if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
   {graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
    obs=obs, num=4, graph=graphChoice, symbol=pch, labvar=c(varChoice1,varChoice2),couleurs=col3)           
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
        
graphique(var1=var, obs=obs, alpha1=alpha1,  num=3, graph=graph, Xpoly=polyX2,
labvar=labvar, symbol=pch, couleurs=col, kernel=kernel)
 
carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,
label=label,cex.lab=cex.lab, symbol=pch2, couleurs=col2, carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes,
lablong=lablong, lablat=lablat,method=method,classe=listvar[,which(listnomvar == varChoice1)],labmod=labmod)

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
point.but <- tkbutton(tt, text="  Points  ", command=pointfunc);
poly.but <- tkbutton(tt, text=" Polygon ", command=polyfunc);
tkgrid(point.but, poly.but, tklabel(tt,text="         "))
tkgrid(tklabel(tt,text="    "))

labelText2 <- tclVar("Select an interval by")
label2 <- tklabel(tt, text=tclvalue(labelText2))
tkconfigure(label2, textvariable=labelText2)
tkgrid(label2,columnspan=2)
intervalle.but <- tkbutton(tt, text="Selecting on Graph", command=interfunc);
intervalle2.but <- tkbutton(tt, text="Specifying bounds", command=choixvalue);
tkgrid(intervalle.but,intervalle2.but, tklabel(tt,text="         "))
tkgrid(tklabel(tt,text="    "))


label1 <- tclVar("To stop selection, leave the cursor on the active graph, click on the right button of the mouse and stop")
label11 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(label1))
tkconfigure(label11, textvariable=label1)
tkgrid(label11,columnspan=2)
tkgrid(tklabel(tt,text="    "))


slider1(tt,refresh.code,c("Alpha"),c(3),c(100),c(1),c(alpha1)  )

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

tkwait.variable(fin)
####################################################
}

return(obs)
  }

