 polyboxplotmap<-
function (long, lat, var1, var2, varwidth=FALSE,listvar = NULL, listnomvar = NULL,
carte = NULL, criteria = NULL, label = "", cex.lab=1, names.arg = "", xlab = "",
ylab= "count", pch = 16, col = "grey", axes = FALSE, lablong = "", lablat = "")
{
# initialisation
  nointer <- FALSE
  nocart <- FALSE
  buble <- FALSE
  z <- NULL
  legmap <- NULL
  legends <- list(FALSE, FALSE, "", "")
  labvar <- c(xlab,ylab)
    
if(names.arg[1]=="") names.arg<-levels(as.factor(var))
    
  var1 = as.matrix(var1)
  var2 = as.matrix(var2)
  lat = as.matrix(lat)
  long = as.matrix(long)
  obs <- vector(mode = "logical", length = length(long))
  graphics.off()
  graphChoice <- ""
  varChoice1 <- ""
  varChoice2 <- ""
  choix <- ""  
  listgraph <- c("Histogram", "Barplot", "Scatterplot")
    
  if((length(listvar) > 0)&&(dim(as.matrix(listvar))[2] == 1)) listvar <- as.matrix(listvar)

# ouverture des fenêtres graphiques    
  dev.new()
  dev.new()    
  fin <- tclVar(FALSE)
    
####################################################
# sélection d'une partie du boxplot
####################################################
  
boxfunc <- function() 
 {
  quit <- FALSE
  while (!quit) 
   {
    dev.set(3)
    loc <- locator(1)
    if (is.null(loc)) 
    {
     quit <- TRUE
     next
    }
    
    obs <<- selectstat(var1 = var2, var2 = var1, obs = obs,
    Xpoly = loc[1], Ypoly = loc[2], method = "Polyboxplot")
   
    graphique(var1 = var2, var2 = var1, obs = obs, num = 3, graph = "Polyboxplot",
    labvar = labvar, symbol = pch, couleurs = col, labmod = names.arg,bin=varwidth)
   
    carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
    lablong=lablong, lablat=lablat, label=label, symbol=pch,carte=carte,nocart=nocart,method="Cluster",
    classe=var1,couleurs=col,legmap=legmap,legends=legends,labmod=names.arg,axis=axes,cex.lab=cex.lab)
            
    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
     {
      graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
      obs=obs, num=4, graph=graphChoice, couleurs=col[1],symbol=pch[1], labvar=c(varChoice1,varChoice2));
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
        dev.off(4)
        choix <<- selectgraph(listnomvar,listgraph)
        varChoice1 <<- choix$varChoice1
        varChoice2 <<- choix$varChoice2
        graphChoice <<- choix$graphChoice
            
        if ((length(graphChoice) != 0) && (length(varChoice1) != 0))
        {
            dev.new()
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, couleurs=col[1], labvar=c(varChoice1,varChoice2))
        }
        else
        {
        tkmessageBox(message="You must choose a variable",icon="warning",type="ok")
        }
    }
    else
    {
        tkmessageBox(message="You must give variables (listvar) and their names (listnomvar)",icon="warning",type="ok")
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
    lablong=lablong, lablat=lablat, label=label, symbol=pch,couleurs=col,carte=carte,nocart=nocart,
    method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=names.arg,axis=axes,cex.lab=cex.lab)    
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
    
     graphique(var1 = var2, var2 = var1, obs = obs, num = 3, graph = "Polyboxplot",
    labvar = labvar, symbol = pch, couleurs = col, labmod = names.arg,bin=varwidth)
   
    carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
    lablong=lablong, lablat=lablat, label=label, symbol=pch,carte=carte,nocart=nocart,method="Cluster",
    classe=var1,couleurs=col,legmap=legmap,legends=legends,labmod=names.arg,axis=axes,cex.lab=cex.lab)
            
    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
     {
      graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
      obs=obs, num=4, graph=graphChoice, couleurs=col[1],symbol=pch[1], labvar=c(varChoice1,varChoice2));
     }        
  }
  
####################################################
# quitter l'application
####################################################

quitfunc<-function() 
{
  tclvalue(fin)<<-TRUE
  tkdestroy(tt);
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
  lablong=lablong, lablat=lablat, label=label, symbol=pch,carte=carte,nocart=nocart,couleurs=col,
  method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=names.arg,axis=axes,cex.lab=cex.lab)     
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
 lablong=lablong, lablat=lablat, label=label, symbol=pch,carte=carte,nocart=nocart,couleurs=col,
 method="Cluster",classe=var1,legmap=legmap,legends=legends,labmod=names.arg,axis=axes,cex.lab=cex.lab)
 
}


####################################################
# représentation graphique
####################################################

graphique(var1 = var2, var2 = var1, obs = obs, num = 3, graph = "Polyboxplot",
labvar = labvar, symbol = pch, couleurs = col, labmod = names.arg,bin=varwidth)
   
carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
lablong=lablong, lablat=lablat, label=label, symbol=pch,carte=carte,nocart=nocart,method="Cluster",
classe=var1,couleurs=col,legmap=legmap,legends=legends,labmod=names.arg,axis=axes,cex.lab=cex.lab)
    
####################################################
# Légende ou non ?
####################################################        
        
    if (interactive()) 
    {
        OnOK <- function() 
        {
            tkdestroy(tt1)
            msg <- paste("Click on the map to indicate the location of the upper left corner of the legend box")
            tkmessageBox(message = msg)
            dev.set(2)
            loc <- locator(1)
            legends <<- list(legends[[1]], TRUE, legends[[4]], loc)
            carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
            lablong=lablong, lablat=lablat, label=label, symbol=pch,carte=carte,nocart=nocart,method="Cluster",
            classe=var1,couleurs=col,legmap=legmap,legends=legends,labmod=names.arg,axis=axes,cex.lab=cex.lab)    
        }
        
        OnOK2 <- function() 
        {
            legends <<- list(legends[[2]], FALSE, legends[[4]], "")
            tkdestroy(tt1)
            carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,
            lablong=lablong, lablat=lablat, label=label, symbol=pch,carte=carte,nocart=nocart,method="Cluster",
            classe=var1,couleurs=col,legmap=legmap,legends=legends,labmod=names.arg,axis=axes,cex.lab=cex.lab)    
        }
        
        if(length(col)==length(levels(as.factor(var1)))||length(pch)==length(levels(as.factor(var1))))
        {
         tt1 <- tktoplevel()
         labelText12 <- tclVar("Do you want a legend for factors")
         label12 <- tklabel(tt1, justify = "center", wraplength = "3i", text = tclvalue(labelText12))
         tkconfigure(label12, textvariable = labelText12)
         tkgrid(label12, columnspan = 2)
         point.but <- tkbutton(tt1, text = "  Yes  ", command = OnOK)
         poly.but <- tkbutton(tt1, text = " No ", command = OnOK2)
         tkgrid(point.but, poly.but)
         tkgrid(tklabel(tt1, text = "    "))
         tkfocus(tt1)
        }
    }
    
####################################################
# Création de la boîte de dialogue
####################################################        
           
    if (interactive()) 
    {
     tt <- tktoplevel()
     labelText1 <- tclVar("Work on the polyboxplot")
     label1 <- tklabel(tt, text = tclvalue(labelText1))
     tkconfigure(label1, textvariable = labelText1)
     tkgrid(label1, columnspan = 2)
     box.but <- tkbutton(tt, text = "Polyboxplot", command = boxfunc)
     tkgrid(box.but, columnspan = 2)
     tkgrid(tklabel(tt, text = "    "))
     
     label1 <- tclVar("To stop selection, let the cursor on the active graph, click on the right button of the mouse and stop")
     label11 <- tklabel(tt, justify = "center", wraplength = "3i",text = tclvalue(label1))
     tkconfigure(label11, textvariable = label1)
     tkgrid(label11, columnspan = 2)
     tkgrid(tklabel(tt, text = "    "))
     
     labelText7 <- tclVar("Preselected sites")
     label7 <- tklabel(tt, justify = "center", wraplength = "3i",text = tclvalue(labelText7))
     tkconfigure(label7, textvariable = labelText7)
     tkgrid(label7, columnspan = 2)
     noint1.but <- tkbutton(tt, text = "  On/Off  ", command = fnointer)
     tkgrid(noint1.but, columnspan = 2)
     tkgrid(tklabel(tt, text = "    "))
    
     labelText6 <- tclVar("Draw spatial contours")
     label6 <- tklabel(tt, justify = "center", wraplength = "3i", text = tclvalue(labelText6))
     tkconfigure(label6, textvariable = labelText6)
     tkgrid(label6, columnspan = 2)
     nocou1.but <- tkbutton(tt, text = "  On/Off  ", command = cartfunc)
     tkgrid(nocou1.but, columnspan = 2)
     tkgrid(tklabel(tt, text = "    "))
     labelText3 <- tclVar("Restore graph")
     
     label3 <- tklabel(tt, justify = "center", wraplength = "3i", text = tclvalue(labelText3))
     tkconfigure(label3, textvariable = labelText3)
     tkgrid(label3, columnspan = 2)
     nettoy.but <- tkbutton(tt, text = "     OK     ", command = SGfunc)
     tkgrid(nettoy.but, columnspan = 2)
     tkgrid(tklabel(tt, text = "    "))
     
     labelText9 <- tclVar("Bubbles")
     label9 <- tklabel(tt, justify = "center", wraplength = "3i", text = tclvalue(labelText9))
     tkconfigure(label9, textvariable = labelText9)
     tkgrid(label9, columnspan = 2)
     bubble.but <- tkbutton(tt, text = "  On/Off  ", command = fbubble)
     tkgrid(bubble.but, columnspan = 2)
     tkgrid(tklabel(tt, text = "    "))
    
     labelText4 <- tclVar("Additional graph")
     label4 <- tklabel(tt, justify = "center", wraplength = "3i", text = tclvalue(labelText4))
     tkconfigure(label4, textvariable = labelText4)
     tkgrid(label4, columnspan = 2)
     autre.but <- tkbutton(tt, text = "     OK     ", command = graphfunc)
     tkgrid(autre.but, columnspan = 2)
     tkgrid(tklabel(tt, text = "    "))
   
     labelText5 <- tclVar("Exit")
     label5 <- tklabel(tt, justify = "center", wraplength = "3i", text = tclvalue(labelText5))
     tkconfigure(label5, textvariable = labelText5)
     tkgrid(label5, columnspan = 2)
     quit.but <- tkbutton(tt, text = "     OK     ", command = quitfunc)
     tkgrid(quit.but, columnspan = 2)
     tkgrid(tklabel(tt, text = "    "))
     tkwait.variable(fin)
    }
    return(obs)
}
