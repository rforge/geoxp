`plot3dmap` <-
function(long,lat,var1,var2,var3,box=TRUE,listvar=NULL, listnomvar=NULL, criteria=NULL,
carte=NULL, label="",cex.lab=1, pch=16, col="blue", xlab="",ylab="",zlab="",
axes=FALSE,lablong="",lablat="")
{

####################################################
# initialisation
####################################################

nointer<-FALSE
nocart<-FALSE
buble<-FALSE
z<-NULL
legmap<-NULL
legends<-list(FALSE,FALSE,"","")
var1=as.matrix(var1)
var2=as.matrix(var2)
var3=as.matrix(var3)
lat=as.matrix(lat)
long=as.matrix(long)
obs<-vector(mode = "logical", length = length(long))
fin <- tclVar(FALSE);
graphChoice <- ""
varChoice1 <- ""
varChoice2 <- ""
choix<-""
method <- ""
listgraph <- c("Histogram","Barplot","Scatterplot")
labmod <- ""
col2 <- "blue"
col3 <- col[1]
pch2 <- pch[1]


# Change data.frame in matrix
if((length(listvar)>0)&&(dim(as.matrix(listvar))[2]==1)) listvar<-as.matrix(listvar)

# Ouverture des fenêtres graphiques
graphics.off()
dev.new()


####################################################
# sélection d'un point
####################################################

pointfunc<-function() 
 {
    quit <- FALSE

    while(!quit)
     {
      #sélection des points

       dev.set(3)
       loc<-locator(1)
       
         if(is.null(loc))
           {
            quit<-TRUE
            next
           }
      
       obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point")

      # graphiques
    if (length(which(obs==TRUE))==0||length(which(obs==TRUE))==length(obs))
     {if(length(which(obs==TRUE))==0)
     plot3d(var1[!obs],var2[!obs],var3[!obs],xlab = xlab, ylab = ylab, zlab = zlab,
     col=col, type="p", size=5,box=box)
     else
     plot3d(var1[obs],var2[obs],var3[obs],xlab = xlab, ylab = ylab, zlab = zlab,
     col="red", type="p", size=6,box=box)    
     }
    else
    { 
      col.prov <- rep(col,length(var1))
  #    size.prov <- rep(5,length(var1))
      
      col.prov[obs]<-"red"
 #     size.prov[obs]<-6 
      
      plot3d(var1,var2,var3,xlab = xlab, ylab = ylab, zlab = zlab,col=col.prov, type="p",
      size=5,box=box)
  

    }
    
      carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
      symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
      lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
  
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=3, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))
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
      lines(polyX,polyY);

      obs <<- selectmap(var1=long, var2=lat, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly")

      # graphiques

    if (length(which(obs==TRUE))==0||length(which(obs==TRUE))==length(obs))
     {if(length(which(obs==TRUE))==0)
     plot3d(var1[!obs],var2[!obs],var3[!obs],xlab = xlab, ylab = ylab, zlab = zlab,
     col=col, type="p", size=5,box=box)
     else
     plot3d(var1[obs],var2[obs],var3[obs],xlab = xlab, ylab = ylab, zlab = zlab,
     col="red", type="p", size=6,box=box)    
     }
    else
    { 
      col.prov <- rep(col,length(var1))
     # size.prov <- rep(5,length(var1))
      
      col.prov[obs]<-"red"
    #  size.prov[obs]<-6 
      
      plot3d(var1,var2,var3,xlab = xlab, ylab = ylab, zlab = zlab,col=col.prov,
      type="p", size=5,box=box)
    }
   

           
      carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
      symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
      lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
  
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
         graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
         obs=obs, num=3, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))
        }
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
    carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
    symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
    lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
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
        dev.off(3)
        choix <<- selectgraph(listnomvar,listgraph)
        varChoice1 <<- choix$varChoice1
        varChoice2 <<- choix$varChoice2
        graphChoice <<- choix$graphChoice
           
        if ((graphChoice != "") && (varChoice1 != ""))
        {
          if (((graphChoice == "Histogram")&&(!is.numeric(listvar[,which(listnomvar == varChoice1)])))||((graphChoice == "Scatterplot")&&((!is.numeric(listvar[,which(listnomvar == varChoice1)]))||(!is.numeric(listvar[,which(listnomvar == varChoice2)]))))) 
           {
            tkmessageBox(message="Variables choosed are not in a good format",icon="warning",type="ok")
           }
          else
           {
            res1<-choix.couleur(graphChoice,listvar,listnomvar,varChoice1,legends,col)
            
            method <<- res1$method
            col2 <<- res1$col2
            col3 <<- res1$col3
            pch2 <<- res1$pch2
            legends <<- res1$legends
            labmod <<- res1$labmod
                     
            dev.new()
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=3, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))    
            
            carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
            symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
            lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
           }
       }   
   }
   else
   {
    tkmessageBox(message="Variables (listvar) and their names (listnomvar) must have been given",icon="warning",type="ok")
   }  
}


####################################################
# rafraichissement des graphiques
####################################################

SGfunc<-function() 
{
 obs<<-vector(mode = "logical", length = length(long));

 # graphiques
 plot3d(var1,var2,var3,xlab = xlab, ylab = ylab, zlab = zlab,col=col, type="p", size=5,box=box) 
      
 carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
 symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
 lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
  
 if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
  {
   graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
   obs=obs, num=3, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))
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
  carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
  symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
  lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
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
  
  carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
  symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
  lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 

}


####################################################
# graphiques 
####################################################

carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 

par3d(windowRect= c(0,32,512,544), userMatrix = rotationMatrix(5*pi/180, 0,1,0) %*% par3d("userMatrix"),family='sans' )
bg3d(color="white")

plot3d(var1,var2,var3,xlab = xlab, ylab = ylab, zlab = zlab,xlim=range(var1),ylim=range(var2),zlim=range(var3),col=col,
size=5,type="n",box=box) 
  
plot3d(var1,var2,var3,xlab = xlab, ylab = ylab, zlab = zlab,xlim=range(var1),ylim=range(var2),zlim=range(var3),col=col,
size=5,type="p",box=FALSE,add=TRUE) 
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
poly.but <- tkbutton(tt, text=" Polygon ", command=polyfunc);
tkgrid(point.but, poly.but)
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

