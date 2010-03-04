`scattermap` <-
function(long,lat,var1,var2,listvar=NULL, listnomvar=NULL,reg.line=TRUE,quantiles=NULL,criteria=NULL,
carte=NULL,label="",cex.lab=1, pch=16, xlab="",ylab="", col="grey", axes=FALSE, lablong="", lablat="")
{
# initialisation
nointer<-FALSE
nocart<-FALSE
buble<-FALSE
maptt<-FALSE
z<-NULL
legmap<-NULL
legends<-list(FALSE,FALSE,"","")
labvar=c(xlab,ylab)
obs<-vector(mode = "logical", length = length(long))
graphics.off()

var1=as.matrix(var1)
var2=as.matrix(var2)
lat=as.matrix(lat)
long=as.matrix(long)

graphChoice <- ""
varChoice1 <- ""
varChoice2 <- ""
choix <- ""
listgraph <- c("Histogram","Barplot","Scatterplot")

labmod <- ""
col2 <- "blue"
col3 <- col[1]
method <- ""
pch2 <- pch[1]

# transformation data.frame en matrix
if((length(listvar)>0)&&(dim(as.matrix(listvar))[2]==1)) listvar<-as.matrix(listvar)

# Ouverture des fenêtres graphique
dev.new()
dev.new()

fin <- tclVar(FALSE)

# au sujet des quantiles conditionnels    
if (reg.line || length(quantiles)!=0)
{
    u1 <- sort(var1)
    u1 <- as.vector(u1)
    z <- seq(min(u1), max(u1), by = ((max(u1)-min(u1))/1000))
    z <- round(z)
    z1 <- z[2:length(z)] - z[1:(length(z) - 1)]
    h <- mean(z1)

    p <- 1 / (1 + (h^3 / 6))
    p1 <- 1 / (1 + (h^3 / 1000)) #60
    p2 <- 1 / (1 + (h^3 / 4)) #0.6
    alpha <- (1 - p) / p
    borne1 <- (1 - p1) / p1
    borne2 <- (1 - p2) / p2

}


####################################################
# sélection d'un point
####################################################

pointfunc<-function() 
{
    quit <- FALSE;

    while(!quit)
    {
        if(maptt) 
        {
            dev.set(2)
            loc<-locator(1)
            if(is.null(loc)) 
            {
              quit<-TRUE
              next
            }           
            obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point"); 
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
            obs<<-selectmap(var1=var1,var2=var2,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point");
        }
        
        #graphiques

      
      graphique(var1=var1, var2=var2, obs=obs, num=3, graph="Scatterplot", labvar=labvar, symbol=pch, couleurs=col,
      opt1=reg.line, quantiles=quantiles, alpha1=alpha)     
      
      carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
      symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
      lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
  
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))
        }

    }
  }

####################################################
# sélection d'un point sur la carte
####################################################

pt1func <- function()
{   maptt <<- TRUE
    pointfunc()
  }

####################################################
# sélection d'un point sur le graphique
####################################################

pt2func <- function()
{
    maptt <<- FALSE
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

    while(!quit)
    {
        if(maptt)
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
            dev.set(3)
            loc<-locator(1)
            if(is.null(loc)) 
            {
                quit<-TRUE
                next
            }   
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

    if (maptt)
    {
        obs <<- selectmap(var1=long, var2=lat, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly");
    }
    else
    {
        obs <<- selectmap(var1=var1, var2=var2, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly");
    }

    #graphiques
      graphique(var1=var1, var2=var2, obs=obs, num=3, graph="Scatterplot", labvar=labvar, symbol=pch, couleurs=col,
      opt1=reg.line, quantiles=quantiles, alpha1=alpha)     
      
      carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
      symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
      lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
  
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))
        }
 }
}

####################################################
# sélection d'un polygone sur la carte
####################################################

poly1func <- function()
{
    SGfunc()
    maptt <<- TRUE
   polyfunc()
  }

####################################################
# sélection d'un polygone sur le graphique
####################################################

poly2func <- function()
{
    SGfunc()
    maptt <<- FALSE
    polyfunc()
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
        dev.off(4)
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
            res1<-choix.couleur(graphChoice,listvar,listnomvar,varChoice1,legends,col,pch)
            
            method <<- res1$method
            col2 <<- res1$col2
            col3 <<- res1$col3
            pch2 <<- res1$pch2
            legends <<- res1$legends
            labmod <<- res1$labmod
                     
            dev.new()
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))    
            
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
  graphique(var1=var1, var2=var2, obs=obs, num=3, graph="Scatterplot", labvar=labvar, symbol=pch, couleurs=col,
  opt1=reg.line, quantiles=quantiles, alpha1=alpha)     
      
  carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
  symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
  lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
 
   if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
    {
     graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
     obs=obs, num=4, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))
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
# modification du alpha de l'estimateur
####################################################
  
refresh.code<-function(...)
{
  alpha<<-slider1(no=1) 
  graphique(var1=var1, var2=var2, obs=obs, num=3, graph="Scatterplot", labvar=labvar, symbol=pch, couleurs=col,
  opt1=reg.line, quantiles=quantiles, alpha1=alpha)     
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
# Représentation des graphiques
####################################################

 graphique(var1=var1, var2=var2, obs=obs, num=3, graph="Scatterplot", labvar=labvar, symbol=pch, couleurs=col,
 opt1=reg.line, quantiles=quantiles, alpha1=alpha)     
      
 carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label,
 symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
 lablong=lablong,lablat=lablat,cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
  

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

point.but <- tkbutton(tt, text="  Point  ", command=pt1func);
poly.but <- tkbutton(tt, text=" Polygon ", command=poly1func);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))

labelText2 <- tclVar("Work on the scattermap")
label2 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText2))
tkconfigure(label2, textvariable=labelText2)
tkgrid(label2,columnspan=2)

p2.but <- tkbutton(tt, text="  Point  ", command=pt2func);
pol2.but <- tkbutton(tt, text=" Polygon ", command=poly2func);
tkgrid(p2.but, pol2.but)
tkgrid(tklabel(tt,text="    "))


label1 <- tclVar("To stop selection, leave the cursor on the active graph, click on the right button of the mouse and stop")
label11 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(label1))
tkconfigure(label11, textvariable=label1)
tkgrid(label11,columnspan=2)
tkgrid(tklabel(tt,text="    "))

if (length(quantiles)!=0)
{
    slider1(tt,refresh.code,c("Reg. Smooth. Spline Parameter"),c(borne1),c(borne2),c((borne2-borne1)/300),c(alpha))
  }



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
}


####################################################

return(obs)
  }

