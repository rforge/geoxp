`driftmap` <-
function(long, lat, var, interpol=TRUE, nuage=FALSE,
listvar=NULL, listnomvar=NULL, carte=NULL, xlab="", col=c("blue","red","blue","pink"),
pch=c(16,16,16,16), lty=c(1,2),cex=0.7,label=NULL,
cex.lab=1,axes=FALSE)
{
# initialisation
  labvar<-xlab
  nocart<-FALSE
  buble<-FALSE
  z<-NULL
  legmap<-NULL
  legends<-list(FALSE,FALSE,"","")
  
  ifelse(interpol, ligne<-"l", ligne<-"p")
  obs<-vector(mode = "logical", length = length(long))
  obs[1:length(obs)]<-TRUE

# option for adding a graph
  graphChoice <- ""
  varChoice1 <- ""
  varChoice2 <- ""
  choix<-""
  method <- ""
  listgraph <- c("Histogram","Barplot","Scatterplot")
  labmod <- ""
  col2 <- col[3]
  col3 <- col[1]
  pch2 <- pch[3]
  

# initialisation des paramètres modifiables
  theta=0
  nbrow=10
  nbcol=10
  
# Ouverture des fenêtres graphiques  
graphics.off()
dev.new()
dev.new()

  fin <- tclVar(FALSE)


####################################################
# Changement d'angle
####################################################

choixangle <- function() 
{
tt1<-tktoplevel()
Name <- tclVar("0")
entry.Name <-tkentry(tt1,width="3",textvariable=Name)
tkgrid(tklabel(tt1,text="Please enter a angle in degree"),entry.Name)


OnOK <- function()
{ 
	theta <- tclvalue(Name)
	tkdestroy(tt1)
       
   if (is.na(as.numeric(theta))||(as.numeric(theta)<0))
     {
      tkmessageBox(message="Sorry, but you have to choose a positive number",icon="warning",type="ok");
     }
   else
     {
      msg <- paste("You choose",theta,"degrees")
      tkmessageBox(message=msg)
      theta<<-as.numeric(theta)
      graphique.drift()
     }
}



OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
tkbind(entry.Name, "<Return>",OnOK)
tkgrid(OK.but)
tkfocus(tt1)

}

####################################################
# sélection d'un point
####################################################

pointfunc<-function() 
 {
    quit <- FALSE

    while(!quit)
     {
      #sélection des points

       dev.set(2)
       loc<-locator(1)
       
         if(is.null(loc))
           {
            quit<-TRUE
            next
           }
      
       obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point")

       if (length(which(obs==TRUE))==0)
         {SGfunc()
          tkmessageBox(message="You have to select at least 1 site to calculate mean and median",icon="warning",type="ok")}
       else
       {
      # graphiques
       graphique.drift()
    
      carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,label=label,
      symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
      cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
  
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))
        }
        
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
      
      if (length(which(obs==TRUE))==0)
      {SGfunc()
      tkmessageBox(message="You have to select at least 1 site to calculate mean and median",icon="warning",type="ok")
      }
      else
      {
      # graphiques
       graphique.drift()
    
      carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,label=label,
      symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
      cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
  
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))
        }
        
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
    carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,label=label,
    symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
    cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)]) 
      
    graphique.drift()
   }
   else
   {
    tkmessageBox(message="Spatial contours have not been given",icon="warning",type="ok")    
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
  
  carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,label=label,
  symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
  cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)])  

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
         
         if((length(pch2)==1)) pch2<<-pch[3]
                     
            dev.new()
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
            obs=obs, num=4, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))    
            
      carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,label=label,
      symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
      cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)])  
           }
       }   
   }
   else
   {
    tkmessageBox(message="Variables (listvar) and their names (listnomvar) must have been given",icon="warning",type="ok")
   }  
}

####################################################
# Choisir numbre of grids
####################################################

choixgrid <- function() 
{
tt1<-tktoplevel()
Name1 <- tclVar("10")
Name2 <- tclVar("10")
entry.Name <-tkentry(tt1,width="3",textvariable=Name1)
tkgrid(tklabel(tt1,text="Enter number of rows"),entry.Name)
entry.Name2 <-tkentry(tt1,width="3",textvariable=Name2)
tkgrid(tklabel(tt1,text="Enter number of columns"),entry.Name2)



OnOK <- function()
{ 
	nbrow <<- tclvalue(Name1)
	nbcol <<- tclvalue(Name2)
	tkdestroy(tt1)
       
   if (is.na(as.numeric(nbrow ))||(as.numeric(nbrow)<2)||(as.numeric(nbcol)<2))
     {
      tkmessageBox(message="Sorry, but you have to choose a number upper than 2",icon="warning",type="ok");
     }
   else
     {

      nbrow<<-as.numeric(nbrow)
      nbcol<<-as.numeric(nbcol)
      graphique.drift()
     }
}



OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
#tkbind(entry.Name, "<Return>",OnOK)
tkgrid(OK.but)
#tkfocus(tt1)

}
####################################################
# Dessin Graphique
####################################################

graphique.drift <- function()
{
dev.set(3)
# calcul des nouvelles coordonnées en fonction de l'angle de rotation theta
coords <- cbind(long[obs],lat[obs])
nvlecoord <- rotation(coords,theta)
nvlong <- nvlecoord[,1]
nvlat <- nvlecoord[,2]
var<-var[obs]

x.lim=c(min(coords[,1],na.rm=TRUE),max(coords[,1],na.rm=TRUE)) 
y.lim=c(min(coords[,2],na.rm=TRUE),max(coords[,2],na.rm=TRUE))
 
asp=1/cos((mean(y.lim) * pi)/180)
if(asp>1.40||asp<0.6) asp=1 

# création de la fenêtre graphique
# x11(width=7, height=7);

nf <- layout(matrix(c(1,2,3,4),2,2,byrow=TRUE), c(1,1), c(1,1), TRUE)
layout.show(nf)

###########################################
# calcul des coordonnées de la grille
###########################################

h1 <- (max(nvlong)-min(nvlong))/nbcol
h2 <- (max(nvlat)-min(nvlat))/nbrow

# colonnes
xind <- min(nvlong)
milcol <- NULL
for (i in 1:(nbcol-1))
{
    xind <- c(xind,min(nvlong)+h1*i)
    milcol <- c(milcol,(xind[i]+xind[i+1])/2)
}
xind <- c(xind,max(nvlong))
milcol <- c(milcol,(xind[nbcol]+xind[nbcol+1])/2)

# lignes
yind <- min(nvlat)
millig <- NULL

 for (j in 1:(nbrow-1))
  {
   yind <- c(yind,min(nvlat)+h2*j)
   millig <- c(millig,(yind[j]+yind[j+1])/2)
  }
  
yind <- c(yind,max(nvlat))
millig <- c(millig,(yind[nbrow]+yind[nbrow+1])/2)

###########################################
# calcul de la moyenne et de la médiane 
###########################################

# pour chaque colonne 
medcol <- NULL
moycol <- NULL
colvide <- rep(FALSE,nbcol)

limmax<-xind[2:(nbcol+1)]
ifelse(limmax[nbcol]>0,limmax[nbcol] <- 2*limmax[nbcol],limmax[nbcol] <- limmax[nbcol]/2)

for (i in 1:nbcol)
{
 ens <- var[which((nvlong >= xind[i]) & (nvlong < limmax[i]))]

    # si la colonne est vide
    if (length(ens) != 0)
    {
        moycol <- c(moycol,mean(ens))
        medcol <- c(medcol,median(ens))
    }
    else
    {
        colvide[i] <- TRUE
        moycol <- c(moycol,0)
        medcol <- c(medcol,0)
    }
}

# pour chaque ligne
medlig <- NULL
moylig <- NULL
ligvide <- rep(FALSE,nbrow)

limmax<-yind[2:(nbrow+1)]
ifelse(limmax[nbrow]>0,limmax[nbrow]<-2*limmax[nbrow],limmax[nbrow]<-limmax[nbrow]/2)

for (i in 1:nbrow)
{
  ens <- var[which((nvlat >= yind[i]) & (nvlat < limmax[i]))]  
    # si la ligne est vide
    if (length(ens) != 0)
    {
        moylig <- c(moylig,mean(ens))
        medlig <- c(medlig,median(ens))
    }
    else
    {
        ligvide[i] <- TRUE;
        moylig <- c(moylig,0)
        medlig <- c(medlig,0)
    }
}

###########################################
# dessin de la carte 
###########################################

par(pty="s",mar=c(2,2,1,1))


# dessin des contours si ces derniers ont été précisés
if (nocart)
{
    nvcarte <- rotation(carte,theta)
    n <- nrow(nvcarte)
    abs1 <- nvcarte[1:(n-1),1]
    ord1 <- nvcarte[1:(n-1),2]
    abs2 <- nvcarte[2:n,1]
    ord2 <- nvcarte[2:n,2]
#    limx <- c(min(nvcarte[,1]),max(nvcarte[,1]));
#    limy <- c(min(nvcarte[,2]), max(nvcarte[,2]));

    plot(range(nvlong),range(nvlat),"n", asp=asp)
    
    segments(abs1,ord1,abs2,ord2)
    points(nvlong,nvlat,col=col[3],pch=pch[3], cex=cex)
}
else
{
    plot(nvlong,nvlat,col=col[3],pch=pch[3],asp=asp,cex=cex,xlim=range(nvlong),ylim=range(nvlat))
}

# dessin de la grille
# colonnes
for (i in 1:(nbcol+1))
{
    segments(xind[i],min(nvlat),xind[i],max(nvlat),col="red")
}

# lignes
for (i in 1:(nbrow+1))
{
    segments(min(nvlong),yind[i],max(nvlong),yind[i],col="red")
}


###########################################
# dessin des graphiques 
###########################################

# lignes
par(mar=c(2,0,1,1))

if (!nuage)
{
 plot(var,nvlat,"n",xlim=c(min(moylig[!ligvide],medlig[!ligvide]),max(moylig[!ligvide],medlig[!ligvide])),
  ylim=range(nvlat),yaxt="n")  
}
else
{
 plot(var,nvlat,col=col[4],pch=pch[4],cex=cex,xlim=range(var),ylim=range(nvlat),yaxt="n")
}


    points(moylig[!ligvide],millig[!ligvide],col=col[1],pch=pch[1])
    points(moylig[!ligvide],millig[!ligvide],col=col[1],pch=pch[1],type=ligne,lty=lty[1])
    
    points(medlig[!ligvide],millig[!ligvide],col=col[2],pch=pch[2])
    points(medlig[!ligvide],millig[!ligvide],col=col[2],pch=pch[2],type=ligne,lty=lty[2])

# colonnes
par(mar=c(0,2,1,1))

if (!nuage)
{
 plot(nvlong,var,"n",ylim=c(min(moycol[!colvide],medcol[!colvide]),max(moycol[!colvide],medlig[!colvide])),
  xlim=range(nvlong),xaxt="n",yaxt="n")  
 axis(side=4) 
}
else
{
 plot(nvlong,var,col=col[4],pch=pch[4],cex=cex,ylim=range(var),xlim=range(nvlong),xaxt="n",yaxt="n")
 axis(side=4)
}

 points(milcol[!colvide],moycol[!colvide],col=col[1],pch=pch[1])
 points(milcol[!colvide],moycol[!colvide],col=col[1],pch=pch[1],type=ligne,lty=lty[1])
 
 points(milcol[!colvide],medcol[!colvide],col=col[2],pch=pch[2])
 points(milcol[!colvide],medcol[!colvide],col=col[2],pch=pch[2],type=ligne,lty=lty[2])
 
###########################################
# précision des indications
###########################################

plot(-2:2,-2:2,"n",xaxt="n",yaxt="n")
# dessin de la rose des vents
loc<-c(0,0)
size<-1
cols <- rep(c("white","black"),8)

# calculating coordinates of polygons
radii <- rep(size/c(1,4,2,4),4)
x <- radii[(0:15)+1]*cos((0:15)*pi/8)+loc[1]
y <- radii[(0:15)+1]*sin((0:15)*pi/8)+loc[2]

nvlecoord.arrox <- rotation(cbind(x,y),theta)
x<-nvlecoord.arrox[,1]
y<-nvlecoord.arrox[,2]

# drawing polygons
for (i in 1:15) 
{
x1 <- c(x[i],x[i+1],loc[1])
y1 <- c(y[i],y[i+1],loc[2])
polygon(x1,y1,col=cols[i])
}
# drawing the last polygon
polygon(c(x[16],x[1],loc[1]),c(y[16],y[1],loc[2]),col=cols[16])

# drawing letters

let<-NULL
for (i in 0:3)
let<-rbind(let,c((size+par("cxy")[1])*cos(i*pi/2)+loc[1],(size+par("cxy")[2])*sin(i*pi/2)+loc[2]))
new.let<-rotation(let,theta)

b <- c("E","N","W","S")
text(new.let,b,cex=cex.lab)


# légende

legend(-1.75,1.75, c("Mean","Median"), col=col, pch=pch,lty=lty, cex=cex.lab)

# nom de la variable étudiée
if (labvar != "")
{
    msg <- paste("Variable : ",labvar, sep="")
    text(0,-1.75, msg, col="black",cex=cex.lab)
}


}


####################################################
# quitter l'application
####################################################

    quitfunc <- function() {
        tclvalue(fin) <<- TRUE
      #  graphics.off()
        tkdestroy(tt)

   #    tkdestroy(tt1)
   #tkwait.variable(fin)
    }

####################################################
# rafraichissement des graphiques
####################################################

SGfunc<-function() 
{
 obs<<-vector(mode = "logical", length = length(long))
 obs[1:length(obs)]<<-TRUE
  
 # graphiques
 graphique.drift()
 
 carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,label=label,
 symbol=pch2, couleurs=col2,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axes, labmod=labmod,
 cex.lab=cex.lab,method=method,classe=listvar[,which(listnomvar == varChoice1)])  

 if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
  {
   graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)],
   obs=obs, num=4, graph=graphChoice, couleurs=col3,symbol=pch, labvar=c(varChoice1,varChoice2))
  }
          
}

####################################################
# Représentation graphique
####################################################

graphique.drift()
carte(long=long, lat=lat, obs=obs,label=label,symbol=pch[3],carte=carte,nocart=nocart,
cex.lab=cex.lab,method="") 

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


labelText12 <- tclVar("Change the number of cells")
label12 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText12))
tkconfigure(label12, textvariable=labelText12)
tkgrid(label12,columnspan=2)


vari12.but <- tkbutton(tt, text="     OK     ", command=choixgrid);
tkgrid(vari12.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

labelText1 <- tclVar("Change the rotation angle")
label1 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)


vari1.but <- tkbutton(tt, text="     OK     ", command=choixangle);
tkgrid(vari1.but,columnspan=2)
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
####################################################

tkwait.variable(fin)
}

}

