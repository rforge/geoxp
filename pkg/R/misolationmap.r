`misolationmap` <-
function (long, lat, object,propneighb=0.4,chisqqu=0.975, listvar=NULL, listnomvar=NULL, criteria=NULL,
carte = NULL, label = "",cex.lab=1,pch = 16,col="blue", xlab = "degree of isolation",
ylab="Pairwise Mahalanobis distances", lablong = "", lablat = "", axes=FALSE)
{
 # initialisation
  xy<-cbind(long, lat)
  nointer<-FALSE
  nocart<-FALSE
  buble<-FALSE
  legends<-list(FALSE,FALSE,"","")
  z<-NULL
  legmap<-NULL
  inout=NULL
  labvar=c(xlab,ylab)
  obs <- matrix(FALSE, nrow = length(long), ncol = length(long))
    graf<-"Neighbourplot1"
  obsref<-obs
  
  outselect=TRUE
  outselect2=TRUE
  orderselect=TRUE
  
  graphics.off()

   W<-nb2mat(object)
   Wref<-W
# Transformation d'un data.frame en matrix

if((length(listvar)>0) && (dim(as.matrix(listvar))[2]==1)) listvar<-as.matrix(listvar)

# ouverture des fenêtres graphiques
dev.new()
dev.new()
fin <- tclVar(FALSE)

# calcul des matrices theta et absvar

n=nrow(listvar)
p=ncol(listvar)
require(robustbase) # for robust Mahalanobis distance

covr=covMcd(listvar,alpha=0.75)
cinv=solve(covr$cov)
MDglobal=sqrt(mahalanobis(listvar, covr$center, cinv, inverted=TRUE))


# TRUE/FALSE for non-outlying/outlying:
qchi=sqrt(qchisq(chisqqu,p))
MDglobalTF <- (MDglobal<qchi)

idx=matrix(1:n,n,n)
se=as.vector(idx[lower.tri(idx)])
hlp=as.matrix(listvar[rep(1:(n-1),seq((n-1),1)),]-listvar[se,])
MDij=sqrt(rowSums((hlp%*%cinv)*hlp))

MDpair=matrix(0,n,n)
MDpair[lower.tri(MDpair)] <- MDij
MDpair=t(MDpair)
MDpair[lower.tri(MDpair)] <- MDij

MDpairN=vector("list", n)

# boundary that should include required proportion of neighbors:
chibound=rep(NA,n)
theta<-matrix(0,n,n)
absvar<-matrix(0,n,n)

for (i in 1:n){
  MDpairN[[i]] <- MDpair[i,object[[i]]]
  nn=max(1,round(length(MDpairN[[i]])*propneighb)) # number of neighbors in tolerance ellipse
  nval=MDpairN[[i]][order(MDpairN[[i]])][nn]  # value of largest neighbor to be included
  chibound[i]=pchisq(nval^2,p,MDglobal[i]^2)
  
  theta[i,object[[i]]]<-chibound[i]
  absvar[i,object[[i]]]<-MDpair[i,object[[i]]]

}

thetaref=theta
absvaref=absvar

thetaref2=thetaref
absvaref2=absvaref

# sort according to values of "chibound" - separately for outliers and non-outliers
idx1=order(chibound[MDglobalTF])
idx0=order(chibound[!MDglobalTF])

idxg<-sort(chibound,index.return=TRUE)

# calcul des distances de Mahalanobis par site

rd <- sqrt(mahalanobis(listvar, center = covr$center, cov = covr$cov))
xarw <- arw(listvar, covr$center, covr$cov, alpha = 0.025)
  ifelse(xarw$cn != Inf, alphab <- sqrt(c(xarw$cn, qchisq(c(0.75, 0.5, 0.25), ncol(listvar)))),
  alphab <- sqrt(qchisq(c(0.975, 0.75, 0.5, 0.25), ncol(listvar))))


chi2.quant<-rep(0,n)
lalpha <- length(alphab)
    for (j in 1:lalpha) {
            if (j == 1) {
                (chi2.quant[which(rd >= alphab[j])]<-lalpha)
            }
            else {
                chi2.quant[which((rd < alphab[j - 1]) & (rd >= alphab[j]))]<-lalpha+1-j

            }
     }



####################################################
# sélection d'un point sur la carte
####################################################

pointfunca<-function()
{
   if (graf=="pairwise") SGfunc()
   graf<<-"Neighbourplot1"
   
    quit <- FALSE;

    while(!quit)
    {
        dev.set(2);
        loc<-locator(1);
        if (is.null(loc))
        {
            quit<-TRUE;
            next;
        }
        obs2<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point");

       obs<<-(W*obs2>0)

       if(!outselect)
        {
         obs[MDglobalTF,]<<-FALSE
        }

    if(!outselect2)
     {
      obs[!MDglobalTF,]<<-FALSE
     }
     
    #    diag(obs)<<-FALSE
        # graphiques
   carte(long = long, lat = lat, obs = obs,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,
   nocart=nocart, lablong = lablong,lablat = lablat,label = label,cex.lab=cex.lab, symbol = pch,method = "pairwise",
   axis=axes,legmap=legmap,legends=legends)
   
   #     carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=16,
   #     method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,buble=buble,criteria=criteria,
   #     nointer=nointer,cbuble=z,carte=carte,nocart=nocart,couleurs="blue",classe=card(object),cex.lab=cex.lab)


   graphique(var1 = theta, var2 = absvar, obs = obs,num = 3, graph = "pairwise", labvar = labvar,
   couleurs=col,symbol = pch, direct=propneighb)

  #  obs <<- matrix(FALSE, nrow=length(long), ncol=length(long));

    }
  }


####################################################
# sélection d'un polygone
####################################################

polyfunca<-function()
{
   if (graf=="pairwise") SGfunc()
   graf<<-"Neighbourplot1"
   
    polyX <- NULL;
    polyY <- NULL;
    quit <- FALSE;

    while(!quit)
    {
        dev.set(2);
        loc<-locator(1);
        if(is.null(loc))
        {
            quit<-TRUE;
            next;
        }

        polyX <- c(polyX, loc[1]);
        polyY <- c(polyY, loc[2]);
        lines(polyX,polyY);
    }

    polyX <- c(polyX, polyX[1]);
    polyY <- c(polyY, polyY[1]);
if (length(polyX)>0)
{
    lines(polyX,polyY);

    obs2 <- selectmap(var1=long, var2=lat, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly");
    obs<<-(W*obs2>0)

    if(!outselect)
     {
      obs[MDglobalTF,]<<-FALSE
     }
     
     if(!outselect2)
     {
      obs[!MDglobalTF,]<<-FALSE
     }

   #    diag(obs)<<-FALSE
       
    # graphiques
   carte(long = long, lat = lat, obs = obs,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,
   nocart=nocart, lablong = lablong,lablat = lablat,label = label,cex.lab=cex.lab, symbol = pch,method = "pairwise",
   axis=axes,legmap=legmap,legends=legends)

   #     carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=16,
   #     method="Neighbourplot1", W=W,axis=axes,legmap=legmap,legends=legends,buble=buble,criteria=criteria,
   #     nointer=nointer,cbuble=z,carte=carte,nocart=nocart,couleurs="blue",classe=card(object),cex.lab=cex.lab)


   graphique(var1 = theta, var2 = absvar, obs = obs,num = 3, graph = "pairwise", labvar = labvar,
   couleurs=col,symbol = pch, direct=propneighb)

 #   obs <<- matrix(FALSE, nrow=length(long), ncol=length(long));

}
  }


####################################################
# sélection d'un point sur l'angleplot
####################################################

   
 pointfunc <- function() 
 {
   if (graf=="Neighbourplot1") SGfunc()
   graf<<-"pairwise"
   
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

   obs <<- selectstat(var1 = theta, var2 = absvar, obs = obs,Xpoly = loc[1], Ypoly = loc[2],
   method = "AnglePoint",long = long, lat = lat)
   
    if(!outselect)
     {
      obs[MDglobalTF,]<<-FALSE
     }

    if(!outselect2)
     {
      obs[!MDglobalTF,]<<-FALSE
     }
     
   diag(obs)<<-FALSE
   
   graphique(var1 = theta, var2 = absvar, obs = obs,num = 3, graph = "pairwise", labvar = labvar,
   couleurs=col,symbol = pch, direct=propneighb)
  
   carte(long = long, lat = lat, obs = obs,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,
   nocart=nocart, lablong = lablong,lablat = lablat,label = label,cex.lab=cex.lab, symbol = pch,method = "pairwise",
   axis=axes,legmap=legmap,legends=legends) 
 }
}


####################################################
# sélection des outliers
####################################################


 outlier <- function()
 {

   outselect<<-!outselect
   labvar=c(xlab,ylab)


   if (!outselect) tkmessageBox(message="You have selected only global outliers")
   if (outselect) tkmessageBox(message="You have remited non global outliers")

   if(outselect)
   {
    theta<<-thetaref
    absvar<<-absvaref
    thetaref2<<-thetaref
    absvaref2<<-absvaref
      if(!orderselect)
       {
       labvar=c("Rank",ylab)
       ind0<-which(theta==0,arr.ind=TRUE)
       theta[idxg$ix,]<<-t(matrix(rep(1:n,each=length(idxg$ix)),length(idxg$ix),length(idxg$ix)))
       theta[ind0]<<-0
       theta<<-theta
       }
    }
   else
   {
   SGfunc()
   if(!outselect2)
    {
    theta<<-thetaref
    absvar<<-absvaref
      if(!orderselect)
       {
       labvar=c("Rank",ylab)
       ind0<-which(theta==0,arr.ind=TRUE)
       theta[idxg$ix,]<<-t(matrix(rep(1:n,each=length(idxg$ix)),length(idxg$ix),length(idxg$ix)))
       theta[ind0]<<-0
       theta<<-theta
       }
    }
    #theta<<-thetaref[!MDglobalTF,]
    #absvar<<-absvaref[!MDglobalTF,]
    theta[MDglobalTF,]<<-0
    absvar[MDglobalTF,]<<-0

    thetaref2<<-theta
    absvaref2<<-absvar
   # obs[MDglobalTF,]<<-FALSE
   # W[MDglobalTF,]<<-0
       }

   graphique(var1 = theta, var2 = absvar, obs = obs,num = 3, graph = "pairwise", labvar = labvar,
   couleurs=col,symbol = pch, direct=propneighb)

   carte(long = long, lat = lat, obs = obs,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,
   nocart=nocart, lablong = lablong,lablat = lablat,label = label,cex.lab=cex.lab, symbol = pch,method = "pairwise",
   axis=axes,legmap=legmap,legends=legends)

}

####################################################
# sélection des non outliers
####################################################


 outlier2 <- function()
 {

   outselect2<<-!outselect2
   labvar=c(xlab,ylab)

   if (!outselect) tkmessageBox(message="You have selected non global outliers")
   if (outselect) tkmessageBox(message="You have remited global outliers")

   if(outselect2)
   {
    theta<<-thetaref
    absvar<<-absvaref
    thetaref2<<-thetaref
    absvaref2<<-absvaref
       if(!orderselect)
       {
       labvar=c("rank",ylab)
       ind0<-which(theta==0,arr.ind=TRUE)
       theta[idxg$ix,]<<-t(matrix(rep(1:n,each=length(idxg$ix)),length(idxg$ix),length(idxg$ix)))
       theta[ind0]<<-0
       theta<<-theta
       }
    }
   else
   {
   SGfunc()
    #theta<<-thetaref[!MDglobalTF,]
    #absvar<<-absvaref[!MDglobalTF,]
    if(!outselect)
    {
    theta<<-thetaref
    absvar<<-absvaref
      if(!orderselect)
       {
       labvar=c("Rank",ylab)
       ind0<-which(theta==0,arr.ind=TRUE)
       theta[idxg$ix,]<<-t(matrix(rep(1:n,each=length(idxg$ix)),length(idxg$ix),length(idxg$ix)))
       theta[ind0]<<-0
       theta<<-theta
       }
    }
    theta[!MDglobalTF,]<<-0
    absvar[!MDglobalTF,]<<-0
    
    thetaref2<<-theta
    absvaref2<<-absvar
   # obs[MDglobalTF,]<<-FALSE
   # W[MDglobalTF,]<<-0
       }

   graphique(var1 = theta, var2 = absvar, obs = obs,num = 3, graph = "pairwise", labvar = labvar,
   couleurs=col,symbol = pch, direct=propneighb)

   carte(long = long, lat = lat, obs = obs,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,
   nocart=nocart, lablong = lablong,lablat = lablat,label = label,cex.lab=cex.lab, symbol = pch,method = "pairwise",
   axis=axes,legmap=legmap,legends=legends)

}

####################################################
# trier les observations
####################################################


 ordering <- function()
 {
   orderselect<<-!orderselect

   if (orderselect) tkmessageBox(message="Degree of isolation on the x-axis")
   if (!orderselect) tkmessageBox(message="Rank on the x-axis")


   if(orderselect)
   {
    theta<<-thetaref
    absvar<<-absvaref
  #  obs<<-obsref
  #  W<<-Wref
    }
   else
   {
    labvar=c("Rank",ylab)
    ind0<-which(theta==0,arr.ind=TRUE)
    theta[idxg$ix,]<<-t(matrix(rep(1:n,each=length(idxg$ix)),length(idxg$ix),length(idxg$ix)))

    theta[ind0]<<-0
  #   print(idxg$ix)
   #  print(theta[1:10,1:10])
  #   theta <- t(theta)
   #  print(theta[1:10,1:10])
     theta<<-theta

    #print(absvar[1:10,1:10])
    #print(idxg$ix)
    #print(absvar[1:10,1:10])
    
#    long<<-long[idxg$ix]
#    lat<<-lat[idxg$ix]
   # obs[MDglobalTF,]<<-FALSE
   # W[MDglobalTF,]<<-0
       }

   graphique(var1 = theta, var2 = absvar, obs = obs,num = 3, graph = "pairwise", labvar = labvar,
   couleurs=col,symbol = pch, direct=propneighb)

   carte(long = long, lat = lat, obs = obs,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,
   nocart=nocart, lablong = lablong,lablat = lablat,label = label,cex.lab=cex.lab, symbol = pch,method = "pairwise",
   axis=axes,legmap=legmap,legends=legends)

}
####################################################
# sélection d'un polygone sur l'angleplot
####################################################


   
 polyfunc <- function() 
 {
   if (graf=="Neighbourplot1") SGfunc()
   graf<<-"pairwise"
   
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

      if (length(polyX)>0)
      {
        lines(polyX, polyY)
      }
    }  
     
   polyX <- c(polyX, polyX[1])
   polyY <- c(polyY, polyY[1])

  if (length(polyX)>0)
    {
     lines(polyX, polyY)
        for (i in 1:length(long)) 
         {
          def <- inout(cbind(theta[, i], absvar[, i]),cbind(polyX, polyY), bound = TRUE)       
          obs[def,i ] <<- !obs[def,i ]    
         }

    if(!outselect)
     {
      obs[MDglobalTF,]<<-FALSE
     }

    if(!outselect2)
     {
      obs[!MDglobalTF,]<<-FALSE
     }

    #  diag(obs)<<-FALSE
      
   graphique(var1 = theta, var2 = absvar, obs = obs, num = 3,graph = "pairwise", labvar = labvar,couleurs=col,
   symbol = pch, direct=propneighb)
   
   carte(long = long, lat = lat, obs = obs,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,
   nocart=nocart, lablong = lablong,lablat = lablat,label = label,cex.lab=cex.lab, symbol = pch,
   method = "pairwise",axis=axes,legmap=legmap,legends=legends)

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
    carte(long=long, lat=lat,criteria=criteria,buble=buble,cbuble=z,nointer=nointer,obs=obs,
    lablong=lablong, lablat=lablat,method="pairwise",label=label,cex.lab=cex.lab, symbol=pch,
    carte=carte,nocart=nocart,axis=axes,legmap=legmap,legends=legends) 
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
    obs<<-matrix(FALSE,nrow=length(long), ncol=length(long));
    
    carte(long=long, lat=lat,criteria=criteria,buble=buble,cbuble=z,nointer=nointer,obs=obs,
    lablong=lablong, lablat=lablat,method="pairwise",label=label,cex.lab=cex.lab, symbol=pch,
    carte=carte,nocart=nocart,axis=axes,legmap=legmap,legends=legends) 
    
    graphique(var1=theta, var2=absvar, obs=obs, num=3, graph="pairwise", labvar=labvar,
    couleurs=col,symbol=pch, direct=propneighb)
}



####################################################
# quitter l'application
####################################################

quitfunc<-function() 
{
    tclvalue(fin)<<-TRUE
#    graphics.off();
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
    carte(long=long, lat=lat,criteria=criteria,buble=buble,cbuble=z,nointer=nointer,obs=obs,
    lablong=lablong, lablat=lablat,method="pairwise",label=label,cex.lab=cex.lab, symbol=pch,
    carte=carte,nocart=nocart,axis=axes,legmap=legmap,legends=legends) 
   
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
  res2<-choix.bubble(buble,cbind(chi2.quant,listvar),c("chi2.quant",listnomvar),legends)
  
  buble <<- res2$buble
  legends <<- res2$legends
  z <<- res2$z
  legmap <<- res2$legmap

  if(legends[[1]])
  {if((legmap[length(legmap)]=="chi2.quant"))
   {
    legmap<<-c(legmap,paste(">",round(alphab[1],2)),paste(round(alphab[2],2),"-",round(alphab[1],2)),
    paste(round(alphab[3],2),"-",round(alphab[2],2)),paste(round(alphab[4],2),"-",round(alphab[3],2)),
    paste("<",round(alphab[4],2)),"Mahalanobis")
   }
  }

  carte(long=long, lat=lat,criteria=criteria,buble=buble,cbuble=z,nointer=nointer,obs=obs,
  lablong=lablong, lablat=lablat,method="pairwise",label=label,cex.lab=cex.lab, symbol=pch,
  carte=carte,nocart=nocart,axis=axes,legmap=legmap,legends=legends) 

}



####################################################
# Représentation graphique
####################################################

carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label,cex.lab=cex.lab, 
      symbol=pch,method="pairwise",axis=axes,legends=legends)

   graphique(var1 = theta, var2 = absvar, obs = obs,num = 3, graph = "pairwise", labvar = labvar,
   couleurs=col,symbol = pch, direct=propneighb)

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

point.but <- tkbutton(tt, text="  Point  ", command=pointfunca);
poly.but <- tkbutton(tt, text="  Polygon  ", command=polyfunca);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))

labelText1 <- tclVar("Work on the graph")
label1 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)

point.but <- tkbutton(tt, text="  Point  ", command=pointfunc);
poly.but <- tkbutton(tt, text=" Polygon ", command=polyfunc);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))

label1 <- tclVar("To stop selection, let the cursor on the active graph, click on the right button of the mouse and stop")
label11 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(label1))
tkconfigure(label11, textvariable=label1)
tkgrid(label11,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText70 <- tclVar("Only Global outliers        Without Global Outliers")
label70 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText70))
tkconfigure(label70, textvariable=labelText70)
tkgrid(label70,columnspan=2)

point.but70 <- tkbutton(tt, text="  On/Off  ", command=outlier);
poly.but70 <- tkbutton(tt, text="  On/Off  ", command=outlier2);
tkgrid(point.but70, poly.but70)
tkgrid(tklabel(tt,text="    "))


labelText71 <- tclVar("Rank on the x-axis")
label71 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText71))
tkconfigure(label71, textvariable=labelText71)
tkgrid(label71,columnspan=2)

noint10.but <- tkbutton(tt, text="  On/Off  ", command=ordering);
tkgrid(noint10.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText7 <- tclVar("Preselected site")
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

