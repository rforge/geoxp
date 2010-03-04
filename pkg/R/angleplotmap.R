`angleplotmap` <-
function (long, lat, var, quantiles=NULL,listvar=NULL, listnomvar=NULL, criteria=NULL,
carte = NULL, label = "",cex.lab=1,pch = 16,col="blue", xlab = "angle",ylab="absolut magnitude",
lablong = "", lablat = "", axes=FALSE) 
{
 # initialisation
  nointer<-FALSE
  nocart<-FALSE
  buble<-FALSE
  legends<-list(FALSE,FALSE,"","")
  z<-NULL
  legmap<-NULL
  inout=NULL
  labvar=c(xlab,ylab)
  obs <- matrix(FALSE, nrow = length(long), ncol = length(long))
  graphics.off()
    
# Transformation d'un data.frame en matrix

if((length(listvar)>0) && (dim(as.matrix(listvar))[2]==1)) listvar<-as.matrix(listvar)

# ouverture des fenêtres graphiques
dev.new()
dev.new()
fin <- tclVar(FALSE)

# calcul des matrices theta et absvar

    long1 <- matrix(rep(t(long), length(long)), ncol = dim(t(long))[2],byrow = FALSE)
    long2 <- matrix(rep(t(long), length(long)), ncol = dim(t(long))[2],byrow = TRUE)
    lat1 <- matrix(rep(t(lat), length(lat)), ncol = dim(t(lat))[2],byrow = FALSE)
    lat2 <- matrix(rep(t(lat), length(lat)), ncol = dim(t(lat))[2],byrow = TRUE)
    theta <- matrix(0, nrow = length(long), ncol = length(long))
    numer <- lat2 - lat1
    denom <- long2 - long1
   

   for (i in 1:length(long)) 
       {
        for (j in 1:length(long)) 
           {
             if (denom[i, j] == 0) 
               {
                theta[i, j] <- pi/2
               }
             else 
               {
                theta[i, j] <- atan(numer[i, j]/denom[i, j])
               }
           }
       }

    theta[which(theta < 0)] <- theta[which(theta < 0)] + pi
    v1 <- matrix(rep(t(var), length(var)), ncol = dim(t(var))[2],byrow = FALSE)
    v2 <- matrix(rep(t(var), length(var)), ncol = dim(t(var))[2],byrow = TRUE)
    absvar <- abs(v1 - v2)
   

####################################################
#choix des bornes des réglettes (inspiré de la documentation de Matlab)
####################################################


        u1 <- sort(theta)
        u1 <- as.vector(u1)
        z <- seq(1, max(u1), by = (max(u1)/3500))
        z <- round(z)
        z1 <- z[2:length(z)] - z[1:(length(z) - 1)]
        h <- mean(z1)
        p <- 1/(1 + (h^3/6))
        p1 <- 1/(1 + (h^3/60))
        p2 <- 1/(1 + (h^3/0.6))
        alpha <- (1 - p)/p
        borne1 <- (1 - p1)/p1
        borne2 <- (1 - p2)/p2

####################################################
# sélection d'un point sur l'angleplot
####################################################

 pointfunc <- function() 
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

   obs <<- selectstat(var1 = theta, var2 = absvar, obs = obs,Xpoly = loc[1], Ypoly = loc[2],
   method = "AnglePoint",long = long, lat = lat)
   
   diag(obs)<<-FALSE
   
   graphique(var1 = theta, var2 = absvar, obs = obs,num = 3, graph = "Angleplot", labvar = labvar,
   couleurs=col,symbol = pch, quantiles = quantiles,alpha1 = alpha)
  
   carte(long = long, lat = lat, obs = obs,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,
   nocart=nocart, lablong = lablong,lablat = lablat,label = label,cex.lab=cex.lab, symbol = pch,method = "Angleplot",
   axis=axes,legmap=legmap,legends=legends) 
 }
}



####################################################
# sélection d'un polygone sur l'angleplot
####################################################


 polyfunc <- function() 
 {
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

   graphique(var1 = theta, var2 = absvar, obs = obs, num = 3,graph = "Angleplot", labvar = labvar,couleurs=col,
   symbol = pch,quantiles = quantiles, alpha1 = alpha)
   
   carte(long = long, lat = lat, obs = obs,buble=buble,criteria=criteria,nointer=nointer,cbuble=z,carte=carte,
   nocart=nocart, lablong = lablong,lablat = lablat,label = label,cex.lab=cex.lab, symbol = pch,
   method = "Angleplot",axis=axes,legmap=legmap,legends=legends) 

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
    lablong=lablong, lablat=lablat,method="Angleplot",label=label,cex.lab=cex.lab, symbol=pch,
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
    lablong=lablong, lablat=lablat,method="Angleplot",label=label,cex.lab=cex.lab, symbol=pch,
    carte=carte,nocart=nocart,axis=axes,legmap=legmap,legends=legends) 
    
    graphique(var1=theta, var2=absvar, obs=obs, num=3, graph="Angleplot", labvar=labvar,
    couleurs=col,symbol=pch,  quantiles=quantiles, alpha1=alpha);
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
    lablong=lablong, lablat=lablat,method="Angleplot",label=label,cex.lab=cex.lab, symbol=pch,
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
  res2<-choix.bubble(buble,listvar,listnomvar,legends)
  
  buble <<- res2$buble
  legends <<- res2$legends
  z <<- res2$z
  legmap <<- res2$legmap
  
  carte(long=long, lat=lat,criteria=criteria,buble=buble,cbuble=z,nointer=nointer,obs=obs,
  lablong=lablong, lablat=lablat,method="Angleplot",label=label,cex.lab=cex.lab, symbol=pch,
  carte=carte,nocart=nocart,axis=axes,legmap=legmap,legends=legends) 

}


####################################################
# Pour le alpha 
####################################################

    refresh.code <- function(...)
     {
        alpha <<- slider1(no = 1)
        graphique(var1 = theta, var2 = absvar, obs = obs, num = 3, 
        graph = "Angleplot",couleurs=col, labvar = labvar, symbol = pch, 
        quantiles = quantiles, alpha1 = alpha)
    }

####################################################
# Représentation graphique
####################################################

carte(long=long, lat=lat, obs=obs, lablong=lablong, lablat=lablat, label=label,cex.lab=cex.lab, 
      symbol=pch,method="Angleplot",axis=axes,legends=legends) 

   graphique(var1 = theta, var2 = absvar, obs = obs,num = 3, graph = "Angleplot", labvar = labvar,
   couleurs=col,symbol = pch, quantiles = quantiles,alpha1 = alpha)

####################################################
# création de la boite de dialogue
####################################################

if(interactive())
{
tt <- tktoplevel()

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

   if(length(quantiles)!=0)
{
        slider1(tt, refresh.code, c("Quantile smooth spline parameter"), 
            c(borne1), c(borne2), c((borne2 - borne1)/100), c(alpha))
   
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

