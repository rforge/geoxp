`carte` <-
function (long, lat, obs,criteria=NULL,buble=FALSE,cbuble=NULL,nointer=FALSE, carte=NULL,nocart=FALSE,
label="",cex.lab=NULL,symbol=16, lablong="", lablat="", method="", W=NULL,couleurs="blue", classe=NULL,
legmap=NULL,legends=list(FALSE,FALSE),labmod="",axis=FALSE)
{

####################################################
# définition des paramètres graphiques
####################################################

dev.set(2)

x.lim=c(min(long,na.rm=TRUE),max(long,na.rm=TRUE)) 
y.lim=c(min(lat,na.rm=TRUE),max(lat,na.rm=TRUE))
 
asp=1/cos((mean(y.lim) * pi)/180)
 
leg.symb<-symbol
 
if(asp>1.40||asp<0.6) asp=1 
  
if ((method == "Cluster")||(method == "Quadrant")||(method == "Neighbourplot1"))
{ 
  if (length(symbol)!=length(levels(as.factor(classe))))
   { symbol=rep(symbol[1],length(long))
    # if(length(obs)==length(long))
   #  {
     symbol[!obs]=16
      leg.symb<-rep(16,length(levels(as.factor(classe))))
   #  }
    # else
    # {symbol[unique(which(obs==TRUE,arr.ind=TRUE)[1])]=leg.symb[1]}     
   }
   else
   { if((method == "Cluster")||(method == "Quadrant"))
     {symbol<-symbol[as.factor(classe)]}
   }
   
   if (length(couleurs)!=length(levels(as.factor(classe))))
   {couleurs=rep(couleurs[1],length(levels(as.factor(classe))))}  
   
 }

####################################################
# Initialisation des bubbles
####################################################

if(buble && (length(cbuble)!=0))
{
  if(min(cbuble)==0)
   {cbuble[which(cbuble==0)]<-min(cbuble[which(cbuble!=0)])/2}

  if(length(legmap)>0)
   {
    if(as.numeric(legmap[3])==0)
     {legmap[3]<-min(cbuble)}
   }
}
else
   {cbuble=rep(0.5,length(long))
    if(class(obs)=="matrix")
     { if(method == "Neighbourplot1") 
       {cbuble[intersect(which(obs==TRUE,arr.ind=TRUE)[,1],which(obs==TRUE,arr.ind=TRUE)[,2])]=1}
       else
        { if(method=="Neighbourplot2")
           {cbuble[unique(which(obs==TRUE,arr.ind=TRUE)[,1])]=1 }
          else
           {cbuble[unique(which(obs==TRUE,arr.ind=TRUE)[,1][which(obs==TRUE,arr.ind=TRUE)[,1]==which(obs==TRUE,arr.ind=TRUE)[,2]])]=1}
        }
     }  
    else
     {cbuble[which(obs==TRUE)]=1 }
    }
  

 


####################################################
# dessin des contours des unités spatiales
####################################################

if(class(carte)!="NULL")
{
if(class(carte)!="list")
{
 x.cont.lim=range(carte[,1],na.rm=TRUE)
 y.cont.ylim=range(carte[,2],na.rm=TRUE)
}
else
 {kol<-length(carte)
 xk.cont.lim<-NULL
 yk.cont.ylim<-NULL
  for (k in 1:kol)
  {cartek<-carte[[k]]
    xk.cont.lim=c(xk.cont.lim,range(cartek[,1],na.rm=TRUE))
    yk.cont.ylim=c(yk.cont.ylim,range(cartek[,2],na.rm=TRUE))
  }
 x.cont.lim=range(xk.cont.lim,na.rm=TRUE)
 y.cont.ylim=range(yk.cont.ylim,na.rm=TRUE)
 }
 asp.cont=1/cos((mean(y.cont.ylim) * pi)/180)
 if(asp.cont>1.40||asp.cont<0.6) asp.cont=1
}



if(nocart)
{                                                                                        
plot(x.cont.lim,y.cont.ylim,"n", xlab=lablong, ylab=lablat, tcl=-.25, las=1, cex=cbuble,asp=asp.cont,axes=axis)
  if(class(carte)!="list")
   {
    n <- nrow(carte);
    abs1 <- carte[1:(n-1),1];
    ord1 <- carte[1:(n-1),2];
    abs2 <- carte[2:n,1];
    ord2 <- carte[2:n,2];
  #  points(long,lat,"n", xlab=lablong, ylab=lablat,xaxt="n",yaxt="n", bty="n", xlim=c(min(long)-(max(long)-min(long))/10, max(long)+(max(long)-min(long))/10), ylim=c(min(lat)-(max(lat)-min(lat))/10, max(lat)+(max(lat)-min(lat))/10));
    segments(abs1,ord1,abs2,ord2,col="black")
   }
   else
   {
      for (k in 1:kol)
       {cartek<-carte[[k]]
        n <- nrow(cartek);
        abs1 <- cartek[1:(n-1),1];
        ord1 <- cartek[1:(n-1),2];
        abs2 <- cartek[2:n,1];
        ord2 <- cartek[2:n,2];
       #  points(long,lat,"n", xlab=lablong, ylab=lablat,xaxt="n",yaxt="n", bty="n", xlim=c(min(long)-(max(long)-min(long))/10, max(long)+(max(long)-min(long))/10), ylim=c(min(lat)-(max(lat)-min(lat))/10, max(lat)+(max(lat)-min(lat))/10));
        segments(abs1,ord1,abs2,ord2,col="black")
       }
   }
}  
else
{
####################################################
# Initialisation du plot
####################################################

plot(x.lim,y.lim,"n", xlab=lablong, ylab=lablat, tcl=-.25, las=1, cex=cbuble,asp=asp,axes=axis)
}
                 
  #       polygon(fleuve,col='royalblue',border="white")
  #       polygon(fleuve2,col='white',border="white")
  #      lines(cont.xx.center)
####################################################
# dessin des points
####################################################

if ((method == "Cluster")||(method == "Quadrant"))
 { 
  points(long[!obs],lat[!obs],col=couleurs[as.factor(classe)][!obs],pch=symbol[!obs],cex=cbuble[!obs])                     
 }
else
 { if (method == "Neighbourplot1")
   {obs2<-diag(obs)
   points(long[!obs2],lat[!obs2],col=couleurs[as.factor(classe)][!obs2],pch=symbol[as.factor(classe)[!obs2]],
   cex=cbuble[!obs2])                     
   }
   else
   {points(long[!obs],lat[!obs],col="blue",pch=16,cex=cbuble[!obs])} 
 } 
#points(long[!obs],lat[!obs],col=couleurs[as.factor(classe)[!obs]],pch=symbols[as.factor(classe)[!obs]],
#          cex=cbuble[!obs])   
  


####################################################
# Pour la légende
####################################################

 if(legends[[1]])
 {
#  legend(max(long)-(max(long)-min(long))/9,min(lat)+(max(lat)-min(lat))/9, c(legmap[1],legmap[2],legmap[3]),pch = 16, pt.cex=c(2,1.125,0.25))
#  text(max(long)-(max(long)-min(long))/15,min(lat)+(max(lat)-min(lat))/7,legmap[4])
 
 if((method == "pairwise")&(legmap[length(legmap)]=="Mahalanobis"))
  {
  legend(legends[[3]]$x,legends[[3]]$y, c(legmap[7],legmap[8],legmap[9],legmap[10],legmap[11]),
  title=legmap[12],pch = 16,cex=cex.lab,
  pt.cex=c(as.numeric(legmap[1]),as.numeric(legmap[2]),as.numeric(legmap[3]),
  as.numeric(legmap[4]),as.numeric(legmap[5])))
 }
 else
 {
  legend(legends[[3]]$x,legends[[3]]$y, c(legmap[4],legmap[5],legmap[6]),
  title=legmap[7],pch = 16,cex=cex.lab, 
  pt.cex=c(as.numeric(legmap[1]),as.numeric(legmap[2]),as.numeric(legmap[3])))
 # text(legends[3]$x,legends[3]$y,legmap[4])
 }



#symbols(legends[[3]]$x,legends[[3]]$y+as.numeric(legmap[3]),circle=as.numeric(legmap[3])/1.1,inches=FALSE,add=TRUE,bg=grey(.5),fg=grey(.2))
#symbols(legends[[3]]$x,legends[[3]]$y+as.numeric(legmap[2]),circle=as.numeric(legmap[2])/1.1,inches=FALSE,add=TRUE,bg=grey(.5),fg=grey(.2))
#symbols(legends[[3]]$x,legends[[3]]$y+as.numeric(legmap[1]),circle=as.numeric(legmap[1])/1.1,inches=FALSE,add=TRUE,bg=grey(.5),fg=grey(.2))


 }

 if(legends[[2]])
 {
legend(legends[[4]]$x,legends[[4]]$y, labmod,cex=cex.lab,col=couleurs,pch = leg.symb, pt.cex=1.125)}




####################################################
# dessin des points sélectionnés
####################################################

if(length(long[obs])!=0)
{

 
    #dans le cas général
    if ((method == "") || (method == "Cluster")||(method == "Quadrant"))
    {    
    
      if ((method == "Cluster")||(method == "Quadrant"))            
           {
             if (length(unique(levels(couleurs)))!=length(levels(as.factor(classe))))
               {couleurs=rep("blue",length(levels(as.factor(classe))))
                couleurs[which(obs==TRUE)]="red" 
                points(long[obs],lat[obs], col=couleurs[obs],pch=symbol[obs],cex=cbuble[obs])}
 
              else   
                 {points(long[obs],lat[obs], col=couleurs[as.factor(classe)[obs]],pch=symbol[obs],cex=cbuble[obs])}
          
  #        if((length(levels(as.factor(symbol)))==1)&&(length(levels(as.factor(couleurs)))!=1) )
#           {points(long[obs],lat[obs], col="red",pch=1,cex=cbuble[obs])}
 #         else
#           {points(long[obs],lat[obs], col="red",pch=symbol[as.factor(classe)[obs]],cex=cbuble[obs])}
           
           }      
      else  
            {points(long[obs],lat[obs], col="red", pch=symbol, cex=cbuble[obs])}


      #écriture des labels de chaque point sélectionné
      text(long[obs], lat[obs], label[obs], col="black", cex=cex.lab, font=3,adj=c(0.75,-0.75));
    }

#    if (method == "Quadrant")
#    {
#        if (symbol == 1)
#        {
#            points(long[obs],lat[obs], col=couleurs[obsq[obs]],pch=symbols[obsq[obs]], cex=1.5);
#        }
#        else
#        {
 #           points(long[obs],lat[obs],col=couleurs[obsq[obs]],pch=16);
 #       }
            
  #  }
    #dans le cas où on sélectionne un point sur la carte dans la fonction Neighbourmap
    if ((method == "Neighbourplot1") || (method == "Neighbourplot3") )
    { 
     ifelse(method == "Neighbourplot1",ppp<-symbol[as.factor(classe)],ppp<-rep(symbol[1],length(long)))
    # ppp<-symbol

        for (j in 1:length(long))
        {
            for (i in 1:length(long))
            {
                if (length(obs) == length(long))
                {
                  if ((W[j,i] != 0) && (obs[j]))
                   { 
                      points(long[j],lat[j],col="red",cex=cbuble[j],pch=ppp[j])                        
                        #écriture des labels de chaque point sélectionné                   
                      segments(long[j], lat[j], long[i], lat[i], col="red");
                      text(long[j], lat[j], label[j], cex=cex.lab,font=3,adj=c(0.75,-0.75));  
                   }                                      
                }
                else 
                {
                    if ((obs[j,i]) & (W[j,i] != 0))
                    {
                      points(long[j],lat[j],col="red",cex=cbuble[j],pch=ppp[j])                      
                      #écriture des labels de chaque point sélectionné                         
                      segments(long[j], lat[j], long[i], lat[i], col="red");
                      text(long[j], lat[j], label[j], cex=cex.lab,font=3,adj=c(0.75,-0.75));
                    }                                                                                            
                }
        }
     }
   }

    #dans le cas où on sélectionne un point sur le graphique dans la fonction Neighbourmap
    if (method == "Neighbourplot2")
    {
        for (j in 1: length(long))
        {
            for (i in 1:length(long))
            {
                if (obs[j,i])
                {  
                 points(long[j],lat[j],col="red",pch=symbol[1],cex=cbuble[j])
 #                points(long[i],lat[i],col="red",pch=16,cex=cbuble[i])
                 segments(long[j], lat[j], long[i], lat[i], col="red")
                 #écriture des labels de chaque point sélectionné
                 text(long[i], lat[i], label[i], cex=cex.lab,adj=c(0.75,-0.75)) 
                 #écriture des labels de chaque point sélectionné
                 text(long[j], lat[j], label[j], cex=cex.lab,adj=c(0.75,-0.75)) 
                }
            }
        }
    }

    if ((method == "Angleplot") || (method == "Variocloud")|| (method == "pairwise"))
    {
        x0<-NULL
        y0<-NULL
        x1<-NULL
        y1<-NULL

       #print(cbuble)
        for (j in 1:length(long))
        {
            for (i in 1:length(long))
            {
                if (obs[j,i])
                {
                  if(length(levels(as.factor(cbuble)))>1)
                  {points(long[j],lat[j],col="red",pch=symbol, cex=cbuble[j]);
                   points(long[i],lat[i],col="red",pch=symbol,cex=cbuble[i]); 
                  }
                  else
                  {
                   points(long[j],lat[j],col="red",pch=symbol, cex= 0.8);
                   points(long[i],lat[i],col="red",pch=symbol,cex= 0.8);                 
                  }  
                  
                  x0<-c(x0,long[j])
                  y0<-c(y0,lat[j])
                  x1<-c(x1,long[i])
                  y1<-c(y1,lat[i])

                 #écriture des labels de chaque point sélectionné
                   text(long[i], lat[i], label[i], cex=cex.lab,adj=c(0.75,-0.75)); 
                   text(long[j], lat[j], label[j], cex=cex.lab,adj=c(0.75,-0.75));            
                }
            }
        }
       segments(x0, y0, x1, y1, col="green")
    }
    
    if (method == "Scatter3d")
    {
        if (symbol == 1)
        {
            points(long[obs],lat[obs], col="green", pch=10, cex=1.5);
        }
        else
        {
            points(long[obs],lat[obs],col="green",pch=16);
        }
    }
  }


####################################################
# dessin des points selctionnés par critère
####################################################

if(nointer)
{points(long[criteria],lat[criteria], pch="X",cex=0.7)}

 }

