`slider1` <-
function(fenetre, refresh.code, names.slide, minima, maxima, resolutions, starts, no=0)
{
 p<-length(names.slide)
 # once the slider created, the function returns the value(s) of the selected slider(s)
 if (no != 0)
 { if(p==1)
   {res<-as.numeric(tclvalue(get(paste("slider.value",1,sep=""),env=baseenv())))}
   else
   {if(exists("slider.value2",env=baseenv()))
    {res<-list(alpha11=as.numeric(tclvalue(get(paste("slider.value",1,sep=""),env=baseenv()))),
    alpha21=as.numeric(tclvalue(get(paste("slider.value",2,sep=""),env=baseenv()))))
    }
    else
    {res<-list(alpha11=20,alpha21=20)
    }
   }
   return(res)
 }
 
 # creation of the slider on the Tk window
for(i in 1:p)
  {
   eval(parse(text=paste("assign(\"slider.value",i,"\",tclVar(starts[i]),env=baseenv())",sep="")))

   lab<-tklabel(fenetre, text=names.slide[i], width="25")
   sc<-tkscale(fenetre, command=refresh.code, from=minima[1], to=maxima[1],showvalue=TRUE, resolution=resolutions[1], orient="horiz")
   assign("sc", sc, env=baseenv())
   tkgrid(lab, sc)
   eval(parse(text=paste("tkconfigure(sc,variable=slider.value",i,")",sep="")),env=list(baseenv(),.GlobalEnv));
   tkgrid.configure(lab, column=0, columnspan=5, sticky="w")
   tkgrid.configure(sc, column=6, columnspan=5, sticky="w")
  }
}

