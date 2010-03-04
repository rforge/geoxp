`slider2` <-
function(fenetre,refresh.code,names,minima,maxima,resolutions,starts,no=0)
{
  if (!exists("slider.env")) slider.env<<-new.env()
  if (no != 0) return(as.numeric(tclvalue(get(paste("slider",no,sep=""),env=slider.env))))
    
   for(i in seq(names))
    {
     eval(parse(text=paste("assign(\"slider",i,"\",tclVar(starts[i]),env=slider.env)",sep="")))
    }

    for(i in seq(names))
    {
      lab<-tklabel(fenetre, text=names[i], width="25")
      sc<-tkscale(fenetre, command=refresh.code, from=minima[i], to=maxima[i],showvalue=TRUE, resolution=resolutions[i], orient="horiz")
      assign("sc",sc,env=slider.env);
      tkgrid(lab,sc)
      eval(parse(text=paste("tkconfigure(sc,variable=slider",i,")",sep="")),env=slider.env);
    }
}

