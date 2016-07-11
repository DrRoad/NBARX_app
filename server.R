

shinyServer(function(input, output){
  
  data.load <- reactive({
    input$get
    isolate({
      d <- if(input$days==1) 1 else 5
      data <- get.yahoo.intraday(input$symb, days = d, var="volume") 
    })
    m <- 10^floor(log10(max(data$y,na.rm=T)))/100
    # m <- 1
    data$y <- round(data$y/m)
    data$scale <- m
    return(data)
  })
  
  plot_window <- reactive({
    data <- data.load()
    days_loaded <- data$days
    y <- window(data$y,
                start=data$ex_hours$open_time[days_loaded - input$days+1],
                end=data$time_range$end,extend=T)
    return(list("y"=y,"name"=data$name, "scale"=data$scale,
                "ex_hours"=data$ex_hours,
                "window_range"=c(start(y),end(y)),"days_loaded"=days_loaded))
  })
  
  output$volume_plot <- renderPlot({
    data <- plot_window()
    days_loaded <- data$days_loaded
    volm_plot <- data$y
    filt.PAR.out <- filt.PAR()
    filt.NBAR.out <- filt.NBAR()
    # filter.PAR.lam <- window(filt.PAR.out$lambda.hat,start=input$plotdates[1],end=input$plotdates[2])
    filter.PAR.lam <- filt.PAR.out$lambda.hat
    filter.NBAR.lam <- filt.NBAR.out$lambda.hat

    plot.intra(volm_plot,ex_hours=data$ex_hours[(days_loaded - input$days+1):days_loaded,],type="S",lwd=2,xlab="",ylab=paste("Counts,",data$scale),main=data$name)
    plot.intra(filter.PAR.lam,new=F,lwd=2,col=col_vec[1]) 
    plot.intra(filter.NBAR.lam,new=F,lwd=2,col=col_vec[2])
    
    forecast_shift <- max( no_overnight_space(time(na.omit(volm_plot))) )
    plot.intra(filt.PAR.out$for.lambda.hat,new=F,x_shift=forecast_shift,lty=2,lwd=2,col=col_vec[1]) 
    plot.intra(filt.PAR.out$for.l.conf,new=F,x_shift=forecast_shift,lty=2,lwd=1,col=col_vec[1])
    plot.intra(filt.PAR.out$for.h.conf,new=F,x_shift=forecast_shift,lty=2,lwd=1,col=col_vec[1])
    
    plot.intra(filt.NBAR.out$for.lambda.hat,new=F,x_shift=forecast_shift,lty=2,lwd=2,col=col_vec[2]) 
    plot.intra(filt.NBAR.out$for.l.conf,new=F,x_shift=forecast_shift,lty=2,lwd=1,col=col_vec[2])
    plot.intra(filt.NBAR.out$for.h.conf,new=F,x_shift=forecast_shift,lty=2,lwd=1,col=col_vec[2])
    
    x_leg <- sum(range(plot.intra.xcord(volm_plot)))*0.5
    rng <- range(volm_plot,filt.PAR.out$for.lambda.hat, filt.PAR.out$for.l.conf, filt.PAR.out$for.h.conf,
                 filt.NBAR.out$for.lambda.hat, filt.NBAR.out$for.l.conf, filt.NBAR.out$for.h.conf)
    y_leg <- rng[1] - diff(rng)*0.2
    par(xpd=TRUE)
    legend(x_leg,y_leg,c("Data","lambda_PAR","lambda_NBAR"),col=c(1,col_vec[1:2]),lwd=2,ncol=3,bty="n",xjust=0.5)
  })
  
  output$qq_plot <- renderPlot({
    input$est
    isolate({
      y <- data.load()$y
      nu <- get.par.list.NBAR()$nu
    })
    PAR.res.out <- pseudo_residuals.P(y,filt.PAR()$lambda.hat)
    NBAR.res.out <- pseudo_residuals.NB(y,lambda.hat=filt.NBAR()$lambda.hat,nu.hat=nu)
    qq_plot(x=PAR.res.out$res.m,type="p",pch=20,cex=0.5,conf=c("KS","Sim"),col=col_vec[1],xlim=c(-4,4),ylim=c(-4,4))
    qq_plot(x=NBAR.res.out$res.m, plot="add",pch=20,cex=0.5,col=col_vec[2])
    legend(-4,4,c("PAR","NBAR"),col=col_vec,pch=20)
  })
  
  est.func <- function(loglike,repam,repam.inv,nu=NULL){
    input$est
    isolate({
      p <- input$p
      q <- input$q
      y <- na.omit(coredata(data.load()$y))
      N <- length(y)
      neg_par <- input$season
    })
    omega0 <- mean(y)*(1-0.3-0.5)
    theta0 <- c(omega0,rep(0.3/p,p),rep(0.5/q,q),nu)+runif(1+p+q+length(nu),-0.05/max(p,q),0.05/max(p,q))
    gamma0 <- repam.inv(theta0,neg_par=neg_par)
    assign("iter",0, envir = .GlobalEnv)
    withCallingHandlers({
      shinyjs::html("optim_mess", "")
      optim.out <- optim(par=gamma0, fn=loglike, gr=NULL,p=p,q=q, y=y,neg_par=neg_par, hessian=T,method="BFGS",control=list(trace=2,maxit=300,reltol=1e-6,REPORT=5))
    },message = function(m) {shinyjs::html(id = "optim_mess", html = m$message, add = T)})
    return(list("N"=N,"out"=optim.out))
  }
  
  
  est.PAR <- reactive(est.func(loglike.PAR,repam.PAR,repam.inv.PAR))
  est.NBAR <- reactive(est.func(loglike.NBAR,repam.NBAR,repam.inv.NBAR,nu=10))
  
  get.par.vec.PAR <- reactive(repam.PAR(est.PAR()$out$par,neg_par=input$season))
  get.par.vec.NBAR <- reactive(repam.NBAR(est.NBAR()$out$par,neg_par=input$season))

  get.par.list.PAR <- reactive(par.vec2list.PAR(get.par.vec.PAR(),p=input$p,q=input$q))
  get.par.list.NBAR <- reactive(par.vec2list.NBAR(get.par.vec.NBAR(),p=input$p,q=input$q))
  
  filt.PAR <- reactive({
    input$forecast
    input$est
    input$days
    isolate({
      window_range <- plot_window()$window_range
      y <- na.omit(plot_window()$y)
      if(plot_window()$days_loaded == 1){
        forecast_time <- seq(end(y),window_range[2],by="60 secs")
      } else {
        forecast_time <- seq(end(y),window_range[2],by="300 secs")
      }
      theta <- get.par.vec.PAR()
      p <- input$p
      q <- input$q
      # h <- input$h
      h <- length(forecast_time)
    })
    filter.PAR(theta,p=p,q=q,y=y,conf=.90,zoo=T,h=h,for.time=forecast_time)
    })
  
  filt.NBAR <- reactive({
    input$forecast
    input$est
    input$days
    isolate({
      window_range <- plot_window()$window_range
      y <- na.omit(plot_window()$y)
      if(plot_window()$days_loaded == 1){
        forecast_time <- seq(end(y),window_range[2],by="60 secs")
      } else {
        forecast_time <- seq(end(y),window_range[2],by="300 secs")
      }
      theta <- get.par.vec.NBAR()
      p <- input$p
      q <- input$q
      # h <- input$h
      h <- length(forecast_time)
    })
    filter.NBAR(theta,p=p,q=q,y=y,conf=.90,zoo=T,h=h,for.time=forecast_time)
    })
  
  summary.PAR <- reactive({
    input$est
    isolate({
      N <- est.PAR()$N
      out <- est.PAR()$out
      neg_par <- input$season
      par.names <- c("omega",paste0("alpha_",1:input$p),paste0("beta_",1:input$q))
    })
    summary.avg_mle(N,out,repam.PAR,par.names = par.names,neg_par=neg_par)
    })
  
  summary.NBAR <- reactive({
    input$est
    isolate({
      N <- est.NBAR()$N
      out <- est.NBAR()$out
      neg_par <- input$season
      par.names <- c("omega",paste0("alpha_",1:input$p),paste0("beta_",1:input$q),"nu")
    })
    summary.avg_mle(N,out,repam.NBAR,par.names = par.names,neg_par=neg_par)
    })
  
  output$par.par <- renderTable({
    return(summary.PAR()$par.table)
  })
  
  output$nbar.par <- renderTable({
    return(summary.NBAR()$par.table)
  })
  
  output$info <- renderTable({
    table <- summary.PAR()$info_crit.table
    table <- cbind(table,summary.NBAR()$info_crit.table)
    colnames(table) <- c("PAR","NBAR")
    return(table)
  })
  
})

