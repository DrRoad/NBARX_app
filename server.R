

shinyServer(function(input, output){
  
  data.load <- reactive({
    input$get
    isolate(symb <- input$symb)
    d <- if(input$days == 1) 1 else 5
    data <- get.yahoo.intraday(symb, days = d, var="volume") 
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
    filt.PARX.out <- filt.PARX()
    filt.NBARX.out <- filt.NBARX()
    # filter.PAR.lam <- window(filt.PAR.out$lambda.hat,start=input$plotdates[1],end=input$plotdates[2])
    filter.PARX.lam <- filt.PARX.out$lambda.hat
    filter.NBARX.lam <- filt.NBARX.out$lambda.hat

    plot.intra(volm_plot,ex_hours=data$ex_hours[(days_loaded - input$days+1):days_loaded,],type="S",lwd=2,xlab="",ylab=paste0("Trade volume, (",data$scale,")"),main=data$name)
    plot.intra(filter.PARX.lam,new=F,lwd=2,col=col_vec[1]) 
    plot.intra(filter.NBARX.lam,new=F,lwd=2,col=col_vec[2])
    
    forecast_shift <- max( no_overnight_space(time(na.omit(volm_plot))) )
    plot.intra(filt.PARX.out$for.lambda.hat,new=F,x_shift=forecast_shift,lty=2,lwd=2,col=col_vec[1]) 
    plot.intra(filt.PARX.out$for.l.conf,new=F,x_shift=forecast_shift,lty=2,lwd=1,col=col_vec[1])
    plot.intra(filt.PARX.out$for.h.conf,new=F,x_shift=forecast_shift,lty=2,lwd=1,col=col_vec[1])
    
    plot.intra(filt.NBARX.out$for.lambda.hat,new=F,x_shift=forecast_shift,lty=2,lwd=2,col=col_vec[2]) 
    plot.intra(filt.NBARX.out$for.l.conf,new=F,x_shift=forecast_shift,lty=2,lwd=1,col=col_vec[2])
    plot.intra(filt.NBARX.out$for.h.conf,new=F,x_shift=forecast_shift,lty=2,lwd=1,col=col_vec[2])
    
    x_leg <- mean(par("xaxp")[1:2])
    rng <- par("yaxp")[1:2]
    y_leg <- rng[1] - diff(rng)*0.15
    par(xpd=TRUE)
    print(x_leg)
    print(y_leg)
    legend(x_leg,y_leg,c("Data","lambda_PARX","lambda_NBARX"),col=c(1,col_vec[1:2]),lwd=2,ncol=3,bty="n",xjust=0.5)
  })
  
  output$qq_plot <- renderPlot({
    input$est
    isolate({
      y <- data.load()$y
      nu <- get.par.list.NBARX()$nu
    })
    PAR.res.out <- pseudo_residuals.P(y,filt.PARX()$lambda.hat)
    NBAR.res.out <- pseudo_residuals.NB(y,lambda.hat=filt.NBAR()$lambda.hat,nu.hat=nu)
    qq_plot(x=PARX.res.out$res.m,type="p",pch=20,cex=0.5,conf=c("KS","Sim"),
            col=col_vec[1],
            xlim=c(-4,4),ylim=c(-4,4),
            main="Q-Q plot, normal pseudo-residuals")
    qq_plot(x=NBARX.res.out$res.m, plot="add",pch=20,cex=0.5,col=col_vec[2])
    legend(-4,4,c("PARX","NBARX"),col=col_vec,pch=20)
  })
  
  est.func <- function(loglike,repam,repam.inv,nu=NULL,gam=NULL){
    input$est
    isolate({
      p <- input$p
      q <- input$q
      y <- na.omit(coredata(data.load()$y))
      sec_market_open <- data.load()$sec_since_market_open  
      # print(sec_market_open)
      x <- cbind(sec_market_open,sec_market_open^2)
      N <- length(y)
      neg_par <- input$season
    })
    omega0 <- mean(y)*(1-0.3-0.5)
    theta0 <- c(omega0,rep(0.3/p,p),rep(0.5/q,q),nu,gam)+runif(1+p+q+length(c(nu,gam)),-0.05/max(p,q),0.05/max(p,q))
    # print(theta0)
    # print(x)
    gamma0 <- repam.inv(theta0,p,q,k_x=2,neg_par=neg_par)
    assign("iter",0, envir = .GlobalEnv)
    withCallingHandlers({
      shinyjs::html("optim_mess", "")
      optim.out <- optim(par=gamma0, fn=loglike, gr=NULL,p=p,q=q, y=y,x=x,
                         fun_x=fun_x_pos,
                         neg_par=neg_par, 
                         hessian=T,
                         method="BFGS",control=list(trace=2,maxit=300,reltol=1e-6,REPORT=5))
    },message = function(m) {shinyjs::html(id = "optim_mess", html = m$message, add = T)})
    # print(loglike)
    # print(optim.out)
    return(list("N"=N,"out"=optim.out))
  }
  
  
  est.PARX <- reactive(est.func(loglike.PARX,repam.PARX,repam.inv.PARX,gam=c(1,1)))
  est.NBARX <- reactive(est.func(loglike.NBARX,repam.NBARX,repam.inv.NBARX,nu=10,gam=c(1,1)))
  
  get.par.vec.PARX <- reactive(repam.PARX(est.PARX()$out$par,p=input$p,q=input$q,k_x=2,neg_par=input$season))
  get.par.vec.NBARX <- reactive(repam.NBARX(est.NBARX()$out$par,p=input$p,q=input$q,k_x=2,neg_par=input$season))

  get.par.list.PARX <- reactive(par.vec2list.PARX(get.par.vec.PARX(),p=input$p,q=input$q,k_x=2))
  get.par.list.NBARX <- reactive(par.vec2list.NBARX(get.par.vec.NBARX(),p=input$p,q=input$q,k_x=2))
  
  filt.PARX <- reactive({
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
      theta <- get.par.vec.PARX()
      # print(theta)
      p <- input$p
      q <- input$q
      sec_market_open <- c(sec_since_market_open_y(y, plot_window$ex_hours),
                           sec_since_market_open_time(forecast_time, plot_window$ex_hours))
      
      x <- cbind(sec_market_open, sec_market_open^2)
      # h <- input$h
      h <- length(forecast_time)
    })
    filt_parx <- filter.PARX(theta,p=p,q=q,y=y,x=x,fun_x=fun_x_pos,conf=.90,zoo=T,h=h,for.time=forecast_time)
    print(filt_parx)
    return(filter_parx)
    })
  
  filt.NBARX <- reactive({
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
      theta <- get.par.vec.NBARX()
      p <- input$p
      q <- input$q
      # h <- input$h
      sec_market_open <- c(sec_since_market_open_y(y, plot_window$ex_hours),
                           sec_since_market_open_time(forecast_time, plot_window$ex_hours))
      x <- cbind(sec_market_open, sec_market_open^2)
      h <- length(forecast_time)
    })
    filter.NBARX(theta,p=p,q=q,y=y,x=x,fun_x=fun_x_pos,conf=.90,zoo=T,h=h,for.time=forecast_time)
    })
  
  summary.PARX <- reactive({
    input$est
    isolate({
      N <- est.PARX()$N
      out <- est.PARX()$out
      neg_par <- input$season
      par.names <- c("omega",paste0("alpha_",1:input$p),paste0("beta_",1:input$q))
    })
    summary.avg_mle(N,out,repam.PARX,par.names = par.names,neg_par=neg_par)
    })
  
  summary.NBARX <- reactive({
    input$est
    isolate({
      N <- est.NBARX()$N
      out <- est.NBARX()$out
      neg_par <- input$season
      par.names <- c("omega",paste0("alpha_",1:input$p),paste0("beta_",1:input$q),"nu")
    })
    summary.avg_mle(N,out,repam.NBARX,par.names = par.names,neg_par=neg_par)
    })
  
  output$par.par <- renderTable({
    return(summary.PARX()$par.table)
  })
  
  output$nbar.par <- renderTable({
    return(summary.NBARX()$par.table)
  })
  
  output$info <- renderTable({
    table <- summary.PARX()$info_crit.table
    table <- cbind(table,summary.NBARX()$info_crit.table)
    colnames(table) <- c("PAR","NBAR")
    return(table)
  })
  
})

