rm(list=ls())
setwd('/home/christian/Dropbox/Speciale/shinyapps/NBARXintraapp')
source('global.R')

data <- get.yahoo.intraday(ticker='^GSPC', days=5)
m <- 10^floor(log10(max(data$y,na.rm=T)))/100
data$y <- round(data$y/m)

plot(data$y)
plot(data$sec_since_market_open)

fun_trend_poly_time <- function(trend,gam){
  pmax(gam[1] + gam[2]*(trend[,1]-14400)+gam[3]*(trend[,2]-14400)^2+gam[4]*(trend[,2]-14400)^4,0)
}


p <- 1
q <- 1

y <- coredata(na.omit(data$y))
trend <- cbind(data$sec_since_market_open,data$sec_since_market_open)

a0 <- 100
a1 <- 10^-3
a2 <- 10^-7
a3 <- 10^-18
gam_trend0 <- c(a0,a1,a2,a3)
plot(y)
lines(fun_trend_poly_time(trend,gam_trend0),lwd=3,col='red')


omega0 <- mean(y)*(1-0.3-0.5)
nu0 <- 10
k_trend <- length(gam_trend0)
neg_par <- FALSE
uniform_random_noise <- runif(1+p+q+length(c(gam_trend0,nu0)),-0.05/max(p,q),0.05/max(p,q))
theta0 <- c(omega0,rep(0.3/p,p),rep(0.5/q,q),gam_trend0,nu0) # + uniform_random_noise
gamma0 <- repam.inv.NBARX(theta0,p,q,k_x=0,k_trend=k_trend,neg_par=neg_par) + uniform_random_noise

# fun_x_poly_time(x,gam0)
loglike.NBARX(gamma0,p=p,q=q,y=y,
              x=NULL,k_x=0,fun_x=NULL,
              trend=trend,k_trend=k_trend,fun_trend=fun_trend_poly_time)

optim.out <- optim(par=gamma0, fn=loglike.NBARX, gr=NULL,p=p,q=q, y=y,
                   x=NULL,k_x = 0, fun_x=NULL,
                   trend=trend,k_trend=k_trend,fun_trend=fun_trend_poly_time,
                   neg_par=neg_par, 
                   hessian=T,
                   method="BFGS",control=list(trace=2,maxit=300,reltol=1e-6,REPORT=5))

theta <- repam.NBARX(gamma=optim.out$par,p,q,k_x=0,k_trend=k_trend,neg_par=F)

plot(y)
lines(fun_trend_poly_time(trend,gam_trend0),lwd=3,col='red')
lines(fun_trend_poly_time(trend,theta[4:7]),lwd=3,col='blue')

y <- na.omit(data$y)
window_range <- c(start(data$y),end(data$y))
forecast_time <- seq(end(y),window_range[2],by="300 secs")
forecast_trend  <- sec_since_market_open_time(forecast_time,data$ex_hours)
h <- length(forecast_time)

trend_for <- rbind(trend,cbind(forecast_trend,forecast_trend))

filt_out <- filter.NBARX(theta,p,q,y,
                         x=NULL,k_x=0,fun_x=NULL,
                         trend=trend_for,k_trend=k_trend,fun_trend=fun_trend_poly_time,
                         conf=.90,zoo=T,h=h,for.time=forecast_time)

forecast_shift <- max( no_overnight_space(time(na.omit(data$y))) )
plot(filt_out$ftrend)

plot.intra(data$y,data$ex_hours,type='l')
plot.intra(filt_out$lambda.hat,data$ex_hours, new=F,col='red')
plot.intra(filt_out$mu.hat,data$ex_hours, new=F,col='blue')

plot.intra(filt_out$for.lambda.hat,new=F,x_shift=forecast_shift,lty=2,lwd=2,col=col_vec[1]) 
plot.intra(filt_out$for.mu.hat,new=F,x_shift=forecast_shift,lty=2,lwd=2,col=col_vec[1]) 
plot.intra(filt_out$for.l.conf,new=F,x_shift=forecast_shift,lty=2,lwd=1,col=col_vec[1])
plot.intra(filt_out$for.h.conf,new=F,x_shift=forecast_shift,lty=2,lwd=1,col=col_vec[1])

