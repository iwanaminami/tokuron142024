##### Code for pseudo data generation
#### Shoya Iwanami
#### 2024/04/19

library(deSolve)
library(ggplot2)


Tmin <- 0.0
Tmax <- 20.0
step_size <- 0.0001

pars <- c(0.057, 8.61*(10.0^-11.0),
          0.057 + 1.75, 3.26*(10.0^4.0), 1.93 + 0.039,
          6.46*(10.0^6.0), 10.0^-1.0, 5.0*(10.0^4.0))


ode_derives <- function(t, var, pars) {
  with(as.list(c(var, pars)), {
    ## var: T(t), I(t), V(t)
    dTdt <- - d * TC - beta * TC * VL
    dIdt <- beta * TC * VL - delta * IC
    dVdt <- p * IC - c * VL
    
    return(list(c(dTdt, dIdt, dVdt)))
  })
}


ODEs <- function(parms) {
  pars <- parms[1:6]
  names(pars) <- c("d", "beta", "delta", "p", "c")
  
  t_list <- seq(Tmin, Tmax, by = step_size)
  var_init <- c(TC = parms[6], IC = parms[7], VL = parms[8])
  out <- ode(y = var_init, times = t_list, func = ode_derives, parms = pars)
  
  as.data.frame(out)
}

head(ODEs(pars))

pars

out <- subset(ODEs(pars), time %% 2 == 0)
out_error <- out

for (i in 1:3) {
  
  out_error[, i + 1] <- exp(log(out_error[, i + 1]) + rnorm(nrow(out_error), mean = 0, sd = 1))
  
}

ggplot() +
  geom_line(data = out, aes(x = time, y = log10(TC)), color = "pink") +
  geom_line(data = out, aes(x = time, y = log10(IC)), color = "skyblue") +
  geom_line(data = out, aes(x = time, y = log10(VL)), color = "lightgreen") +
  geom_point(data = out_error, aes(x = time, y = log10(TC)), color = "pink") +
  geom_point(data = out_error, aes(x = time, y = log10(IC)), color = "skyblue") +
  geom_point(data = out_error, aes(x = time, y = log10(VL)), color = "lightgreen")

write.csv(out_error, file = "data_example.csv")