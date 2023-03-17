Simulation $G^{[P]}/G/1$
================
6/3/2021

# Annex 1. Càlcul de la distribució de temps de serveis de paquets

``` r
Ea <- 82
lambda = 1/Ea


k = 4
Ex <-26.1818
alpha = k/ Ex
mu = 1/Ex

Ep <- 10/alpha
rho = Ep/Ea

EEEE <- 1/alpha *(0.125*4+0.275*8+0.35*12+0.25*16)

E1 = 4/alpha 
E2 = 8/alpha 
E3 = 12/alpha 
E4 = 16/alpha 

VV2 = 0.125*(E1 - EEEE)^2+ 0.275*(E2 - EEEE)^2+0.35*(E3 - EEEE)^2+0.25*(E4 - EEEE)^2
  
ccc2 = sqrt(VV2)/EEEE
rho = EEEE/Ea
```

# Annex 2. Càlcul de les magnituds fonamentals teòriques.

``` r
Lqtt <- rho^2/(1-rho)*(1+ccc2^2)/2
Lqtt  

Ltt = Lqtt + rho
Ltt
```

``` r
Wtt = Ltt/ lambda
Wtt


Wqtt = Lqtt/ lambda
Wqtt
```

# Annex 3. Inicialització de variables i codi de simulació.

Inicialitzem:

``` r
k = 4
Ex <-26.1818
alpha = k/ Ex
mu = 1/Ex
Ea <- 82
lambda = 1/Ea

n = 10000    ## INPUT: Num de paquets a generar

N = 0


t <- numeric()
p <- numeric()
theta <- numeric(0)
thetac <- matrix(nrow = n, ncol = 4)
x <- matrix(nrow = n, ncol = 4)
tsc <- matrix(nrow = n, ncol = 4)
ts <- numeric()
X <- numeric()

Lc <- matrix(nrow = n, ncol = 4)
Lqc <- matrix(nrow = n, ncol = 4)
LTc <- matrix(nrow = n, ncol = 4)
LTqc <- matrix(nrow = n, ncol = 4)
Ltotalc = 0
Ltotalqc = 0
Wc = 0
Wqc = 0

L <- numeric()
LTq <- numeric()
Ltotal = 0
Ltotalq = 0
LT <- numeric()
W = 0
Lq <- numeric()
Wq = 0
  

t[1] <- 0
p[0] <- 0
theta[0] <- 0

tau<- numeric()
```

Bucle (simulació):

``` r
for (i in 1:n){
  
  p[i] <- generacio_p(runif(1))
  
  tsc [i,1] <- max(thetac[i-1,p[i-1]],t[i], na.rm = T) 
  # instant entrada en el SS del client 1 del paquet i
  x[i,1] <- -log(prod(runif(4)))/alpha 
  
 thetac[i,1] <- tsc[i,1] + x[i,1]       
 # instant de sortida del SE del client 1 del paquet i

 
 if(p[i]>1){
 for (j in 2:p[i]){
   
  
    tsc [i,j] <- max(thetac[i,j-1],t[i], na.rm = T)
    # instant entrada en el SS del client j del paquet i
   x[i,j] <- servei(4)   # pk es un 4-Erlang
   thetac[i,j] <- max(thetac[i,j-1],0) + x[i,j]  
 } 
 }

 ts[i] <- max(theta[i-1], t[i], na.rm = T)
 X[i] = sum(x[i,], na.rm = T)
 theta[i] = ts[i]+ X[i]

 
 # Recollida estadístics de clients
 
  for(j in 1:p[i]){
   Lc[i,j] = thetac[i,j] - t[i]     
   # temps de permanència al SE del client j (= wc[j+r])
   Lqc[i,j] = tsc[i,j] - t[i]    
   # temps de permanència cua del client j (= wc[j+r])
   
   Ltotalc = Ltotalc +  Lc[i,j]
   LTc[i,j] = Ltotalc/(t[i])  # Serà lu del gràfic 
   
   Ltotalqc = Ltotalqc +  Lqc[i,j]
   LTqc[i,j] = Ltotalqc/(t[i])    # no se si cal 
   
   Wc = Wc +  Lc[i,j] 
   Wqc = Wqc +  Lqc[i,j]
      }
 
 N = N + p[i]
 
 if(i<n){
   tau[i] = arribades(1)
   t[i+1] = t[i] + tau [i] 
 }
 

 # Recollida estadístics de paquets
 
  L[i] <- theta[i] - t[i]   # (= w[i]) temps de permenència al SE del paquet i 
  Ltotal <- Ltotal + L[i]
  LT[i] <- Ltotal / (t[i]-t[1])
  W <- W + L[i]
  
  
  Lq[i] <- ts[i]-t[i]   # (= wq[i]) temps de permenència en cua del paquet i 
  Ltotalq <- Ltotalq + Lq[i]
  LTq[i] <- Ltotalq / (t[i]-t[1]) #No ho posa 
  Wq <- Wq + Lq[i]
  
  
}
```

# Annex 4. Gràfics

Evolució de $\mathcal{L}$ i $\mathcal{L_q}$:

``` r
plot(LT[2:n], type = "l",  col ="indianred")
lines(LTq[2:n], col = "lightgoldenrod3")
legend ("bottomright", c("L paquets", "Lq paquets"), col = c("indianred", "lightgoldenrod3"), lty = c(1,1))
```

Evolució de $\mathcal{L}$ i $\mathcal{L_q}$:

``` r
plot(as.vector(t(LTc))[4:N], type = "l", col ="indianred")
lines(as.vector(t(LTqc))[4:N], col = "lightgoldenrod3")
legend ("bottomright", c("L clients", "Lq clients"), col = c("indianred", "lightgoldenrod3"), lty = c(1,1))
```

Gràfic temps d’arribada de paquets

``` r
hist(tau, main = "Histograma dels temps d'arribada", col = "lightgoldenrod")
```

Gràfic temps de servei de paquets i clients

``` r
par(mfrow = c(1,2))
hist(X, main = "Histograma temps de servei paquets", col = "lightgoldenrod")
hist(x, main = "Histograma temps de servei clients", col = "lightgoldenrod1")
```
