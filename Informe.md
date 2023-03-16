Avaluació de cues amb arribades per lots $G^{[P]}/G/1$
================
6/3/2021

# Introducció

En aquest petit informe mostrarem els resultats de la implementació d’un
programa que ens permeti avaluar la recurrència d’una cua $G^{[P]}/G/1$.

Les distribucions que tenim són les següents:

-   Distribucions del temps entre arribades (corresponents a paquets de
    $p_i$ clients): **Exponencial** amb
    $E(\tau) = 82 \rightarrow \lambda = \frac{1}{82}$
-   Distribucions de temps de servei (corresponent a cada client):
    **4-Erlang** amb $E(x_i) = 26,1818$

La distribució del número de clients $p_i$ en cada paquet $i$ és:

| j   | $Prob(p_i=j)$ |
|-----|---------------|
| 1   | 0,125         |
| 2   | 0,275         |
| 3   | 0,350         |
| 4   | 0,250         |

# Aproximació teòrica

Abans de fer la implementació del programa , calculem els valors de
$\rho$, $L$, $L_q$, $W$ i $W_q$ (teòrics) mitjançant l’**aproximació
d’Allen Cuneen**, que posteriorment, compararem amb els valors simulats.

Si la distribució del temps entre servei dels clients és una 4-Erlang,
hem de calcular la distribució de servei dels paquets. S’ha de tenir en
compte que els paquets poden ser de 1, 2, 3 o 4 clients amb les
probabilitats corresponents a la distribució de $p_i$.

$E(x_i) = \frac{4}{\alpha}= 26.1818 \rightarrow \alpha = 0.1527779$

En el cas de que hi hagi 1 client per paquet, l’esperança del temps de
servei del paquet serà de $E(X^{[1]}) = 26.1818$, en cas que hi hagi 2
clients per paquet $E(X^{[2]}) = 2 \cdot \frac{4}{\alpha} = 52.3636$, en
cas que hi hagi 3 clients per paquet
$E(X^{[3]}) = 2 \cdot \frac{4}{\alpha} = 78.5454$, i en cas que en
siguin 4, $E(X^{[4]}) = 2 \cdot \frac{4}{\alpha} = 104.7272$

De tal manera que l’esperança del temps de servei dels paquets serà:

$$E(X) = 0.125\cdot E(X^{[1]}) + 0.275\cdot E(X^{[2]}) + 0.350\cdot E(X^{[3]}) + 0.250\cdot E(X^{[4]}) = 71.3454$$

La variància de la distribució del temps de servei de paquets la podem
calcular:

$$V(X) = 0.125\cdot (E(X^{[1]}) -E(X))^2+ 0.275\cdot (E(X^{[2]}) -E(X))^2 + 0.350\cdot (E(X^{[3]}) -E(X))^2 + 0.250\cdot (E(X^{[4]}) -E(X))^2 = 650.7839$$

Per veure els càlculs de la distribució del temps de servei de paquets
anar a l’*ANNEX 1*.

El coeficient de desviació serà:
$C_X = \frac{\sqrt{V(X)}}{E(X)} = 0.3575628$.

Per tant, el model de cues dels paquets serà **$M/ G/ 1$** on
$\lambda = \frac{1}{82}$ i $\mu = \frac{1}{71.3454}$.

El factor de càrrega és $\rho = \frac{\lambda}{\mu} = 0.8700659$.

L’aproximació d’**Allen Cuneen** és la següent:

$$L_q \thickapprox Lq_{M/M/1} \cdot \frac{C_{\tau}^2 + C_X^2 }{2} = \frac{\rho^2}{1-\rho} \cdot \frac{C_{\tau}^2 + C_X^2 }{2} = 3.285512$$

A partir de les fòrmules de Little podem calcular la resta de magnituds
fonamentals:

-   Ocupacio mitjana al S.E.: $L = Lq + \rho = 4.155578$
-   Temps mig de permenència al S.E.:
    $W = \frac{L}{\lambda } = 340.7574$
-   Temps mig de permenència en cua:
    $Wq = \frac{Lq}{\lambda } = 269.412$

Per veure els càlculs d’aquestes magnituds anar a l’*ANNEX 2*.

# Generador de variables aleatòries

Generació **$p_i$** (número de clients per paquet):

``` r
generacio_p <- function (u){
  if (u <= 0.125){return(1)}else if (u>0.125 && u<=0.4){return(2)}
  else if(u>0.4 && u<=0.75) {return(3)}else  {return(4)}
}
```

Generació del **temps entre arribades** per a paquets de mida $p_i$
clients:

``` r
arribades <- function(u){
return(-log(runif(u))/lambda)
  }
```

Generació del **temps de servei** per a clients:

``` r
servei <- function(u){
return(-log(prod(runif(u)))/alpha)
}
```

# Implementació del programa

Per tal de dur a terme el bucle que ens calcula tota la simulació del
model de cues, s’ha realitzat mitjançant codi R. Per veure el codi, anar
al document *Informe.Rmd* o a l’*ANNEX 3*. Els resultats de la simulació
que es mostren a la pràctica, corresponen a una simulació d’un total de
n = 10.000 paquets.

# Presentació de resultats de la simulació

## El factor de càrrega $\rho$

``` r
mean(X)/mean(tau)
```

    ## [1] 0.8639986

## La ocupació mitjana del S.E. $\mathcal{L}$ i de la cua $\mathcal{L}_q$ en número de paquets.

L paquets:

``` r
Lresultat = Ltotal/t[n];  Lresultat
```

    ## [1] 4.223545

Lq paquets:

``` r
Lqresultat = Ltotalq/t[n];  Lqresultat
```

    ## [1] 3.35946

### Evolució de $\mathcal{L}$ i $\mathcal{L}_q$ paquets

<img src="Informe_files/figure-gfm/unnamed-chunk-12-1.png" width="70%" style="display: block; margin: auto;" />

Observem que el gràfic corresponent a la L i Lq dels paquets s’ha
estabilitzat amb una simulació de n = 10.000 clients.

## La ocupació mitjana del S.E. $L$ i de la cua $L_q$ en número de clients.

L clients:

``` r
Lresultatc = Ltotalc/t[n];  Lresultatc
```

    ## [1] 10.92432

Lq clients:

``` r
Ltotalqc/t[n]
```

    ## [1] 10.06023

### Evolució de $\mathcal{L}$ i $\mathcal{L}_q$ clients:

<img src="Informe_files/figure-gfm/unnamed-chunk-15-1.png" width="70%" style="display: block; margin: auto;" />

Observem que el gràfic corresponent a la L i Lq dels clients s’ha
estabilitzat amb una simulació de n = 10.000 clients.

## El temps mig per client de permanència en el S.E $\mathcal{W}$ i en la cua $\mathcal{W_q}$ dels paquets.

W de paquets:

``` r
W/n
```

    ## [1] 349.0824

Wq de paquets:

``` r
Wq/n
```

    ## [1] 277.6645

## El temps mig per client de permanència en el S.E $W$ i en la cua $W_q$ dels clients.

W de clients:

``` r
Wc/N
```

    ## [1] 331.1007

Wq de clients:

``` r
Wqc/N
```

    ## [1] 304.9115

## Histogrames

**Gràfic temps d’arribada de paquets**

<img src="Informe_files/figure-gfm/unnamed-chunk-20-1.png" width="60%" style="display: block; margin: auto;" />

**Gràfic temps de servei de paquets i clients**

<img src="Informe_files/figure-gfm/unnamed-chunk-21-1.png" width="80%" style="display: block; margin: auto;" />

Per veure el codi de la realització dels gràfics anar a l’*ANNEX 4*.

# Comparació resultats de l’aproximació amb la simulació

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Simulació Clients
</th>
<th style="text-align:right;">
Simulació Paquets
</th>
<th style="text-align:right;">
Aproximació (teòrica) paquets
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Wq
</td>
<td style="text-align:right;">
304.91150
</td>
<td style="text-align:right;">
277.664495
</td>
<td style="text-align:right;">
269.411963
</td>
</tr>
<tr>
<td style="text-align:left;">
W
</td>
<td style="text-align:right;">
331.10070
</td>
<td style="text-align:right;">
349.082430
</td>
<td style="text-align:right;">
340.757368
</td>
</tr>
<tr>
<td style="text-align:left;">
Lq
</td>
<td style="text-align:right;">
10.06023
</td>
<td style="text-align:right;">
3.359460
</td>
<td style="text-align:right;">
3.285512
</td>
</tr>
<tr>
<td style="text-align:left;">
L
</td>
<td style="text-align:right;">
10.92432
</td>
<td style="text-align:right;">
4.223545
</td>
<td style="text-align:right;">
4.155578
</td>
</tr>
</tbody>
</table>

Compararem els resultats que hem obtingut de les magnituds corresponents
als paquets, tant les magnituds teòriques com les simulades mitjançant
el codi implementat. El número de paquets en cua i el número de paquets
en el sistema d’espera és molt semblant entre el càlcul teòric i el
simulat, per tant, això és bon indicador.

Destacar, que el temps mig d’espera en cua pels clients és major que el
temps mig en el sistema d’espera en cua dels paquets. Respecte la
comparativa entre les magnituds de temps, tenim que les magnituds dels
paquets del temps d’espera en cua i de temps en el S.E són lleugerament
superiors a les teòriques (però molt semblants). El temps mig d’espera
en cua dels paquets teòric és de 269.412, mentre que el simulat és de
277.664. El temps d’espera teòric dels paquets en el S.E és de 340.7574
i el simulat 349.082, són força semblants malgrat que la simulació ha
sobreestimat la magnitud.
