# G-G-P-queue-simulation
Evaluation of queues with batch arrivals G[P]/G/1. Simulation and theoretical values estimation. 


In this  report we will show the results of the implementation of a program that allows us to evaluate the recurrence of a queue $G^{[P]}/G/1$.

The distributions we have are the following:

- Time distributions between arrivals (corresponding to packets from $p_i$ clients): **Exponential** with $E(\tau) = 82 \rightarrow \lambda = \frac{1}{82}$
- Service time distributions (corresponding to each client): **4-Erlang** with $E(x_i) = $26.1818
