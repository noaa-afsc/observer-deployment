
---
title: "Background information related to allocation"
output:
    bookdown::html_document2:
     toc: true
     toc_depth: 2
     code_folding: hide
     keep_md: yes
---








# Background

three things to consider that could design allocation around
1) sample size - number of units selected - drives variance and probability of
   getting into a box
2) front matter - variance savings that one gets with sample size and sample
   rate (proportion of population sampled)   
3) expansion of trips - how many trips will each sampled trip expand to on
   average
   

## Probability of selecting a cell

Setting some basic population parameters: *N* is the population size, *K*
is the number of successes in population (the number of fishing trips in the
area of interest), *k_sample* is the number of successes in the sample, and
*n_sample* the sample size.
 
The probability of getting no monitored trips in the area of interest varies
with *K* and sample size. We can compute this using base R and the function
dhyper(x, m, n, k) where (from the R documentation) x = number of successes
drawn, n= number failures in population, =number sucesses in population
(m+n=N=population size), and k=number of balls drawn from urn (see Eq.
\@ref(eq:hypergeom-probability) below).  

In our application this would be P(*k_sample* = x) = dhyper(x, *K*, *N* - *K*,
*n_sample*)  = *k_sample*. Note that to be consistent with other texts, this
is often noted as phyper(k_lc,N-K,K,n_sample),
see https://www.vrcbuzz.com/hypergeometric-distribution-probabilities-using-r/
As an example, if the number of success is m <- 5, the number of
failures is n <- 11, the sample size is k <- 4, then the probability
P(X=0) should be dhyper(0,m,n,k) =  0.1813187.


```r
N <- c(300, 700, 1200) # population size; note three pop sizes could be considered analygous to strata
K <- c(1,2,5,seq(10, 40, 10)) # vector of the number of successes in the population
k_sample <- 0 # number of successes in sample
n_sample <- seq(15,500, 25) # number of selected items (sample size)

#build the base population; no domains, uses population parameters defined above
base_popul <- expand.grid(N = N, k_sample = k_sample, K = K, n_sample = n_sample)
base_popul$sample_15_pct <- round(0.15*base_popul$N, 0) # the sample size at 15% sample rate, refernce point only
base_popul$sample_rate <- base_popul$n_sample/base_popul$N

## compute the probability of getting exactly k sample units with some characteristic (e.g., blue ones)
##    the number of sample units is defined above (currently zero)
## IMPORTANT: this statement would need to be updated to compute probabilities for k or more (or less) items.
base_popul$prob_k_sample <- round(dhyper(base_popul$k_sample,base_popul$K,base_popul$N-K,base_popul$n_sample),4)

## compute parameters associated with variance scaling: fpc, 1/n, and the front matter (i.e., scaling factor)
base_popul$fpc <- 1-(base_popul$n/base_popul$N) # alternatively fpc = 1-rate = (N-n)/N
base_popul$n_inv <- 1/base_popul$n
base_popul$front_matter <- (base_popul$fpc * (1/base_popul$n_sample))
base_popul$front_matter <- (base_popul$fpc * (1/base_popul$n_sample))
base_popul$CV_scaler <- sqrt(base_popul$front_matter)

base_popul$front_matter_10_pct_rate <- (0.9)*(1/(round(0.1*base_popul$N,0)))
base_popul$CVreduction <- base_popul$front_matter_10_pct_rate - base_popul$front_matter

reference_lines_dat <- data.frame(N = c(300, 700, 1200)) 
reference_lines_dat$sample_15_pct <- 0.15*reference_lines_dat$N
		
samplerate_prob0_line_plt <- (ggplot(base_popul, aes(x = n_sample/N, y = 1 - prob_k_sample)) 
			+ geom_line(aes(color = factor(K), linetype = factor(K)))
			+ scale_color_hue('Number trips in cell')
			+ facet_wrap(. ~ N, ncol = 5, scales = "free_x")
			+ scale_linetype('Number trips in cell')
			+ geom_vline(xintercept = 0.15, color = 'grey60')
			#+ geom_vline(xintercept = 0.10, color = 'grey60')
			+ labs(y = paste("Probability of getting more than", k_sample,"monitored trips"),
					x = "Sample rate")
			+ theme_grey()
			+ theme(legend.position = "bottom")
			+ guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))
			)

samplesize_prob0_line_plt <- (ggplot(base_popul, aes(x = n_sample, y = 1 - prob_k_sample)) 
			+ geom_line(aes(color = factor(K), linetype = factor(K)))
			+ facet_wrap(. ~ N, ncol = 5, scales = "free_x")
			+ scale_color_hue('Number trips in cell')
			+ scale_linetype('Number trips in cell')
			+ geom_vline(data = reference_lines_dat, aes(xintercept = sample_15_pct), color = 'grey60')
			#+ geom_hline(yintercept = 0.10, color = 'grey60')
			+ labs(y = paste("Probability of getting more than", k_sample,"monitored trips"),
					x = "Sample size")
			+ theme_grey()
			+ theme(legend.position = "bottom")
			+ guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))
			)

#############################

samplerate_prob0_contour_plt <- (ggplot(base_popul, 
					aes(x = n_sample/N, z = 1 - prob_k_sample, y=K/N)) 
			+ geom_contour_filled(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8))
			+ facet_wrap(. ~ N, ncol = 5, scales = "free")
			+ scale_fill_brewer(paste("Probability of getting more than", k_sample,"\nmonitored trips"))
			+ geom_vline(xintercept = 0.15)
			+ labs(y = "Proportion of trips in cell", x='Sample Rate')
				+ theme(legend.position = "bottom")
			)


samplesize_prob0_contour_plt <- (ggplot(base_popul, 
					aes(x = n_sample, z = 1 - prob_k_sample, y=K)) 
			+ geom_contour_filled(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8))
			+ facet_wrap(. ~ N, ncol = 5, scales = "free")
			+ scale_fill_brewer(paste("Probability of getting more than", k_sample,"\nmonitored trips"))
			+ geom_vline(data = reference_lines_dat, aes(xintercept = sample_15_pct))
			+ labs(y = "Number of trips in cell", x='Sample Size')
			+ theme(legend.position = "bottom")
			)
```
If we define a population of 1500 trips where the number of trips in a cell is
1, 2, 5, 10, 20, 30, 40 and sample sizes range from 15 to 490,
we can see how the probability of a sample containing no trips from a given
area changes with sample size (or rate) and the size of the cell
(Figure \@ref(fig:gg-prob0-line-plts))

<div class="figure">
<img src="images/gg-prob0-line-plts-1.png" alt="Probability of a sample containing no monitored trips as a function of sample size for a range of cell sizes. Vertical reference line on each plot indicates 15% sample rate."  />
<p class="caption">(\#fig:gg-prob0-line-plts)Probability of a sample containing no monitored trips as a function of sample size for a range of cell sizes. Vertical reference line on each plot indicates 15% sample rate.</p>
</div>
Similarly, the probability of a cell containing no data increases with both
decreasing sample rate and the number of trips in a cell (Figure
\@ref(fig:gg-prob0-contour-plts)).

<div class="figure">
<img src="images/gg-prob0-contour-plts-1.png" alt="Probability of a sample containing no monitored trips as a function of sample size for a range of cell sizes. Vertical reference line on each plot indicates 15% sample rate."  />
<p class="caption">(\#fig:gg-prob0-contour-plts)Probability of a sample containing no monitored trips as a function of sample size for a range of cell sizes. Vertical reference line on each plot indicates 15% sample rate.</p>
</div>

## Impacts of sample size on variance


```r
## make some plots for use later
##  these are based on the same population descibed above and show 1) how 
n_scaling_plt <- (ggplot(base_popul[which(base_popul$sample_rate < 0.35),], 
					aes(sample_rate, n_inv))
			+ geom_line(aes(sample_rate, n_inv, color = factor(N), linetype = factor(N)))			
			+ labs(x = 'Sampling Rate', y = 'Variance Denominator (1/n)')
			+ scale_color_hue('Number trips in cell')
			+ scale_linetype('Number trips in cell')
			+ geom_vline(xintercept = 0.15)
			+ theme_grey()
			+ theme(legend.position = "bottom")
)


fpc_scaling_plt <- (ggplot(base_popul[which(base_popul$sample_rate < 0.35),], 
					aes(sample_rate, fpc))
			+ geom_line(aes(sample_rate, fpc,color = factor(N), linetype = factor(N)),
					position = position_dodge(width = 0.005))
			+ labs(x='Sampling Rate', y='Finite Population Correction\n(N-n)/N')
			+ scale_color_hue('Number trips in cell')
			+ scale_linetype('Number trips in cell')
			+ geom_vline(xintercept = 0.15)
			+ theme_grey()
			+ theme(legend.position = "bottom")
			)

totl_scaling_n_plt <- (ggplot(base_popul[which(base_popul$sample_rate < 0.35),], 
					aes(n_sample, CV_scaler))
			+ geom_line(aes(n_sample, CV_scaler, color = factor(N), linetype = factor(N)))
			+ labs(x = 'Number Samples', y = 'CV Scaling\n(SqRt(FPC*(1/n))')
			+ scale_color_hue('Number trips in cell')
			+ scale_linetype('Number trips in cell')
			+ geom_vline(data = reference_lines_dat, aes(xintercept = sample_15_pct), color = 'grey80')
			+ theme_grey()
			+ theme(legend.position = "bottom")
			)

totl_scaling_rate_plt <- (ggplot(base_popul[which(base_popul$sample_rate < 0.35),], 
					aes(sample_rate, CV_scaler))
			+ geom_line(aes(sample_rate, CV_scaler, color = factor(N), linetype = factor(N)))
			+ labs(x = 'Sampling Rate', y = 'CV Scaling\n(SqRt(FPC*(1/n))')
			+ scale_color_hue('Number trips in cell')
			+ scale_linetype('Number trips in cell')
			+ geom_vline(xintercept = 0.15, color = 'grey80')
			+ theme_grey()
			+ theme(legend.position = "bottom")
			)
```

The variance in estimates from a population is a function of both the sample
size and the overall proportion of the population that is sampled. Thile these
are related quantities, they have different effects on the variance estimate.

All populations have a base variance, Eq \@ref(eq:base-var); the variability in
a measured parameter (e.g., length) between sample units. 

$$\begin{equation} 
  Var(x) = \frac{\sum_{i=1}^{N} (x_i - \bar{x})^2 }{N-1}  
  (\#eq:base-var)
\end{equation}$$

The variance of the mean is the base variance (the
population variance or variance of the data) divided by the sample size, Eq.
\@ref(eq:var-of-estimate). As the sample rate increases, variance will
decrease by $\frac{1}{n}$. Note that we are not summing only over those sample
units that were sampled, but all samples in the population (i.e., *i* = 1 to *N*
rather than *i* = 1 to *n*)

$$\begin{equation} 
  Var(\bar{x}) = \frac{\sum_{i=1}^{N}{\left(x_i - \bar{x}\right)^2}}{n\left(N-1\right)}  
  (\#eq:var-of-estimate)
\end{equation}$$

Unfortunaly, we rarely know the population variance and must rely variance
estimates fro previous studies or on the sample data only. In estimating the
parameter mean based on a sample, we don't have information from every sample
unit and hence will also need to estimate the variance
(Eq. \@ref(eq:est-var-of-estimate). 

$$\begin{equation} 
  V\hat{a}r(\bar{x}) = \frac{\sum_{i=1}^{n}{\left(x_i - \bar{x}\right)^2}}{n\left(n-1\right)}  
  (\#eq:est-var-of-estimate)
\end{equation}$$

We can see the impact of sample size on variance by plotting $\frac{1}{n}$
against the sample rate, $\frac{n}{N}$, Fig. \@ref(fig:gg-n-fpc-scaling-plt).

<div class="figure">
<img src="images/gg-n-fpc-scaling-plt-1.png" alt="something"  />
<p class="caption">(\#fig:gg-n-fpc-scaling-plt)something</p>
</div>
In addition, as we sample a larger and larger portion of the population, our
uncertainty about the estimate decereases. HERE BE THE FPC discussion, Fig.
\@ref(fig:gg-n-fpc-scaling-plt).

some things to note -
generally don't apply FPC when sample rates below 5%, doesn't make a difference
don't know actual variance - makes sample size analysis challenging, Fig.
\@ref(fig:gg-totl-scaling-plt).

<div class="figure">
<img src="images/gg-totl-scaling-plt-1.png" alt="something"  />
<p class="caption">(\#fig:gg-totl-scaling-plt)something</p>
</div>

# Cost-weighted boxes

## Probability of monitoring as a function of sample size

In the
[Cost-weighted Boxes Deployment
Design](https://docs.google.com/document/d/1KxOkCigvOXgbxbzxSeJIGwBzLBWQgVX3iKs1wj4kdhI/edit#)
(<https://docs.google.com/document/d/1KxOkCigvOXgbxbzxSeJIGwBzLBWQgVX3iKs1wj4kdhI/edit#>),
the proposed allocation method is built on having spatial-temporal coverage that
minimizes the proportion of post-strata likely to have no data, weighted by
sampling costs, with the assumption that this will give us the best coverage per
unit cost. 

Starting with the simpler case that ignores costs and is based on the more
traditional minimization of variance. The allocation scheme that minimizes
variance of an estimate, for example the mean $\bar{x}$, based on a stratified
sample with *h* = 1 to *H* strata and $\sum_{h=1}^{H}n_h = n$ is the vector of
sample sizes that minimizes the stratified variance estimate (Eq. \@ref(eq:stratified-variance)):

$$\begin{equation} 
  V\hat{a}r(\bar{x}) = \sum_{h = 1}^{H}
W_h V\hat{a}r(\hat{x}_{h})\left(\frac{1}{n_h} - \frac{1}{N_h}\right)  
 (\#eq:stratified-variance) 
\end{equation}$$ 
 
where $W_h = \frac{N_h}{N}$ and $n= \sum{n_h}$

This can be minimized by setting the strata-specific sample sizes,
$n_h$, proportional to the standard deviation in each stratum. In
otherwords, when $\frac{n_h}{N_h} = kS_h$ where
$S_h = \sqrt{V\hat{a}r(\hat{x}_{h})}$ and *k* is a constant such that 
$\sum(n_h) = n$. 
More specifically, the overall variance will be minimized when

$$\begin{equation}  
  \hat{n}_{h} = n \frac{w_{h} \sigma_{h}}{\sum_{i = 1}^{H} w_{h} \sigma_i}
  (\#eq:Neyman-sample-size) 
\end{equation}$$

Note that the relies on knowing $Var(\bar{x})$, which is rarely the case.
Generally, the estimated variance is used under the assumption that the
estimate will accurately reflect the true variability in each stratum. This is
the approach used above. 

Allocation will be optimized when the sample rates in the strata are
proportional to their standard deviations and inversely proportional to the
stratum-specific cost of sampling, Eq.\@ref(eq:Neyman-optimal-sample-size),
below.

$$\begin{equation} 
  \hat{n}_{h} = n \frac{\frac{w_h \sigma_h}{\sqrt{c_h}}}{\sum_{h
= 1}^{H}\frac{w_h \sigma_h}{\sqrt{c_h}}}   
  (\#eq:Neyman-optimal-sample-size) 
\end{equation}$$
where $c_h$ is the cost associated with sampling stratum *h*.

### One stage deployment
For the cost weighted box design, we would replace $\sigma_h$ (variance) with
proportion of post-strata within stratum *h* with a less than a pre-specified
probability of having no monitored trips.

The probability of taking a sample of size *n* from a population *N*
total trips where *K* trips are of interest (in a given cell), and getting *k*
successes in the sample is given by \@ref(eq:hypergeom-probability).

$$\begin{equation}
  P\left(X = k\right) = \frac{\binom{K}{k}\binom{N - K}{n - k}}{\binom{N}{n}}
  (\#eq:hypergeom-probability) 
\end{equation}$$

The probability of the sample having no monitored trips in a given cell is given
by \@ref(eq:hypergoem-prob-zero).

$$\begin{equation}
  P\left(k = 0\right) = \frac{\binom{K}{0}\binom{N - K}{n - 0}}{\binom{N}{n}} =
\frac{\binom{N - K}{n}}{\binom{N}{n}} = \frac{(N - K)!}{(N - K - n)!}
  (\#eq:hypergoem-prob-zero) 
\end{equation}$$

If we know these probabilities for each cell in the stratum, we can define a 
"not monitored cell" as any cell where the probability of no data is greater
than *c*, (i.e., $\ge 0.1$) based on the the number of trips in the cell (*K*), 
total number of trips in the population (*N*), and the sample size (*n*), Eq.
\@ref(eq:hypergoem-prob-zero).

Letting $I(P_i(k_i = 0) \gt c)$ be the indicator of no data, for a given
stratum, we have the proportion of cells with no data as:
$$\begin{equation}
  T_h = \frac{\sum_{i}^{B_h} I_{ih}\left(P_{ih}(k_{ih} = 0) \gt c\right)}{B_h} 
  (\#eq:proport-cells-no-data-stratum) 
\end{equation}$$
where *i* from 1 to *B* indexes the total number of cells in the stratum.

Intuitively, we want to allocate more sample to strata that have a higher
proportion of cells with few trips (*i.e.*, strata where there are a higher
number of cells where *K* is small relative to *N*, see Figure
\@ref(fig:gg-prob0-line-plts)). 

Refering back to our original minimization problem and substituting in for $P_i(k_i = 0)$, we
could find the set of $n_h$ that minimizes the sum over all strata of having no
data, *i.e., minimizes \@ref(eq:mean-proport-cells-no-data-stratum).
$$\begin{equation}
  \bar{T} = \frac{1}{H} \sum_{h}T_h = \sum_{h}
\left[\frac{\sum_{i}^{B_h} I_i\left(\frac{(N_{ih} - K_{ih})!}{(N_{ih} - K_{ih} - n_{ih})!} \gt c\right)}{B_h}\right]
  (\#eq:mean-proport-cells-no-data-stratum) 
\end{equation}$$

Intuitively, we want to allocate more sample to strata that have a higher
proportion of cells with few trips (*i.e.*, strata where there are a higher
number of cells where *K* is small relative to *N*, see Figure
\@ref(fig:gg-prob0-line-plts)). 

By using the mean of the proportions, $T = \frac{\sum{T_h}}{H}$ (Eq.
\@ref(eq:mean-proport-cells-no-data-stratum)), each stratum would be weighted
equally while using the sum of the proportions (divided by the total number of
boxes, i.e., the weighted mean), sampling strata with larger numbers of cells
will contribute more to the overall proportion (metric being minimized). 

$$\begin{equation}
  \bar{T} = \frac{\sum_{h}\sum_{i}^{B_h} I_{ih}\left(P_{ih}(k_{ih} = 0)
\gt c\right)}{\sum_{h}{B_h}} = \frac{\sum_{h}\sum_{i}^{B_h}
I_{ih}I_i\left(\frac{(N_{ih} - K_{ih})!}{(N_{ih} - K_{ih} - n_{ih})!} \gt
c\right)}{\sum_{h}B_h} 
  (\#eq:proport-ALL-cells-no-data-stratum) 
\end{equation}$$

If our goal is to minimize the total number of cells without
data(across strata), Eq. \@ref(eq:proport-ALL-cells-no-data-stratum), weighted
mean proportion is the more appropriate metric. If we want to ensure that we
minimize the proprtion of cells without data in each stratum, the mean of the
proportions metric is more appropriate.

### Two-stage deployment





is defined as the rate at which the proportion of post-strata that have a
prespecified probability of having at least one trip (or delivery) monitored. 


Rank areas by the number of trips in each; areas will add into the proportion
in that order, causing stair steps in some cases.

for a given number of post-strata (areas), each with a given number of trips,
the sample size needed to acheive a pre-specified $P(X=0)$ is difficult to
compute since it involves solving the probability for *n*: 

$$
P(X=0) = \frac{(N-K)!(N-n)!}{(N-K-n)!N!}
$$

In addition, this will vary for each post-strata based on the number of trips
in the post-strata. However, after a lot of math, we notice that only some
terms vary with n and that some factorials cancel,leaving only falling products.

For example, $\frac{(N-K)!}{N!}$ equals the falling product from *N* to
*N-K+1*, $\prod_{x = a}^{b} f(x)$






