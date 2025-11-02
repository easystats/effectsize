# Converting Between Probabilities, Odds (Ratios), and Risk Ratios

The `effectsize` package contains function to convert among indices of
effect size. This can be useful for meta-analyses, or any comparison
between different types of statistical analyses.

## Converting Between *p* and Odds

Odds are the ratio between a probability and its complement:

Odds = \frac{p}{1-p}

p = \frac{Odds}{Odds + 1} Say your bookies gives you the odds of
Doutelle to win the horse race at 13:4, what is the probability
Doutelle’s will win?

Manually, we can compute \frac{13}{13+4}=0.765. Or we can

Odds of 13:4 can be expressed as (13/4):(4/4)=3.25:1, which we can
convert:

``` r

library(effectsize)

odds_to_probs(13 / 4)
```

    > [1] 0.765

``` r

# or
odds_to_probs(3.25)
```

    > [1] 0.765

``` r

# convert back
probs_to_odds(0.764)
```

    > [1] 3.24

Will you take that bet?

### Odds are *not* Odds Ratios

Note that in logistic regression, the non-intercept coefficients
represent the (log) odds ratios, not the odds.

OR = \frac{Odds_1}{Odds_2} = \frac{\frac{p_1}{1-p_1}}{\frac{p_2}{1-p_2}}
The intercept, however, *does* represent the (log) odds, when all other
variables are fixed at 0.

## Converting Between Odds Ratios, Risk Ratios and Absolute Risk Reduction

Odds ratio, although popular, are not very intuitive in their
interpretations. We don’t often think about the chances of catching a
disease in terms of *odds*, instead we instead tend to think in terms of
*probability* or some event - or the *risk*. Talking about *risks* we
can also talk about the *change in risk*, either as a *risk ratio*
(*RR*), or a(n *absolute) risk reduction* (ARR).

For example, if we find that for individual suffering from a migraine,
for every bowl of brussels sprouts they eat, their odds of reducing the
migraine increase by an OR = 3.5 over a period of an hour. So, should
people eat brussels sprouts to effectively reduce pain? Well, hard to
say… Maybe if we look at *RR* we’ll get a clue.

We can convert between *OR* and *RR* for the following formula (Grant
2014):

RR = \frac{OR}{(1 - p0 + (p0 \times OR))}

Where p0 is the base-rate risk - the probability of the event without
the intervention (e.g., what is the probability of the migraine
subsiding within an hour without eating any brussels sprouts). If it the
base-rate risk is, say, 85%, we get a *RR* of:

``` r

OR <- 3.5
baserate <- 0.85

(RR <- oddsratio_to_riskratio(OR, baserate))
```

    > [1] 1.12

That is - for every bowl of brussels sprouts, we increase the chances of
reducing the migraine by a mere 12%! Is if worth it? Depends on you
affinity to brussels sprouts…

Similarly, we can look at ARR, which can be converted via

ARR = RR \times p0 - p0

``` r

riskratio_to_arr(RR, baserate)
```

    > [1] 0.102

Or directly:

``` r

oddsratio_to_arr(OR, baserate)
```

    > [1] 0.102

Note that the base-rate risk is crucial here. If instead of 85% it was
only 4%, then the *RR* would be:

``` r

oddsratio_to_riskratio(OR, 0.04)
```

    > [1] 3.18

That is - for every bowl of brussels sprouts, we increase the chances of
reducing the migraine by a whopping 318%! Is if worth it? I guess that
still depends on your affinity to brussels sprouts…

## References

Grant, Robert L. 2014. “Converting an Odds Ratio to a Range of Plausible
Relative Risks for Better Communication of Research Findings.” *Bmj*
348: f7450.
