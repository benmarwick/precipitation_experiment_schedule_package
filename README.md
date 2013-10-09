This is a quick guide to the precipitation experiment schedule file.
Files will be named FIELDSITEschedule.csv, where FIELDSITE will be
replaced with the name of your country or site (e.g.
Cardososchedule.csv, FrenchGuianaschedule.cv) The description follows
every column name:

**trt.name** = treatment name, following the naming convention suggested
by Diane. Treatments are named after the two parameters of the negative
binomial distribution (*mu* and *k*). Names are in the form “muXkY”,
where X and Y are numbers which multiply *mu* and *k*, respectively, in
different treatments.

**mu** = the actual parameter values used in this field site. The
“mu1k1” treatment was calculated from the original data, the others were
derived from mu1k1 by multiplying those values of *mu* and *k* by the
range of factors agreed upon by the working group (*mu*: 0.1, 0.2, 0.4,
0.6, 0.8, 1, 1.5, 2, 2.5, 3 and *k*: 0.5, 1, 2)

**k** = as above, but for *k*

**temporal.block** = after intended.mu and intended.k were calculated,
the treatments were divided into three random groups; these groups begin
on different days (the 1st, 2nd and 3rd days of the experiment). The
grouping was tested with an ANOVA to confirm that there is no
significant difference in intended.mu or intended.k.

**day.1-day.68** = these columns represent “days from beginning of
experiment”. They do **not** represent calendar dates. Each column gives
the amount of water **in mm** that should be added to each plant.
Remember to multiply these by the catchment area of your bromeliad
species, *and* by the correction factor for the canopy, as described by
Diane and her team in Costa Rica. Therefore, treatments are applied to
bromeliads over a period of **68 days**, not including the preparation
of bromeliads beforehand, the calculation of correction factors, and the
final destructive sampling at the end. Plants receive no water on days
marked both NA and 0. (NAs represent days added by the experimental
design, for example the temporal blocks, in contrast to 0s which are
predicted by our model of rainfall). Additionally, plants are not
watered on days marked “sample” or “insects”. These represent days set
aside for the collection of protist samples (*sample*) and destructive
sampling for macroinvertebrates (*insects*).

Looking along each row (rather than down each column), the pattern of
treatments for each bromeliad is as follows:

-   0–2 days delay, depending on the temporal.block (marked NA)\*

-   1 day median water of the mu1k1 treatment

-   *sample* the protist community (*no water* on this day)

-   60 days of treatment watering

-   1 day treatment median

-   *sample* the protist community again (*no water* on this day)

-   one NA day with no watering or sampling

-   destructively sample the bromeliad for all macroinvertebrates
    (*insects*)

-   0–2 days delay, depending on temporal.block\*

\*The number of these days always sums to 2 (i.e. they are either at the
beginning or end of the experiment, depending on temporal.block)

Thus the full treatment period takes **68 days**.

Note that the NA days at the end (i.e. on days 67 or 68) are simply
included to keep the schedule looking “square”.
