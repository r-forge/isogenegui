\title{IsoGeneGUI Graphical User Interface for the IsoGene package}
\name{IsoGeneGUI-package}
\alias{IsoGeneGUI-package}

\docType{package}

\description{
The IsoGene Graphical User Interface (IsoGene-GUI) is a user friendly interface of the IsoGene package which is aimed to identify for genes with a monotonic trend in the expression levels with respect to the increasing doses using several test statistics: global likelihood ratio test (E2), Bartholomew 1961, Barlow et al. 1972 and Robertson et al. 1988), Williams (1971, 1972), Marcus (1976), the M (Hu et al. 2005) and the modified M (Lin et al. 2007). 

The p-values of the global likelihood ratio test (E2) are obtained using the exact distribution and permutation. The other four test statistics are obtained using permutation . 

Several p-values adjustment are provided:  Bonferroni, Holm (1979), Hochberg (1988), and Sidak procedures for controlling the family-wise Type I error rate (FWER), and BH (Benjamini and Hochberg 1995) and BY (Benjamini and Yekutieli 2001) procedures are used for controlling the FDR.
}


\details{
The IsoGene Graphical User Interface (IsoGene-GUI) is a user friendly interface of the IsoGene package.
}


\author{
Author: Setia Pramana

Maintainer: Setia Pramana <setia.pramana@uhasselt.be>
}
\references{
Barlow, R., Bartholomew, D., Bremner, M. and Brunk, H. (1972) Statistical Inference
Under Order Restriction. New York: Wiley.

Bartholomew, D. (1961) Ordered tests in the analysis of variance. Biometrika, 48, 325-
332.

Benjamini, Y. and Yekutieli, D. (2001) The control of the false discovery rate in multiple
testing under dependency. ANN STAT, 29 (4), 1165-1188.

Hu, J., Kapoor, M., Zhang, W., Hamilton, S. and Coombes, K. (2005) Analysis of dose
response effects on gene expression data with comparison of two microarray platforms.
Bioinformatics, 21(17), 3524-3529.

Lin, D., Shkedy, Z., Yekutieli, D., Burzykowski, T., Gohlmann, H., De Bondt, A., Perera,
T., Geerts, T. and Bijnens, L. (2007) Testing for trends in dose-response microarray
experiments: A comparison of several testing procedures, multiplicity and resampling-
based inference. Statistical Applications in Genetics and Molecular Biology, 6(1), Article
26.

Marcus, R. (1976) The powers of some tests of the quality of normal means against an
ordered alternative. Biometrika, 63, 177-83.
 
Robertson, T., Wright, F. and Dykstra, R. (1988) Order Restricted Statistical Inference.
Wiley.

S. Holm (1979). A simple sequentially rejective multiple test procedure. Scand. J. Statist.. Vol. 6:
65-70.

Y. Benjamini and Y. Hochberg (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. J. R. Statist. Soc. B. Vol. 57: 289-300.


Y. Hochberg (1988). A sharper Bonferroni procedure for multiple tests of significance, Biometrika.
Vol. 75: 800-802.

Williams, D. (1971) A test for differences between treatment means when several dose
levels are compared with a zero dose control. Biometrics, 27, 103-117.

Williams, D. (1972) The comparison of several dose levels with a zero dose control. Bio-
metrics, 28, 519-531.

}

\keyword{package}
\examples{
\dontrun{
library(IsoGeneGUI)
IsoGeneGUI()
}
}
