\title{IsoGeneGUI Graphical User Interface for the IsoGene package}
\name{IsoGeneGUI-package}
\alias{IsoGeneGUI-package}

\docType{package}

\description{
The IsoGene Graphical User Interface (IsoGene-GUI) is a user friendly interface of the IsoGene package which is aimed to identify for genes with a monotonic trend in the expression levels with respect to the increasing doses. Additionally, GUI extension of original package contains various tools to perform clustering of dose-response profiles. Testing is addressed through several test statistics: global likelihood ratio test (E2), Bartholomew 1961, Barlow et al. 1972 and Robertson et al. 1988), Williams (1971, 1972), Marcus (1976), the M (Hu et al. 2005) and the modified M (Lin et al. 2007). 

The p-values of the global likelihood ratio test (E2) are obtained using the exact distribution and permutations. The other four test statistics are obtained using permutations. Several p-values adjustment are provided: Bonferroni, Holm (1979), Hochberg (1988), and Sidak procedures for controlling the family-wise Type I error rate (FWER), and BH (Benjamini and Hochberg 1995) and BY (Benjamini and Yekutieli 2001) procedures are used for controlling the FDR. The inference is based on resampling methods, which control the False Discovery Rate (FDR), for both permutations (Ge et al., 2003) and the Significance Analysis of Microarrays (SAM, Tusher et al., 2001). 

Clustering methods are outsourced from CRAN packages ORCME, ORIClust. The package ORCME is based on delta-clustering method (Cheng and Church, 2000) and ORIClust on Order Restricted Information Criterion (Liu et al., 2009), both perform same task but from different perspective and their outputs are clusters of genes. Additionally, profile selection for given gene based on Generalized ORIC (Kuiper et al., 2014) from package goric and permutation test for E2 based on package orQA are included in IsoGene-GUI. None of these four packages has GUI. 
}


\details{
The IsoGene Graphical User Interface (IsoGene-GUI) is a user friendly interface of the IsoGene package enriched by methods from packages ORIClust, ORCME, orQA and goric.
}

\author{
Author: Setia Pramana, Dan Lin, Philippe Haldermans, Tobias Verbeke, Martin Otava

Maintainer: Setia Pramana <setia.pramana@ki.se>
}
\references{

Pramana S., Haldermans P., and Verbeke, T. (2012) A User Friendly Interface for Analyzing Dose-response Studies in Microarray Experiments: The IsoGeneGUI package. In Lin, D., Shkedy, Z., Amaratunga,D., Yekutieli, D and Bijnens, L. (Editors), Modeling of Dose-response Microarray Data in Early Drug Development Experiments Using R. Springer.

Barlow, R., Bartholomew, D., Bremner, M. and Brunk, H. (1972) Statistical Inference Under Order Restriction. New York: Wiley.

Bartholomew, D. (1961) Ordered tests in the analysis of variance. Biometrika, 48, 325-332.

Benjamini, Y. and Yekutieli, D. (2001) The control of the false discovery rate in multiple testing under dependency. Annals of Statistics, 29 (4), 1165-1188.

Cheng, Y. and Church, G. M. (2000). Biclustering of expression data. In: Proceedings of the Eighth International Conference on Intelligent Systems for Molecular Biology, 1, 93-103

Hu, J., Kapoor, M., Zhang, W., Hamilton, S. and Coombes, K. (2005) Analysis of dose
response effects on gene expression data with comparison of two microarray platforms.
Bioinformatics, 21(17), 3524-3529.

Kuiper, R. M., Gerhard, D., and Hothorn, L. A. (2014), Identification of the Minimum Efeective Dose for Normally Distributed Endpoints Using a Model Selection Approach. Statistics in Biopharmaceutical Research, 6(1), 55-66.

Lin, D., Shkedy, Z., Yekutieli, D., Burzykowski, T., Goehlmann, H.W.H., De Bondt, A., Perera,
T., Geerts, T. and Bijnens, L. (2007) Testing for trends in dose-response microarray
experiments: A comparison of several testing procedures, multiplicity and resampling-
based inference. Statistical Applications in Genetics and Molecular Biology, 6(1), Article
26.

Lin, D., Shkedy, Z., Yekutieli, D., Amaratunga, D. and Bijnens, L., editors (2012) Modeling Dose-response Microarray Data in Early Drug Development Experiments Using R. Springer.

Liu, T., Lin, N., Shi, N. and Zhang, B. (2009) Information criterion-based clustering with order-restricted candidate profiles in short time-course microarray experiments. BMC Bioinformatics, 10, 146

Marcus, R. (1976) The powers of some tests of the quality of normal means against an ordered alternative. Biometrika, 63, 177-83.
 
Robertson, T., Wright, F. and Dykstra, R. (1988) Order Restricted Statistical Inference. Wiley.

S. Holm (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6, 65-70.

Y. Benjamini and Y. Hochberg (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society: Series B, 57, 289-300.

Y. Hochberg (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika, 75, 800-802.

Williams, D. (1971) A test for differences between treatment means when several dose levels are compared with a zero dose control. Biometrics, 27, 103-117.

Williams, D. (1972) The comparison of several dose levels with a zero dose control. Biometrics, 28, 519-531.

}

\keyword{package}
\examples{
\dontrun{
library(IsoGeneGUI)
IsoGeneGUI()
}
}
