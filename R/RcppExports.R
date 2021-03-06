# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

BIMCpp <- function(oneboostImmigrate, train_xx, train_yy, nIter = 10L, max_iter = 10L, removesmall = FALSE, sigstart = 0.02, sigend = 4) {
    .Call('_Immigrate_BIMCpp', PACKAGE = 'Immigrate', oneboostImmigrate, train_xx, train_yy, nIter, max_iter, removesmall, sigstart, sigend)
}

IM4ECpp <- function(oneIM4E, train_xx, train_yy, epsilon = 0.01, sig = 1, lambda = 1, max_iter = 10L, removesmall = FALSE) {
    .Call('_Immigrate_IM4ECpp', PACKAGE = 'Immigrate', oneIM4E, train_xx, train_yy, epsilon, sig, lambda, max_iter, removesmall)
}

ImmigrateCpp <- function(oneImmigrate, train_xx, train_yy, w0, epsilon = 0.01, sig = 1, max_iter = 10L, removesmall = FALSE) {
    .Call('_Immigrate_ImmigrateCpp', PACKAGE = 'Immigrate', oneImmigrate, train_xx, train_yy, w0, epsilon, sig, max_iter, removesmall)
}

ImmigrateSampleCpp <- function(onesampleImmigrate, train_xx, train_yy, sample_wt, W, epsilon = 0.01, sig = 1, max_iter = 10L, removesmall = FALSE) {
    .Call('_Immigrate_ImmigrateSampleCpp', PACKAGE = 'Immigrate', onesampleImmigrate, train_xx, train_yy, sample_wt, W, epsilon, sig, max_iter, removesmall)
}

