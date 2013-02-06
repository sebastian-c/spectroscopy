context("Strip spectra - valid input")

wavs <- 350:2500
r <- 10
c <- length(wavs)

sample_data <- matrix(runif(r*c), nrow=r)

# expect error due to invalid wavlimits
expect_error(strip_spectra(sample_data, datawavs=350:2500, wavlimits=1:3, which=5), "wavlimits should be of length 2")