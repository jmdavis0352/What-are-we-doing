## 

library(plotly)


## https://physics.stackexchange.com/questions/314353/does-all-visible-light-pass-through-the-atmosphere
## https://agupubs.onlinelibrary.wiley.com/doi/pdf/10.1029/1998JD200053


divergence <- function(w, a){
  
  return(w/(pi*a))
}

beamsize <- function(beamdiv, km){
  meters <- km*1000
  return(pi*(beamdiv*meters)^2)
}

################
wl <- 450e-9
aperture <- .2


bdiv <- divergence(wl, aperture)

powerReq <- function(aperture, energyDensity , wavelength = 450, distance, efficiency = .2, atmosphere = .1, M2 ){
  
beamdivergence <- M2*wavelength/(pi*aperture)
spotsize  <- pi*(beamdivergence*distance*1000)^2
  
return(list(Watts = (166666*spotsize)/(atmosphere*efficiency), BeamArea = spotsize))
  
}

ap <- seq(.1, 1, .01)
wl <- seq(445, 490, 5)
m2 <- seq(3,20, 1)

LaserParam <- expand.grid(Aperture = ap, Wavelength = wl, M2 = m2 )

## Geosynch orbit = 35786km

LaserParam$PowerReq <- apply(LaserParam, 1, FUN = function(x){powerReq(aperture = x[1], M2= x[3], energyDensity = 166666, wavelength = x[2]*(1e-9), distance = 35786)$Watts})
LaserParam$BeamArea <- apply(LaserParam, 1, FUN = function(x){powerReq(aperture = x[1], M2= x[3], energyDensity = 166666, wavelength = x[2]*(1e-9), distance = 35786)$BeamArea})
LaserParam$MWatts <- LaserParam$PowerReq/1e7
LaserParam$BeamDiameter <- sqrt(LaserParam$BeamArea/pi)*2
## rough guess at burning is a 1 Watt retail laser with a 2x5mm beam 

.002*.005
