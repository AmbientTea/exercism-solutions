planet_years <- list(
    "mercury" = 0.2408467,
    "venus"   = 0.61519726,
    "earth"   = 1.0,
    "mars"    = 1.8808158,
    "jupiter" = 11.862615,
    "saturn"  = 29.447498,
    "uranus"  = 84.016846,
    "neptune" = 164.79132
)

earth_year_seconds = 31557600

space_age <- function(seconds, planet) {
  earth_years <- seconds / earth_year_seconds
  planet_years <- earth_years / planet_years[[planet]]
  round(planet_years, digits = 2)
}
