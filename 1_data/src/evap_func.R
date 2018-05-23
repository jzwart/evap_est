evap_func <- function(airT,jDay,lat){
  #saturated Vapor Density
  svd<-5.018+(.32321*airT)+(0.0081847*airT^2)+(0.00031243*airT^3)

  #Daylength
  degToRad<-2*pi/360
  radToDeg<-180/pi

  #day angle gamma (radians)
  dayAngle<-2*pi*(jDay-1)/365

  #declination of the sun 'delta' (radians)
  dec<-0.006918-0.399912*cos(dayAngle)+0.070257*sin(dayAngle)-
    0.006758*cos(2*dayAngle)+0.000907*sin(2*dayAngle)-0.002697*
    cos(3*dayAngle)+0.00148*sin(3*dayAngle)

  # sunrise hour angle 'omega' (degrees)
  latRad<-lat*degToRad
  sunriseHourAngle<-acos(-tan(latRad)*tan(dec))*radToDeg

  #sunrise and sunset times (decimal hours, relative to solar time)
  sunrise<-12-sunriseHourAngle/15
  sunset<-12+sunriseHourAngle/15
  dayLength<-sunset-sunrise #day length in hours

  evap = 0.55*((dayLength/12)^2)*(svd/100)*25.4 #calculates evaporation for each jDay (units are mm/day)
  return(evap)
}
