read.temp <- function(file) {
  a <- read.csv(file)
  a$datetime <- as.POSIXct(a$timestamp, origin="1970-01-01")
  a
}

tempplot <- function(t, main="Lämpötilakäppyrä") {
  plot(t$datetime, t$value, type="l", format="%d.%m.%Y",
       main=main,xlab="aika",ylab="lämpötila, °C")
}

multitempplot <- function(a,b, main="Yhdistelmäkäppyrä", colors=c("red","blue")) {
  datetimes <- c(a$datetime, b$datetime)
  values <- c(a$value, b$value)

  plot(datetimes, values,
       type="n", format="%d.%m.%Y",main=main,
       xlab="aika",ylab="lämpötila, °C")
  
  lines(a$datetime, a$value, col=colors[1])
  lines(b$datetime, b$value, col=colors[2])
  
  legend(min(datetimes),relativepos(values,0.15),c("ulkoilma","laitetila"),col=colors,pch=3,bg="white")

}

relativepos <- function(xs,percent) {
  limits <- range(xs)
  limits[1] + (limits[2]-limits[1])*percent
}
