x <- c(1,3,2,5)
x

x = c(1,6,2)
x

y = c(1,4,3)

length (x)

length (y)

x + y


ls()

ls()

rm(list=ls())


x=matrix (data=c(1,2,3,4) , nrow=2, ncol =2)

x

x=matrix (c(1,2,3,4) ,2,2)

matrix (c(1,2,3,4) ,2,2,byrow =TRUE)

sqrt(x)

x^2

x=rnorm (50)
x
x
x=rnorm (50)
x

x=rnorm (50)
y=x+rnorm (50, mean=50, sd=.1)
cor(x,y)

set.seed (1303)
rnorm (50)
set.seed (1503)
rnorm (50)
set.seed (1503)
rnorm (50)



set.seed (3)
y=rnorm (100)
mean(y)
var(y)
sqrt(var(y))
sd(y)


x=rnorm (100)
y=rnorm (100)
plot(x,y)
plot(x,y,xlab=" this is the x-axis",ylab=" this is the y-axis",
       main=" Plot of X vs Y")

x=rnorm (100)
y=rnorm (100)
plot(x,y)
plot(x,y,xlab=" this is the x-axis",ylab=" this is the y-axis",
     main=" Plot of X vs Y")

x=rnorm (50)
y=rnorm (50)
plot(x,y)
plot(x,y,xlab=" this is the x-axis",ylab=" this is the y-axis",
     main=" Plot of X vs Y")

pdf (" Figure .pdf ")
plot(x,y,col =" green ")
dev.off ()


x=seq (1 ,10)
x

x=1:10
x

x=seq(-pi ,pi ,length =50)

y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour (x,y,f,nlevels =45, add=T)
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)


image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa ,theta =30)
persp(x,y,fa ,theta =30, phi =20)
persp(x,y,fa ,theta =30, phi =70)
persp(x,y,fa ,theta =30, phi =40)

A=matrix (1:16 ,4 ,4)
A

A[2,3]
A[c(1,3) ,c(2,4) ]

A[1:3 ,2:4]

A[1:2 ,]

A[ ,1:2]

A[1,]

A[-c(1,3) ,]

dim(A)



