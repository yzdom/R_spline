#make splines 
#inputs: x,y vectors of the same length, x a vector from 1 to n 
#pts number of points to be interpolated between each value in x 
#the output cubic spline, mtd methods to be used
#outputs: extended x, y vectors with length = tot_pts
spline0=function(x,y,pts,mtd){
  res=NA
  
  #check if pts valid
  if(pts < 0 | 
     length(x) != length(y) |
     length(x) < 3){res='invalid inputs'}
  else{
    
    #make outputs,  leading y vectors, previous y vectors, derivative vectors
    n=length(x)
    nn=1+(n-1)*(pts+1)
    xx=x
    inc=(c(x[-1],x[n])-x)/(pts+1)
    x+inc
    yy=rep(NA,nn)
    y_n1=c(y[-1],y[n])
    y_n2=c(y[-c(1,2)],y[c(n,n)])
    y_p1=c(c(y[1],y[-n]))
    y_d1=rep(NA,n)
    y_d2=rep(NA,n)
    
    #local maximum, minimum, plateau have frist derivative 0
    mmp = (y <= y_p1 & y <= y_n1) | (y >= y_p1 & y >= y_n1)
    y_d1[mmp]=0
    
    
    
    res=
      #list(x=xx,y=yy)
      #data.frame(xx,yy)
      data.frame(y,y_n1,y_n2,y_p1,y_d1,y_d2,mmp,y_d1)
  }
  
}

set.seed(123)
a=rnorm(23,0,1)
b=spline0(seq(1,23),inp,2)
a
View(b)
plot(a)
lines(seq(23),a)