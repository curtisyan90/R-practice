# ctrl(cmd)+Enter to compile by line
#help(fun_name) search function
#?fun_name search about function
#??fun_name search all about function (?
#shift+cmd+c to comment


"hello world"
5+5
plot(1:10)
plot(cumsum(1:10), col="purple", type = "c")

sum(100:1)
cumsum(100:1)

prod(1:10) #10!

pl=c(100,-70,50,30,-50,70,-90)
pl[pl<0]
mean(pl[pl<0])
plot(pl, type="h", lwd=5, col="blue")
sum(pl)

plot(pl, type="l", lwd=5, col="blue")

sign(-10)

3<=3

(!(3<3))+(3<=3)

a=6
b<-6

x=5:1
x>3
x[4]
x[x<4] #find value < 4 in x

rep(5,7)
seq(1,100,2) #from 1 to 100 step 2

rm(list = ls()) #clear all values

#for loop
x=NULL
for(i in 1:10){
  x=i
}

#while
i=j=0
while (i<100) {
  i=i+1
  j=j+i
}

?sample
sample(1:9,8) #取8
sample(1:9,8,replace=TRUE) # replace=TRUE => 取後放回

sort(sample(1:49,6),decreasing = TRUE)


num=1:6
count=count3=count4=count5=count6=0
while (sum(sort(sample(1:49,6))==num)!=4) {
  if (sum(sort(sample(1:49,6))==num)==3){
    count3=count3+1
  }
  count=count+1
  print(count)
}
paste("buy",count,"lottery, cost",count*50,"$")

# cbind() combine table by column

# library(showtext)
# showtext.auto(enable = TRUE)
