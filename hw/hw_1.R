
count=count3=count4=count5=0
luck=sort(c(1,20,10,49,40,30))

while (sum(sort(sample(1:49,6))==luck)!=6) {
  if (sum(sort(sample(1:49,6))==luck)==3){
    count3=count3+1
  }else if(sum(sort(sample(1:49,6))==luck)==4){
    count4=count4+1
  }else if(sum(sort(sample(1:49,6))==luck)==5){
    count5=count5+1
  }
  count=count+1
  print(count)
}
paste("中了",count3,"張3碼")
paste("中了",count4,"張4碼")
paste("中了",count5,"張5碼")
paste("共買了",count,"張才中頭獎，花了",count*50,"元")
paste("總獎金為",count3*400+count4*2000+count5*3000000+1000000000,"元")
