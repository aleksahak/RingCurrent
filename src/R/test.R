for(i in dir("Rlib", full.names=T)){
  source(i)
}


DATA<-RingCurrent(c(0.7,0.5, -3),c(1,1,0),c(1.5,0.5,0),c(1,0,0),c(0.5,0,0),c(0,0.5,0),c(0.5,1,0))
print(DATA$gfPople)
system("./GetRingCurrent.exe 6 0.7 0.5 -3 1 1 0 1.5 0.5 0 1 0 0 0.5 0 0 0 0.5 0 0.5 1 0")



DATA<-RingCurrent(c(0.7,0.5, -3),c(1,1,0),c(1.5,0.5,0),c(1,0,0),c(0.5,0,0),c(0,0.5,0))
print(DATA$gfPople)
system("./GetRingCurrent.exe 5 0.7 0.5 -3 1 1 0 1.5 0.5 0 1 0 0 0.5 0 0 0 0.5 0")


DATA<-RingCurrent(c(0.3,0.2, -3),c(0.5,1,0),c(0,0.5,0),c(0.5,0,0),c(1,0,0),c(1.5,0.5,0.3),c(1,1,0.4))
print(DATA$gfPople)

system("./GetRingCurrent.exe 6 0.3 0.2 -3 0.5 1 0 0 0.5 0 0.5 0 0 1 0 0 1.5 0.5 0.3 1 1 0.4")






