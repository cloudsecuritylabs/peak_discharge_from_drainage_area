#Drainage-area-only regional regression equations forestimating 
#peak discharges of streams in Virginia
#A, drainage area, in square miles. 
#Peak-discharge regions are shown in figure 1 and plate 11
#Source: http://pubs.usgs.gov/wri/wri944148/pdf/wrir_94-4148_a.pdf

A.acre=500 #update the area of the drainage basin.
A=A.acre*0.0015625 #convert acre to sq. miles
A
frequency=c(93,162,230,341,441,557,691,902)

names(frequency)=c('2yr','5yr','10yr','25yr','50yr','100yr','200yr','500yr')

frequency
pow=c(.84,.828,.809,0.784,.767,.751,.736,.717)
SE.percent=c(32.7,19.9,17.8,20.7,24.0,27.8,31.4,36.3) #Standard Error associated with each regression equation


discharge={}
min={}
max={}

for(i in 1:8){
  #print(i)
  discharge[i]=c(frequency[i]*A^pow[i])
  min[i]=discharge[i]-discharge[i]*SE.percent[i]/100
  max[i]=max[i]=discharge[i]+discharge[i]*SE.percent[i]/100
}

table(discharge)
names(discharge)=c('2yr','5yr','10yr','25yr','50yr','100yr','200yr','500yr')
names(min)=c('2yr','5yr','10yr','25yr','50yr','100yr','200yr','500yr')
names(max)=c('2yr','5yr','10yr','25yr','50yr','100yr','200yr','500yr')

print("PEAK FLOW discharge IN CUBIC FEET PER SECOND")
round(discharge,2)
round(min,2)
round(max,2)

tab=cbind(discharge,min,max)
#names(tab)=c('discharge(cfs)', 'min(cfs)', 'max(cfs)')
round(tab,2)

#In Gallons per Min
round(tab*448.8,0)




