library(tidyverse)

generateData <- function(n = 100, seed) {
  set.seed(seed)
  
  names <- c("Acipayam Sheep","Adal Sheep","Afghan Arabi Sheep","Africana Sheep","Alai Sheep","Alcarrena Sheep","Algarve Churro Sheep","Algerian Arab Sheep","Altai Sheep","Altay Sheep","American Blackbelly Sheep","Apennine Sheep","Arabi Sheep","Arapawa Island Sheep","Awassi Sheep","Balkhi Sheep","Baluchi Sheep","Balwen Welsh Mountain Sheep","Barbados Blackbelly Sheep","Bavarian Forest Sheep","Bentheimer Landschaf Sheep","Bergamasca Sheep","Beulah Speckled-Face Sheep","Bibrik Sheep","Biellese Sheep","Black Welsh Mountain Sheep","Blackhead Persian Sheep","Bleu du Maine Sheep","Bluefaced Leicester Sheep","Bond Sheep","Booroola Merino Sheep","Border Leicester Sheep","Boreray Sheep","Bovska Sheep","Braunes Bergschaf Sheep","Brazilian Somali Sheep","Brecknock Hill Cheviot Sheep","British Milk Sheep","Brillenschaf Sheep","Bündner Oberland Sheep","California Red Sheep","California Variegated Mutant Sheep","Campanian Barbary Sheep","Castlemilk Moorit Sheep","Charollais Sheep","Cheviot Sheep","Chios Sheep","Cholistani Sheep","Clun Forest Sheep","Coburger Fuchsschaf Sheep","Columbia Sheep","Comeback Sheep","Comisana Sheep","Coopworth Sheep","Cormo Sheep","Corriedale Sheep","Cotswold Sheep","Criollo Sheep","Daglic Sheep","Dala Sheep","Dalesbred Sheep","Damani Sheep","Damara Sheep","Danish Landrace Sheep","Dartmoor Sheep","Debouillet Sheep","Delaine Merino Sheep","Derbyshire Gritstone Sheep","Dorper Sheep","Devon Closewool Sheep","Deutsches Blaukoepfiges Fleischschaf Sheep","Dorset Down Sheep","Dorset Sheep","Drysdale Sheep","Elliottdale Sheep","Exmoor Horn Sheep","Fabrianese Sheep","Faeroes Sheep","Finnsheep Sheep","Fonthill Merino Sheep","Friesian Milk Sheep","Galway Sheep","Gansu Alpine Fine-Wool Sheep","Gentile di Puglia Sheep","German Blackheaded Mutton Sheep","German Mountain Sheep",
             "German Mutton Merino Sheep","German Whiteheaded Mutton Sheep","Gotland Sheep","Graue Gehoernte Heidschnucke Sheep","Gromark Sheep","Gulf Coast Native Sheep","Gute Sheep","Hampshire Sheep","Han Sheep","Harnai Sheep","Hasht Nagri Sheep","Hazaragie Sheep","Hebridean Sheep","Herdwick Sheep","Hill Radnor Sheep","Hog Island Sheep","Hu Sheep","Icelandic Sheep","Ile-de-France Sheep","Istrian Pramenka Sheep","Jacob Sheep","Jezersko-Solcava Sheep","Kachhi Sheep","Kajli Sheep","Karakul Sheep","Katahdin Sheep","Kerry Hill Sheep","Kooka Sheep","Langhe Sheep","Lati Sheep","Leicester Longwool Sheep","Leineschaf Sheep","Lincoln Sheep","Llanwenog Sheep","Lleyn Sheep","Lohi Sheep","Lonk Sheep","Luzein Sheep","Manx Loaghtan Sheep","Masai Sheep","Massese Sheep","Medium-Wool Merino Sheep","Mehraban Sheep","Merinolandschaf Sheep","Moghani Sheep","Montadale Sheep","Morada Nova Sheep","Mouflon Sheep","Navajo-Churro Sheep","Norfolk Horn Sheep","North Country Cheviot Sheep","Norwegian Fur Sheep","Old Norwegian Sheep","Orkney Sheep","Ossimi Sheep","Oxford Sheep","Pagliarola Sheep","Pelibüey Sheep","Perendale Sheep","Pinzirita Sheep","Pitt Island Sheep","Poll Merino Sheep","Polwarth Sheep","Polypay Sheep","Pomeranian Coarsewool Sheep","Portland Sheep","Priangan Sheep","Qashqai Sheep","Qinghai Black Tibetan Sheep","Qinghai Semifinewool Sheep","Quadrella Sheep","Quanglin Large-Tail Sheep","Rabo Largo Sheep","Racka Sheep","Rambouillet Sheep","Rasa Aragonesa Sheep","Red Engadine Sheep","Rhoenschaf Sheep","Rideau Arcott Sheep","Romanov Sheep","Romney Sheep","Rouge de I`Quest Sheep","Rough Fell Sheep","Royal White Sheep","Rya Sheep","Ryeland Sheep","Rygja Sheep","Sahel-type Sheep","Santa Cruz Sheep","Santa Inês Sheep","Sardinian Sheep","Sar Planina Sheep","Scottish Blackface Sheep","Sicilian Barbary Sheep","Shetland Sheep","Shropshire Sheep","Skudde Sheep","Soay Sheep","Somali Sheep","Sopravissana Sheep","South African Merino Sheep","South Afrian Mutton Merino Sheep","South Suffolk Sheep","Southdown Sheep","South Wales Mountain Sheep","Spaeslau Sheep","Spiegel Sheep","St. Croix (Virgin Island White) Sheep","Steigar Sheep","Steinschaf Sheep","Strong Wool Merino Sheep","Suffolk Sheep","Sumavska Sheep","Swaledale Sheep","Swedish Fur Sheep","Targhee Sheep","Teeswater Sheep","Texel Sheep","Thalli Sheep","Tong Sheep","Touabire Sheep","Tsurcana Sheep","Tunis Sheep","Tyrol Mountain Sheep","Uda Sheep","Ujumqin Sheep","Ushant Sheep","Valais Blacknose Sheep","Vendéen Sheep","Walachenschaf Sheep","Wallis Country Sheep","Waziri Sheep","Weisse Hornlose Heidschnucke Sheep","Welsh Hill Speckled Face Sheep","Welsh Mountain Sheep","Welsh Mountain Badger Faced Sheep","Wensleydale Sheep","West African Dwarf Sheep","White Suffolk Sheep","Whiteface Dartmoor Sheep","Whiteface Woodland Sheep","Wiltshire Horn Sheep","Xinjiang Finewool Sheep","Yankasa Sheep","Yemen White Sheep","Yemeni Sheep","Yiecheng Sheep","Yoroo Sheep","Yunnan Semifinewool Sheep","Zaghawa Sheep","Zagoria Sheep","Zaian Sheep","Zaire Long-Legged Sheep","Zakynthos Sheep","Zeeland Milk Sheep","Zel Sheep","Zelazna Sheep","Zemmour Sheep","Zeta Yellow Sheep","latusha Sheep","Zoulay Sheep")
  
  nobs <- n
  name <- vector()
  
  if (n > length(names)) {
    nobs <- length(names)
    name <- names
  }
  else {
    name <- sample(names, nobs)
  }
  
  x_1 <- round(rnorm(n=nobs,mean=100,sd=100*0.3)) # Male weight (kg)
  
  x_2 <- vector() # Female weight (kg)
  for (i in 1:nobs) {
    # female are always lighter than male
    x_2 <- c(x_2, round(sample((x_1[i]*0.6):(x_1[i]*0.9),1)))
  }
  
  x_3 <- round(rnorm(n=nobs,mean=50,sd=15), 2) # Meat result (%)
  for (i in 1:length(x_3)) {
    # minimal meat result is 30% and maximum is 75%
    if (x_3[i] < 30) x_3[i] <- round(sample(30:50, 1), 2) 
    if (x_3[i] > 75) x_3[i] <- round(sample(50:75, 1), 2) 
  }
  
  x_4 <- round(rnorm(n=nobs,mean=0.2,sd=0.12), 3) # Live weight gain per kilogram of feed (kg)
  for (i in 1:length(x_4)) {
    # minimal weight gain per kilogram is 0.1kg
    if (x_4[i] < 0.1) x_4[i] <- round((sample((0.1*10000):(0.2*10000), 1)/10000), 3) 
  }
  
  x_5 <- round(rnorm(n=nobs,mean=250,sd=100), 2) # Fertility (%)
  for (i in 1:length(x_5)) {
    # minimal fertility is 100%
    if (x_5[i] < 100) x_5[i] <- round(sample(100:200, 1), 3)
  }
  
  # BINARY
  x_6 <- sample(c(T,F),size=nobs,replace=TRUE,prob=c(0.5,0.5)) # Presence of a breeder of this breed in the country
  
  # CATEGORICAL - UNORDERED
  x_7 <- as.factor(sample(c("hybrid","thoroughbred"),size=nobs,replace=TRUE,prob=c(0.5,1)))
  
  x_1_q1 <- quantile(x_1, 0.25)
  x_1_q2 <- quantile(x_1, 0.5)
  x_1_q3 <- quantile(x_1, 0.75)
  print(x_1_q1)
  print(x_1_q2)
  print(x_1_q3)
  tmp <- vector()
  for (i in 1:nobs) {
    # lighter and hybrid breeds sheep have better immunity
    prob <- data.frame(weak = 1, normal = 1, strong = 1, weight = x_1[i])
    
    if (x_1[i] <= x_1_q1) {
      prob$weak = prob$weak*0.1
      prob$normal = prob$normal*0.3
      prob$strong = prob$strong*3
    }
    else if (x_1[i] > x_1_q1 & x_1[i] <= x_1_q2) {
      prob$weak = prob$weak*0.5
      prob$normal = prob$normal*1.5
      prob$strong = prob$strong*2
    }
    else if (x_1[i] > x_1_q2 & x_1[i] <= x_1_q3) {
      prob$weak = prob$weak*2
      prob$normal = prob$normal*1.5
      prob$strong = prob$strong*0.5
    }
    else if (x_1[i] > x_1_q3) {
      prob$weak = prob$weak*3
      prob$normal = prob$normal*0.3
      prob$strong = prob$strong*0.1
    }
    
    if (x_7[i] == "hybrid") {
      prob$weak = prob$weak*0.3
      prob$normal = prob$normal*1
      prob$strong = prob$strong*3
    }
    else if (x_7[i] == "thoroughbred") {
      prob$weak = prob$weak*3
      prob$normal = prob$normal*1
      prob$strong = prob$strong*0.3
    }
    
    # CATEGORICAL - ORDERED
    tmp <- c(tmp, sample(c("weak","normal","strong"),size=1,prob=c(prob$weak,prob$normal,prob$strong)))
  }
  x_8 <- factor(tmp,levels=c("weak","normal","strong"),ordered=TRUE)
  
  eps <- rnorm(n, m=0, sd=3)
  
  y <- 2*x_1/100 + 2*x_2/100 + 3*x_3/100 + 5*x_4*10 + 3*x_5/50 + 10*(x_8=="strong") + 3*(x_8=="normal") - 7*(x_8=="weak") + eps
  
  return(data.frame(
    name=name,
    weight_male=x_1,
    weight_female=x_2,
    meat_result=x_3,
    weight_per_feed=x_4,
    fertility=x_5,
    availability_in_poland=x_6,
    breed_type=x_7,
    immunity=x_8,
    eps=eps,
    efficiency=y
  ))
}

nobs <- 100

result <- generateData(nobs, 3333)

fit.lm <- lm(result$efficiency ~ result$meat_result, data = result)

fit.lm2 <- lm(
  result$efficiency ~ 
    result$weight_male + 
    result$weight_female + 
    result$meat_result + 
    result$weight_per_feed + 
    result$fertility +
    result$breed_type +
    result$immunity, 
  data = result)

y_hat <- predict(fit.lm, newdata = result)
y_hat

summary(result$efficiency - y_hat)

n <- nrow(result)

r <- cor(result$efficiency,result$weight_male)^2 

par(mar = c(1, 1, 1, 1))

plot(dta$x,dta$y,col="grey")
xgrid <- seq(from=min(result$meat_result), to=max(result$meat_result), length.out=100)
y_hat <- predict(fit.lm, newdata = data.frame(x=xgrid) )
xgrid

plot(x=result$meat_result,y=result$efficiency,col="grey",pch=16, cex=0.7, xlab="x",ylab="y")
lines(x=xgrid, y=f(xgrid),col="green",lty=1)    # true f(x)
lines(x=xgrid, y=y_hat,col="red",lty=1)         # fitted y_hat = f_hat(x)

write_csv(result, "~/Dev/study/data_science/data-generation/sheep-efficiency_clean.csv")

generateNa <- function(dataframe, nobs, percent) {
  newDf <- dataframe
  coef <- percent/100
  
  for (i in colnames(newDf)){
    v_na <- replace(newDf[[i]], sample(c(T, F), size = nobs, replace = T, prob = c(1-coef, coef)), NA)
    newDf[[i]] <- v_na
  }
  
  return (newDf)
}

result_na <- generateNa(result, nobs, 95)
write_csv(result_na, "~/Dev/study/data_science/data-generation/sheep-efficiency_na.csv")