---
title: "WayFair"
author: "Wilson Sithole"
date: "08/05/2019"
output:
  pdf_document: default
  html_document: default
---

  
  library(dplyr)
	library(ggplot2)
	library(rpart)
	library(rpart.plot)
	library(party)
	library(ROCR)
	library(caret)

	
#Read data from source


Data1_Source <- read.csv("~/WayFair/df_training_scholarjet.csv", header = TRUE)
HoldOutSet1<- read.csv("~/WayFair/df_holdout_scholarjet.csv", header = TRUE)
			#View(Data1_Source)
			str(Data1_Source)
			#View(HoldOutSet1)
			str(HoldOutSet1)

				#Match levels of Hold outSet to levels of Training data
					HoldOutSet1[,which(HoldOutSet1$currentstatus == "Unconfirmed")] <- "Inactive"
	



#VisualiseData
	tail(Data1_Source)

	# Select categorical column
		factor <- data.frame(select_if(Data1_Source,is.factor))
			ncol(factor)

	# Create graph for each column
		graph <- lapply(names(factor),
			function(x)
				ggplot(factor, aes(get(x))) + 
					geom_bar() +
						theme( axis.text.x = element_text(angle = 90)))
							graph



# Select numeric column
	Numeric <- data.frame(select_if(Data1_Source,is.numeric))
		ncol(Numeric)



# Analysis of correlations
	CorrelationsCon <-cor(Data1_Source$convert_30,Numeric[,5:174])
		plot(t(data.frame(CorrelationsCon)))

	CorrelationsRev <-cor(Data1_Source$convert_30,Numeric[,5:174])
		plot(t(data.frame(CorrelationsRev)))



#Cleaning Data
	 Data1_Source[Data1_Source == ""]<-0
		 Data1_Source[is.na(Data1_Source)]<-0
			HoldOutSet1[HoldOutSet1 == ""]<-0
				 HoldOutSet1[is.na(HoldOutSet1)]<-0
					tail(Data1_Source)
						tail(HoldOutSet1)


#Shuffling Data
	shuffle_index <- sample(1:nrow(Data1_Source))
		head(shuffle_index)
			Data2_Shuffled1 <- Data1_Source[shuffle_index,]
				 head(Data2_Shuffled1)


##Standardisation of continuous Variables
	Data2_ShuffledB <- Data2_Shuffled1[,4:184] %>%
		mutate_if(is.numeric, funs(as.numeric(scale(.))))

			#Join Actual Revenue To Standardised CoVariates
					Data2_Shuffled <- cbind(Data2_Shuffled1$convert_30,Data2_ShuffledB)
						names(Data2_Shuffled)[1]<- "convert_30"

##Standardisation of Holdout Set
	HoldOutSet <- HoldOutSet1[,3:182] %>%
		mutate_if(is.numeric, funs(as.numeric(scale(.))))


 
#DropVariables
Data3_Clean <- Data2_Shuffled[,c(1,3:182)]



#CreateTrain/TestSet
	set.seed(1234)
		create_train_test <- function(data, size = 0.8, train = TRUE) {
  			  n_row = nrow(data)
    				total_row = size * n_row
    					train_sample <- 1: total_row
  					  if (train == TRUE) {
   					     return (data[train_sample, ])
   						 } else {
   							 return (data[-train_sample, ])

    }
}
	set.seed(1234)
	data_train <- create_train_test(Data3_Clean, 0.8, train = TRUE)
		data_test <- create_train_test(Data3_Clean, 0.8, train = FALSE)
			dim(data_train)
				dim(data_test)
					prop.table(table(data_train$convert_30))

	set.seed(1234)


#TrainModel
	Model <- ctree(convert_30 ~.,data = data_train)
		 predict <- predict(Model, data_test, type = 'prob')


 # confusion matrix
	ConfusionMatrix <- table(data_test$convert_30, predict > 0.5)
		



#PlotROC 
	predict1 <- as.numeric(unlist(predict))
		testset<- data_test[,1]
			ROCRpred <-prediction(predict1,testset)
				ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
					plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))



	

#AUC
	table_mat <- table(testset, predict1 > 0.5)
		table_mat
 			prediction.obj <- prediction(predict1, testset)
				perf <- performance(prediction.obj, measure = "auc")
					print(perf@y.values)





#FinalPredictions
		FinalPrediction <- predict(Model,HoldOutSet) 



#Write to csv file
	#Use trunc(Convert_30Probability*2) to change probabilities above (0.5 -> 1) and those below( 0.5 -> 0)
		ProbabilityConvert_30 <- FinalPrediction
			PredictedConvert_30 <- trunc(ProbabilityConvert_30*2)
				cuid <- HoldOutSet1$cuid
	 				FirstNewdata <-cbind( cuid,ProbabilityConvert_30)
 						#View(FirstNewdata)



#ExtractValueswithRevenueData
	Data4_revenueSource <- Data1_Source[which(Data1_Source$convert_30 !=0),]
		#View(Data4_revenueSource)


#DataVisualisation
	#(if ggplot fails*) dev.off()
		ggplot(Data4_revenueSource, aes(x = as.numeric(Data4_revenueSource$revenue_30)))+
			 geom_density(alpha = .8, fill = "#FF8999")


#DataIsExpontentiallyDistributed
	#We Use Gamma(α,λ)
		

HoldOutSet[HoldOutSet == ""]<-0
				 HoldOutSet[is.na(HoldOutSet)]<-0

#Standardisation of continuous Variables
	Data4_RescaledRevenueSource <- Data4_revenueSource[,5:184] %>%
		mutate_if(is.numeric, funs(as.numeric(scale(.))))
			head(Data4_RescaledRevenueSource)


				#Join Actual Revenue To Standardised CoVariates
					data5_RescaledRevenueSource <- cbind(Data4_revenueSource$revenue_30,Data4_RescaledRevenueSource)
						names(data5_RescaledRevenueSource)[1]<- "revenue_30"
							 #View(data5_RescaledRevenueSource)

#Standardisation of Holdout Set
	HoldOutSet_Rescaled <- HoldOutSet %>%
		mutate_if(is.numeric, funs(as.numeric(scale(.))))
			head(HoldOutSet_Rescaled)




#CreateTrain/TestSet
	set.seed(885548)
		create_train_test <- function(data, size = 0.8, train = TRUE) {
  			  n_row = nrow(data)
    				total_row = size * n_row
    					train_sample <- 1: total_row
  					  if (train == TRUE) {
   					     return (data[train_sample, ])
   						 } else {
    	
    return (data[-train_sample, ])

    }
}
	data_train_revenue <- create_train_test(data5_RescaledRevenueSource, 0.8, train = TRUE)
		data_test_revenue <- create_train_test(data5_RescaledRevenueSource, 0.8, train = FALSE)
			dim(data_train_revenue)
				dim(data_test_revenue)

#DropVariables
	DataRev_Clean <- subset(data_train_revenue, select = c(revenue_30,	roll_up,	companytypegroup,	team,	accrole,	num_employees,	num_purchases_year,	cost_purchases_year,	enrollmentmethod,	numorderone,	numorderthreeone,	numorderseventhree,	numorderthirtyseven,	numordersixtythirty,	numorderyearsixty,	sumrevone,	sumrevthreeone,	sumrevseventhree,	sumrevthirtyseven,	sumrevsixtythirty,	sumrevyearsixty,	numbamorder,	aov,	dayssincelastord,	cuidshare,	numstores,	pospercentage,	numinf,	numinfconnect,	numinfphone,	numinfquote,	percentlarge,	percdirtythirty, numbilling,	numreturn,	numwims,	numproblem,	numother,	percentresolved,	minnps,	avgnps,	maxnps,	nps_count,	numquote,	numorderfromquote,	quoteconrate,	avgquoteprice,	avgconquoteprice,	numvisitone,	numvisitthreeone,	numvisitseventhree,	numvisitthirtyseven,	numvisitsixtythirty,	numvisityearsixty,	numloggedinone,	numloggedinthreeone,	numloggedinseventhree,	numloggedinthirtyseven,	numloggedinsixtythirty,numloggedinyearsixty			,numsecondsonsiteone,	numsecondsonsitethreeone,	numsecondsonsiteseventhree,	numsecondsonsitethirtyseven,	numsecondsonsitesixtythirty,	numsecondsonsiteyearsixty,	numatcone,	numatcthreeone,	numatcseventhree,	numatcthirtyseven,	numatcsixtythirty,	numatcyearsixty,	dayssincelastvisit,	sumatcprice,	avgatcprice,	numsearchtermsone,	numsearchtermsthreeone,	numsearchtermsseventhree,	numsearchtermsthirtyseven,	numsearchtermssixtythirty,	numsearchtermsyearsixty,	avgpriceone,	avgpricethreeone,	avgpriceseventhree,	avgpricethirtyseven))

	
#TrainModel
	Revenue_Model <- glm(revenue_30 ~.,family = Gamma(link = "log"),data =DataRev_Clean)
		 PredRevMean <- predict(Revenue_Model, data_test_revenue, type = 'response')


#Get Values From Gamma(α,λ) Using PredRevMean As Distribution Mean
	set.seed(1234)
		FinalPredRev <- rgamma(n = length(PredRevMean), shape = PredRevMean*0.835,scale = 0.835)
			RMSE(FinalPredRev,data_test_revenue$revenue_30)


#Predict On Holdoutset
	PredRevHoldOutSet <- predict(Revenue_Model, HoldOutSet_Rescaled, type = 'response')
		FinalPredRevHoldOutSet <- rgamma(n = length(PredRevHoldOutSet), shape = PredRevHoldOutSet*0.835,scale = 0.835)


#Control Against Extreme Values
			PredictedRevenue_30 <-ifelse(FinalPredRevHoldOutSet < max(Data1_Source$revenue_30), trunc(FinalPredRevHoldOutSet)*PredictedConvert_30,max(Data1_Source$revenue_30))
				SecondNewdataRevenue_30 <-cbind(FirstNewdata,PredictedRevenue_30)
					write.csv(SecondNewdataRevenue_30, file = "OutputData.csv")






 			




