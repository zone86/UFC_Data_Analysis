######################################################################################
1.0. Data Description
This project explores UFC fight data from 2013. It contains a mixed data set that contains information on fighter attribues (e.g. age, weight, height, etc), as well as actual fight data metrics (such as how many strikes were thrown in bout A with fighters X and Y, takedowns, etc), who won (the blue figher or red figher), how they won (DEC,TKO), and where and when the fight took place.


######################################################################################
2.0. How to use

* To retrieve relevant data, source the get_UFCData.R file. 

* To create fighter statistics, such as striking, takedown percentages, etc, source the createFighterStatistics.R script.

* UFC_featSel.R contains initial data exploration and simple statistical analysis, as well as some basic analysis and modelling. Models here are created without the use of fighter metric percentages.

* The modelAllData.R script can be used to be sourced both the 
createFighterStatistics.R & get_UFCData.R files. And contains a second attempt to perform feature selection and model the data with the mlr package. Models here are created with the use of fighter metric percentages, and compared to performance with the simple features used in UFC_featSel.R.


######################################################################################
3.0. Data Exploration & Modelling
I initally explored the data in UFC_featSel.R. Here I found that the red fighter, age (being younger generally, but peak winning age was 31 years old), and a weight advantage were significantly correlated with winning. I later discovered that the red fighter advantage is due to the fact the red fighter is designated by the UFC based on the more popular and ranked fighter. In championship fights, the reigning champion is designated the red fighter status. This is a tradition that comes from boxing. Therefore, I beleive there is likely an interaction between the red fighter status and being the younger opponent, as well as being the heavier opponent, and decided to include a "red younger" and "red heavier" as a binary features in modelling the data.

When predicting if the red fighter or the blue fighter would win, the best performing model (a neural net) containing the aforementioned features alone intially produced average model accuracy of % (Brier: .; AUC: .) over 30 random seed iterations. The learning curve for this model showed that after sampling 100% of the data performance was: Brier: .231; AUC: .626; Acc: .620. 

After performing feature filering on fighter metric percentages, and experimenting with a variety features using a number of different feature selection methods, it seemed as though the features in the inital model were the most important, and ultimately performance could not be imporved upon.

Therefore knowing striking percentages might be mostly irrelevent when you consider the size of the effect of the age's of the two fighters, the difference between the two, and if the red fighter has a weight advantage. 

In summary, the most likely outcome for a red fighter's victory is when the red fighter is closer to their prime fighting age (about 31) than their blue opponent, and when the blue opponent is past their prime fighting age.

######################################################################################
Limitations And Future Exploration

However, while I do concede that by creating fight metrics from the data that was used to predict the outcome would mean that the methodology suffers from data leakage, I do beleive that it would be more useful to leverage the data to try and predict an outcome based on prior information (e.g. the fightrer's age, if they're the red fighter, if there is a weight advantage, and other fight metric information such as significant strike percentages), than than attempt to predict the outcome once the information is already known (i.e. who would win if we knew how many strikes were thrown, how many takedowns there were, etc).

Furthermore, it also would be good to get a wider range of non correlated features, as the current model relies perhaps too heavily on feaures related to age. And with regards to the With regards to what could still be explored, it could be interesting to see if there are clusters of related and non-related features within the fight metric data, perhaps the model could be improved by discovering a wider variety of non correlated features and perhaps using the principal components from this data could be used as new features.

 



