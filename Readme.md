Working of the Script "run_analysis.R"
-----------------------------------------------
At the very start, the library "dplyr" is loaded so that various functions for data cleaning are available.
Then, the given datasets are loaded into respective dataframes.
The activity codes (i.e. Numbers 1 through 6) are replaced by the actualy activity name. This is done for both the dataframes i.e. Training & Test.
For training dataframe, all the columns are binded. So, at this step, we create one dataframe having the Volunteer Code, Activity Name and 561 feature variables.
The same is done for test dataframe as well.
Next, both the combined Test and Training dataframes are merged into one single dataframe.
Only the required columns regarding mean and standard deviations of feature variables are extracted. Also extracted are Volunteer and Activity Name.
The names of the variables are standardized so as to have meaningful names.
Finally, the tidy dataset is created which has the averages of the readings - grouped on volunteer and activity name.
