#HMS 520 Final Project
#Kelly Compton

#Ensure all relevant packages are installed: dplyr, tidyr, data.table, ggplot2
#Data was downloaded from: https://ard-request.air-resource.com/data.aspx 

#Analysis 
  #Step 1: Read in dataset into dataframe
    df_temperature_2015 <- read.csv("temperature.csv")
    View(df_temperature_2015)

  #Step 2: Clean datasets
    #Update column names and national park variable names to be more readable
      df_temperature_2015_new <- rename(
        df_temperature_2015,
        Location = Users.of.this.data.file.should.acknowledge.the.National.Park.Service.,
        Date_Time_Local = X,
        Date_Time_UTC = X.1,
        Temp_Deg_C = X.2
      )
      View(df_temperature_2015_new)
      
      df_temperature_2015_new[df_temperature_2015_new == "DEVA-PV"] <- "Death Valley National Park"
      df_temperature_2015_new[df_temperature_2015_new == "GLAC-WG"] <- "Glacier National Park"
      df_temperature_2015_new[df_temperature_2015_new == "LAVO-ML"] <- "Lassen Volcanic National Park"
     
    #Cut out miscellaneous information at top of dataframes
      df_temperature_2015_new <- df_temperature_2015_new[-c(1:11), ]
    
    #Separate date time columns into date | time  
      df_temperature_2015_new <- separate(
        data = df_temperature_2015_new,
        col = Date_Time_Local,
        sep = " ",
        into = c("Date_Local", "Time_Local")
      )
      
      df_temperature_2015_new <- separate(
        data = df_temperature_2015_new,
        col = Date_Time_UTC,
        sep = " ",
        into = c("Date_UTC", "Time_UTC")
      )
      
    #Remove extraneous columns  
      df_temperature_2015_new <- df_temperature_2015_new[ , c(1:6)]
      
    #Convert temperature column to numeric  
      df_temperature_2015_new$Temp_Deg_C <- as.numeric(df_temperature_2015_new$Temp_Deg_C)
      
    #Remove error values (showing up in dataframe as -999)  
      df_temperature_2015_new <- subset(df_temperature_2015_new, Temp_Deg_C > -100)
      
    #Create new column that converts temperature in Celsius to temperature in Fahrenheit  
      df_temperature_2015_new <- mutate(
        df_temperature_2015_new,
        Temp_Deg_F = (Temp_Deg_C*(9/5)+32)
      )
    
    #Find the minimum temperature recorded in the entire dataframe and print corresponding date, time, and location    
      np_min_total <- min(df_temperature_2015_new$Temp_Deg_F)
      print(np_min_total)
      
      print(df_temperature_2015_new[which(df_temperature_2015_new$Temp_Deg_F == np_min_total), ])
      
    #Find the minimum temperature recorded in the entire dataframe and print corresponding date, time, and location    
      np_max_total <-max(df_temperature_2015_new$Temp_Deg_F)
      print(np_max_total)
      
      print(df_temperature_2015_new[which(df_temperature_2015_new$Temp_Deg_F == np_max_total), ])
      
    #Find the day in Death Valley National Park with the largest range in temperature over time period analyzed
      df_temperature_dt <-copy(df_temperature_2015_new)
      dt_temperature <- setDT(df_temperature_dt)[ , ID := .GRP, by = "Location"]
      
      dt_temperature_DV <- dt_temperature[ID < 2, ]
      
      dt_temperature_DV_range <- dt_temperature_DV[, (max(Temp_Deg_F)-min(Temp_Deg_F)),by=list(Date_Local)]
      
      dt_temperature_DV_range <- rename(
        dt_temperature_DV_range,
        Temp_Deg_F_range = V1,
      )
      
      DV_max_Temp_range <- max(dt_temperature_DV_range$Temp_Deg_F_range)
      print(DV_max_Temp_range)
      print(dt_temperature_DV_range[which(dt_temperature_DV_range$Temp_Deg_F_range == DV_max_Temp_range), ])
      
      print(dt_temperature_DV[which(dt_temperature_DV$Date_Local == "4/14/2015")])
      
    #Find the day in Glacier National Park with the largest range in temperature over time period analyzed  
      dt_temperature_Gl <- dt_temperature[(ID > 1) & (ID < 3), ]
      dt_temperature_Gl_range <- dt_temperature_Gl[, (max(Temp_Deg_F)-min(Temp_Deg_F)),by=list(Date_Local)]
      dt_temperature_Gl_range <- rename(
        dt_temperature_Gl_range,
        Temp_Deg_F_range = V1,
      )
      
      Gl_max_Temp_range <- max(dt_temperature_Gl_range$Temp_Deg_F_range)
      print(Gl_max_Temp_range)
      print(dt_temperature_Gl_range[which(dt_temperature_Gl_range$Temp_Deg_F_range == Gl_max_Temp_range), ])
      
      print(dt_temperature_Gl[which(dt_temperature_Gl$Date_Local == "8/1/2015")])
     
    #Find the day in Lassen Volcanic National Park with the largest range in temperature over time period analyzed    
      dt_temperature_LV <- dt_temperature[ID > 2, ]
      dt_temperature_LV_range <- dt_temperature_LV[, (max(Temp_Deg_F)-min(Temp_Deg_F)),by=list(Date_Local)]
      dt_temperature_LV_range <- rename(
        dt_temperature_LV_range,
        Temp_Deg_F_range = V1,
      )
      
      LV_max_Temp_range <- max(dt_temperature_LV_range$Temp_Deg_F_range)
      print(LV_max_Temp_range)
      print(dt_temperature_LV_range[which(dt_temperature_LV_range$Temp_Deg_F_range == LV_max_Temp_range), ])
      
      print(dt_temperature_LV[which(dt_temperature_LV$Date_Local == "8/1/2015")])
      
    #Plot daily average temperature over time - Death Valley National Park
      dt_temperature_DV_dailyav <- dt_temperature_DV[, (mean(Temp_Deg_F)),by=list(Date_Local)]
      
      dt_temperature_DV_dailyav <- rename(
        dt_temperature_DV_dailyav,
        Temp_Deg_F_mean = V1,
      )
      
      DV_tempF_plot <- ggplot(dt_temperature_DV_dailyav) + 
        geom_point(aes(Date_Local, Temp_Deg_F_mean))
      
      print(DV_tempF_plot + 
        ggtitle("Daily average temperature (degrees F) reported \nin Death Valley National Park, 2015-2021") + 
        labs(y="Daily average temperature (degrees F)", x = "Date")
      )
      
    #Plot daily average temperature over time - Glacier National Park 
      dt_temperature_Gl_dailyav <- dt_temperature_Gl[, (mean(Temp_Deg_F)),by=list(Date_Local)]
      
      dt_temperature_Gl_dailyav <- rename(
        dt_temperature_Gl_dailyav,
        Temp_Deg_F_mean = V1,
      )
      
      Gl_tempF_plot <- ggplot(dt_temperature_Gl_dailyav) + 
        geom_point(aes(Date_Local, Temp_Deg_F_mean))
      
      print(Gl_tempF_plot + 
              ggtitle("Daily average temperature (degrees F) reported \nin Glacier National Park, 2015-2021") + 
              labs(y="Daily average temperature (degrees F)", x = "Date")
      )
      
    #Plot daily average temperature over time - Lassen Volcanic National Park 
      dt_temperature_LV_dailyav <- dt_temperature_LV[, (mean(Temp_Deg_F)),by=list(Date_Local)]
      
      dt_temperature_LV_dailyav <- rename(
        dt_temperature_LV_dailyav,
        Temp_Deg_F_mean = V1,
      )
      
      LV_tempF_plot <- ggplot(dt_temperature_LV_dailyav) + 
        geom_point(aes(Date_Local, Temp_Deg_F_mean))
      
      print(LV_tempF_plot + 
              ggtitle("Daily average temperature (degrees F) reported \nin Lassen Volcanic National Park, 2015-2021") + 
              labs(y="Daily average temperature (degrees F)", x = "Date")
      )
      
  #Note: the above method returned an error in my plot results; as date was not being read in date format, it was not plotting temperature readings over time; 
      #rather, the plot shows all temperature readings in January (1/) clustered at the left of the plot, then all temperature readings in October (10/). The intended 
      #would have been a time-dependent plot that shows 1/1/2015, then 1/2/2015, then 1/3/2015... etc. all the way to 10/31/2021 on the far right. For the purpose of 
      #HMS520, I was not able to resolve this issue in the time allotted; however, this is something that I am curious to resolve and further explore as a research project
      #on my own time.
      