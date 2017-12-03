


#### Libraries ####
# library(sp)
library(dplyr)
install.packages("ncdf4") #
library(ncdf4) # for most netcdf stuff
#library(RNetCDF) # better header output
# library(chron)
library(reshape2) #
library(ggplot2)
#### functions ####

#### STUFF ####
setwd("D:/My Folders/R/2016/blog/20160804_r_netcdf_cheatsheet")


# Get a list of netcdf files in your data folder
flist <- list.files(path = "data/", pattern = "^.*\\.(nc|NC|Nc|Nc)$")

# Open a connection to one file to get an idea about the data inside it.
nc <- nc_open(paste0("data/", flist[1]))

# print out important info about whats inside the NC file
print(nc)
# if the info is difficult to navigate and keep referring to in my console, I like to sink it to a text file.
sink(paste0("data/", flist[1], ".txt"))
print(nc)
sink()

# we can also reference the details from the above print function via the nc files attributes.
# for examplecalling the attributes of the nc file, we can get
attributes(nc)
print(paste("The file has",nc$nvars,"variables"))
print(paste("The file has",nc$ndims,"dimensions"))
print(paste("The file has",nc$natts,"attributes"))

# calling the attributes of the nc variables allows us to get a list of the attribute names.
# These match wat we see in the text file but allow us to avoid scribing errors.
attributes(nc$var)$names
attributes(nc$var)$names[1]
attributes(nc$var)$names[2]
attributes(nc$var)$names[3]

# lets get all the variables using the ncvar_get function
chla_mean <- ncvar_get(nc, attributes(nc$var)$names[1])
chla_flags <- ncvar_get(nc, attributes(nc$var)$names[2])
chla_error <- ncvar_get(nc, attributes(nc$var)$names[3])

# focusing ont he chla_mean variable, we can get an idea of the matrixes dimensions
dim(chla_mean)

# lets have a look at the dimensions attribute of the nc file and see if this is what we expect.
attributes(nc$dim)$names
attributes(nc$dim)$names[1]
attributes(nc$dim)$names[2]

lat <- ncvar_get( nc, attributes(nc$dim)$names[1])
lon <- ncvar_get( nc, attributes(nc$dim)$names[2])

dim(lat)
dim(lon)

# So we see the domain of the data covers 453 longitudes and 256 latitudes
# and the chl data is arranged in lon(rows) by lat(cols).
# this is what we would expect when inspecting the CHL1_mean variable in the text file we created earlier.

# lets take a quick look at a random section of this data
chla_mean[35:40, 245:240]
# So now we'll change the dimension names of our matrix to 'lon' and 'lat', and the row and column names to the latitude and longitude values.
dimnames(chla_mean) <- list(lon=lon, lat=lat)
chla_mean[35:40, 245:240]

# lastly, you may need to transpose this matrix to make it easier to think about

chla_mean <- t(chla_mean)
chla_mean[245:240, 35:40]

# now lets take a quick look at the attributes.
# we saw above that there are 52 global attributes in this file and they contain all kinds of useful info.
# Lets pull those attributes out of the nc file and investigate a bit

nc_atts <- ncatt_get(nc, 0)

names(nc_atts)

# now if there are any important attributes you can easily retrieve them.
# What you want to store is really up to your analysis but for this example, we'll just look at dates.
# for instnace:

# date-times
date_time_start <- as.POSIXct(nc_atts$start_time, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
date_time_end <- as.POSIXct(nc_atts$end_time, format = "%Y%m%dT%H%M%SZ", tz = "UTC")

nc_close(nc.tmp)
# Ok, so we've investigated the various properties of a netCDF file and seen how to extract and store variables.
# This is daily chlorophyll-a data, so we have a whole directory of files to investigate.
# We'll loop through them and add the data to a BLAH



process_nc <- function(files){

    for (i in 1:length(files)){
        # open a conneciton to the ith nc file
        nc_tmp <- nc_open(paste0("data/", files[i]))
        # store values from variables and atributes
        nc_chla <- ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[1])
        nc_lat <- ncvar_get(nc_tmp, attributes(nc_tmp$dim)$names[1])
        nc_lon <- ncvar_get(nc_tmp, attributes(nc_tmp$dim)$names[2])
        nc_atts <- ncatt_get(nc_tmp, 0)
        nc_start_date <- as.Date(nc_atts$period_start_day, format = "%Y%m%d", tz = "UTC")
        # close the connection sice were finished
        nc_close(nc_tmp)
        # set the dimension names and values of your matrix to the appropriate latitude and longitude values
        dimnames(nc_chla) <- list(lon=nc_lon, lat=nc_lat)

        # I'm choosing to store all the data in long format.
        # depending on your workflow you can make different choices here...
        # Your variable may get unmanageably large here if you have high spatial and temporal resolution nc data.
        tmp_chl_df <- melt(nc_chla, value.name = "chla")
        tmp_chl_df$date_start <- nc_start_date

        # set the name of my new variable and bind the new data to it
        if (exists("chla_data_monthly")){
            chla_data_monthly <- bind_rows(chla_data_monthly, tmp_chl_df)
        }else{
            chla_data_monthly <- tmp_chl_df
        }
        # tidy up, not sure if necesarry really, but neater
        rm(nc_chla, nc_lat, nc_lon, nc_tmp, nc_atts, nc_start_date, tmp_chl_df)
    }

    return(chla_data_monthly)
}


data <- process_nc(flist)




