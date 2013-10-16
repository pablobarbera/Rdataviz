###############################################################################
## ggplot2 basics: grammar of graphics, geom types
## Author: Pablo Barberá
## Data Visualization with R and ggplot2
## October 15th 2013
###############################################################################

###############################################################################
## Data: public Facebook posts from Oct. 2 mentioning "shutdown"
###############################################################################

library(Rfacebook)
## get your token from 'https://developers.facebook.com/tools/explorer/'
## and paste it here:
token <- 'XXXXXXXXXXXXXXXXXXXXXXX'
posts <- searchFacebook("shutdown", token, n=200)
users <- getUsers(posts$from_id, token)

## merging data into a single data frame
names(users)[1] <- "from_id"
users <- users[!duplicated(users$from_id),]
fb.data <- merge(posts, users, by="from_id")


###############################################################################
## Analysis: what posts get more likes?
###############################################################################

## random sample of N=2000 
df <- fb.data[sample(1:length(fb.data$from_id), 2000),]

# load ggplot2
library(ggplot2)

############################################
## UNIVARIATE ANALYSIS, CONTINUOUS VARIABLE
############################################

# base layer
p <- ggplot(df, aes(x=likes_count))

# histogram
p + geom_histogram() ## histogram of likes for each post

# density plot
p + geom_density() ## density of likes for each post

# transforming scale to log10
p + geom_histogram() + scale_x_log10()
p + geom_density() + scale_x_log10()

############################################
## UNIVARIATE ANALYSIS, CATEGORICAL VARIABLE
############################################

df$language <- substr(df$locale, 1, 2) ## first two characters of 'locale'
                                       ## indicate language of user

p <- ggplot(df, aes(x=language))

# bar chart
p + geom_bar() ## number of posts by language

# bar chart (horizontal)
p + geom_bar() + coord_flip()

df$country <- substr(df$locale, 4, 5) ## last two characters of 'locale'
                                      ## indicate country of user

p <- ggplot(df, aes(x=country))
p + geom_bar()  ## number of posts by country

###############################################
## BIVARIATE ANALYSIS, TWO CONTINUOUS VARIABLES
###############################################

# base layer
p <- ggplot(df, aes(x=likes_count, y=comments_count))

# scatter plot
p + geom_point() ## relationship between number of likes and number of comments

p + geom_point() + scale_x_log10() + scale_y_log10() ## log scales

p + geom_point() + stat_smooth(na.rm=T) ## ERROR! why? log(0) = -Inf

p + geom_point() + scale_x_log10() + scale_y_log10() + 
    stat_smooth(na.rm=T, data=df[df$likes_count>0 & df$comments_count>0, ])
                        # applying smooth only where counts > 0

# line plot
fb.data$time <- substr(fb.data$created_time, 1, 13) ## date + hour
fb.data$time <- as.POSIXct(fb.data$time, format="%Y-%m-%dT%H") ## fixing format
fb.data$count <- 1 ## counting number of posts per hour
counts <- aggregate(fb.data$count, by=list(time=fb.data$time), FUN=sum)

p <- ggplot(counts, aes(x=time, y=x))
p + geom_line() ## line: posts about shutdown per hour


############################################################################
## BIVARIATE ANALYSIS, ONE CATEGORICAL VARIABLE AND ONE CONTINUOUS VARIABLE
############################################################################

p <- ggplot(df, aes(x=type, y=likes_count))
p + geom_boxplot() ## number of likes by type of post

p + geom_boxplot() + scale_y_log10()

p + geom_violin() + scale_y_log10()

p <- ggplot(df, aes(x=likes_count)) ## same with density plot
p + geom_density(aes(color=type)) + scale_x_log10()

################################################
## BIVARIATE ANALYSIS, TWO CATEGORICAL VARIABLES
################################################

tab <- data.frame(table(df$gender, df$type)) ## types of posts, by gender
names(tab) <- c("gender", "type", "count")

p <- ggplot(tab, aes(x=gender, y=type))
p + geom_tile(aes(fill=count))


#####################################################
## MULTIVARIATE ANALYSIS, THREE CONTINUOUS VARIABLES
#####################################################

p <- ggplot(df, aes(x=likes_count, y=comments_count, size=shares_count))

p + geom_point()

p + geom_point() + scale_size(trans="log10") + 
        scale_y_log10() + scale_x_log10()



###########################################################################
## MULTIVARIATE ANALYSIS, TWO CONTINUOUS VARIABLES AND ONE CATEGORICAL VAR.
###########################################################################

p <- ggplot(df, aes(x=likes_count, y=comments_count))
p + geom_point() + scale_x_log10() + scale_y_log10() + 
        facet_wrap(~type, nrow=2) ## grid of plots: 2x2, by post type


p <- ggplot(df, aes(x=likes_count, y=comments_count, label=country))
p + geom_text() + scale_x_log10() + scale_y_log10()
        ## geom_text() to use country names instead of points


## counting number of posts by gender and hour
counts.m <- aggregate(fb.data$count[fb.data$gender=="male"], 
                by=list(time=fb.data$time[fb.data$gender=="male"]), FUN=sum)

counts.f <- aggregate(fb.data$count[fb.data$gender=="female"], 
                by=list(time=fb.data$time[fb.data$gender=="female"]), FUN=sum)

counts.m$gender <- "male"; counts.f$gender <- "female"
counts <- rbind(counts.m, counts.f)

p <- ggplot(counts, aes(x=time, y=x, group=gender))
p + geom_line(aes(color=gender)) ## line: posts about shutdown per hour and gender

## scatter plot with dots colored by type of post
p <- ggplot(df, aes(x=likes_count, y=comments_count))
p + geom_point(aes(color=type)) + scale_x_log10() + scale_y_log10()

## same for point shape
p <- ggplot(df, aes(x=likes_count, y=comments_count))
p + geom_point(aes(shape=type)) + scale_x_log10() + scale_y_log10()

## combining both
p <- ggplot(df, aes(x=likes_count, y=comments_count))
p + geom_point(aes(shape=type, color=type)) + scale_x_log10() + scale_y_log10()


## this can be very easily extended to multiple scales
p <- ggplot(df, aes(x=likes_count, y=comments_count))
p + geom_point(aes(shape=gender, color=language, size=shares_count)) + 
        scale_x_log10() + scale_y_log10() + scale_size(trans="log10") +
        facet_wrap(~type, nrow=2)   


###########################################################################
## DEALING WITH OVERPLOTTING
###########################################################################

## jittering points (useful for counts)
p <- ggplot(df, aes(x=likes_count, y=comments_count))
p + geom_jitter(position = position_jitter(width = .5, height=.5)) + 
    scale_x_log10() + scale_y_log10() 

## transparency (note that here I'm using full dataset)
p <- ggplot(fb.data, aes(x=likes_count, y=comments_count))
p + geom_jitter(position = position_jitter(width = .5, height=.5), alpha=1/25) + 
    scale_x_log10() + scale_y_log10()

## hexbin
p <- ggplot(fb.data[fb.data$likes_count>0 & fb.data$comments_count>0,], 
        aes(x=likes_count, y=comments_count))
p + geom_hex() + scale_x_log10() + scale_y_log10() + 
    scale_fill_continuous(trans="log10")











