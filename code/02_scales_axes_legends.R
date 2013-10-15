###############################################################################
## ggplot2: axes, scales, legends, polishing plots
## Author: Pablo Barberá
## Data Visualization with R and ggplot2
## October 15th 2013
###############################################################################

###############################################################################
## Data: public Facebook posts from Oct. 2 mentioning "shutdown"
###############################################################################

## backup: 65K public Facebook posts from Oct. 2nd
load("backup/fb.data")

## random sample of N=2000 
df <- fb.data[sample(1:length(fb.data$from_id), 2000),]

# load ggplot2
library(ggplot2)

###############################################################################
## customizing axes
###############################################################################

# changing axis titles
p <- ggplot(df, aes(x=likes_count))
p + geom_histogram() + scale_x_continuous("Number of likes") + 
    scale_y_continuous("Post count")

# changing axis limits (continuous variables)
p <- ggplot(df, aes(x=likes_count))
p + geom_histogram() + scale_x_continuous(limits=c(0, 100)) ## continuous
p + geom_histogram() + scale_x_log10(limits=c(1, 100)) ## in log scale
    

# changing axis limits (categorical variables)
df$language <- substr(df$locale, 1, 2) 
p <- ggplot(df, aes(x=language))
p + geom_bar() + scale_x_discrete(limits=c("en", "es", "ru"))
    # note that this is equivalent to setting labels manually

# changing axis breaks and labels (continuous variables)
p <- ggplot(df, aes(x=likes_count))
p + geom_histogram() + scale_x_log10(breaks=c(1, 10, 100, 1000))
p + geom_histogram() + scale_x_log10(breaks=c(1, 10, 100, 1000, 10000),
    labels=c(1, 10, 100, "1K", "10K"))

# changing axis breaks and labels (categorical variables)
p <- ggplot(df, aes(x=language))
p + geom_bar() + scale_x_discrete(limits=c("en", "es", "ru"),
    labels=c("en" = "English", "es" = "Spanish", "ru" = "Russian"))

# changing axis breaks and labels (date variables)
fb.data$time <- substr(fb.data$created_time, 1, 13)
fb.data$time <- as.POSIXct(fb.data$time, format="%Y-%m-%dT%H")
fb.data$count <- 1
counts <- aggregate(fb.data$count, by=list(time=fb.data$time), FUN=sum)

p <- ggplot(counts, aes(x=time, y=x))
p + geom_line() + scale_x_datetime(breaks="2 hours")

library(scales) # loading additional library scales
p + geom_line() + scale_x_datetime(breaks="2 hours", 
    labels = date_format("%H"))


###############################################################################
## customizing scales (properties of geoms) and legends
###############################################################################

# note that all of these can be linked to a factor variable, as in previous 
# script, and then it becomes an aesthetics; or it can go out as a property
# of the geom and then applies to all of them

p <- ggplot(df, aes(x=likes_count))
p + scale_x_log10() + geom_histogram(color="red") # border of geoms
p + scale_x_log10() + geom_histogram(fill="red") # area of geoms

p <- ggplot(df, aes(x=likes_count, y=comments_count))
p + scale_x_log10() + scale_y_log10() + geom_point(color="red")
p + scale_x_log10() + scale_y_log10() + geom_point(shape=15)
p + scale_x_log10() + scale_y_log10() + geom_point(size=1)

# for a complete list for each geom, type ?geom_point, ?geom_line, ...

# scales of aesthetics can also be modified manually very easily
p <- ggplot(df, aes(x=likes_count, y=comments_count))
p + geom_point(aes(color=type)) + scale_x_log10() + scale_y_log10() +
    scale_color_manual("Post type",
        limits = c("link", "status", "video", "photo"),
        values=c("blue", "grey", "red", "yellow"))

p + geom_point(aes(shape=type)) + scale_x_log10() + scale_y_log10() +
    scale_shape_manual("Post type",
        limits = c("link", "status", "video", "photo"),
        values=c(1, 3, 4, 5)) ## anything from 1 to 25

# changing labels in legend
p + geom_point(aes(shape=type)) + scale_x_log10() + scale_y_log10() +
    scale_shape_manual("Post type",
        limits = c("link", "status", "video", "photo"),
        labels = c("Link", "Status", "Video", "Photo"),
        values=c(1, 3, 4, 5)) ## anything from 1 to 25

###############################################################################
## customizing facets
###############################################################################

# multiple plots (1 factor variable)
p + geom_point() + scale_x_log10() + scale_y_log10() +
    facet_wrap(~type)

p + geom_point() + scale_x_log10() + scale_y_log10() +
    facet_wrap(~type, nrow=4)

p + geom_point() + scale_x_log10() + scale_y_log10() +
    facet_wrap(~type, ncol=4)

# multiple plots (2 factor variables)
p + geom_point() + scale_x_log10() + scale_y_log10() +
    facet_grid(gender~type)

###############################################################################
## changing themes
###############################################################################

p + geom_point() + scale_x_log10() + scale_y_log10() + theme_bw()

p + geom_point() + scale_x_log10() + scale_y_log10() + theme_grey()

p + geom_point() + scale_x_log10() + scale_y_log10() + theme_minimal()

p + geom_point() + scale_x_log10() + scale_y_log10() + theme_classic()

## all theme options can be edited manually using 'theme', e.g.

p + geom_point() + scale_x_log10() + scale_y_log10() +
    theme( axis.ticks = element_blank() ) # removing axis ticks

## see ?theme for all possible options. We will see more examples later.


###############################################################################
## saving plots
###############################################################################

# different ways of doing this, easiest is with 'ggsave'

pq <- p + geom_point() + scale_x_log10() + scale_y_log10() +
    facet_grid(gender~type)

ggsave(pq, file="plots/grid_plot.pdf", height=6, width=6)


###############################################################################
## multiple types of plots on a grid
###############################################################################

library(gridExtra) ## adds more functions to grid

# posts by type of post
p1 <- ggplot(fb.data, aes(x=type)) + geom_bar() + theme_minimal() +
    scale_y_continuous("Number of posts") + 
    scale_x_discrete("Type of post", 
        labels=c("Link", "Photo", "Status", "Video")) +
    theme (axis.ticks.x = element_blank())

# posts by gender (recoding gender into M/F/Page)
table(fb.data$gender, exclude=NULL)
fb.data$gender[is.na(fb.data$gender)] <- "Page"
fb.data$gender[fb.data$gender == "male (hidden)"] <- "male"
table(fb.data$gender, exclude=NULL)

p2 <- ggplot(fb.data, aes(x=gender)) + geom_bar() + theme_minimal() +
     scale_x_discrete("Gender of User", 
        labels=c("Female", "Male", "Page")) +   
    theme (axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())

# average number of likes by type of post
p3 <- ggplot(fb.data, aes(x=type, y=likes_count)) + 
    stat_summary(fun.y="mean", geom="point", size=5, shape=15) +
    theme_minimal() + scale_y_continuous("Average likes count") +
    scale_x_discrete("Type of post", 
        labels=c("Link", "Photo", "Status", "Video")) +
    theme (axis.ticks.x = element_blank()) 

# average number of likes by type of gender
p4 <- ggplot(fb.data, aes(x=gender, y=likes_count)) + 
    stat_summary(fun.y="mean", geom="point", size=5, shape=15) +
    theme_minimal() + scale_x_discrete("Gender of User", 
        labels=c("Female", "Male", "Page")) +   
    theme (axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank())

pdf("plots/multiple_plots_grid.pdf", height=6, width=6)
grid.arrange(arrangeGrob(p1, p2, p3, p4, ncol=2, heights=c(0.6, 0.4)))
dev.off()

 









