###############################################################################
## Using ggplot2 to plot maps
## Author: Pablo Barberá
## Data Visualization with R and ggplot2
## October 15th 2013
###############################################################################

###############################################################################
## EXAMPLE 1: WORLD MAP WITH TWEETS COLORED BY LANGUAGE
###############################################################################

# load libraries
library(ggplot2)
library(cshapes)
library(scales)
library(grid)

# preparing data frame with country boundaries
world.data <- fortify(cshp(date=as.Date("2012-06-30")))

# base layer: world map
p <- ggplot(world.data, aes(long,lat,group=group)) + 
    geom_polygon(fill="black", color="grey80", size=0.25)
p

# removing axes, ticks, changing background...
pq <- p + theme(
    # dark background
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill="black"),
        # removing axis lines and ticks
        axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    )
pq

# adding tweets from Sept. 15, 2013, colored by language
df <- read.csv("data/loc_lang.csv", stringsAsFactors=F)
df <- df[df$lang!="und",] ## removing "undetermined" language
tab <- sort(table(df$lang), dec=TRUE) ## table for tweets by language
top15lang <- names(tab)[1:15] ## top 15 languages
df <- df[df$lang %in% top15lang,] ## keeping only tweets from top 15 languages

pq <- pq + geom_point(data = df, 
    aes(x = lon, y=lat, color=lang, group=NA), size=0.3)
pq

# customizing legend
langs <- c("Arabic", "German", "English", "Spanish", "French", 
    "Indonesian", "Italian", "Japanese", "Dutch", "Portuguese",
    "Russian", "Thai", "Tagalog", "Turkish", "Vietnamese")
pq <- pq + theme(
        # dark background
        legend.background = element_rect(colour = F, fill = "black"),
        legend.key = element_rect(fill = "black", colour = F),
        # title and text in white
        legend.title = element_text(color="white"),
        legend.text = element_text(color="white"),
        # position and orientation of legend
        legend.position = "bottom",
        legend.direction = "horizontal"
    ) +
    guides(color = guide_legend(nrow=2, override.aes = list(size=5))) +
    scale_color_discrete("Language", labels = langs)
pq

# now only tweets from Europe, with top 20 languages
df <- read.csv("data/loc_lang.csv", stringsAsFactors=F)
df <- df[df$lang!="und",] ## removing "undetermined" language
df <- df[df$lat>32.5 & df$lon>(-14.5) & df$lat<61 & df$lon<35,]
tab <- sort(table(df$lang), dec=TRUE) ## table for tweets by language
top20lang <- names(tab)[1:20] ## top 20 languages
df <- df[df$lang %in% top20lang,]

# fixing area of map using 'coord_map'
# where do coordinates come from? you can just use a website like:
# http://itouchmap.com/

pq <- ggplot(world.data, aes(long,lat,group=group)) + 
    geom_polygon(fill="black", color="grey80", size=0.25) +
    geom_point(data = df, 
        aes(x = lon, y=lat, color=lang, group=NA), size=0.3) +
    coord_map("lagrange", xlim=c(-14.5, 35), ylim=c(32.5,61)) +
    theme(
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill="black"),
        axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        legend.background = element_rect(colour = F, fill = "black"),
        legend.key = element_rect(fill = "black", colour = F),
        legend.title = element_text(color="white"),
        legend.text = element_text(color="white", size=10)
    ) +
    guides(color = guide_legend(override.aes = list(size=5)))
# adding language labels
langs <- c("Arabic", "Danish", "German", "English", "Spanish", "Estonian",
    "French", "Haitian", "Indonesian", "Italian", "Latvian", "Dutch",
    "Polish", "Portuguese", "Russian", "Slovak", "Slovenian", "Swedish", 
    "Tagalog", "Turkish")
pq <- pq + scale_color_discrete("Language", labels = langs)
pq

## saving plot to file
ggsave(pq, file="plots/euro_lang_final.pdf", height=5.5, width=8)


###############################################################################
## EXAMPLE 2: U.S. MAP WITH STATES COLORED BY NUMBER OF TWEETS
###############################################################################
## inspiration: http://cf.datawrapper.de/QgoRl/4/

# location of tweets mentioning "shutdown", October 1st
df <- read.csv("data/shutdown_locations.csv", stringsAsFactors=F)
str(df)

# now we convert each pair of coordinates to a state using the solution here:
# http://stackoverflow.com/q/8751497/1007532

library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per state (plus DC, minus HI & AK)
    states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
    states_sp <- map2SpatialPolygons(states, IDs=IDs,
                     proj4string=CRS("+proj=longlat +datum=wgs84"))

    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                    proj4string=CRS("+proj=longlat +datum=wgs84"))

    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, states_sp)

    # Return the state names of the Polygons object containing each point
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    stateNames[indices]
}

# now we get the state for each pair
df$state <- latlong2state(df)

# how many tweets from each state
tab <- table(df$state, exclude=NULL)
tab <- data.frame(tab); names(tab) <- c("state", "tweets")

## map of the US, where each state is colored according to "tweets"
library(ggplot2)
library(maps)
states_map <- map_data('state')

p <- ggplot(tab, aes(map_id=state))
p + geom_map(aes(fill=tweets), map = states_map) +
         expand_limits(x = states_map$long, y = states_map$lat)

## changing color/size of borders
p + geom_map(aes(fill=tweets), map = states_map, color="yellow") +
         expand_limits(x = states_map$long, y = states_map$lat)

pq <- p + geom_map(aes(fill=tweets), map = states_map, color="grey", size=.1) +
         expand_limits(x = states_map$long, y = states_map$lat)
pq

## changing colors of the scale
pq + scale_fill_continuous(low="white", high="black") ## same as...

pq + scale_fill_gradient(low="white", high="black") ## smoothing btw 2 colors

pq + scale_fill_gradientn(colours = rainbow(7)) ## for >2 colors

pq + scale_fill_gradientn(colours = c("red", "white", "blue")) ## for >2 colors
 
library(RColorBrewer) ## by far the best. Check: http://colorbrewer2.org/
colors <- rev(brewer.pal(7,"YlOrRd"))

pq + scale_fill_continuous(low=colors[7], high=colors[1])

## changing breaks and labels of scale
pq + scale_fill_continuous(trans="log")

pq + scale_fill_continuous(trans="log", breaks=c(10, 100, 1000),
    labels = c("10", "100", "1K"))

## removing axes/background
pq <- pq + theme(
        # removing axes
        axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank(),
        # removing background / borders
        panel.background = element_blank(), plot.background = element_blank(),
        panel.border = element_blank(), panel.grid = element_blank()
    )
pq

## decreasing margin
library(grid)
pq <- pq + theme(plot.margin = unit(c(0, 0, -0.5, -1), "cm"))
## top, right, bottom, left
pq

## decreasing margin wrt legend
pq + theme(legend.margin=unit(-2.5, "cm"))

## fixing margin so that legend is inside plot
pq + theme(legend.margin=unit(-2.5, "cm"),
    plot.margin = unit(c(0, 1, -0.5, -1), "cm"))

# moving legend inside plot
pq + theme(legend.position = c(.90, .30))

# changing orientation and location of legend
pq + theme(legend.direction="horizontal", 
    legend.position = c(.20, .10),
    legend.key.size = unit(c(.8), "cm"))


# plotting proportions instead of counts
library(XML)
url <- "http://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population"
table <- readHTMLTable(url, stringsAsFactors=F) ## 
## after checking content of 'table', we choose what we need
pop <- data.frame(table[[2]][1:52,3:4])
names(pop) <- c("state", "pop")
pop$pop <- as.numeric(gsub(",", "", pop$pop)) ## removing commas
pop$state <- tolower(pop$state) ## state names to lower case for merging
tab <- merge(tab, pop) ## merging with data.frame with tweet counts
tab$prop <- tab$tweets / (tab$pop / 1000000)
## 'prop' is number of tweets per million population


## same map as above, with tweets per 1M citizens
p <- ggplot(tab, aes(map_id=state))
p + geom_map(aes(fill=prop), map = states_map) +
         expand_limits(x = states_map$long, y = states_map$lat)

## why is everything dark?
tab[tab$state=="district of columbia",]
tab$prop[tab$state=="district of columbia"] <- NA

p <- ggplot(tab, aes(map_id=state))
p + geom_map(aes(fill=prop), map = states_map) +
        expand_limits(x = states_map$long, y = states_map$lat) +
        scale_fill_continuous("Tweets per\n1M population", 
            low=colors[7], high=colors[1]) +
        theme(
            axis.line = element_blank(), axis.text = element_blank(), 
            axis.ticks = element_blank(), axis.title = element_blank(),
            panel.background = element_blank(), 
            plot.background = element_blank(),
            panel.border = element_blank(), panel.grid = element_blank()
        )
 
## multiple maps on one plot
df <- read.csv("data/obamacare_locations.csv", stringsAsFactors=F)
df$state <- latlong2state(df)
tab2 <- table(df$state, exclude=NULL)
tab2 <- data.frame(tab2); names(tab2) <- c("state", "obamacare")
tab <- merge(tab, tab2)
tab$prop2 <- tab$obamacare / (tab$pop / 1000000)

## keeping only what we need and reshaping
tab <- tab[,c("state", "prop", "prop2")]
library(reshape)
tb <- melt(tab, id.vars="state")
tb <- tb[tb$state!="district of columbia",]
levels(tb$variable) <- c("Shutdown", "Obamacare")

p <- ggplot(tb, aes(map_id=state))
pq <- p + geom_map(aes(fill=value), map = states_map) +
        facet_grid(~variable,) +
        expand_limits(x = states_map$long, y = states_map$lat) +
        scale_fill_continuous("Tweets per 1M population", 
            low=colors[7], high=colors[1]) +
        theme(
            axis.line = element_blank(), axis.text = element_blank(), 
            axis.ticks = element_blank(), axis.title = element_blank(),
            panel.background = element_blank(), 
            plot.background = element_blank(),
            panel.border = element_blank(), panel.grid = element_blank()
        )
pq

# moving legend to the bottom to fit plot into a slide
pq <- pq + theme(legend.position="bottom") + 
        guides(fill = guide_colorbar(
        barwidth = 15, barheight = 0.8, 
        label.theme=element_text(size=8, angle=0),
        title.theme=element_text(size=8, angle=0), 
        title.position="left"))

# finally, fixing margins to reduce white space
pq <- pq + theme( legend.margin = unit(-1.75, "cm") )
pq

pq <- pq + theme( plot.margin=unit(c(0,0,1,-.5), "cm") )
pq

## saving plot to file
ggsave(pq, file="plots/shutdown_tweets_final.pdf", height=3, width=7)


