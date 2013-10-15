###############################################################################
## Plotting evolution of unemployment in the US
## Author: Pablo Barberá
## Data Visualization with R and ggplot2
## October 15th 2013
## Source: ggplot2 book, pages 85-87
###############################################################################

# reading unemployment data
# source: http://data.bls.gov/timeseries/LNS14000000
library(gdata)
d <- read.xls("data/unemployment_data.xls", stringsAsFactors=F)

# looking at the data, we subset what we need...
months <- as.character(d[9, 2:13])
years <- as.character(d[10:75, 1])
d <- as.numeric(unlist(t(d[10:75, 2:13])))

# and we put it together into a data.frame
df <- data.frame(expand.grid(months, years))
names(df) <- c("month", "year")
df$unemp <- d/100

# removing missing values
df <- df[!is.na(df$unemp),]

# note that we need to create a new variable with date in R format
df$date <- paste("01", df$month, df$year)
df$date <- as.Date(df$date, format="%d %b %Y")
# see ?strptime for mroe info on date formats in R

# plot with default options
library(ggplot2)
p <- ggplot(df, aes(x=date, y=unemp))
p + geom_line()

# customizing line
p + geom_line(color="red")

p + geom_line(linetype=3) ## from 1 to 6
# see: http://www.cookbook-r.com/Graphs/Shapes_and_line_types/

p + geom_line(size=10) ## line width

p + geom_line(size=0.1)

pq <- p + geom_line() ## choosing the default

# changing the x axis
library(scales)
pq + scale_x_date(labels = date_format("%Y/%m/%d")) ## full date

pq + scale_x_date(labels = date_format("%b%Y")) ## month and year

pq + scale_x_date(labels = date_format("%B %Y")) ## full month and year

pq + scale_x_date(labels = date_format("%Y"), ## every 10 years
    breaks = date_breaks("10 years"))

pq + scale_x_date(labels = date_format("%Y"), ## every 5 years
    breaks = date_breaks("5 years"))

pq + scale_x_date(labels = date_format("%Y"), ## every 15 years
    breaks = date_breaks("15 years"))

pq + scale_x_date(labels = date_format("%Y"),  ## manual breaks
    breaks = as.Date(c("1950-01-01", "1965-01-01", "1980-01-01",
        "1980-01-01", "1995-01-01", "2010-01-01")))

# changing limits of y axis and also labels to %
pq + scale_y_continuous("Unemployment Rate") ## adding label

pq + scale_y_continuous("Unemployment Rate", labels = percent) ## adding label

pq + scale_y_continuous("Unemployment Rate", labels = percent,
            limits = c(0.02, 0.12)) ## adding limits

pq <- pq + scale_y_continuous("Unemployment Rate", labels = percent,
            limits = c(0.02, 0.115), breaks=c(0.02, 0.04, 0.06, 0.08, 0.10))

# changing theme
pq + theme_bw()

pq + theme_classic()

pq + theme_grey()

pq + theme_minimal()


# adding background colors for different presidents
name <- c("Truman", "Eisenhower", "Kennedy", "Johnson", "Nixon",
        "Ford", "Carter", "Reagan", "Bush I", "Clinton", "Bush II",
        "Obama")
start <- as.Date(c("1945-04-12", "1953-01-20", "1961-01-20", "1963-11-22",
        "1969-01-20", "1974-08-09", "1977-01-20", "1981-01-20",
        "1989-01-20", "1993-01-20", "2001-01-20", "2009-01-20"))
end <- c(start[-1], as.Date("2013-10-15"))
party <- c("D", "R", "D", "D", "R", "R", "D", "R", "R", "D", "R", "D")
pres <- data.frame(name, start, end, party, stringsAsFactors=F)
yrng <- range(df$unemp, na.rm=TRUE)
xrng <- range(df$date, na.rm=TRUE)

pq + geom_rect(data=pres, aes(NULL, NULL, xmin=start, xmax=end,
    fill=party), ymin=yrng[1], ymax=yrng[2])

# using transparency
pq + geom_rect(data=pres, aes(NULL, NULL, xmin=start, xmax=end,
    fill=party), ymin=yrng[1], ymax=yrng[2]) +
    scale_fill_manual(values = alpha(c("blue", "red"), 0.2))

# fixing axes so that background covers entire plot
pq <- pq + geom_rect(data=pres, aes(NULL, NULL, xmin=start, xmax=end,
    fill=party), ymin=yrng[1]-0.05, ymax=yrng[2]+0.05) +
    scale_fill_manual(values = alpha(c("blue", "red"), 0.2)) +
    scale_x_date(limits=xrng, expand=c(0,0))
pq

# adding presidents' names
pq + geom_text(data=pres, aes(x=start, y=yrng[1], label=name))

pq + geom_text(data=pres, aes(x=start, y=yrng[1], label=name),
    size=3, hjust=0)

yrng[2] <- yrng[2] + 0.005 
pq <- pq + geom_text(data=pres, aes(x=start, y=yrng[2], label=name),
    size=3, hjust=1, vjust=1.25, angle=90) 
pq

# adding lines to separate presidencies
pq + geom_vline(data=pres, aes(xintercept=as.numeric(start)))

pq <- pq + geom_vline(data=pres, aes(xintercept=as.numeric(start)),
    color="grey50", linetype=5, size=0.5)
pq

# finally, moving legend and removing x axis title
library(grid)
pq <- pq + theme(
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.margin = unit(-1, "cm") ) +
    scale_fill_manual("Party of President", 
        values=alpha(c("blue", "red"), 0.2),
        labels= c("Democratic", "Republican"))

pq

## adding annotations for specific events
pq + annotate(geom="point", x=as.Date("1982-11-01"), y=0.108, color="red")

pq + annotate(geom="rect", xmin=as.Date("1982-04-01"),
    xmax=as.Date("1983-10-31"), ymin=yrng[1], ymax=yrng[2], 
    color="red", fill="yellow", alpha=1/2)

ggsave(pq, file="plots/unemp_final.pdf", height=5, width=8)




