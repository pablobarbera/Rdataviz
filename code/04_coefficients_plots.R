###############################################################################
## regression coefficient plots using ggplot2
## Author: Pablo Barberá
## Data Visualization with R and ggplot2
## October 15th 2013
###############################################################################

###############################################################################
## Analysis: determinants of popularity on Facebook
###############################################################################

## backup: 65K public Facebook posts from Oct. 2nd
load("backup/fb.data")

## PREPARING DATA SET

# DEPENDENT VARIABLES
summary(fb.data$likes_count)
summary(fb.data$comments_count)
summary(fb.data$shares_count)

# INDEPENDENT VARIABLES
# gender
table(fb.data$gender, exclude=NULL)
fb.data$gender[is.na(fb.data$gender)] <- "Page"
fb.data$gender[fb.data$gender == "male (hidden)"] <- "male"
table(fb.data$gender, exclude=NULL)

# post type
table(fb.data$type, exclude=NULL)

# language: English vs others
fb.data$language <- substr(fb.data$locale, 1, 2)
fb.data$english <- ifelse(
    fb.data$language == "en" & !is.na(fb.data$language), 
    "English", "Others")
table(fb.data$english, exclude=NULL)

# mention specific keywords: obamacare, boehner, furlough
fb.data$obamacare <- grepl("obamacare", fb.data$message, ignore.case=TRUE)
fb.data$boehner <- grepl("boehner", fb.data$message, ignore.case=TRUE)
fb.data$furlough <- grepl("furlough", fb.data$message, ignore.case=TRUE)

# time of day
fb.data$time <- substr(fb.data$created_time, 12, 13)
fb.data$night <- fb.data$time %in% c("00", "01", "02", "03", "04", "05")
fb.data$morning <- fb.data$time %in% c("06", "07", "08", "09", "10", "11")
fb.data$afternoon <- fb.data$time %in% as.character(12:17)
fb.data$evening <- fb.data$time %in% as.character(18:23)
table(fb.data$night, exclude=NULL)
table(fb.data$morning, exclude=NULL)
table(fb.data$afternoon, exclude=NULL)
table(fb.data$evening, exclude=NULL)


## RUNNING REGRESSIONS
(r1 <- summary(lm(log(likes_count+1) ~ gender + type + english + obamacare + 
    boehner + furlough + morning + afternoon + evening, data=fb.data)))

(r2 <- summary(lm(log(comments_count+1) ~ gender + type + english + obamacare + 
    boehner + furlough + morning + afternoon + evening, data=fb.data)))

(r3 <- summary(lm(log(shares_count+1) ~ gender + type + english + obamacare + 
    boehner + furlough + morning + afternoon + evening, data=fb.data)))

## SAVING REGRESSIONS RESULTS INTO A DATA.FRAME
df <- lapply(list(r1, r2, r3), function(x)
     data.frame(
    var = rownames(x$coefficients)[2:13],
    coef = x$coefficients[2:13, "Estimate"],
    sd = x$coefficients[2:13, "Std. Error"])
)
df <- do.call(rbind, df)
df$dv <- rep(c("likes count", "comments count",
    "shares count"), each=12)

# changing variable labels
levels(df$var)
levels(df$var) <- c("Posted in afternoon", 'Mentions "boehner"', 
    "Post in English", "Posted in evening", 'Mentions "furlough*"',
    "Author is male", "Author is page", "Posted in morning",
    'Mentions "obamacare"', "Post is a photo", "Post is a status update",
    "Post is a video")
df$var <- factor(df$var, levels=c("Author is male", "Author is page",
    "Posted in morning", "Posted in afternoon", "Posted in evening",
    "Post in English", 'Mentions "boehner"', 'Mentions "furlough*"',
    'Mentions "obamacare"', "Post is a photo", "Post is a status update",
    "Post is a video"))


## PLOTTING COEFFICIENT ESTIMATES
library(ggplot2)
p <- ggplot(df, aes(y=coef, x=var))
p + geom_point(aes(color=dv)) + coord_flip()


# adding error bars
p  + geom_point(aes(color=dv)) + coord_flip() +
    geom_errorbar(aes(x=var, ymin=coef-2*sd, ymax=coef+2*sd))

# adding lines to indicate uncertainty
p  + geom_point(aes(color=dv)) + coord_flip() +
    geom_linerange(aes(x=var, ymin=coef-2*sd, ymax=coef+2*sd))

# order matters! also use position_dodge to avoid overlap
p <- ggplot(df, aes(y=coef, x=var, group=dv))
pq <- p + coord_flip() +
    geom_linerange(aes(ymin=coef-2*sd, ymax=coef+2*sd, color=dv),
    position=position_dodge(.5)) + 
    geom_point(aes(color=dv), position = position_dodge(.5))
pq

# fixing axes, legends, line at 0... and putting it all together
library(scales)
p <- ggplot(df, aes(y=coef, x=var))
pq <- p + coord_flip() +
    # adding first (long, thin) line for coef +- 2 sd
    geom_linerange(aes(ymin=coef-2 *sd, ymax=coef+2*sd, color=dv),
    position=position_dodge(.5)) +
    # adding second (short, thick) line for coef +- 1 sd
    geom_linerange(aes(ymin=coef-sd, ymax=coef+sd, color=dv),
    position=position_dodge(.5), size=1) + 
    # changing y axis title and scale (note it's y and not bc of coord_flip)
    scale_y_continuous("% increase in popularity metric", labels = percent) + 
    # adding line at 0
    geom_hline(yintercept=0, linetype=5, color="grey50") +
    # changing theme and removing axis title
    theme_bw() + theme( axis.title.y=element_blank() ) +
    # changing title of legend
    scale_color_discrete("Effect on...") +
    # adding horizontal lines to separate different types of variables
    geom_vline(xintercept=2.5, color="grey60") +
    geom_vline(xintercept=5.5, color="grey60") +
    geom_vline(xintercept=6.5, color="grey60") +
    geom_vline(xintercept=9.5, color="grey60") +
    # adding labels indicating types of variables
    annotate("text", x=9.8, y=-.32, label="Format", 
        size=3, color="grey60", hjust=0) +
    annotate("text", x=6.8, y=-.32, label="Content", 
        size=2.8, color="grey60", hjust=0) +
    annotate("text", x=5.8, y=-.32, label="Language", 
        size=2.8, color="grey60", hjust=0) +
    annotate("text", x=2.8, y=-.32, label="Time", 
        size=2.8, color="grey60", hjust=0) +
    annotate("text", x=0.8, y=-.32, label="Author", 
        size=2.8, color="grey60", hjust=0)

pq

# saving final result
ggsave(pq, file="plots/reg_coef_final.pdf", height=5, width=8)





