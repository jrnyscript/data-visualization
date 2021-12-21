library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggwordcloud)
library(knitr)

pw <- read.csv("common_passwords.csv", header=TRUE)

# look at duplicates
pw[duplicated(pw$password),] # "abcdef" and "easy"; records 485 and 3508, won't affect top 100

# make sure that they are truly duplicates over the whole record
pw[which(pw$password=="abcdef"),]
pw[which(pw$password=="easy"),]

#remove the two duplicates
pw <- pw[!duplicated(pw),]

# word cloud, easier to look at than just a list
library(ggwordcloud)

head(pw, 100) %>% 
  ggplot(aes(label = password)) +
  geom_text_wordcloud() +
  theme_minimal()

# look at whether special characters count as part of num_chars (no)
pw[head(which(pw$num_special>1)),]

# look at spread of length variable
plot(pw$length)
hist(pw$length)

# sort the password vector for easier searching
pw_sorted <- sort(pw$password)

# look at how many common passwords contain special characters at all
pw[which(pw$num_special>0),]$password # there are twelve, including *, ?, _, and .

# look at how many passwords just have special characters only
pw[which(pw$length==pw$num_special),] # there are just four
# *****, ******, ?????, ??????

# look at how many passwords just have digits only
nrow(pw[which(pw$length==pw$num_digits),]) # 1981

# look at how many passwords just have chars only
nrow(pw[which(pw$length==pw$num_digits),]) # 7175

# 838 passwords that are some mix of digits, chars, and special characters
nrow(pw) - (7175 + 1981 + 4)

char_mix <- data.frame(Type = c("Alphabetic Character Only", "Digits Only", 
                                "Special Character Only", "Alpha/Digit/Special Mix"),
                       Value = c(7175, 1981, 4, 838))

# pie chart for makeup of passwords - had trouble adding labels, and special char doesn't show up (only 4 records)
char_mix %>% 
  ggplot(aes(x = "", y = Value, fill = Type)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar(theta = "y", start = 0)

# 1 password that is a mix of digits, chars, and special characters 
nrow(pw[intersect(intersect(which(pw$num_chars > 0), which(pw$num_digits > 0)), which(pw$num_special > 0)),])

# 830 passwords that are a mix of just digits and chars
nrow(pw[intersect(intersect(which(pw$num_chars > 0), which(pw$num_digits > 0)), which(pw$num_special == 0)),])

# 4 passwords that are a mix of just chars and special chars
nrow(pw[intersect(intersect(which(pw$num_chars > 0), which(pw$num_digits == 0)), which(pw$num_special > 0)),])

# 3 passwords that are a mix of just digits and special chars
nrow(pw[intersect(intersect(which(pw$num_chars == 0), which(pw$num_digits > 0)), which(pw$num_special > 0)),])
