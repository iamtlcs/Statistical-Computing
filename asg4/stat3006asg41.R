# Q1
rm(list=ls())
library(parallel)

height <- read.table("~/Downloads/STAT3006/asg1/40197366.txt", header = TRUE)
height <- height$height

EM_algo <- function(y, p, mu1, mu2, sigma1, sigma2, t) {
  for (k in 1:t) {
    denom <- p*dnorm(y, mu1, sigma1) + (1-p)*dnorm(y, mu2, sigma2)
    w <- (p*dnorm(y, mu1, sigma1)+2*(1-p)*dnorm(y, mu2, sigma2))/denom
    p <- sum(2-w)/length(y)
    mu1 <- sum((2-w)*y)/sum(2-w)
    mu2 <- sum((w-1)*y)/sum(w-1)
    sigma1 <- sqrt(sum((2-w)*(y-mu1)^2)/sum(2-w))
    sigma2 <- sqrt(sum((w-1)*(y-mu2)^2)/sum(w-1))
  }
  result <- c(p, mu1, mu2, sigma1, sigma2)
  return (result)
}

pred <- function(y, p, mu1, mu2, sigma1, sigma2) {
  denom <- p*dnorm(y, mu1, sigma1) + (1-p)*dnorm(y, mu2, sigma2)
  w <- (p*dnorm(y, mu1, sigma1)+2*(1-p)*dnorm(y, mu2, sigma2))/denom
  return (w)
}

# original version
system.time(EM_algo(height, p=0.5, mu1=160, mu2=170, sigma1=2, sigma2=2, t = 100000))

# parallel version
num_core = detectCores()
cl = makeCluster(num_core, type = "FORK")
system.time(EM_algo(height, p=0.5, mu1=160, mu2=170, sigma1=2, sigma2=2, t = 100000))
stopCluster(cl)

# Q2
rm(list=ls())
#install.packages("RMySQL")
library("RMySQL")
drv=dbDriver("MySQL")
con=dbConnect(drv,user="student", password="HappyStudy2023",
              dbname="Library", port=3306,
              host="rds-mysql-statclass.czyn7pdbk60s.us-west-2.rds.amazonaws.com")

dbListTables(con)

# Part a
dbReadTable(con, "Record")
dbGetQuery(con,"SELECT * FROM Record;")

# Part b
dbReadTable(con, "Student")
dbReadTable(con, "Book")
dbGetQuery(con, "SELECT Student.StudentID, Major FROM Student, Record, Book
    where (Book.Classification = 'Philosophy' OR Book.Classification = 'History')
    And Record.BookNumber = Book.BookNumber
    And Record.StudentID = Student.StudentID;")

# Part c
dbGetQuery(con, "SELECT Student.StudentID, EntryYear FROM Student, Record
           where  Record.StudentID = Student.StudentID
           And TIMESTAMPDIFF(day, BorrowingTime, ReturnTime)>20;")

dbDisconnect(con)
dbUnloadDriver(drv)

# Q3
rm(list=ls())
#install.packages("XML")
#install.packages("httr")
#install.packages("RCurl")
library(XML)
library(httr)
library(RCurl)
#install.packages("DBI")


# Part a
url_complist = "https://www.slickcharts.com/nasdaq100"
doc_complist = htmlTreeParse(rawToChar(GET(url_complist)$content), useInternalNodes = TRUE)

comp_info = xpathSApply(doc_complist, "//table/tbody/tr/td/a")
comp_info = readHTMLTable(doc_complist, which = 1)
comp = comp_info[, c(2:3)]

# Part b
comp$url = paste0("https://ycharts.com/companies/", comp$Symbol)

empty_cols = c("MarketCap", "PriceToBookValue", "DividendYield")
comp[, empty_cols] = NA

n = nrow(comp)
root = "./Downloads/STAT3006/asg4/"
file_address = paste0(root, comp[, 'Symbol'], ".html")
for (i in 1:n) {
  url_testlist = comp[i, 'url']
  destfile = file_address[i]
  # download the .html files to local disk if there's no target file
  if(!file.exists(destfile)){
    download.file(url_testlist, destfile)
  }
  doc_file = htmlTreeParse(destfile, useInternalNodes = TRUE)
  
  market_temp = xpathSApply(doc_file, "//div[1]/table/tbody[1]/tr[1]/td[2]/text()", xmlValue)[1]
  if (!is.na(market_temp)){
    market_cap_temp = gsub("\n", "", market_temp) # delete '\n' in the string
    market_cap = gsub(" ","",market_cap_temp) # delete the white-space
    comp[i, 'MarketCap'] = market_cap
  }
  
  price_temp = xpathSApply(doc_file, "//div[1]/table/tbody[2]/tr[4]/td[2]/text()", xmlValue)
  if (!is.na(price_temp)){
    price_value_temp = gsub("\n", "", price_temp) # delete '\n' in the string
    price_to_book_value = gsub(" ", "", price_value_temp) # delete the white-space
    comp[i, 'PriceToBookValue'] = price_to_book_value
  }
  
  dividend_temp = xpathSApply(doc_file, "//div[2]/table/tbody[1]/tr[1]/td[2]/text()", xmlValue)[1]
  if (!is.na(dividend_temp)){
    dividned_yield_temp = gsub("\n", "", dividend_temp) # delete '\n' in the string
    dividend_yield = gsub(" ","",dividned_yield_temp) # delete the white-space
    comp[i, 'DividendYield'] = dividend_yield
  }
}

col_to_export = c("Company", "Symbol", "MarketCap", "PriceToBookValue", "DividendYield")
data_to_export = comp[, col_to_export]

# export the table out
library(gridExtra)
png("Q3b_data.png", height = 50*nrow(data_to_export), width = 200*ncol(data_to_export))
grid.table(data_to_export)
dev.off()

# Part c
mc_cols = c("Company", "Symbol", "PriceToBookValue")
mc_df = comp[, mc_cols]
mc_df['PriceToBookValue'] = as.numeric(mc_df$PriceToBookValue)
highest_three = head(mc_df[ , 'PriceToBookValue'], 3)
row_ind = which(mc_df$PriceToBookValue == highest_three)
print(mc_df[row_ind,])
