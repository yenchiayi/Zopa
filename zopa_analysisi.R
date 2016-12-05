install.packages('ggplot2')
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggthemes)
library(scales)
library(extrafont)

# Data Input
df <- read.csv('data/data_for_loanbook_extract_2016-12-01.csv')


# check colnames
df %>% colnames()
# [1] "Snapshot.Date"           
# [2] "Encrypted.Loan.ID"       
# [3] "Encrypted.Member.ID"     
# [4] "Disbursal.date"          
# [5] "Original.Loan.Amount"    
# [6] "Principal.Collected"     
# [7] "Interest.Collected"      
# [8] "Total.number.of.payments"
# [9] "Last.payment.date"       
# [10] "Term"                    
# [11] "Lending.rate"            
# [12] "Latest.Status"           
# [13] "Date.of.Default"         
# [14] "PostCode"  

# Summary Statistics
summary(df)
#* updated to 2016/12/1

# Split Disbursal.date into year, month, and day
df <- df %>% mutate(
  yyyy = Disbursal.date %>% substr(1,4) %>% as.integer(),
  mm = Disbursal.date %>% substr(6,7) %>% as.integer(),
  dd = Disbursal.date %>% substr(9,10) %>% as.integer()
)

df %>% head()

# Q:Growth trend
amt <- df %>% group_by(yyyy) %>% summarise(
  loan_amount=sum(Original.Loan.Amount)
) 
amt %>% ggplot(aes(x=yyyy, y=loan_amount)) + geom_line()


# Someone says that: The Peer-to-Peer Finance Association (P2PFA) expects the sector to double in size every six months for the foreseeable future.
amt <- amt %>% mutate(ln_amt=log(loan_amount), 
                      rate=100*c(NA, diff(ln_amt)),
                      rate=round(rate, 2))

amt %>% ggplot(aes(x=yyyy, y=rate, label=rate)) + geom_line() + geom_text()


# Q:Interest rate
g <- df %>% 
  ggplot(aes(x=Lending.rate, fill=factor(yyyy))) + 
  geom_density(alpha=.3)

g + facet_wrap(~yyyy)

df %>% group_by(yyyy) %>% 
  summarise(mean=mean(Lending.rate),
            median=median(Lending.rate),
            sd=sd(Lending.rate))


# Q:What is the most popular loan term 
# *1. Term ranges from 6 to 60
# *2. What's the most popular product
terms <- df %>% group_by(yyyy, Term) %>% summarise(
  int=mean(Lending.rate),
  counts=n())

g <- terms %>% 
  ggplot(aes(x=yyyy, y=counts, fill=factor(Term))) + 
  geom_bar(stat="identity", position="fill")  
g


# Q:term structure 
# * price is not monotonically increasing in term period
df %>% group_by(Term) %>% 
  summarise(int=mean(Lending.rate),
            counts=n()) %>% 
  ggplot(aes(x=Term, y=int)) + geom_line()


# increasing slope or decreasing slope?
df %>% group_by(yyyy, Term) %>% 
  summarise(int=mean(Lending.rate),
          counts=n()) %>% 
  ggplot(aes(x=Term, y=int)) + geom_line() + facet_wrap(~ yyyy)


#Q: default rate
defl <- df %>% 
  mutate(default= ifelse(Latest.Status=='Default', 1, 0)) %>% 
  group_by(yyyy) %>% 
  summarise(counts=n(), 
            n_default=sum(default)/counts)


defl %>% ggplot(aes(x=yyyy, y=n_default)) + geom_line() + geom_text(aes(label=round(100*n_default, 2)))
defl


# Status analysis
n_status <- df %>% 
  group_by(yyyy, Latest.Status) %>% 
  summarise(counts=n()) 
  
g <- n_status %>% ggplot(aes(x=yyyy, y=counts, fill=factor(Latest.Status))) + 
  geom_bar(stat="identity", position="fill")  

g

# Old Custom rate
n_active <- df %>% filter(Latest.Status=='Active') %>% select(Encrypted.Member.ID) %>% unique() %>% nrow()
n_active/(df %>% select(Encrypted.Member.ID) %>% unique %>% nrow)
