from datetime import date, datetime

def jday_calc(date_string):
    date = datetime.strptime(date_string, "%m/%d/%Y")
    days =  date.strftime('%j') # TEXT(W2-DATE(YEAR(W2),1,0),"000") -> calculates days from start of year
    year = date_string[-2:] # RIGHT(YEAR(W2),2)

    formated = year + days 
    return formated
