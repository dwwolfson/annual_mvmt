# Helper functions

# This function coverts the julian date from the july 1 numeric date (which is an index in days since July 1),
# and converts it to a 'proper' julian date (# of days since Jan 1),
# and then converts this from a numeric format to a date format
jul_to_date<-function(input_jul){
  corrected_jul<-ifelse(input_jul<186, input_jul+181, input_jul-185)
  x<-as.Date(corrected_jul, origin="2019-12-31")
  return(x)
}

