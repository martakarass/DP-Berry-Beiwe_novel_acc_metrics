
# days of difference between ALSFRS-RSE and wearable observation
DAYS_DIFF_MAX = 7

# minimum number of minutes 
WEAR_TIME_MINUTES_MIN = (24 * 60) - (3 * 60)

# VAR_NAME_VEC <- c(
#   "Cf45","Cf90", "Cf135",
#   "Ce45",  "Ce90", "Ce135",             
#   "Cfe45", "Cfe90","Cfe135",                
#   "Cs45","Cs90" , "Cs135" ,                
#   "Cp45","Cp90" , "Cp135" ,                
#   "Csp45","Csp90" ,"Csp135",                
#   "Df45","Df90" ,"Df135" ,                
#   "De45" ,"De90", "De135" ,                
#   "Dfe45" ,"Dfe90","Dfe135" ,               
#   "Ds45", "Ds90", "Ds135" ,                
#   "Dp45","Dp90" ,"Dp135"  ,               
#   "Dsp45","Dsp90" , "Dsp135" 
# )

# ------------------------------------------------------------------------------

VAR_NAME_VEC <- c(
  "Cf45",
  "Ce45",
  "Cfe45",
  "Cs45",
  "Cp45",
  "Csp45",
  "Df45",
  "De45" ,
  "Dfe45" ,
  "Ds45", 
  "Dp45",
  "Dsp45"
)

VAR_NAME_VEC_45angle <- c(
  "Cf45",
  "Ce45",
  "Cfe45",
  "Cs45",
  "Cp45",
  "Csp45",
  "Df45",
  "De45" ,
  "Dfe45" ,
  "Ds45", 
  "Dp45",
  "Dsp45"
)

VAR_NAME_VEC_90angle <- c(
  "Cf90",
  "Ce90",
  "Cfe90",
  "Cs90",
  "Cp90",
  "Csp90",
  "Df90",
  "De90" ,
  "Dfe90" ,
  "Ds90", 
  "Dp90",
  "Dsp90"
)

VAR_NAME_VEC_135angle <- c(
  "Cf135",
  "Ce135",
  "Cfe135",
  "Cs135",
  "Cp135",
  "Csp135",
  "Df135",
  "De135" ,
  "Dfe135" ,
  "Ds135", 
  "Dp135",
  "Dsp135"
)


# ------------------------------------------------------------------------------
VAR_LABEL_VEC <- c(
  "Cf45",
  "Ce45",
  "Cfe45",
  "Cs45",
  "Cp45",
  "Csp45",
  "Df45",
  "De45" ,
  "Dfe45" ,
  "Ds45", 
  "Dp45",
  "Dsp45"
)

VAR_LABEL_VEC_45angle <- c(
  "Cf45",
  "Ce45",
  "Cfe45",
  "Cs45",
  "Cp45",
  "Csp45",
  "Df45",
  "De45" ,
  "Dfe45" ,
  "Ds45", 
  "Dp45",
  "Dsp45"
)


VAR_LABEL_VEC_90angle <- c(
  "Cf90",
  "Ce90",
  "Cfe90",
  "Cs90",
  "Cp90",
  "Csp90",
  "Df90",
  "De90" ,
  "Dfe90" ,
  "Ds90", 
  "Dp90",
  "Dsp90"
)

VAR_LABEL_VEC_135angle <- c(
  "Cf135",
  "Ce135",
  "Cfe135",
  "Cs135",
  "Cp135",
  "Csp135",
  "Df135",
  "De135" ,
  "Dfe135" ,
  "Ds135", 
  "Dp135",
  "Dsp135"
)


Y_LEVEL_VEC <- c(
  "ALSFRS-RSE total score",
  "ALSFRS-RSE Q4-7",
  "ALSFRS-RSE Q4",
  "ALSFRS-RSE Q5",
  "ALSFRS-RSE Q6",
  "ALSFRS-RSE Q7"     
)
Y_LABEL_VEC <- c(
  "ALSFRS-RSE tot.",
  "ALSFRS-RSE Q4-7",
  "ALSFRS-RSE Q4",
  "ALSFRS-RSE Q5",
  "ALSFRS-RSE Q6",
  "ALSFRS-RSE Q7"     
)

Y_LABEL_VEC2 <- c(
  "Q1-12",
  "Q4-7",
  "Q4",
  "Q5",
  "Q6",
  "Q7"     
)

message("The file config.R was read.")

message(paste0("DAYS_DIFF_MAX = ", DAYS_DIFF_MAX))
message(paste0("WEAR_TIME_MINUTES_MIN = ", WEAR_TIME_MINUTES_MIN))
message(paste0("VAR_NAME_VEC"))
message(paste0("VAR_LABEL_VEC"))


