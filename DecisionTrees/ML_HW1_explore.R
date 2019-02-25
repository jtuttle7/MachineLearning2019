# Machine Learning HW 1

# Librarys ----



# *** ----




# Functions ----

entrpy <- function(probs){
  
  if(is.na(sum(probs))){
    return(0)
  }
  
  val <- 0
  
  for(i in 1:length(probs)){
    
    if(probs[i] == 0){
      
    }else{
      val <- val + probs[i]*log(probs[i],2)
    }
    
  }
  
  result <- -val
  
  return(result)
  
}





# majorError <- function(data,col_inds){
#   
#   
#   
#   
#   
# }




info_gain <- function(S_purity,weights,subS_purity){
  
  subS_sum <- 0
  
  for(i in 1:length(subS_purity)){
    
    subS_sum <- subS_sum + weights[i]*subS_purity[i]
    
  }
  
  gain <- S_purity - subS_sum
  
  return(gain)
  
  
}


# *** ----


# Part 1 ----

# Problem 1 ----

# Total Entropy 

total_entrpy <- entrpy(c(2/7,5/7))


# Total Set (first branch Info Gain)

x1_entrpy_1 <- entrpy(c(1/2,1/2))
x1_entrpy_0 <- entrpy(c(4/5,1/5))

info_gn_x1 <- info_gain(total_entrpy,c(2/7,5/7),c(x1_entrpy_1,x1_entrpy_0))


x2_entrpy_1 <- entrpy(c(4/4,0/4))
x2_entrpy_0 <- entrpy(c(2/3,1/3))

info_gn_x2 <- info_gain(total_entrpy,c(4/7,3/7),c(x2_entrpy_1,x2_entrpy_0))


x3_entrpy_1 <- entrpy(c(1/3,2/3))
x3_entrpy_0 <- entrpy(c(1/4,3/4))

info_gn_x3 <- info_gain(total_entrpy,c(3/7,4/7),c(x3_entrpy_1,x3_entrpy_0))


x4_entrpy_1 <- entrpy(c(2/3,1/3))
x4_entrpy_0 <- entrpy(c(0/4,4/4))

exp_entrpy_x4 <- 3/7*x4_entrpy_1 + 4/7*x4_entrpy_0

info_gn_x4 <- info_gain(total_entrpy,c(3/7,4/7),c(x4_entrpy_1,x4_entrpy_0))



firstsplit_gains <- data.frame("Info_Gain"=c(info_gn_x1,info_gn_x2,info_gn_x3,info_gn_x4))

row.names(firstsplit_gains) = c("x1","x2","x3","x4")

firstsplit_gains

# Index of attribute to split on

which(firstsplit_gains==max(firstsplit_gains))



# Split on "x2"

# x2 = 1 -> label = "0"

# x2 = 0

sp1_tot_entrpy <- entrpy(c(2/3,1/3))

sp1_x2_0_x1_entrpy_1 <- entrpy(c(1/1,0/1))
sp1_x2_0_x1_entrpy_0 <- entrpy(c(1/2,1/2))

sp1_info_gn_x1 <- info_gain(sp1_tot_entrpy,c(1/3,2/3),c(sp1_x2_0_x1_entrpy_1,sp1_x2_0_x1_entrpy_0))


sp1_x2_0_x3_entrpy_1 <- entrpy(c(1/2,1/2))
sp1_x2_0_x3_entrpy_0 <- entrpy(c(1/1,0/1))

sp1_info_gn_x3 <- info_gain(sp1_tot_entrpy,c(1/3,2/3),c(sp1_x2_0_x3_entrpy_1,sp1_x2_0_x3_entrpy_0))


sp1_x2_0_x4_entrpy_1 <- entrpy(c(2/2,0/2))
sp1_x2_0_x4_entrpy_0 <- entrpy(c(0/1,1/1))

sp1_exp_entrpy_x4 <- 2/3*sp1_x2_0_x4_entrpy_1 + 1/3*sp1_x2_0_x4_entrpy_0

sp1_info_gn_x4 <- info_gain(sp1_tot_entrpy,c(2/3,1/3),c(sp1_x2_0_x4_entrpy_1,sp1_x2_0_x4_entrpy_0))



secondsplit_gains <- data.frame("Info_Gain_sp2"=c(sp1_info_gn_x1,sp1_info_gn_x3,sp1_info_gn_x4))

row.names(secondsplit_gains) = c("x1","x3","x4")

secondsplit_gains

# Index of attribute to split on

which(secondsplit_gains==max(secondsplit_gains))


# Second Split (on "x4")

# x4 = 1 -> label = "1"

# x4 = 0 -> label = "0"



# *** ----




# Problem 3a ----

# Missing Data - (Outlook: Missing, Temperature: Mild, Humidity: Normal, Wind: Weak, Play: Yes)

# Most Common Value in DATASET -> Sunny

total_entrpy <- entrpy(c(10/15,5/15))


# Total Set (first branch Info Gain)

O_entrpy_S <- entrpy(c(3/6,3/6))
O_entrpy_O <- entrpy(c(4/4,0/5))
O_entrpy_R <- entrpy(c(3/5,2/5))

info_gn_O <- info_gain(total_entrpy,c(6/15,4/15,5/15),c(O_entrpy_S,O_entrpy_O,O_entrpy_R))


T_entrpy_H <- entrpy(c(2/4,2/4))
T_entrpy_M <- entrpy(c(5/7,2/7))
T_entrpy_C <- entrpy(c(3/4,1/4))

info_gn_T <- info_gain(total_entrpy,c(4/15,7/15,4/15),c(T_entrpy_H,T_entrpy_M,T_entrpy_C))


H_entrpy_H <- entrpy(c(3/7,4/7))
H_entrpy_N <- entrpy(c(7/8,1/8))
H_entrpy_L <- entrpy(c(0/0,0/0))

info_gn_H <- info_gain(total_entrpy,c(7/15,8/15,0/15),c(H_entrpy_H,H_entrpy_N,H_entrpy_L))


W_entrpy_S <- entrpy(c(3/6,3/6))
W_entrpy_W <- entrpy(c(7/9,2/9))

info_gn_W <- info_gain(total_entrpy,c(6/15,9/15),c(W_entrpy_S,W_entrpy_W))



firstsplit_gains <- data.frame("Info_Gain"=c(info_gn_O,info_gn_T,info_gn_H,info_gn_W))

row.names(firstsplit_gains) = c("O","T","H","W")

firstsplit_gains

# Index of attribute to split on

split_Ind <- which(firstsplit_gains==max(firstsplit_gains))
rownames(firstsplit_gains)[split_Ind]


# *** ----




# Problem 3b ----

# Missing Data - (Outlook: Missing, Temperature: Mild, Humidity: Normal, Wind: Weak, Play: Yes)

# Most Common Value w/similar label in DATASET -> Overcast

total_entrpy <- entrpy(c(10/15,5/15))


# Total Set (first branch Info Gain)

O_entrpy_S <- entrpy(c(2/5,3/5))
O_entrpy_O <- entrpy(c(5/5,0/5))
O_entrpy_R <- entrpy(c(3/5,2/5))

info_gn_O <- info_gain(total_entrpy,c(6/15,4/15,5/15),c(O_entrpy_S,O_entrpy_O,O_entrpy_R))


T_entrpy_H <- entrpy(c(2/4,2/4))
T_entrpy_M <- entrpy(c(5/7,2/7))
T_entrpy_C <- entrpy(c(3/4,1/4))

info_gn_T <- info_gain(total_entrpy,c(4/15,7/15,4/15),c(T_entrpy_H,T_entrpy_M,T_entrpy_C))


H_entrpy_H <- entrpy(c(3/7,4/7))
H_entrpy_N <- entrpy(c(7/8,1/8))
H_entrpy_L <- entrpy(c(0/0,0/0))

info_gn_H <- info_gain(total_entrpy,c(7/15,8/15,0/15),c(H_entrpy_H,H_entrpy_N,H_entrpy_L))


W_entrpy_S <- entrpy(c(3/6,3/6))
W_entrpy_W <- entrpy(c(7/9,2/9))

info_gn_W <- info_gain(total_entrpy,c(6/15,9/15),c(W_entrpy_S,W_entrpy_W))



firstsplit_gains <- data.frame("Info_Gain"=c(info_gn_O,info_gn_T,info_gn_H,info_gn_W))

row.names(firstsplit_gains) = c("O","T","H","W")

firstsplit_gains

# Index of attribute to split on

split_Ind <- which(firstsplit_gains==max(firstsplit_gains))
rownames(firstsplit_gains)[split_Ind]


# *** ----




# Problem 3c ----

# Missing Data - (Outlook: Missing, Temperature: Mild, Humidity: Normal, Wind: Weak, Play: Yes)

# Fractional Counts

total_entrpy <- entrpy(c(10/15,5/15))


# Total Set (first branch Info Gain)

O_entrpy_S <- entrpy(c((2+(5/14))/(5+(5/14)),3/(5+(5/14))))
O_entrpy_O <- entrpy(c((4+(4/14))/(4+(4/14)),0/(4+(4/14))))
O_entrpy_R <- entrpy(c((3+(5/14))/(5+(5/14)),2/(5+(5/14))))

info_gn_O <- info_gain(total_entrpy,c((5+(5/14))/15,(4+(4/14))/15,(5+(5/14))/15),c(O_entrpy_S,O_entrpy_O,O_entrpy_R))


T_entrpy_H <- entrpy(c(2/4,2/4))
T_entrpy_M <- entrpy(c(5/7,2/7))
T_entrpy_C <- entrpy(c(3/4,1/4))

info_gn_T <- info_gain(total_entrpy,c(4/15,7/15,4/15),c(T_entrpy_H,T_entrpy_M,T_entrpy_C))


H_entrpy_H <- entrpy(c(3/7,4/7))
H_entrpy_N <- entrpy(c(7/8,1/8))
H_entrpy_L <- entrpy(c(0/0,0/0))

info_gn_H <- info_gain(total_entrpy,c(7/15,8/15,0/15),c(H_entrpy_H,H_entrpy_N,H_entrpy_L))


W_entrpy_S <- entrpy(c(3/6,3/6))
W_entrpy_W <- entrpy(c(7/9,2/9))

info_gn_W <- info_gain(total_entrpy,c(6/15,9/15),c(W_entrpy_S,W_entrpy_W))



firstsplit_gains <- data.frame("Info_Gain"=c(info_gn_O,info_gn_T,info_gn_H,info_gn_W))

row.names(firstsplit_gains) = c("O","T","H","W")

firstsplit_gains

# Index of attribute to split on

split_Ind <- which(firstsplit_gains==max(firstsplit_gains))

# Attribute Name to Split On
rownames(firstsplit_gains)[split_Ind]


# Split on "Outlook"

spO_S_total_entrpy <- entrpy(c((2+(5/14))/(5+(5/14)),3/(5+(5/14))))


# Individual Entropies


spO_S_T_entrpy_H <- entrpy(c(2/2,0/2))
spO_S_T_entrpy_M <- entrpy(c((1+(5/14))/(2+(5/14)),(5/14)/(2+(5/14))))
spO_S_T_entrpy_C <- entrpy(c(1/1,0/1))

spO_S_info_gn_T <- info_gain(spO_S_total_entrpy,c(2/(5+(5/14)),(2+(5/14))/(5+(5/14)),1/(5+(5/14))),c(spO_S_T_entrpy_H,spO_S_T_entrpy_M,spO_S_T_entrpy_C))


spO_S_H_entrpy_H <- entrpy(c(0/3,3/3))
spO_S_H_entrpy_N <- entrpy(c((2+(5/14))/(2+(5/14)),0/(2+(5/14))))
spO_S_H_entrpy_L <- entrpy(c(0/0,0/0))

spO_S_info_gn_H <- info_gain(spO_S_total_entrpy,c(3/(5+(5/14)),(2+(5/14))/(5+(5/14)),0/(5+(5/14))),c(spO_S_H_entrpy_H,spO_S_H_entrpy_N,spO_S_H_entrpy_L))


spO_S_W_entrpy_S <- entrpy(c(1/2,1/2))
spO_S_W_entrpy_W <- entrpy(c((1+(5/14))/(3+(5/14)),2/(3+(5/14))))

spO_S_info_gn_W <- info_gain(spO_S_total_entrpy,c(2/(5+(5/14)),(3+(5/14))/(5+(5/14))),c(spO_S_W_entrpy_S,spO_S_W_entrpy_W))



spO_S_split_gains <- data.frame("Info_Gain"=c(spO_S_info_gn_T,spO_S_info_gn_H,spO_S_info_gn_W))

row.names(spO_S_split_gains) = c("T","H","W")

spO_S_split_gains

# Index of attribute to split on

spO_S_split_Ind <- which(spO_S_split_gains==max(spO_S_split_gains))

# Attribute Name to Split On
rownames(spO_S_split_gains)[spO_S_split_Ind]



# Overcast Set -> Label = YES



# Rainy Set

spO_R_total_entrpy <- entrpy(c((3+(5/14))/(5+(5/14)),2/(5+(5/14))))

# Individual Entropies


spO_R_T_entrpy_H <- entrpy(c(0/0,0/0))
spO_R_T_entrpy_M <- entrpy(c((2+(5/14))/(3+(5/14)),1/(3+(5/14))))
spO_R_T_entrpy_C <- entrpy(c(1/2,1/2))

spO_R_info_gn_T <- info_gain(spO_R_total_entrpy,c(0/(5+(5/14)),(3+(5/14))/(5+(5/14)),2/(5+(5/14))),c(spO_R_T_entrpy_H,spO_R_T_entrpy_M,spO_R_T_entrpy_C))


spO_R_H_entrpy_H <- entrpy(c(1/2,1/2))
spO_R_H_entrpy_N <- entrpy(c((2+(5/14))/(3+(5/14)),1/(3+(5/14))))
spO_R_H_entrpy_L <- entrpy(c(0/0,0/0))

spO_R_info_gn_H <- info_gain(spO_R_total_entrpy,c(2/(5+(5/14)),(3+(5/14))/(5+(5/14)),0/(5+(5/14))),c(spO_R_H_entrpy_H,spO_R_H_entrpy_N,spO_R_H_entrpy_L))


spO_R_W_entrpy_S <- entrpy(c(0/2,2/2))
spO_R_W_entrpy_W <- entrpy(c((3+(5/14))/(3+(5/14)),0/(3+(5/14))))

spO_R_info_gn_W <- info_gain(spO_R_total_entrpy,c(2/(5+(5/14)),(3+(5/14))/(5+(5/14))),c(spO_R_W_entrpy_S,spO_R_W_entrpy_W))



spO_R_split_gains <- data.frame("Info_Gain"=c(spO_R_info_gn_T,spO_R_info_gn_H,spO_R_info_gn_W))

row.names(spO_R_split_gains) = c("T","H","W")

spO_R_split_gains

# Index of attribute to split on

spO_R_split_Ind <- which(spO_R_split_gains==max(spO_R_split_gains))

# Attribute Name to Split On
rownames(spO_R_split_gains)[spO_R_split_Ind]
