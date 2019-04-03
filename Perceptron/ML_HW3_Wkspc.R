# ML_HW3_Wrkspc


# Librarys ----

ggplot2_Flag <- require(ggplot2)
RColorBrewer_Flag <- require(RColorBrewer)

if(!ggplot2_Flag){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!RColorBrewer_Flag){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

source("Perceptron_Funcs.R")


# *** ----
# Functions ----




# *** ----




# Read In Bank Note Data ----


bnkNte_trn_data <- read.csv("bank-note/train.csv",header=F,stringsAsFactors = F)

bnkNte_tst_data <- read.csv("bank-note/test.csv",header=F,stringsAsFactors = F)


# *** ----




# Calculate Perceptrons ----

stnd_wgts <- trn_stnd_Perceptron(trndata=bnkNte_trn_data,lrnRate=0.2)

stnd_res <- eval_stnd_Perceptron(bnkNte_tst_data,stnd_wgts)
paste0("Standard Perceptron Test Error is : ",stnd_res$Error)
paste0("")


vote_Pcptrn <- trn_vote_Perceptron(trndata=bnkNte_trn_data,lrnRate=0.2)

vote_res <- eval_vote_Perceptron(bnkNte_tst_data,vote_Pcptrn)
paste0("Voted Perceptron Test Error is : ",vote_res$Error)
paste0("")


avg_Pcptrn <- trn_avg_Perceptron(trndata=bnkNte_trn_data,lrnRate=0.2)

avg_res <- eval_avg_Perceptron(bnkNte_tst_data,avg_Pcptrn)
paste0("Average Perceptron Test Error is : ",avg_res$Error)
paste0("")

# Configure voted weights so I can put them in the report
present <- matrix(0,nrow=length(vote_Pcptrn$Wgt_Counts),ncol=5)

for(i in 1:nrow(present)){
  for(j in 1:5){
    if(vote_Pcptrn$Wgt_Vecs[[i]][1] != 0){
      present[i,j] = vote_Pcptrn$Wgt_Vecs[[i]][j]/vote_Pcptrn$Wgt_Vecs[[i]][1]
    }else{
      present[i,j] = vote_Pcptrn$Wgt_Vecs[[i]][j]
    }
  }
}



# Count = 2     Weights = {0  0.000  0.000  0.000  0.000} 
# Count = 3     Weights = {1 -0.118 -1.579  8.030 -0.028} 
# Count = 1     Weights = {0 -0.394  0.219 -1.265  0.354} 
# Count = 1     Weights = {1 -4.095 -5.802 -0.727  1.300} 
# Count = 27    Weights = {1 -2.690 -1.265 -1.247 -0.980}  
# Count = 1     Weights = {1 -1.442 -0.448 -1.712 -0.617} 
# Count = 4     Weights = {1 -1.512 -5.806 -1.091  2.007} 
# Count = 2     Weights = {1 -1.345 -2.871 -1.116  0.798} 
# Count = 14    Weights = {1 -1.436 -1.326 -1.408  0.049}  
# Count = 3     Weights = {1 -1.993  0.193 -1.095 -1.070} 
# Count = 28    Weights = {1 -2.503 -1.442 -1.636 -1.104}  
# Count = 1     Weights = {1 -2.455 -5.302 -1.738  0.352}  
# Count = 3     Weights = {1 -2.403 -3.204 -1.902 -0.404} 
# Count = 35    Weights = {1 -2.495 -1.659 -1.638 -1.176}  
# Count = 2     Weights = {1 -1.909 -1.199 -1.749 -0.881} 
# Count = 12    Weights = {1 -1.736 -1.510 -0.910 -0.540}  
# Count = 2     Weights = {1 -2.386 -0.615 -2.420 -0.617} 
# Count = 10    Weights = {1 -2.713 -1.336 -0.508 -0.576}  
# Count = 21    Weights = {1 -2.151 -0.572 -1.086 -0.909}  
# Count = 35    Weights = {1 -1.854 -0.479 -1.059 -0.701}  
# Count = 34    Weights = {1 -1.848 -1.108 -0.285 -0.541}  
# Count = 3     Weights = {1 -1.618 -0.567 -0.733 -1.047} 
# Count = 6     Weights = {1 -1.813 -0.178 -1.264 -1.053} 
# Count = 3     Weights = {1 -1.989 -1.385  0.450 -1.209} 
# Count = 2     Weights = {1 -1.813 -1.251  0.373 -1.028} 
# Count = 23    Weights = {1 -1.831 -1.438 -0.335 -1.176}  
# Count = 5     Weights = {1 -2.279 -1.101 -1.004 -1.294}  
# Count = 4     Weights = {1 -1.872 -0.587 -1.365 -1.424} 
# Count = 4     Weights = {1 -1.981 -1.602 -1.123 -0.817} 
# Count = 11    Weights = {1 -1.648 -1.074 -1.433 -0.960}  
# Count = 4     Weights = {1 -1.651 -1.552 -0.719 -0.769}  
# Count = 1     Weights = {1 -1.446 -1.364 -0.844 -0.677}  
# Count = 1     Weights = {1 -1.219 -0.984 -1.123 -0.807}  
# Count = 26    Weights = {1 -1.332 -1.777 -0.925 -0.388}  
# Count = 36    Weights = {1 -1.680 -1.030 -0.859 -0.625}  
# Count = 108   Weights = {1 -1.849 -0.858 -1.162 -0.597}   
# Count = 17    Weights = {1 -1.764 -0.997 -0.814 -0.483}  
# Count = 112   Weights = {1 -1.511 -0.807 -0.961 -0.451}   
# Count = 15    Weights = {1 -1.656 -0.631 -1.250 -0.413}  
# Count = 7     Weights = {1 -1.632 -1.588 -1.315  0.161}  
# Count = 26    Weights = {1 -1.402 -1.317 -1.444  0.141}  
# Count = 7     Weights = {1 -1.149 -0.925 -1.680 -0.062}  
# Count = 25    Weights = {1 -1.122 -1.290 -1.210 -0.019}  
# Count = 19    Weights = {1 -1.177 -0.988 -1.186 -0.089}  
# Count = 2     Weights = {1 -1.335 -1.395 -0.428 -0.164} 
# Count = 6     Weights = {1 -1.181 -1.276 -0.527 -0.112} 
# Count = 34    Weights = {1 -1.344 -1.079 -1.013 -0.003}  
# Count = 8     Weights = {1 -1.217 -0.828 -1.178 -0.105}  
# Count = 3     Weights = {1 -1.347 -1.439 -0.272 -0.215}  
# Count = 11    Weights = {1 -1.380 -1.188 -0.289 -0.261}  
# Count = 2     Weights = {1 -1.316 -0.979 -0.477 -0.443}  
# Count = 47    Weights = {1 -1.433 -0.848 -0.766 -0.434}  
# Count = 52    Weights = {1 -1.530 -0.727 -0.958 -0.406}  
# Count = 2     Weights = {1 -1.361 -0.607 -1.054 -0.377}  
# Count = 5     Weights = {1 -1.440 -0.881 -0.527 -0.400}  
# Count = 1     Weights = {1 -1.333 -0.803 -0.606 -0.364}  
# Count = 4     Weights = {1 -1.409 -0.637 -0.841 -0.336}  
# Count = 1     Weights = {1 -1.260 -0.531 -0.933 -0.312}  
# Count = 3     Weights = {1 -1.230 -0.839 -0.615 -0.268}  
# Count = 4     Weights = {1 -1.139 -0.658 -0.756 -0.335}  
# Count = 3     Weights = {1 -1.153 -1.084 -0.630 -0.036}  
# Count = 39    Weights = {1 -1.386 -0.681 -0.596 -0.218}  
# Count = 25    Weights = {1 -1.248 -0.480 -0.765 -0.314}  
# Count = 4     Weights = {1 -1.224 -0.814 -0.729 -0.253}  
# Count = 313   Weights = {1 -1.098 -0.712 -0.816 -0.243}   
# Count = 6     Weights = {1 -1.155 -0.562 -1.032 -0.214} 
# Count = 1     Weights = {1 -1.177 -0.724 -0.753 -0.190} 
# Count = 5     Weights = {1 -1.071 -0.542 -0.894 -0.272}  
# Count = 43    Weights = {1 -1.083 -0.868 -0.524 -0.274}  
# Count = 48    Weights = {1 -1.142 -0.779 -0.658 -0.248}  
# Count = 123   Weights = {1 -1.206 -0.683 -0.802 -0.220}   
# Count = 19    Weights = {1 -1.275 -0.580 -0.957 -0.189}  
# Count = 62    Weights = {1 -1.350 -0.835 -0.473 -0.218}  
# Count = 52    Weights = {1 -1.244 -0.732 -0.568 -0.215}  
# Count = 6     Weights = {1 -1.143 -0.675 -0.631 -0.178}  
# Count = 1     Weights = {1 -1.044 -0.515 -0.772 -0.258}  
# Count = 11    Weights = {1 -1.159 -0.941 -0.170 -0.356}  
# Count = 19    Weights = {1 -1.231 -0.842 -0.336 -0.359}  
# Count = 22    Weights = {1 -1.362 -0.647 -0.632 -0.379}  
# Count = 21    Weights = {1 -1.389 -0.500 -0.965 -0.292}  
# Count = 10    Weights = {1 -1.502 -0.932 -0.379 -0.335}  
# Count = 11    Weights = {1 -1.429 -0.765 -0.529 -0.475}  
# Count = 12    Weights = {1 -1.347 -0.703 -0.588 -0.440}  
# Count = 6     Weights = {1 -1.418 -0.538 -0.895 -0.356}  
# Count = 14    Weights = {1 -1.422 -0.923 -0.822 -0.159}  
# Count = 61    Weights = {1 -1.322 -0.773 -0.933 -0.203}  
# Count = 5     Weights = {1 -1.198 -0.596 -1.068 -0.293}  
# Count = 57    Weights = {1 -1.301 -0.826 -0.647 -0.330}  
# Count = 80    Weights = {1 -1.219 -0.741 -0.731 -0.318}  
# Count = 19    Weights = {1 -1.275 -0.659 -0.854 -0.296}  
# Count = 62    Weights = {1 -1.274 -1.023 -0.784 -0.111}  
# Count = 31    Weights = {1 -1.211 -0.834 -0.927 -0.304}  
# Count = 28    Weights = {1 -1.110 -0.752 -0.990 -0.288}  
# Count = 46    Weights = {1 -1.123 -0.883 -0.758 -0.274}  
# Count = 118   Weights = {1 -1.047 -0.751 -0.855 -0.313}   
# Count = 13    Weights = {1 -1.095 -0.675 -0.974 -0.293}  
# Count = 2     Weights = {1 -1.181 -0.900 -0.577 -0.328}  
# Count = 10    Weights = {1 -1.112 -0.814 -0.651 -0.315}  
# Count = 20    Weights = {1 -1.172 -0.726 -0.807 -0.316}  
# Count = 17    Weights = {1 -1.076 -0.575 -0.926 -0.385}  
# Count = 105   Weights = {1 -1.085 -0.874 -0.842 -0.175}   
# Count = 32    Weights = {1 -1.128 -0.765 -1.006 -0.151}  
# Count = 18    Weights = {1 -1.193 -0.956 -0.640 -0.163}  
# Count = 36    Weights = {1 -1.110 -0.815 -0.757 -0.230}  
# Count = 8     Weights = {1 -1.158 -0.743 -0.870 -0.208}  
# Count = 2     Weights = {1 -1.208 -0.667 -0.990 -0.184}  
# Count = 60    Weights = {1 -1.192 -0.742 -0.849 -0.144}  
# Count = 12    Weights = {1 -1.238 -0.630 -1.013 -0.118}  
# Count = 50    Weights = {1 -1.341 -0.793 -0.649 -0.146}  
# Count = 64    Weights = {1 -1.268 -0.710 -0.722 -0.139}  
# Count = 275   Weights = {1 -1.321 -0.635 -0.834 -0.114}   
# Count = 32    Weights = {1 -1.304 -0.893 -0.808 -0.061}  
# Count = 8     Weights = {1 -1.222 -0.810 -0.875 -0.063}  
# Count = 14    Weights = {1 -1.129 -0.733 -0.936 -0.061}  
# Count = 1     Weights = {1 -1.222 -1.082 -0.429 -0.149}  
# Count = 16    Weights = {1 -1.150 -0.956 -0.527 -0.188}  
# Count = 27    Weights = {1 -1.080 -0.840 -0.619 -0.221}  
# Count = 41    Weights = {1 -0.996 -0.702 -0.731 -0.285}  
# Count = 2     Weights = {1 -1.018 -0.550 -0.970 -0.252}  
# Count = 6     Weights = {1 -1.076 -0.863 -0.546 -0.250}  
# Count = 17    Weights = {1 -1.118 -0.800 -0.642 -0.230}  
# Count = 125   Weights = {1 -1.162 -0.733 -0.743 -0.210}   
# Count = 37    Weights = {1 -1.069 -0.594 -0.854 -0.281}  
# Count = 21    Weights = {1 -1.144 -0.792 -0.513 -0.310}  
# Count = 66    Weights = {1 -1.199 -0.713 -0.649 -0.311}  
# Count = 30    Weights = {1 -1.122 -0.572 -0.764 -0.386}  
# Count = 3     Weights = {1 -1.044 -0.513 -0.819 -0.371}  
# Count = 110   Weights = {1 -1.106 -0.646 -0.553 -0.382}   
# Count = 18    Weights = {1 -1.147 -0.580 -0.644 -0.367}  
# Count = 13    Weights = {1 -1.069 -0.521 -0.702 -0.358}  
# Count = 35    Weights = {1 -1.050 -0.731 -0.677 -0.321}  
# Count = 55    Weights = {1 -0.971 -0.602 -0.782 -0.379}  
# Count = 3     Weights = {1 -0.962 -0.874 -0.728 -0.242}  
# Count = 131   Weights = {1 -0.900 -0.799 -0.786 -0.239}   
# Count = 28    Weights = {1 -0.931 -0.708 -0.920 -0.221}  
# Count = 4     Weights = {1 -0.985 -1.008 -0.515 -0.231}  
# Count = 31    Weights = {1 -0.930 -0.939 -0.582 -0.224}  
# Count = 72    Weights = {1 -0.887 -0.806 -0.681 -0.349}  
# Count = 23    Weights = {1 -0.813 -0.684 -0.779 -0.407}  
# Count = 2     Weights = {1 -0.844 -0.624 -0.869 -0.394}  
# Count = 1     Weights = {1 -0.895 -0.761 -0.610 -0.405}  
# Count = 2     Weights = {1 -0.928 -0.703 -0.696 -0.391}  
# Count = 37    Weights = {1 -0.962 -0.642 -0.786 -0.377}  
# Count = 30    Weights = {1 -0.966 -0.891 -0.713 -0.203}  
# Count = 18    Weights = {1 -0.975 -0.798 -0.939 -0.141}  
# Count = 12    Weights = {1 -1.008 -0.704 -1.080 -0.119}  
# Count = 49    Weights = {1 -1.092 -0.824 -0.780 -0.126}  
# Count = 48    Weights = {1 -1.022 -0.688 -0.886 -0.202}  
# Count = 1     Weights = {1 -1.083 -0.997 -0.446 -0.257}  
# Count = 13    Weights = {1 -1.164 -0.920 -0.584 -0.253}  
# Count = 52    Weights = {1 -1.111 -0.810 -0.670 -0.293}  
# Count = 190   Weights = {1 -1.161 -0.737 -0.799 -0.293}   
# Count = 7     Weights = {1 -1.203 -0.674 -0.896 -0.276}  
# Count = 15    Weights = {1 -1.247 -0.607 -0.999 -0.258}  
# Count = 1     Weights = {1 -1.293 -0.758 -0.710 -0.274} 
# Count = 30    Weights = {1 -1.225 -0.656 -0.791 -0.302}  
# Count = 11    Weights = {1 -1.190 -0.926 -0.778 -0.172}  
# Count = 97    Weights = {1 -1.135 -0.813 -0.860 -0.215}  
# Count = 3     Weights = {1 -1.051 -0.685 -0.958 -0.279} 
# Count = 18    Weights = {1 -1.139 -0.807 -0.681 -0.292}  
# Count = 98    Weights = {1 -1.068 -0.747 -0.732 -0.281}  
# Count = 76    Weights = {1 -1.100 -0.661 -0.856 -0.264}  
# Count = 26    Weights = {1 -1.162 -0.943 -0.451 -0.311}  
# Count = 64    Weights = {1 -1.235 -0.846 -0.662 -0.284}  
# Count = 29    Weights = {1 -1.166 -0.718 -0.763 -0.352}  
# Count = 88    Weights = {1 -1.205 -0.659 -0.853 -0.337}  
# Count = 41    Weights = {1 -1.242 -0.795 -0.595 -0.339}  
# Count = 30    Weights = {1 -1.293 -0.725 -0.717 -0.340}  
# Count = 88    Weights = {1 -1.213 -0.605 -0.814 -0.394}  
# Count = 53    Weights = {1 -1.254 -0.543 -0.905 -0.380}  
# Count = 22    Weights = {1 -1.239 -0.744 -0.887 -0.346}  
# Count = 18    Weights = {1 -1.163 -0.683 -0.936 -0.337}  
# Count = 70    Weights = {1 -1.153 -0.745 -0.821 -0.296}  
# Count = 9     Weights = {1 -1.192 -0.686 -0.912 -0.280}  
# Count = 61    Weights = {1 -1.196 -0.781 -0.744 -0.265}  
# Count = 57    Weights = {1 -1.232 -0.696 -0.868 -0.249}  
# Count = 32    Weights = {1 -1.273 -0.635 -0.962 -0.231}    
# Count = 18    Weights = {1 -1.331 -0.933 -0.530 -0.290}  
# Count = 11    Weights = {1 -1.277 -0.885 -0.578 -0.281}  
# Count = 13    Weights = {1 -1.215 -0.789 -0.655 -0.307}  
# Count = 41    Weights = {1 -1.250 -0.706 -0.774 -0.291}  
# Count = 47    Weights = {1 -1.291 -0.646 -0.864 -0.275}  
# Count = 60    Weights = {1 -1.291 -0.906 -0.814 -0.142}  
# Count = 5     Weights = {1 -1.224 -0.833 -0.868 -0.141}  
# Count = 53    Weights = {1 -1.142 -0.711 -0.961 -0.204}  
# Count = 48    Weights = {1 -1.206 -0.880 -0.661 -0.233}  
# Count = 56    Weights = {1 -1.141 -0.757 -0.758 -0.298}  
# Count = 43    Weights = {1 -1.073 -0.701 -0.806 -0.292}  
# Count = 39    Weights = {1 -1.080 -0.924 -0.741 -0.135}  
# Count = 126   Weights = {1 -1.010 -0.808 -0.831 -0.190}   
# Count = 10    Weights = {1 -0.947 -0.754 -0.876 -0.182}  
# Count = 8     Weights = {1 -1.001 -1.017 -0.499 -0.229}  
# Count = 2     Weights = {1 -1.033 -0.970 -0.573 -0.213}  
# Count = 34    Weights = {1 -1.053 -0.872 -0.575 -0.233}  
# Count = 33    Weights = {1 -1.062 -0.794 -0.760 -0.181}  
# Count = 36    Weights = {1 -0.999 -0.741 -0.805 -0.173}  
# Count = 19    Weights = {1 -0.931 -0.635 -0.889 -0.228}  
# Count = 8     Weights = {1 -0.961 -0.582 -0.970 -0.212}  
# Count = 10    Weights = {1 -1.034 -0.825 -0.616 -0.272}  
# Count = 180   Weights = {1 -1.052 -0.710 -0.800 -0.247}   
# Count = 7     Weights = {1 -1.085 -0.657 -0.881 -0.232}  
# Count = 131   Weights = {1 -1.152 -0.756 -0.639 -0.235}   
# Count = 29    Weights = {1 -1.097 -0.697 -0.689 -0.233}  
# Count = 53    Weights = {1 -1.036 -0.647 -0.735 -0.228}  
# Count = 5     Weights = {1 -1.041 -0.851 -0.674 -0.083}  
# Count = 64    Weights = {1 -1.121 -0.676 -0.659 -0.169}  
# Count = 70    Weights = {1 -1.154 -0.625 -0.734 -0.153}  
# Count = 97    Weights = {1 -1.185 -0.547 -0.843 -0.136}  
# Count = 122   Weights = {1 -1.154 -0.769 -0.833 -0.025}   
# Count = 22    Weights = {1 -1.186 -0.691 -0.948 -0.004}  
# Count = 33    Weights = {1 -1.238 -0.961 -0.562 -0.059}  
# Count = 7     Weights = {1 -1.284 -0.901 -0.672 -0.054}  
# Count = 50    Weights = {1 -1.237 -0.802 -0.761 -0.139}  
# Count = 30    Weights = {1 -1.179 -0.741 -0.809 -0.138}  
# Count = 43    Weights = {1 -1.215 -0.688 -0.890 -0.121}  
# Count = 16    Weights = {1 -1.141 -0.581 -0.974 -0.177}  
# Count = 3     Weights = {1 -1.184 -0.824 -0.643 -0.177}  
# Count = 36    Weights = {1 -1.227 -0.763 -0.751 -0.174}  
# Count = 223   Weights = {1 -1.263 -0.711 -0.831 -0.157}    
# Count = 64    Weights = {1 -1.196 -0.659 -0.876 -0.154}  
# Count = 15    Weights = {1 -1.244 -0.919 -0.506 -0.201}  
# Count = 16    Weights = {1 -1.307 -0.806 -0.681 -0.196}  
# Count = 169   Weights = {1 -1.331 -0.688 -0.869 -0.169}   
# Count = 64    Weights = {1 -1.342 -0.906 -0.807 -0.013}  
# Count = 36    Weights = {1 -1.378 -0.829 -0.924  0.009}  
# Count = 53    Weights = {1 -1.312 -0.727 -1.005 -0.045}  
# Count = 6     Weights = {1 -1.239 -0.617 -1.088 -0.101}  
# Count = 4     Weights = {1 -1.278 -0.871 -0.746 -0.112}  
# Count = 121   Weights = {1 -1.212 -0.817 -0.790 -0.106}   
# Count = 103   Weights = {1 -1.244 -0.743 -0.899 -0.088}   
# Count = 1     Weights = {1 -1.280 -0.690 -0.982 -0.069}  
# Count = 20    Weights = {1 -1.321 -0.936 -0.645 -0.070}  
# Count = 2     Weights = {1 -1.368 -0.877 -0.755 -0.066}  
# Count = 22    Weights = {1 -1.315 -0.816 -0.805 -0.062}  
# Count = 180   Weights = {1 -1.255 -0.716 -0.880 -0.108}   
# Count = 18    Weights = {1 -1.191 -0.665 -0.921 -0.106}  
# Count = 3     Weights = {1 -1.241 -0.904 -0.578 -0.149}  
# Count = 11    Weights = {1 -1.272 -0.834 -0.679 -0.132}  
# Count = 21    Weights = {1 -1.224 -0.740 -0.757 -0.180}  
# Count = 30    Weights = {1 -1.156 -0.640 -0.837 -0.231}  
# Count = 67    Weights = {1 -1.097 -0.594 -0.876 -0.223}  
# Count = 63    Weights = {1 -1.083 -0.754 -0.861 -0.192}  
# Count = 1     Weights = {1 -1.114 -0.706 -0.937 -0.177}  
# Count = 35    Weights = {1 -1.146 -0.656 -1.016 -0.161}  
# Count = 32    Weights = {1 -1.221 -0.743 -0.796 -0.175}  
# Count = 156   Weights = {1 -1.255 -0.694 -0.872 -0.159}   


