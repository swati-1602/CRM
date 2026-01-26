##### Plot for paper-1 for MSE in case of N(0,1)
####parameter for beta{1}
s_size<-seq(50,1000,50)

MSE_CL1N<-c(0.4485 ,  0.2270  , 0.1497  , 0.1125  , 0.0895 ,  0.0737, 0.0664 ,  0.0583  , 0.0510  ,  0.0461  ,  
            0.0421  ,  0.0366,  0.0354  ,  0.0318  ,  0.0308  ,  0.0270  ,  0.0286  ,  0.0250,  0.0233 ,  0.0229)

MSE_WM1N<-c(0.4981 ,  0.2386 ,  0.1587  , 0.1163  , 0.0918  , 0.0747,  0.0675 ,  0.0595 ,  0.0517  ,  0.0471  ,  
            0.0425  ,  0.0369,  0.0356  ,  0.0323  ,  0.0312  ,  0.0272  ,  0.0288  ,  0.0251, 0.0233 , 0.0231)

MSE_LC1N<-c(0.3854 ,  0.1761 ,  0.1168 ,  0.0835 ,  0.0681 ,  0.0546 , 0.0490 ,  0.0427 ,  0.0383  ,  0.0350 , 
            0.0307  ,  0.0279 ,  0.0258 ,  0.0253  ,  0.0223  ,  0.0211  ,  0.0211  ,  0.0193,  0.0178 ,0.0174)

####parameter for beta{2}


MSE_CL2N<-c( 0.2869,   0.1334  , 0.0879  , 0.0697 ,  0.0553  , 0.0451, 0.0400 ,  0.0355 ,  0.0303  ,  0.0278  , 
             0.0248  ,  0.0223,  0.0204  ,  0.0190  ,  0.0178   , 0.0165  ,  0.0172  ,  0.0156,  0.0144 , 0.0134)

MSE_WM2N<-c(0.3097 ,  0.1401  , 0.0914  , 0.0719 ,  0.0561 ,  0.0457, 0.0406 ,  0.0358  , 0.0306  ,  0.0282  ,  0.0251   , 
            0.0223,  0.0205  ,  0.0192  ,  0.0180  ,  0.0166  ,  0.0172  ,  0.0156, 0.0143 , 0.0134)

MSE_LC2N<-c(0.2368 ,  0.1033 ,  0.0672 ,  0.0515 ,  0.0410 ,  0.0332 , 0.0287 ,  0.0265 ,  0.0227  ,  0.0209 , 
            0.0178  ,  0.0166 ,  0.0150 ,   0.0144  ,  0.0128 ,   0.0121  ,  0.0123  ,  0.0113,  0.0105 , 0.0097)

####parameter for beta{3}

MSE_CL3N<-c(0.0612,   0.0302 ,   0.0196 ,  0.0149 ,  0.0118 ,  0.0098, 0.0087 ,  0.0077 ,  0.0066 ,   0.0061  , 
            0.0055  ,  0.0049,    0.0045  ,  0.0042   , 0.0040  ,  0.0035   , 0.0038   , 0.0033,  0.0030,  0.0029)

MSE_WM3N<-c(0.0674 ,  0.0319  , 0.0207 ,  0.0154  , 0.0120 ,  0.0100, 0.0088 ,  0.0078  , 0.0067  ,  0.0062  , 
            0.0056   , 0.0049,  0.0046 ,   0.0042  ,  0.0040   , 0.0035   , 0.0038 ,   0.0033,  0.0030 , 0.0030)

MSE_LC3N<-c(0.0523 ,  0.0237 ,  0.0152 ,  0.0110 ,  0.0088 ,  0.0073 , 0.0063 ,  0.0057 ,  0.0049  ,  0.0046 , 
            0.0040  ,  0.0037 ,  0.0033 ,   0.0032  ,  0.0028 ,   0.0026  ,  0.0028  , 0.0025 ,  0.0023 , 0.0022)

####parameter for rho[1]

MSE_CLRHON<-c(0.0985,   0.0492  , 0.0324  , 0.0239 ,  0.0195  , 0.0158, 0.0140 ,  0.0118 ,  0.0107  ,  0.0102  ,
              0.0087 ,   0.0076,   0.0075  ,  0.0069  ,  0.0067  ,  0.0057  ,  0.0059 ,   0.0055, 0.0050 , 0.0047)

MSE_WMRHON<-c(0.1043 ,  0.0508 ,  0.0335  , 0.0244  , 0.0196  , 0.0160,  0.0142 ,  0.0119 ,  0.0107  ,  0.0103  , 
              0.0087 ,   0.0076,  0.0075 ,   0.0069 ,   0.0067  ,  0.0057  ,  0.0059  ,  0.0055, 0.0050 , 0.0047)

MSE_LCRHON<-c(0.0764 ,  0.0369 ,  0.0233 ,  0.0172 ,  0.0134 ,  0.0113 , 0.0099 ,  0.0086 ,  0.0075  ,  0.0074  ,
              0.0060 ,   0.0056, 0.0053 ,   0.0050  ,  0.0046 ,   0.0040  ,  0.0043  ,  0.0039, 0.0036 , 0.0034)

##### Plot for paper-1 for MSE in case of laplace
####parameter for beta 1

MSE_CL1L<-c(0.5169,   0.2187 ,  0.1419,   0.0989,   0.0759 ,  0.0677, 0.0567 ,  0.0465 ,  0.0385  ,  0.0363 , 
            0.0314  ,  0.0281, 0.0274  ,  0.0251  ,  0.0229 ,   0.0218 ,   0.0197 ,   0.0186, 0.0175,  0.0165)

MSE_WM1L<-c( 0.5287,   0.2153,   0.1398,   0.0982 ,  0.0751,   0.0668, 0.0559 ,  0.0465 ,  0.0384 ,   0.0362 , 
             0.0312 ,   0.0279,  0.0274 ,   0.0251  ,  0.0229 ,   0.0216 ,   0.0197 ,   0.0184, 0.0174,  0.0165 )

MSE_LC1L<-c(0.5044,  0.2185,   0.1422 ,  0.1062 ,  0.0808 ,  0.0736, 0.0613,   0.0517,   0.0450, 
            0.0415 ,   0.0362 ,   0.0326, 0.0325,    0.0289 ,   0.0264,    0.0250,    0.0242,    0.0226, 0.0208, 0.0204)


####parameter for beta 2

MSE_CL2L<-c(0.3128 ,  0.1295 ,  0.0829 ,  0.0606 ,  0.0446 ,  0.0394, 0.0346 ,  0.0280,   0.0239 ,   0.0221 , 
            0.0197 ,   0.0171, 0.0164 ,   0.0148 ,   0.0145,    0.0129,    0.0124 ,   0.0114,  0.0104,  0.0103)

MSE_WM2L<-c(0.3205  , 0.1283 ,  0.0808  , 0.0603  , 0.0442  , 0.0388,  0.0340 ,  0.0279,   0.0236 ,   0.0221 , 
            0.0195 ,   0.0170,  0.0164,    0.0149,    0.0145,    0.0128,    0.0124,    0.0113,  0.0103,  0.0102)

MSE_LC2L<-c(0.2979,   0.1279,   0.0822,   0.0643,   0.0465,   0.0415, 0.0373,   0.0307,   0.0263,    0.0248 , 
            0.0215 ,   0.0190,  0.0189,    0.0166 ,   0.0161,    0.0142,    0.0140,    0.0129,  0.0117,  0.0117)

####parameter for beta 3

MSE_CL3L<-c( 0.0692 ,  0.0285 ,  0.0186 ,  0.0134 ,  0.0100 ,  0.0088, 0.0076 ,  0.0062 ,  0.0052,    0.0048, 
             0.0042 ,   0.0037, 0.0036 ,   0.0033 ,   0.0030 ,   0.0028 ,   0.0027 ,   0.0024,  0.0023,  0.0022)

MSE_WM3L<-c( 0.0714 ,  0.0283 ,  0.0183  , 0.0132 ,  0.0099,   0.0087, 0.0075,   0.0062,   0.0052 ,   0.0048,  
             0.0042 ,   0.0037, 0.0036,    0.0033,    0.0030,    0.0028 ,   0.0027,    0.0024,  0.0023,  0.0022)

MSE_LC3L<-c(0.0678 ,  0.0286 ,  0.0187 ,  0.0141 ,  0.0106 ,  0.0095, 0.0081,   0.0068,   0.0059,    0.0055,  
            0.0047 ,   0.0042, 0.0042 ,   0.0037,    0.0034 ,   0.0031,    0.0031,    0.0029,  0.0027 , 0.0027 )

####parameter for rho[1]

MSE_CLRHOL<-c(0.1186 ,  0.0476 ,  0.0286 ,  0.0220 ,  0.0159 ,  0.0134, 0.0114 ,  0.0097 ,  0.0085,    0.0074,  
              0.0069 ,   0.0060,  0.0057 ,   0.0051 ,   0.0048,    0.0044,    0.0044 ,   0.0039, 0.0036,  0.0036)

MSE_WMRHOL<-c(0.1212  , 0.0476  , 0.0282  , 0.0217 ,  0.0157 ,  0.0132, 0.0112,   0.0096,   0.0084,    0.0074 ,  
              0.0068,    0.0060, 0.0057,    0.0051,    0.0048 ,   0.0044,    0.0043,    0.0039,  0.0036, 0.0035)

MSE_LCRHOL<-c(0.1110,   0.0477,   0.0290 ,  0.0231,   0.0167,   0.0141,  0.0123,   0.0105,   0.0094,    0.0084,  
              0.0077,    0.0068,  0.0068,    0.0059 ,   0.0055  ,  0.0050  ,  0.0049 ,   0.0046,  0.0042,  0.0042)

##### Plot for paper-1 for MSE in case of t_{3}

##parameter beta1

MSE_CL1T<-c(0.6911,   0.2860,   0.1815,   0.1403,   0.1099,   0.0949,  0.0769,   0.0710,   0.0586,    0.0542,  
            0.0461,    0.0431,0.0415,    0.0385,    0.0337,    0.0340,    0.0302,    0.0296,   0.0281,  0.0264)

MSE_WM1T<-c(0.6870,   0.2937,   0.1811,   0.1404,   0.1112,   0.0952, 0.0770,   0.0710,   0.0587,    0.0548,  
            0.0465,    0.0430, 0.0415,    0.0383,    0.0335,    0.0341,    0.0301,    0.0296, 0.0282,  0.0264)

MSE_LC1T<-c(0.5891,   0.2525,   0.1592,   0.1235,   0.0964,   0.0846, 0.0638,   0.0608,   0.0505 ,   0.0467 , 
            0.0404,    0.0386,  0.0366,    0.0326,    0.0302 ,   0.0296,    0.0274 ,   0.0255,  0.0251,  0.0237)

##parameter beta2


MSE_CL2T<-c(0.4413,   0.1732,   0.1107,   0.0844,   0.0656,   0.0554,   0.0470,   0.0408,   0.0350,    0.0334, 
            0.0296,    0.0257,  0.0243,    0.0229,    0.0209,    0.0197,    0.0175,    0.0180, 0.0159,  0.0158)

MSE_WM2T<-c(0.4245,   0.1774,   0.1096,   0.0846,   0.0667,   0.0550, 0.0472,   0.0408,   0.0350,    0.0335, 
            0.0296,    0.0257, 0.0242,    0.0228,    0.0209,    0.0196,    0.0174,    0.0179,  0.0158,  0.0158)

MSE_LC2T<-c( 0.3681,   0.1480,   0.0967,   0.0730,   0.0566,   0.0477,  0.0398,   0.0343 ,  0.0298,  
             0.0284,    0.0243,    0.0219,   0.0215,    0.0191,    0.0180,    0.0167,    0.0148,    0.0152, 0.0138,  0.0133)

##parameter beta3


MSE_CL3T<-c(0.0971,   0.0379,   0.0238,   0.0185,   0.0141,   0.0125, 0.0104,   0.0091,   0.0077,    0.0073, 
            0.0063,    0.0056,    0.0054,    0.0051,    0.0044 ,   0.0043 ,   0.0039 ,   0.0039,  0.0036,  0.0035)

MSE_WM3T<-c(0.0948,   0.0389,   0.0237,  0.0186,   0.0144,   0.0124, 0.0104,   0.0092,   0.0077,    0.0074,   
            0.0063,    0.0056,   0.0054,    0.0050,    0.0044,    0.0043 ,   0.0039,    0.0039,   0.0036,  0.0035)

MSE_LC3T<-c( 0.0813,   0.0332,   0.0209,   0.0162,   0.0122,   0.0109,  0.0086,   0.0078,   0.0066,    0.0063 ,
             0.0053,    0.0050,   0.0048,    0.0042,    0.0039,    0.0038,    0.0034,    0.0033,  0.0032,  0.0031)

##parameter rho[1]

MSE_CLRHOT<-c(0.1525,   0.0623,   0.0382,   0.0276,   0.0248,   0.0200,  0.0169,   0.0139,   0.0121,    0.0117,  
              0.0098,    0.0094, 0.0085,    0.0079,    0.0071 ,   0.0072 ,   0.0063 ,   0.0064,  0.0057,  0.0057)

MSE_WMRHOT<-c(0.1507,   0.0632,   0.0381,   0.0279,   0.0251,   0.0199,  0.0170,   0.0139,   0.0121,    0.0117, 
              0.0098,    0.0094, 0.0084,    0.0078,    0.0071,    0.0072,    0.0063,    0.0063,  0.0057,  0.0056)

MSE_LCRHOT<-c(0.1263,   0.0531,   0.0329,   0.0243,   0.0205,   0.0163, 0.0139,   0.0118,   0.0103,    0.0098, 
              0.0081,    0.0078,   0.0074,    0.0065,    0.0060,    0.0061,    0.0053,    0.0054, 0.0048 ,0.0048 )

##### Plot for paper-1 for MSE in case of hetrocedastic error

##parameter beta1

MSE_CL1H<-c(0.5856,   0.2550 ,  0.1586 ,  0.1150,   0.0927,   0.0756,  0.0676,   0.0590,   0.0512,    0.0470, 
            0.0422,   0.0371,   0.0342 ,   0.0334,    0.0305 ,   0.0269,    0.0286 ,   0.0250, 0.0234 , 0.0223)

MSE_WM1H<-c(0.5874,   0.2583,   0.1621,   0.1169 ,  0.0923 ,  0.0761,  0.0682,   0.0588,   0.0511,    0.0473 ,  
            0.0421,    0.0369, 0.0343,    0.0333,    0.0307,    0.0269 ,   0.0287,    0.0249,  0.0233,  0.0223)

MSE_LC1H<-c(0.5494,   0.2338 ,  0.1531 ,  0.1072 ,  0.0863 ,  0.0732, 0.0636,   0.0548 ,  0.0481 ,   0.0451 ,  
            0.0392 ,   0.0356, 0.0320,    0.0312 ,   0.0282,    0.0256,    0.0268 ,   0.0240,  0.0219 , 0.0218)

##parameter beta2

MSE_CL2H<-c(0.4398,   0.1846,   0.1187,   0.0912,   0.0737,   0.0592,  0.0523,   0.0464,   0.0400,    0.0364,  
            0.0323,    0.0291,  0.0260 ,   0.0252,    0.0230,    0.0215,    0.0219,    0.0199, 0.0189,  0.0172)

MSE_WM2H<-c( 0.4368,   0.1857,   0.1213 ,  0.0929,   0.0734,   0.0596,  0.0527,   0.0461,   0.0400,    0.0364, 
             0.0323,    0.0292,  0.0261,    0.0250 ,   0.0230,    0.0214,    0.0219,    0.0200,  0.0188,  0.0172)

MSE_LC2H<-c(0.3891,   0.1632 ,  0.1095  , 0.0817 ,  0.0649  , 0.0536, 0.0458 ,  0.0411 ,  0.0354  ,  0.0326 , 
            0.0281  ,  0.0262, 0.0235,    0.0221,    0.0197,    0.0189,    0.0190 ,   0.0174,   0.0166,  0.0154)

##parameter beta3

MSE_CL3H<-c(0.1002,   0.0450,   0.0278,   0.0203,   0.0162,   0.0138, 0.0116,   0.0105,   0.0089,    0.0085,  
            0.0074 ,   0.0067, 0.0060 ,   0.0059,    0.0054 ,   0.0047,    0.0051,    0.0045, 0.0041 , 0.0039)

MSE_WM3H<-c( 0.1007,   0.0453,   0.0282,   0.0206,   0.0162 ,  0.0139,  0.0118,   0.0105,   0.0089,    0.0085, 
             0.0074 ,   0.0067,  0.0060 ,   0.0059 ,   0.0054 ,   0.0047 ,   0.0051,   0.0045,   0.0040 , 0.0039)

MSE_LC3H<-c( 0.0926,   0.0405 ,  0.0262 ,  0.0186 ,  0.0147 ,  0.0129,    0.0107 ,  0.0096,   0.0082,    0.0079 , 
             0.0068 ,   0.0063,  0.0055 ,   0.0053 ,   0.0048  ,  0.0043 ,   0.0047 ,   0.0041, 0.0037 ,0.0037)

##parameter rho[1]

MSE_CLRHOH<-c(0.1507,   0.0641,   0.0431,   0.0302,   0.0243,   0.0198, 0.0177,   0.0149,   0.0133,    0.0130,  
              0.0108,    0.0095, 0.0092 ,   0.0087 ,   0.0083 ,   0.0071,    0.0073 ,   0.0067, 0.0062,  0.0058)

MSE_WMRHOH<-c(0.1509,   0.0646,   0.0435,   0.0308,   0.0244,   0.0199,   0.0177,   0.0149,   0.0133,    0.0130,  
              0.0108,    0.0095, 0.0092  ,  0.0087 ,   0.0083,    0.0071,    0.0073,    0.0067,  0.0062, 0.0058)

MSE_LCRHOH<-c(0.1291,   0.0575 ,  0.0376 ,  0.0271  , 0.0209  , 0.0180,  0.0156,   0.0135  , 0.0118  ,  0.0115  ,
                0.0093 ,   0.0086, 0.0082 ,   0.0077 ,   0.0072,    0.0061 ,   0.0066 ,   0.0059,  0.0055 , 0.0052)



#### ploting

# Set the desired folder path
folder_path <- "C:/Users/Math 564 Cabin 1/Downloads/"  # Replace with your actual folder path

# Create the full file path for the PDF
pdf_file <- file.path(folder_path, "N(0,1).pdf")

# Open the PDF device
pdf(pdf_file, width = 8, height = 10)

# Define layout: 4 plots in a 2x2 grid with an additional row for the legend
layout(matrix(c(1, 2,3, 4,5, 5), nrow = 3, byrow = TRUE), heights = c(4, 4, 1))

##################plot for N(0,1)
# Plot 1
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL1N,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.4485, 0.4981, 0.3854))  ,ylab = expression(bold("MSE ")),font.lab=2)
lines(s_size, MSE_WM1N, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC1N, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[1]" text annotation above 
mtext(expression(bold(hat(beta)[0])), side = 3, line = 1, cex = 1.15, font = 2)

# Plot 2
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL2N,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.2869, 0.3097, 0.2368)) ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WM2N, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC2N, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[2]" text annotation above 
mtext(expression(bold(hat(beta)[1])), side = 3, line = 1, cex = 1.15, font = 2)

# Plot 3
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL3N,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.0612, 0.0674, 0.0523)) ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WM3N, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC3N, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[3]" text annotation above 
mtext(expression(bold(hat(beta)[2])), side = 3, line = 1, cex = 1.15, font = 2)

# Plot 4
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CLRHON,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.0985, 0.1043, 0.0764)),ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WMRHON, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LCRHON, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "rho" text annotation above 
mtext(expression(bold(hat(rho)[1])), side = 3, line = 1, cex = 1.15, font = 2)

# Add main title for the first row
#mtext("N(0,1)", side = 3, outer = TRUE,line = -2, cex = 1.3, font = 2)

# Combined Legend (centered)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("CLAD", "WME", "CLCE"),
       col = c("hotpink", "navyblue", "dodgerblue"), 
       lty = c(1, 2, 1), lwd = c(2, 2, 2), pch = c(19, 15, 8), 
       cex = 2.0, horiz = TRUE)

# Close the PDF device
dev.off()

##### Laplace distribution
# Set the desired folder path
folder_path <- "C:/Users/Math 564 Cabin 1/Downloads/"  # Replace with your actual folder path

# Create the full file path for the PDF
pdf_file <- file.path(folder_path, "DE(0,1).pdf")

# Open the PDF device
pdf(pdf_file, width = 8, height = 10)

# Define layout: 4 plots in a 2x2 grid with an additional row for the legend
layout(matrix(c(1, 2,3, 4,5, 5), nrow = 3, byrow = TRUE), heights = c(4, 4, 1))


##################plot for DE(0,1)
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL1L,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.5169,  0.5287, 0.5044)),ylab = expression(bold("MSE")),font.lab =2)
lines(s_size, MSE_WM1L, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC1L, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[1]" text annotation above 
mtext(expression(bold(hat(beta)[0])), side = 3, line = 1, cex = 1.15, font = 2)

# Plot 2
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL2L,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  ,xlab = expression(bold("Sample Size")), ylim=c(0,max(0.3128,  0.3205, 0.2979)) ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WM2L, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC2L, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[2]" text annotation above 
mtext(expression(bold(hat(beta)[1])), side = 3, line = 1, cex = 1.15, font = 2)

# Plot 3
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL3L,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  ,xlab = expression(bold("Sample Size")), ylim=c(0,max(0.0692,  0.0714, 0.0678)) ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WM3L, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC3L, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[3]" text annotation above 
mtext(expression(bold(hat(beta)[2])), side = 3, line = 1, cex = 1.15, font = 2)

# Plot 4
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CLRHOL,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.1186,  0.1212, 0.1110)) ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WMRHOL, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LCRHOL, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "rho" text annotation above 
mtext(expression(bold(hat(rho)[1])), side = 3, line = 1, cex = 1.15, font = 2)


# Add main title for the first row
#mtext("DE(0,1)", side = 3, outer = TRUE, line = -2, cex = 1.3, font = 2)

# Combined Legend (centered)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("CLAD", "WME", "CLCE"),
       col = c("hotpink", "navyblue", "dodgerblue"), 
       lty = c(1, 2, 1), lwd = c(2, 2, 2), pch = c(19, 15, 8), 
       cex = 2.0, horiz = TRUE)

# Close the PDF device
dev.off()

##################plot for t_{3}
# Set the desired folder path
folder_path <- "C:/Users/Math 564 Cabin 1/Downloads/"  # Replace with your actual folder path

# Create the full file path for the PDF
pdf_file <- file.path(folder_path, "t_{3}.pdf")

# Open the PDF device
pdf(pdf_file, width = 8, height = 10)

# Define layout: 4 plots in a 2x2 grid with an additional row for the legend
layout(matrix(c(1, 2,3, 4,5, 5), nrow = 3, byrow = TRUE), heights = c(4, 4, 1))

par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL1T,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.6911, 0.6870, 0.5891)) ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WM1T, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC1T, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[1]" text annotation above 
mtext(expression(bold(hat(beta)[0])), side = 3, line = 1, cex = 1.15, font = 2)


# Plot 2
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL2T,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.4413, 0.4245, 0.3681)),ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WM2T, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC2T, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[2]" text annotation above 
mtext(expression(bold(hat(beta)[1])), side = 3, line = 1, cex = 1.15, font = 2)


# Plot 3
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL3T,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.0971, 0.0948, 0.0813))         
     ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WM3T, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC3T, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[3]" text annotation above 
mtext(expression(bold(hat(beta)[2])), side = 3, line = 1, cex = 1.15, font = 2)


# Plot 4
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CLRHOT,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  ,xlab = expression(bold("Sample Size")), ylim=c(0,max(0.1525, 0.1507, 0.1263)),ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WMRHOT, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LCRHOT, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "rho" text annotation above 
mtext(expression(bold(hat(rho)[1])), side = 3, line = 1, cex = 1.15, font = 2)


# Add main title for the first row
 #mtext(expression(t[3]), side = 3, outer = TRUE, line = -2, cex = 1.3, font = 2)

# Combined Legend (centered)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("CLAD", "WME", "CLCE"),
       col = c("hotpink", "navyblue", "dodgerblue"), 
       lty = c(1, 2, 1), lwd = c(2, 2, 2), pch = c(19, 15, 8), 
       cex = 2.0, horiz = TRUE)

# Close the PDF device
dev.off()



##################plot for Hetrocedastic error

# Set the desired folder path
folder_path <- "C:/Users/Math 564 Cabin 1/Downloads/"  # Replace with your actual folder path

# Create the full file path for the PDF
pdf_file <- file.path(folder_path, "het_.pdf")

# Open the PDF device
pdf(pdf_file, width = 8, height = 10)

# Define layout: 4 plots in a 2x2 grid with an additional row for the legend
layout(matrix(c(1, 2,3, 4,5, 5), nrow = 3, byrow = TRUE), heights = c(4, 4, 1))


par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL1H,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.5856, 0.5874, 0.5494))
     ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WM1H, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC1H, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[1]" text annotation above 
mtext(expression(bold(hat(beta)[0])), side = 3, line = 1, cex = 1.15, font = 2)



# Plot 2
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL2H,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.4398, 0.4368, 0.3891))
     ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WM2H, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC2H, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)
# Add the "beta[2]" text annotation above 
mtext(expression(bold(hat(beta)[1])), side = 3, line = 1, cex = 1.15, font = 2)


# Plot 3
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_CL3H,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.1002, 0.1007, 0.0926))
     ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WM3H, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LC3H, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "beta[3]" text annotation above 
mtext(expression(bold(hat(beta)[2])), side = 3, line = 1, cex = 1.15, font = 2)


# Plot 4
par(cex.axis = 1.5, cex.lab = 1)
plot(s_size, MSE_CLRHOH,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")), ylim=c(0,max(0.1507, 0.1509, 0.1291))
     ,ylab = expression(bold("MSE")),font.lab=2)
lines(s_size, MSE_WMRHOH, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_LCRHOH, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h = 0, col = "black", lty = 3, lwd = 2)

# Add the "rho" text annotation above 
mtext(expression(bold(hat(rho)[1])), side = 3, line = 1, cex = 1.15, font = 2)


# Add main title for the first row
#mtext( expression(bold(N(0, sigma[x[i]]^2)) * ", " * 
  #bold(sigma[x[i]]^2 == (0.5 * (x[1] + x[2]))^2)), side = 3, outer = TRUE, line = -3, cex = 1.3, font = 2)


# Combined Legend
par(mar = c(0, 0, 0, 0))  # Set margins to zero for the legend plot
plot.new()  # Initialize a blank plot for the legend
legend("center", legend = c("CLAD", "WME", "CLCE"),
       col = c("hotpink", "navyblue", "dodgerblue"), lty = c(1, 2, 1),lwd = c(2,2,2),pch = c(19, 15, 8), cex =
         2.0, horiz = TRUE)

# Close the PDF device
dev.off()


