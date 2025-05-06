##### Plot for paper-1 for MSE in case of N(0,1)
####parameter for beta{1}
s_size<-seq(50,1000,50)
MSE_c1<-c(0.53422418, 0.18796543, 0.11490246, 0.08702980, 0.06635697, 0.04829900, 0.04193374,0.03647833, 0.03103689, 0.02720531, 
          0.02634389, 0.02247679, 0.02163619, 0.01781813,0.01847049, 0.01526510, 0.01530239, 0.01358006, 0.01388947, 0.01378735)

MSE_h1<-c(2.58241669, 0.33254709, 0.13232949, 0.07069747, 0.05556929, 0.04372783, 0.03701784, 0.03179873, 0.02882116, 0.02462287, 
          0.02262632, 0.02085342, 0.01942516, 0.01763014,0.01598776, 0.01522260, 0.01371372, 0.01319454, 0.01317679, 0.01226784)

MSE1<-c(2.57972822, 1.20961727, 0.09342537, 0.05206915, 0.04862719, 0.03464750, 0.03270336,0.02682941, 0.02557054, 0.02286136, 
        0.01990847, 0.01794131, 0.01723930, 0.01652965,0.01620363, 0.01440211, 0.01478042, 0.01330762, 0.01352338, 0.01283640)

####parameter for beta{2}

MSE_c2<-c(0.056672648, 0.029665919, 0.018706184, 0.014357718, 0.011478120, 0.008461756,0.007749771, 0.006797797, 0.006050629, 0.005474534, 
          0.004624178, 0.004492091, 0.004208351, 0.004031772, 0.003611813, 0.003437074, 0.003272088, 0.002995495, 0.002873546, 0.002701663)

MSE_h2<-c(0.056868474, 0.025364510, 0.016192589, 0.012042972, 0.009136280, 0.008475867,0.006674426, 0.005909583, 0.005176325, 0.004811634, 
          0.004158988, 0.003675066,0.003393572, 0.003307742, 0.002941949, 0.002928241, 0.002875923, 0.002555360,0.002421301, 0.002205662)

MSE2<-c(0.045412894, 0.019621109, 0.012749632, 0.009462727, 0.007994525, 0.006848496, 0.005786441, 0.005356290, 0.004358423, 0.004190701, 
        0.003621664, 0.003636529, 0.003224204, 0.003078344, 0.002892647, 0.002673714, 0.002596849, 0.002499809,0.002289466, 0.002381544)

####parameter for beta{3}

MSE_c3<-c(1.09759011, 0.51691741, 0.32184924, 0.23701532, 0.17904268, 0.12907217, 0.10803149,0.09719974, 0.08052781, 0.07230987, 
          0.06803853, 0.05691956, 0.05530532, 0.04476139,0.04597707, 0.03972995, 0.03803952, 0.03366438, 0.03583825, 0.03470428)

MSE_h3<-c(13.86396073,  1.23650004,  0.38140885,  0.17808720,  0.12659243,  0.10181695,0.08367910, 0.07004946,  0.06522109,  0.05544659,  
          0.05009400,  0.04743793,0.04245174,  0.03907189,  0.03605692,  0.03419162,  0.03106738,  0.02927702,0.02670132, 0.02804145)

MSE3<-c(9.122254409,  4.84779347,  0.24150451,  0.13258821,  0.11391158,  0.07997550,0.07528175,  0.06007730,  0.05499736,  0.04953410,  
        0.04369167,  0.03718283,0.03570783, 0.03421479,  0.03271323,  0.02969645,  0.02840883,  0.02718598,0.02583118, 0.02524414)


####parameter for rho[1]

MSE_rho1<-c(1.14956020, 0.52221620, 0.32087632, 0.24160114, 0.18509067, 0.13050509, 0.11372069,0.09996743, 0.08333837, 0.07457852, 
            0.06920423, 0.05991891, 0.05712722, 0.04623906, 0.04706512, 0.04141461, 0.04033975, 0.03472661, 0.03749510, 0.03587740)

MSE_rho2<-c(13.99978931,  1.25544883,  0.38959854,  0.18127431,  0.12707814,  0.10280657,0.08620137,  0.07241430,  0.06625366,  0.05714130,  
            0.05070992,  0.04864782, 0.04343540,  0.04051438,  0.03662931,  0.03462398, 0.03219231,  0.02937135,0.02646147,  0.02880885)

MSE_rho3<-c(9.095233109,  4.83702675,  0.24377841,  0.13529036,  0.11261727,  0.08038809,0.07627290,  0.06026525,  0.05268210,  0.04951117,  
            0.04248665,  0.03691321,0.03581552,  0.03442529, 0.03231353,  0.02931718,  0.02757355,  0.02678296,0.02421908,  0.02458941)

##### Plot for paper-1 for MSE in case of laplace
####parameter for beta 1

MSE_c1_d<-c(0.615553568, 0.178697400, 0.124907747, 0.068943844, 0.055090543, 0.041276984,0.033305760, 0.032329382, 0.026373218, 0.021861387, 
            0.019632907, 0.017156323,0.014856782, 0.014913046, 0.012174583, 0.011665179, 0.011766076, 0.010013218,0.009881292, 0.009229923)

MSE_h1_d<-c(5.86539119, 0.31871630, 0.13771781, 0.10885601, 0.06969051, 0.05923209, 0.04976211,0.04504697, 0.03819544, 0.03403845, 
            0.03069775, 0.02794476, 0.02478309, 0.02238842,0.02194625, 0.02045692, 0.01850090, 0.01814390, 0.01659735, 0.01586119)

MSE1_d<-c(2.88562079, 0.26816876, 0.17918084, 0.07006928, 0.05722232, 0.04675417, 0.04002332,0.03479344, 0.03120841, 0.02708985, 
          0.02662447, 0.02423500, 0.02260163, 0.02187788,0.01983046, 0.01873106, 0.01937021, 0.01675310, 0.01561833, 0.01725088)

####parameter for beta 2

MSE_c2_d<-c(0.615553568, 0.178697400, 0.124907747, 0.068943844, 0.055090543, 0.041276984,0.033305760, 0.032329382, 0.026373218, 0.021861387, 
            0.019632907, 0.017156323,0.014856782, 0.014913046, 0.012174583, 0.011665179, 0.011766076, 0.010013218,0.009881292, 0.009229923)

MSE_h2_d<-c(0.094868158, 0.036973093, 0.022644800, 0.017138920, 0.012986765, 0.010744449,0.009340308, 0.007884292, 0.007283171, 0.006308484, 
            0.005610497, 0.005223455,0.004775186, 0.004755911, 0.004348491, 0.003804422, 0.003724638, 0.003679239,0.003253444, 0.003102857)

MSE2_d<-c(0.058969137, 0.025401354, 0.016311632, 0.012504075, 0.009698468, 0.008258120,0.007156525, 0.006226904, 0.005630521, 0.005326878, 
          0.004845466, 0.004787706,0.004006366, 0.003965622, 0.003516666, 0.003342052, 0.003152584, 0.003034164,0.002851055, 0.003029477)

####parameter for beta 3

MSE_c3_d<-c(1.20660745, 0.56761549, 0.37495474, 0.19421621, 0.14837964, 0.10813428, 0.08804370,0.08543424, 0.07069534, 0.05723838, 
            0.05108829, 0.04326724, 0.03794259, 0.03804887,0.03189489, 0.02992819, 0.02930211, 0.02634299, 0.02577110, 0.02334095)

MSE_h3_d<-c(20.35194996,  0.96661924,  0.35469758,  0.26123174,  0.15633397,  0.13850334, 0.11408719,  0.09753530,  0.08743601,  0.07394390,  
            0.06737557,  0.06049393,0.05487029,  0.04720957,  0.04666109,  0.04201322,  0.04133867,  0.03951139,0.03666819,  0.03360503)

MSE3_d<-c(11.32780684,  0.90236417,  0.48086275,  0.17554525,  0.14187079,  0.10964313,0.09122196,  0.07673938,  0.06721353,  0.05861105,  
          0.05235956,  0.04889078,0.04460268,  0.04162279,  0.03781625,  0.03554622,  0.03631147,  0.02966703,0.02717744,  0.03032014)

####parameter for rho[1]

MSE_rho1_d<-c(1.22579064, 0.59724597, 0.38946762, 0.19602298, 0.15284678, 0.11391185, 0.09102287,0.08597170, 0.07303648, 0.05989620, 
              0.05313891, 0.04505203, 0.03939821, 0.03905785,0.03285383, 0.03157776, 0.03086988, 0.02831322, 0.02688747, 0.02409865)

MSE_rho2_d<-c(20.36532249,  0.97968295,  0.35678979,  0.26216308,  0.16363982,  0.14184104,0.11634186,  0.09817608,  0.08978946,  0.07563172,  
     0.06863552,  0.06076915,0.05617140,  0.04815756,  0.04869551,  0.04202034,  0.04247957,  0.04073797,0.03753961,  0.03398304)

MSE_rho3_d<-c(11.25292169,  0.89553032,  0.47906067,  0.17846899,  0.14220188, 0.11055501,0.09061390,  0.07725973,  0.06687628,  0.05718982,  
             0.05207348, 0.04919687,0.04186464,  0.03948884,  0.03735846,  0.03476767,  0.03470667,  0.02913541,0.02691338, 0.02847245)

##### Plot for paper-1 for MSE in case of t_{3}

##parameter beta1

MSE_c1_t<-c(0.42501101, 0.19872571, 0.13052851, 0.09997806, 0.07504971, 0.06305208, 0.04877984,0.04532312, 0.03975014, 0.03297544, 
            0.03199032, 0.02804336, 0.02504797, 0.02228275,0.02098845, 0.02078079, 0.01803264, 0.01724889, 0.01646226, 0.01516746)

MSE_h1_t<-c(3.93490383, 0.38382196, 0.19820785, 0.11071712, 0.08455776, 0.06705280, 0.05281307,0.04552005,0.04097271, 0.03747777, 
            0.03285012, 0.02863764, 0.02784645, 0.02556434,0.02369540, 0.02090155, 0.02032048, 0.01964999, 0.01834793, 0.01752346)

MSE1_t<-c(9.91260906, 1.05208252, 0.16104874, 0.07806337, 0.06152454, 0.05360486, 0.04635603,0.03868851, 0.03572326, 0.03150537, 
          0.02860186, 0.02753646, 0.02705778, 0.02269981,0.02402782, 0.02045245, 0.02019072, 0.01999446, 0.01991675, 0.01845772)
##parameter beta2


MSE_c2_t<-c(0.079721759, 0.035344538, 0.024530167, 0.018526814, 0.014100898, 0.010245942,0.009410598, 0.008517901, 0.007056800, 0.006569650, 
            0.005852939, 0.004982374,0.004619651, 0.004500647, 0.004201182, 0.004220974, 0.003632533, 0.003551767,0.003333866, 0.003027420)

MSE_h2_t<-c(0.090955903, 0.038358489, 0.024153077, 0.018311893, 0.014347369, 0.011941246,0.009935336, 0.008800784, 0.007861624, 0.006790044, 
            0.006034858, 0.005695184,0.005517569, 0.004928724, 0.004850142, 0.004180872, 0.004068081, 0.003754243,0.003400734, 0.003290274)

MSE2_t<-c(0.065666194, 0.029894313, 0.019644615, 0.014478899, 0.012123283, 0.009932258,0.008239170, 0.006791427, 0.006960731, 0.006282151, 
          0.005367321, 0.004783607,0.004643921, 0.004420060, 0.004240542, 0.004039214, 0.003872131, 0.003694131,0.003369413,0.003234320)

##parameter beta3


MSE_c3_t<-c(1.17613115, 0.54551787, 0.35673118, 0.26821059, 0.19145368, 0.16869151, 0.12710469,0.11488467, 0.10145750, 0.08907269, 
            0.08386069, 0.07119394, 0.06490217, 0.05811982,0.05335476, 0.05345342, 0.04648513, 0.04498538, 0.04040081, 0.03653218)

MSE_h3_t<-c(15.79084452,  1.25536742,  0.63902274,  0.26493476,  0.20995555,  0.15408688,0.11547431,  0.10367854,  0.08670643,  0.08561007,  
            0.06871802,  0.06500426,0.05953061,  0.05666929,  0.04902889 , 0.04588016,  0.04560586,  0.04180115,0.03894364 , 0.03698269)

MSE3_t<-c(26.95635741,  2.42722430,  0.33259164,  0.21486832,  0.14581436 , 0.11943385,0.09963215,  0.07775781,  0.07685639,  0.07097305,  
          0.05647825,  0.05815299,  0.05058617,  0.04762580,  0.04343613,  0.04031969,  0.03645085,  0.03553221, 0.03563369, 0.03521779)

##parameter rho[1]

MSE_rho1_t<-c(1.24474698, 0.56588024, 0.36734884, 0.27374295, 0.19991361, 0.17442695, 0.12989911,0.11843090, 0.10137264, 0.09261323, 
              0.08704993, 0.07279982, 0.06811807, 0.05948887,0.05542922, 0.05495759, 0.04910405, 0.04533179, 0.04105150, 0.03746441)
MSE_rho2_t<-c(15.74634732,  1.28947043,  0.64576070,  0.27073111,  0.21471762 , 0.15739139,0.12088584,  0.10618227,  0.08722493,  0.08815354,  
              0.06992571 , 0.06798930,0.06110678 , 0.05735653,  0.05058997,  0.04666087,  0.04734043,  0.04236611,0.03959574,  0.03815836)

MSE_rho3_t<-c(27.04489217,  2.44305574,  0.32854744,  0.21321119 , 0.14746010,  0.11664329,0.09956624 , 0.07641139,  0.07573089,  0.06880793 , 
             0.05440025,  0.05572941, 0.04884865,  0.04535820,  0.04272614 , 0.03878695 , 0.03480406 , 0.03441442, 0.03396401 , 0.03373680)

##### Plot for paper-1 for MSE in case of hetrocedastic error

##parameter beta1

MSE_c1_h<-c(1.94551317, 1.76153072, 0.80323942, 0.56474242, 0.41164337, 0.30550320 ,0.26618170,0.22388833, 0.18106661, 0.15835970, 
            0.13766007, 0.12026822, 0.10228220, 0.09831134, 0.09039394, 0.08765309, 0.07686381, 0.06666843, 0.06781754 ,0.05914297)

MSE_h1_h<-c(75.3349611, 20.0792984,  6.1312836,  6.5017489,  1.7064067,  1.3464431,  0.9270001,0.8543408,  0.6646637,  0.5843068,  
            0.4450589,  0.4360592,  0.3628129,  0.3422486,0.2887970,  0.2574362,  0.2811986 , 0.2369129,  0.2428522 , 0.2252328)

MSE1_h<-c(53.33461557,  5.02105574,  1.05514918,  0.52576462,  0.40416040,  0.32046915,0.27543206,  0.22962229,  0.18846557,  0.16801778,  
          0.14663762,  0.14830119,0.12664903,  0.12138881,  0.11466439,  0.10885436,  0.10482077,  0.09703012,0.09597205,  0.09125781)


##parameter beta2

MSE_c2_h<-c(1.16799163, 0.61285403, 0.32152498 ,0.22403566, 0.18337522, 0.14147240, 0.11837168,0.09307983 ,0.08376518, 0.07595713, 
            0.06589343, 0.06034605, 0.05577298, 0.04828337,0.04527965, 0.04414279, 0.03935527 ,0.03510810 ,0.03575044 ,0.03335252)
MSE_h2_h<-c(6.01785272, 1.85277670, 1.07504260, 0.51376051, 0.42003741 ,0.33777474, 0.27079035,0.23190445, 0.20361201, 0.17824026, 
            0.14211884, 0.13889743 ,0.12221193, 0.11167141,0.10353407, 0.09641661, 0.09442479, 0.08123609 ,0.08263369 ,0.07469692)
MSE2_h<-c(1.78140508, 0.61007091, 0.34487226, 0.20202232, 0.15557432, 0.13354812, 0.11244607,0.09781030, 0.08549650, 0.07759884,
          0.07257407, 0.07148392, 0.06544374, 0.05882615,0.05892212, 0.05385190, 0.05045388, 0.05101194, 0.05054017, 0.04699394)

##parameter beta3

MSE_c3_h<-c(7.15244978,  4.3088797,  2.2244243,  1.6086518 , 1.1725049,  0.9391616,  0.8014887,0.6675617,  0.5818867,  0.5081843, 
            0.4430094,  0.3979105,  0.3258480,  0.3184799,0.2979852,  0.2828240,  0.2558171,  0.2301212,  0.2135569,  0.1904878)
MSE_h3_h<-c(184.6647660 , 44.1043482,   8.1836697,  17.8210002,   3.3250496,   2.4577901,1.9911616 ,  1.6799920,   1.3863543,   1.1576786,   
            0.9940511,   0.9644980,0.7984510,   0.7773062,   0.6719051 ,  0.5806783,   0.6198694,   0.5430519,0.5288492,   0.5140530)
MSE3_h<-c(123.2325834,  15.1647879,   2.8420844 ,  1.6232082,   1.3177866,   0.9973974, 0.8281218,   0.7007390,   0.5968025,   0.5177201,   
          0.4547013,   0.4413726,   0.4007129,   0.3658947,   0.3350715,   0.3059357 ,  0.2989658,   0.2765468,  0.2717126,   0.2482944)


##parameter rho[1]

MSE_rho1_h<-c(6.81239205,  4.3402942,  2.1588249,  1.5331263,  1.1700276,  0.8936119,  0.7413392,0.6172918,  0.5302925,  0.4822812,  
              0.4060306,  0.3582418,  0.3114977,  0.3040985,0.2610331,  0.2377237,  0.2319444,  0.1976496,  0.1942097,  0.1714677)

MSE_rho2_h<-c(176.3563936,  41.8283164,   8.4138949,  17.5261315,   3.2639048,   2.3073482,1.9053971,   1.5598553,  1.2728603,   1.0513688,   
              0.9553606,   0.8516182, 0.7918612 ,  0.6761200 ,  0.6152214,   0.5731625 ,  0.5711545,   0.4890668, 0.4615789 ,  0.4585740)

MSE_rho3_h<-c(123.1875355,  15.2469201,   2.6595331,   1.6222782,   1.2208139,   0.8711375,0.7645319,   0.6337023,   0.5216392 ,  0.4699744,   
             0.4063001 ,  0.3835083,0.3349349,   0.3052050,   0.3094422,   0.2627270,   0.2543457 ,  0.2360319,0.2287737,   0.2057975)


#### ploting

# Set the desired folder path
folder_path <- "C:/Users/DELL/Downloads/paper-1 code/paper-1 code"  # Replace with your actual folder path

# Create the full file path for the PDF
pdf_file <- file.path(folder_path, "N(0,1)_.pdf")

# Open the PDF device
pdf(pdf_file, width = 8, height = 10)

# Define layout: 4 plots in a 2x2 grid with an additional row for the legend
layout(matrix(c(1, 2,3, 4,5, 5), nrow = 3, byrow = TRUE), heights = c(4, 4, 1))

##################plot for N(0,1)
# Plot 1
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c1,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size"))  ,ylab = expression(bold("MSE of") ~ bold(hat(beta[1]))),font.lab=2)
lines(s_size, MSE_h1, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE1, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[1]" text annotation above 
mtext(expression(bold(hat(beta[1]))), side = 3, line = 0, cex = 1.15, font = 2)

# Plot 2
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c2,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")) ,ylab = expression(bold("MSE of") ~ bold(hat(beta[2]))),font.lab=2)
lines(s_size, MSE_h2, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE2, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[2]" text annotation above 
mtext(expression(bold(hat(beta[2]))), side = 3, line = 0, cex = 1.15, font = 2)

# Plot 3
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c3,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")) ,ylab = expression(bold("MSE of") ~ bold(hat(beta[3]))),font.lab=2)
lines(s_size, MSE_h3, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE3, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[3]" text annotation above 
mtext(expression(bold(hat(beta[3]))), side = 3, line = 0, cex = 1.15, font = 2)

# Plot 4
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_rho1,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")),ylab = expression(bold("MSE of") ~ bold(hat(rho[1]))),font.lab=2)
lines(s_size, MSE_rho2, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_rho3, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "rho" text annotation above 
mtext(expression(bold(hat(rho[1]))), side = 3, line = 0, cex = 1.15, font = 2)

# Add main title for the first row
mtext("N(0,1)", side = 3, outer = TRUE,line = -2, cex = 1.3, font = 2)

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
folder_path <- "C:/Users/DELL/Downloads/paper-1 code/paper-1 code"  # Replace with your actual folder path

# Create the full file path for the PDF
pdf_file <- file.path(folder_path, "DE(0,1)_.pdf")

# Open the PDF device
pdf(pdf_file, width = 8, height = 10)

# Define layout: 4 plots in a 2x2 grid with an additional row for the legend
layout(matrix(c(1, 2,3, 4,5, 5), nrow = 3, byrow = TRUE), heights = c(4, 4, 1))


##################plot for DE(0,1)
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c1_d,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")),ylab = expression(bold("MSE of") ~ bold(hat(beta[1]))),font.lab =2)
lines(s_size, MSE_h1_d, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE1_d, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[1]" text annotation above 
mtext(expression(bold(hat(beta[1]))), side = 3, line = 0, cex = 1.15, font = 2)

# Plot 2
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c2_d,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  ,xlab = expression(bold("Sample Size")) ,ylab = expression(bold("MSE of") ~ bold(hat(beta[2]))),font.lab=2)
lines(s_size, MSE_h2_d, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE2_d, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[2]" text annotation above 
mtext(expression(bold(hat(beta[2]))), side = 3, line = 0, cex = 1.15, font = 2)

# Plot 3
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c3_d,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  ,xlab = expression(bold("Sample Size")) ,ylab = expression(bold("MSE of") ~ bold(hat(beta[3]))),font.lab=2)
lines(s_size, MSE_h3_d, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE3_d, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[3]" text annotation above 
mtext(expression(bold(hat(beta[3]))), side = 3, line = 0, cex = 1.15, font = 2)

# Plot 4
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_rho1_d,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")) ,ylab = expression(bold("MSE of") ~ bold(hat(rho[1]))),font.lab=2)
lines(s_size, MSE_rho2_d, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_rho3_d, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "rho" text annotation above 
mtext(expression(bold(hat(rho[1]))), side = 3, line = 0, cex = 1.15, font = 2)


# Add main title for the first row
mtext("DE(0,1)", side = 3, outer = TRUE, line = -2, cex = 1.3, font = 2)

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
folder_path <- "C:/Users/DELL/Downloads/paper-1 code/paper-1 code"  # Replace with your actual folder path

# Create the full file path for the PDF
pdf_file <- file.path(folder_path, "t_{3}_.pdf")

# Open the PDF device
pdf(pdf_file, width = 8, height = 10)

# Define layout: 4 plots in a 2x2 grid with an additional row for the legend
layout(matrix(c(1, 2,3, 4,5, 5), nrow = 3, byrow = TRUE), heights = c(4, 4, 1))

par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c1_t,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size")) ,ylab = expression(bold("MSE of") ~ bold(hat(beta[1]))),font.lab=2)
lines(s_size, MSE_h1_t, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE1_t, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[1]" text annotation above 
mtext(expression(bold(hat(beta[1]))), side = 3, line = 0, cex = 1.15, font = 2)


# Plot 2
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c2_t,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size")),ylab = expression(bold("MSE of") ~ bold(hat(beta[2]))),font.lab=2)
lines(s_size, MSE_h2_t, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE2_t, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[2]" text annotation above 
mtext(expression(bold(hat(beta[2]))), side = 3, line = 0, cex = 1.15, font = 2)


# Plot 3
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c3_t,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size"))         
     ,ylab = expression(bold("MSE of") ~ bold(hat(beta[3]))),font.lab=2)
lines(s_size, MSE_h3_t, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE3_t, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[3]" text annotation above 
mtext(expression(bold(hat(beta[3]))), side = 3, line = 0, cex = 1.15, font = 2)


# Plot 4
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_rho1_t,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  ,xlab = expression(bold("Sample Size")),ylab = expression(bold("MSE of") ~ bold(hat(rho[1]))),font.lab=2)
lines(s_size, MSE_rho2_t, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_rho3_t, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "rho" text annotation above 
mtext(expression(bold(hat(rho[1]))), side = 3, line = 0, cex = 1.15, font = 2)


# Add main title for the first row
mtext(expression(t[3]), side = 3, outer = TRUE, line = -2, cex = 1.3, font = 2)

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
folder_path <- "C:/Users/DELL/Downloads/paper-1 code/paper-1 code"  # Replace with your actual folder path

# Create the full file path for the PDF
pdf_file <- file.path(folder_path, "het_.pdf")

# Open the PDF device
pdf(pdf_file, width = 8, height = 10)

# Define layout: 4 plots in a 2x2 grid with an additional row for the legend
layout(matrix(c(1, 2,3, 4,5, 5), nrow = 3, byrow = TRUE), heights = c(4, 4, 1))


par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c1_h,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size"))
     ,ylab = expression(bold("MSE of") ~ bold(hat(beta[1]))),font.lab=2)
lines(s_size, MSE_h1_h, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE1_h, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[1]" text annotation above 
mtext(expression(bold(hat(beta[1]))), side = 3, line = 0, cex = 1.15, font = 2)



# Plot 2
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c2_h,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size"))
     ,ylab = expression(bold("MSE of") ~ bold(hat(beta[2]))),font.lab=2)
lines(s_size, MSE_h2_h, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE2_h, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[2]" text annotation above 
mtext(expression(bold(hat(beta[2]))), side = 3, line = 0, cex = 1.15, font = 2)


# Plot 3
par(cex.axis = 2, cex.lab = 1)
plot(s_size, MSE_c3_h,type = "o", pch=19,lty=1,lwd = 2 ,col= "hotpink"  , xlab = expression(bold("Sample Size"))
     ,ylab = expression(bold("MSE of") ~ bold(hat(beta[3]))),font.lab=2)
lines(s_size, MSE_h3_h, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE3_h, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "beta[3]" text annotation above 
mtext(expression(bold(hat(beta[3]))), side = 3, line = 0, cex = 1.15, font = 2)


# Plot 4
par(cex.axis = 1.5, cex.lab = 1)
plot(s_size, MSE_rho1_h,type = "o", pch=19,lty=1,lwd = 2, ,col= "hotpink"  , xlab = expression(bold("Sample Size"))
     ,ylab = expression(bold("MSE of") ~ bold(hat(rho[1]))),font.lab=2)
lines(s_size, MSE_rho2_h, col= "navyblue" ,type="o",pch=15,lty=2,lwd = 2)
lines(s_size, MSE_rho3_h, col=  "dodgerblue",type="o",pch=8,lty=1,lwd = 2)
grid()
abline(h=0,col="black")

# Add the "rho" text annotation above 
mtext(expression(bold(hat(rho[1]))), side = 3, line = 0, cex = 1.15, font = 2)


# Add main title for the first row
mtext( expression(bold(N(0, sigma[x[i]]^2)) * ", " * 
                    bold(sigma[x[i]]^2 == (0.5 * (x[1] + x[2]))^2)), side = 3, outer = TRUE, line = -3, cex = 1.3, font = 2)


# Combined Legend
par(mar = c(0, 0, 0, 0))  # Set margins to zero for the legend plot
plot.new()  # Initialize a blank plot for the legend
legend("center", legend = c("CLAD", "WME", "CLCE"),
       col = c("hotpink", "navyblue", "dodgerblue"), lty = c(1, 2, 1),lwd = c(2,2,2),pch = c(19, 15, 8), cex =
         2.0, horiz = TRUE)

# Close the PDF device
dev.off()


