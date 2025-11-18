# /bin/bash

# data to collect
# inside marchfrom3.315_DOWN
#declare -a dir=(
#dim3.1000
#dim3.1010
#dim3.1030
#dim3.1040
#dim3.1050
#dim3.1060
#dim3.1070
#dim3.1080
#dim3.1090
#dim3.1100
#dim3.1110
#dim3.1120
#dim3.1130
#dim3.1140
#dim3.1150
#dim3.1160
#dim3.1170
#dim3.1180
#dim3.1190
#dim3.1200
#dim3.1210
#dim3.1220
#dim3.1230
#dim3.1240
#dim3.1250
#dim3.1260
#dim3.1270
#dim3.1280
#dim3.1290
#dim3.1300
#dim3.1310
#dim3.1320
#dim3.1330
#dim3.1340
#dim3.1350
#dim3.1400
#)

## inside base directory
#declare -a dir=(
#dim3.1450
#dim3.1500
#dim3.1550
#dim3.1600
#dim3.1650
#dim3.1700
#dim3.1750
#dim3.1800
#)

## inside base directory PERT
#declare -a dir=(
#dim3.2000
#dim3.2500
#dim3.3000
#dim3.3500
#dim3.4000
#dim3.4500
#dim3.5000
#dim3.5500
#dim3.6000
#dim3.6500
#dim3.7000
#dim3.7100
#dim3.7200
#dim3.7300
#dim3.7400
#dim3.7500
#dim3.7550
#dim3.7554
#dim3.7555
#dim3.7556
#dim3.7557
#dim3.75572
#dim3.755726
#dim3.7557262
#dim3.75572621
#dim3.75572622
#dim3.75572623
#dim3.75573
#dim3.7558
#dim3.7559
#dim3.7560
#dim3.7600
#dim3.7700
#dim3.7800
#dim3.7900
#dim3.8000
#dim3.8500
#dim3.9000
#dim3.9500
#dim4.0000
#dim4.0500
#dim4.1000
#dim4.1500
#dim4.2000
#dim4.2500
#dim4.3000
#dim4.3500
#dim4.3550
#dim4.3600
#dim4.3650
#dim4.3700
#dim4.3750
#dim4.3800
#dim4.3850
#)

## inside base directory Taylorord 2
#declare -a dir=(
#dim4.3900
#dim4.3950
#dim4.4000
#dim4.4050
#dim4.4100
#dim4.4150
#dim4.4200
#dim4.4250
#dim4.4300
#dim4.4350
#dim4.4400
#dim4.4450
#dim4.4500
#dim4.4550
#dim4.4600
#dim4.4650
#dim4.4700
#dim4.4750
#dim4.4800
#dim4.4850
#dim4.4900
#dim4.4950
#dim4.5000
#dim4.5050
#dim4.5100
#dim4.5150
#dim4.5200
#dim4.5250
#dim4.5300
#dim4.5350
#dim4.5400
#dim4.5450
#dim4.5500
#dim4.5550
#dim4.5600
#dim4.5650
#dim4.5700
#dim4.5750
#dim4.5800
#dim4.5850
#dim4.5900
#dim4.5950
#dim4.6000
#dim4.6050
#dim4.6100
#dim4.6150
#dim4.6160
#dim4.6200
#dim4.6220
#dim4.6250
#dim4.6280
#dim4.6300
#dim4.6320
#dim4.6340
#dim4.6360
#dim4.6380
#dim4.6400
#dim4.6420
#dim4.6440
#dim4.6460
#dim4.6480
#dim4.6500
#dim4.6520
#dim4.6540
#dim4.6560
#dim4.6580
#dim4.6600
#dim4.6620
#dim4.6640
#dim4.6650
#dim4.6660
#dim4.6670
#dim4.6680
#dim4.6690
#dim4.6700
#dim4.6710
#dim4.6720
#dim4.6730
#dim4.6740
#dim4.6750
#dim4.6760
#dim4.6770
#dim4.6780
#dim4.6790
#dim4.6800
#dim4.6810
#dim4.6820
#dim4.6830
#dim4.6840
#dim4.6850
#dim4.6860
#dim4.6870
#dim4.6880
#dim4.6890
#dim4.6900
#dim4.6910
#dim4.6920
#dim4.6930
#dim4.6940
#dim4.6950
#dim4.6960
#dim4.6970
#dim4.6980
#dim4.6990
#dim4.7000
#dim4.7010
#dim4.7020
#dim4.7030
#dim4.7040
#dim4.7050
#dim4.7060
#dim4.7070
#dim4.7080
#dim4.7090
#dim4.7100
#dim4.7110
#dim4.7120
#dim4.7130
#dim4.7140
#dim4.7150
#dim4.7160
#dim4.7170
#dim4.7180
#dim4.7190
#dim4.7200
#dim4.7210
#dim4.7220
#dim4.7230
#dim4.7240
#dim4.7250
#dim4.7260
#dim4.7270
#dim4.7280
#dim4.7290
#dim4.7300
#dim4.7310
#dim4.7320
#dim4.7330
#dim4.7340
#dim4.7350
#dim4.7360
#dim4.7370
#dim4.7380
#dim4.7390
#dim4.7400
#dim4.7410
#dim4.7420
#dim4.7430
#dim4.7440
#dim4.7450
#dim4.7460
#dim4.7470
#dim4.7480
#dim4.7490
#dim4.7500
#dim4.7510
#dim4.7520
#dim4.7530
#dim4.7540
#dim4.7550
#dim4.7560
#dim4.7570
#dim4.7580
#dim4.7590
#dim4.7600
#dim4.7610
#dim4.7620
#dim4.7630
#dim4.7640
#dim4.7650
#dim4.7660
#dim4.7670
#dim4.7680
#dim4.7690
#dim4.7700
#dim4.7710
#dim4.7720
#dim4.7730
#dim4.7740
#dim4.7750
#dim4.7760
#dim4.7770
#dim4.7780
#dim4.7790
#dim4.7800
#dim4.7810
#dim4.7820
#dim4.7830
#dim4.7840
#dim4.7850
#dim4.7860
#dim4.7870
#dim4.7880
#dim4.7890
#dim4.7900
#dim4.7950
#)

## inside base directory Taylor ord 3
#declare -a dir=(
#dim4.8000
#dim4.8050
#dim4.8100
#dim4.8150
#dim4.8200
#dim4.8250
#dim4.8300
#dim4.8350
#dim4.8400
#dim4.8450
#dim4.8500
#dim4.8550
#dim4.8600
#dim4.8650
#dim4.8700
#dim4.8750
#dim4.8800
#dim4.8850
#dim4.8900
#dim4.8950
#dim4.9000
#dim4.9050
#dim4.9100
#dim4.9150
#dim4.9200
#dim4.9250
#dim4.9300
#dim4.9350
#dim4.9400
#dim4.9450
#dim4.9500
#dim4.9550
#dim4.9600
#dim4.9650
#dim4.9700
#dim4.9750
#dim4.9800
#dim4.9850
#dim4.9900
#dim4.9950
#dim5.0000
#dim5.0050
#dim5.0250
#dim5.0450
#dim5.0650
#dim5.0850
#dim5.1050
#dim5.1250
#dim5.1350
#dim5.1450
#dim5.1490
#dim5.1510
#dim5.1530
#)


## inside marchfrom5.155_UP
declare -a dir=(
dim5.1550
dim5.1570
dim5.1590
dim5.1690
dim5.1790
dim5.1890
dim5.1990
dim5.2090
dim5.2190
dim5.2290
dim5.2390
dim5.2490
dim5.2590
dim5.2690
dim5.2790
dim5.3190
dim5.3490
dim5.3590
dim5.3690
dim5.3790
dim5.3890
dim5.4090
dim5.4190
dim5.4590
dim5.4890
dim5.4990
dim5.5000
)

# inside marchfrom5.155_UP/above_dim5.5
#declare -a dir=(
#dim5.5090
#dim5.5190
#dim5.5290
#dim5.5390
#dim5.5490
#dim5.5590
#dim5.5690
#dim5.5790
#dim5.5890
#dim5.5990
#dim5.6090
#dim5.6190
#dim5.6290
#dim5.6390
#dim5.6490
#dim5.6590
#dim5.6690
#dim5.6790
#dim5.6890
#dim5.6990
#dim5.7090
#dim5.7190
#dim5.7290
#dim5.7390
#dim5.7490
#dim5.7590
#dim5.7690
#dim5.7790
#dim5.7890
#dim5.7990
#dim5.8090
#dim5.8190
#dim5.8290
#dim5.8390
#dim5.8490
#dim5.8590
#dim5.8690
#dim5.8790
#dim5.8890
#dim5.8990
#dim5.9090
#dim5.9190
#dim5.9290
#dim5.9390
#dim5.9490
#dim5.9590
#dim5.9690
#)

# collecting data
for dirNow in ${dir[@]}; do
 echo $dirNow
 mkdir $dirNow
 cd $dirNow
 mkdir 512
 mkdir 256
 mkdir 128
 # inside marchfrom3.135_DOWN
 #cp ../../marchfrom3.135_DOWN/$dirNow/512e-4/*.out 	512/.
 #cp ../../marchfrom3.135_DOWN/$dirNow/512e-4/*.par 	512/.
 #cp ../../marchfrom3.135_DOWN/$dirNow/512e-4/pert/gam.out 	512/.
 # base directory e-4
 #cp ../../$dirNow/512e-4/*.out 		512/.
 #cp ../../$dirNow/512e-4/*.par 		512/.
 #cp ../../$dirNow/512/pert/gam.out 	512/.
 #cp ../../$dirNow/256/*.out 		256/.
 #cp ../../$dirNow/256/*.par 		256/.
 #cp ../../$dirNow/256/pert/gam.out 	256/.
 #cp ../../$dirNow/128/*.out 		128/.
 #cp ../../$dirNow/128/*.par 		128/.
 #cp ../../$dirNow/128/pert/gam.out 	128/.
 # base directory PERT
 #cp ../../$dirNow/512/pert/*.out 		512/.
 #cp ../../$dirNow/512/pert/*.par 		512/.
 #cp ../../$dirNow/512/pert/gam.out 	512/.
 #cp ../../$dirNow/256/*.out 		256/.
 #cp ../../$dirNow/256/*.par 		256/.
 #cp ../../$dirNow/256/pert/gam.out 	256/.
 #cp ../../$dirNow/128/*.out 		128/.
 #cp ../../$dirNow/128/*.par 		128/.
 #cp ../../$dirNow/128/pert/gam.out 	128/.
 # base directory
 #cp ../../$dirNow/512/*.out 		512/.
 #cp ../../$dirNow/512/*.par 		512/.
 #cp ../../$dirNow/512/pert/gam.out 	512/.
 #cp ../../$dirNow/256/*.out 		256/.
 #cp ../../$dirNow/256/*.par 		256/.
 #cp ../../$dirNow/256/pert/gam.out 	256/.
 #cp ../../$dirNow/128/*.out 		128/.
 #cp ../../$dirNow/128/*.par 		128/.
 #cp ../../$dirNow/128/pert/gam.out 	128/.
 # inside marchfrom5.155_UP
 cp ../../marchfrom5.155_UP/$dirNow/512/*.out 		512/.
 cp ../../marchfrom5.155_UP/$dirNow/512/*.par 		512/.
 cp ../../marchfrom5.155_UP/$dirNow/512/pert/gam.out 	512/.
 #cp ../../marchfrom5.155_UP/$dirNow/256/*.out 		256/.
 #cp ../../marchfrom5.155_UP/$dirNow/256/*.par 		256/.
 #cp ../../marchfrom5.155_UP/$dirNow/256/pert/gam.out 	256/.
 # inside marchfrom5.155_UP/above_dim5.5
 #cp ../../marchfrom5.155_UP/above_dim5.5/$dirNow/512/*.out 		512/.
 #cp ../../marchfrom5.155_UP/above_dim5.5/$dirNow/512/*.par 		512/.
 #cp ../../marchfrom5.155_UP/above_dim5.5/$dirNow/512/pert/gam.out	512/.
 #cp ../../marchfrom5.155_UP/above_dim5.5/$dirNow/256/*.out 		256/.
 #cp ../../marchfrom5.155_UP/above_dim5.5/$dirNow/256/*.par 		256/.
 #cp ../../marchfrom5.155_UP/above_dim5.5/$dirNow/256/pert/gam.out	256/.
 cd ..
done
