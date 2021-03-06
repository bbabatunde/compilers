#!/bin/bash

#############################################
#
# Execution script 
# ----------------
#
# by Julian Gutierrez
#
# Note: Run from main folder.
#############################################

#############################################
#
# Static configuration variables
#
#############################################

export OMP_NUM_THREADS=32
export OMP_NESTED=TRUE

# Set block sizes to be tested
bslist="16"
#bslist="2 4 8 16 32"
#bslist="2 4 8 16 32 64 128"

#############################################
#
# On the run configuration variables
#
#############################################

# Message
echo "****************************"
echo "* Running All of the tests *"
echo "****************************"
echo "By Julian Gutierrez"
echo ""

echo -n "Algorithm you want to run: "
read alg

echo -n "Algorithm version you want to run: "
read algver

echo -n "Name for the test to run: "
read testname

echo -n "How many iterations of the tests? Num: "
read iterations

echo -n "Run profiler for each test? (Takes longer) Yes/no: "
read profiler

if (( "$iterations" < 0 )) 
then
	iterations=1
elif (( "$iterations" > 5 )) 
then
	echo "Not saving images to avoid significant disk usage."
	answer="no"
else
	echo -n "Save images or not? Yes/no: "
	read answer
fi


# Code Versions
codes=$(ls algorithms/$algver)
#codes=$(ls algorithms/$algver | grep -v omp) 
#"cuda-basic cuda-basic-4 cuda-opt"

# Set tests name

#Hardcoding fix to error in coins experiment for lss-omp experiment. NO IDEA!
if [ "$algver" == lss-omp ]; then	
	tests=$(ls inputs | grep -v coins)
else
	tests=$(ls inputs)
fi

#Create synthetic input
cd inputs/synthetic/
make -s clean
make -s all
make -s run
cd ../../

cd inputs/circle/
make -s clean
make -s all
make -s run
cd ../../

#############################################
#
# Script execution
#
#############################################

for test in $tests
do

	echo "*********"
	echo "* Running $test test"
	echo "*********"
	
	for code in $codes
	do
		#Set input files
		imagefolder="inputs/$test"

		#Set result folder
		result="results/$testname/$test/"
		
		#Set algorithm folder
		algfolder="algorithms/$algver/$code"
		
		#Remove previous results
		rm -rf $result/$code
		mkdir -p $result/$code

		echo "* Executing $code code for test $test"
		echo "***********"
		
		for bs in $bslist
		do
			#compile cuda code
			echo "* Compiling for block size $bs"
			cd $algfolder
			sed -i.bak 's/define BLOCK_TILE_SIZE [0-9]\+/define BLOCK_TILE_SIZE '"$bs"'/' lib/config.h
			make -s clean 
			make -s all
			cd ../../../
			
			for (( i=1; i<=$iterations; i++ ))
			do			
				echo "* Running iteration $i"
				if [ "$alg" == lss ]; then
					$algfolder/$alg \
					--image $imagefolder/$test.intensities.pgm \
					--labels $imagefolder/$test.label.pgm \
					--params $imagefolder/$test.params  > $result/$code/$bs-$i.log
				else 
					$algfolder/$alg \
					--image $imagefolder/$test.intensities.pgm > $result/$code/$bs-$i.log
				fi
				if [ "$answer" == Yes ]; then
					mv result.ppm $result/$code/$bs-$i.ppm
				else
					rm result.ppm
				fi
			done
			#run profiler
			if [ "$profiler" == Yes ]; then
				if [ "$alg" == lss ]; then
					nvprof --metrics all $algfolder/$alg \
					--image $imagefolder/circle-$test.intensities.pgm \
					--labels $imagefolder/circle-$test.label-$pref.pgm \
					--params $imagefolder/circle.params &> $result/$code/$bs.nvprof.log
				else 
					nvprof --metrics all $algfolder/$alg \
					--image $imagefolder/circle-$test.intensities.pgm &> $result/$code/$bs.nvprof.log
				fi
				rm result.ppm
			fi
		done
	done
done
