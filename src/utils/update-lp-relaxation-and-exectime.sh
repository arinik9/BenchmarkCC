
srcMainFoldr="propMispl=0.4000-bckp/propNeg=0.6800"
trgtMainFldr="propMispl=0.4000/propNeg=0.6800"

for i in `seq 2 1 2`; do
	srcFoldr="$srcMainFoldr""/""network="$i"/ExCC/signed-unweighted"
	trgtFldr="$trgtMainFldr""/""network="$i"/ExCC/signed-unweighted"

	echo $srcFoldr
	echo $trgtFldr

	cp $srcFoldr"/used-ram-memory.txt" $trgtFldr
	cp $srcFoldr"/membership0.txt" $trgtFldr
	mv $trgtFldr"/exec-time.txt" $trgtFldr"/exec-time_orig.txt"
	cp $srcFoldr"/exec-time.txt" $trgtFldr

	mv $trgtFldr"/logcplex.txt" $trgtFldr"/logcplex_orig.txt"
	cp $srcFoldr"/logcplex.txt" $trgtFldr
	mv $trgtFldr"/log.txt" $trgtFldr"/log_orig.txt"
	cp $srcFoldr"/log.txt" $trgtFldr

	exectime=`cat $trgtFldr/exec-time_orig.txt`
	export LC_NUMERIC="en_US.UTF-8"
	exectimeformated=`echo "$exectime" | xargs printf "%.0f"`;
	echo $exectimeformated

	line=`tail -1 $trgtFldr/logcplex_orig.txt`
	lpvalue=`echo "${line/Iteration:     1   Dual objective     =           /}"`
	lpvalueformated=`echo "$lpvalue" | xargs printf "%.1f"`;
	echo $lpvalueformated

	cd $trgtFldr
	awk -v var="$exectimeformated" '{ if (NR == 3) print "\tcp time: " var "s"; else print $0}' log.txt > log2.txt
	awk -v var="$lpvalueformated" '{ if (NR == 7) print "\tlast CP relax.: " var; else print $0}' log2.txt > log3.txt
	mv log3.txt log.txt
	rm log2.txt


#	line=`awk '{ if (NR == 3) print $0;}' log.txt`
#	line2=`echo "${line/cp time:/}"`
#	num1=`echo "${line2::-1}"`
#	echo $num1

#	line=`awk '{ if (NR == 4) print $0;}' log.txt`
#	line2=`echo "${line/time BB:/}"`
#	num2=`echo "${line2::-1}"`
#	echo $num2
#	echo "---"

#	num=$((num1 + num2))
#	echo $num
#	echo $num > exec-time.txt

	cd ../../../../..

done
