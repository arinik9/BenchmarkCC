#inputFolder="/users/narinik/eclipse/workspace/CoH-SosoCC/out/partitions-temp"
outputFolder="/users/narinik/eclipse/workspace/CoH-SosoCC/out/partitions"
networkFolderName="n=50_l0=3_dens=1.0000"

#for networkFolderName in `ls "$inputFolder" | sort -V` ; do
	outNetworkFolder="$outputFolder""/""$networkFolderName"

	for propMisplFolderName in `ls "$outNetworkFolder" | sort -V` ; do
		outPropMisplFolder="$outNetworkFolder""/""$propMisplFolderName"

		for propNegFolderName in `ls "$outPropMisplFolder" | sort -V` ; do
			outPropNegFolder="$outPropMisplFolder""/""$propNegFolderName"

            #echo "!!!!!!!"
			for i in `seq 1 30`; do
				outIterNoFolder="$outPropNegFolder""/network=""$i""/ExCC/signed-unweighted"

                if [ -f "$outIterNoFolder""/log.txt" ]; then
                    echo "BEGIN -----------"
                    echo "$outIterNoFolder"
                    grep -A 3 "last CP" "$outIterNoFolder""/log.txt"
                    #head -4  "$outIterNoFolder""/log.txt"                    
                    echo "END -----------"
                fi
				
			done
		done
	done
#done
