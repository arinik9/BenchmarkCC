inputFolder="/users/narinik/eclipse/workspace/CoH-SosoCC/out/partitions-temp"
outputFolder="/users/narinik/eclipse/workspace/Sosocc-Enum/out/partitions"

for networkFolderName in `ls "$inputFolder" | sort -V` ; do
	inNetworkFolder="$inputFolder""/""$networkFolderName"
	outNetworkFolder="$outputFolder""/""$networkFolderName"

	for propMisplFolderName in `ls "$inNetworkFolder" | sort -V` ; do
		inPropMisplFolder="$inNetworkFolder""/""$propMisplFolderName"
		outPropMisplFolder="$outNetworkFolder""/""$propMisplFolderName"

		for propNegFolderName in `ls "$inPropMisplFolder" | sort -V` ; do
			inPropNegFolder="$inPropMisplFolder""/""$propNegFolderName"
			outPropNegFolder="$outPropMisplFolder""/""$propNegFolderName"

            #echo "!!!!!!!"
			for i in `seq 1 30`; do
				inIterNoFolder="$inPropNegFolder""/network=""$i""/ExCC/signed-unweighted"
                #echo "BEGIN -----------"
                #echo "$inIterNoFolder"
                #grep -A 3 "last CP" "$inIterNoFolder""/log.txt"
                #echo "END -----------"

                if [ -f "$inIterNoFolder""/strengthedModelAfterRootRelaxation.lp" ]; then

				    outIterNoFolder="$outPropNegFolder""/network=""$i""/ExCC/signed-unweighted"
                    echo "$inIterNoFolder"

                    echo "------"
                    
                    if [ ! -f "$outIterNoFolder""/strengthedModelAfterRootRelaxation.lp" ]; then
                        echo "$outIterNoFolder"
				        
      					# do nothing if $outIterNoFolder exists.
				        cp "$inIterNoFolder""/strengthedModelAfterRootRelaxation.lp" "$outIterNoFolder""/strengthedModelAfterRootRelaxation.lp"
                        #echo "$inIterNoFolder""/edit-dist-mtrx.csv"
                        #echo "$outIterNoFolder"
                    
                    fi

                fi
				
			done
		done
	done
done
