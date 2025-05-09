#!/bin/bash

path_dst=$1


Sources=(
	Models/FHN/CU/CU.png

	Models/Karma_Ramp/CU/CU.png
	Models/Karma_Step/CU/CU.png

	Models/Karma_Ramp/Nullclines/NullclineComparisons.png
	Models/Karma_Step/Nullclines/NullclineComparisons.png

	Models/MorrisLecar/CU/CU.png
	Models/MorrisLecar/Nullclines/NullclineComparisons.png
)


Names=(
	FHN_CU.png

	Karma_Ramp_CU.png
	Karma_Step_CU.png

	Karma_Ramp_Nullclines.png
	Karma_Step_Nullclines.png

	MorrisLecar_CU.png
	MorrisLecar_Nullclines.png
)





for i in "${!Sources[@]}"; do
    basename "${Sources[$i]}"
    f="${Names[$i]}"
    echo $filename
    file_dst="${path_dst}/${f}"

    echo $file_dst

    cp "${Sources[$i]}" "$file_dst"
    echo cp "${Sources[$i]}" "$file_dst"
done

