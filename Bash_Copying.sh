#!/bin/bash

path_dst=$1


Sources=(
	Models/FHN/CU/CU.png

)


Names=(
	FHN_CU.png
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

