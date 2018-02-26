#!/bin/bash

#Create directory with outputs
rm -r ./automatas/outputs
mkdir ./automatas/outputs

# echo "=======TESTS: -i parameter======="

# # Run tests with -i parameter
# for input_file in ./automatas/inputs/*.in; do
#     ./dka-2-mka -i $input_file >> ./automatas/outputs/${input_file##*/}".i"
# done

# # Compare outputs with expected outputs
# for input_file in ./automatas/inputs/*.in; do
#     diff $input_file ./automatas/outputs/${input_file##*/}".i" >> 
# done


echo "=======TESTS: -t parameter======="

# Run tests with -i parameter
for input_file in ./automatas/inputs/*.in; do

    fn_without_ext=$(echo ${input_file} | cut -d "/" -f 4 | cut -d "." -f 1)

    ./dka-2-mka -t $input_file >> ./automatas/outputs/${fn_without_ext}".out" 2> /dev/null
done

Compare outputs with expected outputs
for input_file in ./automatas/valid_outputs/*.out; do
    fn_without_ext=$(echo ${input_file} | cut -d "/" -f 4 | cut -d "." -f 1)
    
    echo ${fn_without_ext}
    diff $input_file ./automatas/outputs/${fn_without_ext}".out"
    echo ${?}
done
