#!/usr/bin/env bash

# Synopsis:
# Run the representer on a solution.

# Arguments:
# $1: exercise slug
# $2: absolute path to solution folder
# $3: absolute path to output directory

# Output:
# Writes the test mapping to a mapping.json file in the passed-in output directory.
# The test mapping are formatted according to the specifications at https://github.com/exercism/docs/blob/main/building/tooling/representers/interface.md

# Example:
# ./bin/run.sh two-fer /absolute/path/to/two-fer/solution/folder/ /absolute/path/to/output/directory/

# If any required arguments is missing, print the usage and exit
if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ]; then
    echo "usage: ./bin/run.sh exercise-slug /absolute/path/to/two-fer/solution/folder/ /absolute/path/to/output/directory/"
    exit 1
fi

slug="$1"
input_dir="${2%/}"
output_dir="${3%/}"
representation_txt_file="${output_dir}/representation.txt"
representation_json_file="${output_dir}/representation.json"
mapping_file="${output_dir}/mapping.json"
representer_dir=src

# Create the output directory if it doesn't exist
mkdir -p "${output_dir}"

echo "${slug}: creating representation..."


# TODO(FAP): Load compiled files instead? For which platform do we have to do nativecomp?
emacs -batch -L "${representer_dir}" --init-directory="$representer_dir" -l "${representer_dir}/init.el" -l "${representer_dir}/representer.el" --eval "(exercism/represent \"${slug}\" \"${input_dir}\" \"${output_dir}\")"
# TODO(FAP): use jq to pretty print and order the json output instead of slow elisp?

echo "${slug}: done"
