#http://stackoverflow.com/questions/8802734/sorting-and-removing-duplicate-words-in-a-line

function normalize ()
{
    cut -d: -f2 <<< "$1" | xargs -n1 | sort -u | xargs
}

test_dir=tests
success=0
failure=0

# $1 is exec
function correction ()
{
    for f in ${test_dir}/*.expected ;
    do
	local expected=$(cat "$f")
	local output=$("$1" "$(basename $f .expected)")
	if normalize "${expected}" = normalize "${output}"
	then success=$((${success}+1))
	else failure=$((${failure}+1))
	fi
    done
}

correction "$1"

total=$((${success} + ${failure}))

echo "Success: ${success}/${total}"
echo "Failure: ${failure}/${total}"
