
DIR=`pwd`
find -L $DIR/craq_test $DIR/src $DIR/include  -name "*.erl" -o -name "*.hrl" > $DIR/cscope_source.files
cscope -bq -i $DIR/cscope_source.files  -f cscope_source.out


#ctags -R *.erl *.hrl

FILE="$DIR/craq_test $DIR/src $DIR/include $DIR/trace_log"

for i in $FILE
do
	cp .vimrc $i
done

