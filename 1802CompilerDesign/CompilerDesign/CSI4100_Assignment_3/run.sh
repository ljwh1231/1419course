echo "==== start build ===="
echo ""
./gradlew jarNoScanner
echo ""
echo "build finished..."


echo ""
echo ""
echo "==== start test ===="
echo ""
for i in {1..42}
#for i in {65..65}
do
   java -jar build/libs/MiniC-AstGen.jar -u result/$i.mc.u MiniC/Parser/tst/base/AST_testcases/c$i.mc
   diff result/$i.mc.u MiniC/Parser/tst/base/AST_solutions_unparsed/c$i.mc.u > result/diff$i.txt
   echo "testcase $i has been tested"
done
find . -name "*.txt" -size -1 -delete
echo ""
echo "test finished..."

echo ""
echo ""
echo "====failed test list===="
echo ""
find result/ -name "diff*"
