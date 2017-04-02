
var b : Int = 0;
while ( b < 7 )
{
    b++;
    print( (\a, b -> a + b) (b,b)  );
}
