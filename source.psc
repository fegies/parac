
var a : String;

a = "test";

var b : Int = 0;
while ( b < 7 )
{
    b++;
    print(a);
    print( function(a : Int){ a + 1; } );
}
